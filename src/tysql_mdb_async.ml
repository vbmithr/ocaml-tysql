(*---------------------------------------------------------------------------
   Copyright (c) 2019 Vincent Bernardoff. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Core
open Async

module Wait = struct
  module S = Mariadb.Nonblocking.Status
  module IO = struct
    type 'a future = 'a Deferred.t
    let (>>=) = (>>=)
    let return = Deferred.return
  end

  let is_ready = function
    | `Ready -> true
    | `Bad_fd | `Closed -> false

  let ready (rt, wt, tt) =
    let r = ref false in
    let w = ref false in
    let t = ref false in
    let rc = Deferred.choice rt (fun x -> r := is_ready x) in
    let wc = Deferred.choice wt (fun x -> w := is_ready x) in
    let tc = Deferred.choice tt (fun _ -> t := true) in
    Deferred.enabled [rc; wc; tc] >>= fun f ->
    ignore (f ());
    Deferred.return (!r, !w, !t)

  let wait mariadb status =
    let fd =
      Fd.create
        (Fd.Kind.Socket `Active)
        (Mariadb.Nonblocking.fd mariadb)
        (Info.of_string "<mariadb fd>") in
    assert (S.read status || S.write status || S.timeout status);
    let idle = Deferred.never () in
    let rt = if S.read status then Fd.ready_to fd `Read else idle in
    let wt = if S.write status then Fd.ready_to fd `Write else idle in
    let tt =
      let tmout = float (Mariadb.Nonblocking.timeout mariadb) in
      if S.timeout status then Clock.after (Time.Span.of_sec tmout)
      else idle in
    ready (rt, wt, tt) >>= fun (read, write, timeout) ->
    Fd.close ~file_descriptor_handling:Fd.Do_not_close_file_descriptor fd
    >>= fun () ->
    Deferred.return @@ S.create ~read ~write ~timeout ()
end

module Mdb = Mariadb.Nonblocking.Make(Wait)
include Mdb

let time_of_ptime t =
  Mdb.Time.utc_timestamp (Ptime.to_float_s t)

let ptime_of_time_opt t =
  let y = Mdb.Time.year t in
  let m = Mdb.Time.month t in
  let d = Mdb.Time.day t in
  let hh = Mdb.Time.hour t in
  let mm = Mdb.Time.minute t in
  let ss = Mdb.Time.second t in
  let ms = Mdb.Time.microsecond t in
  let open Option.Monad_infix in
  Ptime.of_date_time ((y, m, d), ((hh, mm, ss), 0)) >>= fun t ->
  Ptime.Span.of_d_ps
    (0, Int64.(of_int ms * 1_000_000_000L)) >>| fun span ->
  match Ptime.add_span t span with
  | None -> failwith "ptime_of_time"
  | Some t -> t

let ptime_of_time_exn t =
  match ptime_of_time_opt t with
  | None -> invalid_arg "ptime_of_time_exn"
  | Some t -> t

open Tysql

let or_failwith = function
  | Error (_, s) -> failwith s
  | Ok v -> v

let connect_url ?socket ?flags url =
  let host = Uri.host url in
  let port = Uri.port url in
  let db = Filename.basename (Uri.path url) in
  let user = Uri.user url in
  let pass = Uri.password url in
  Mdb.connect ?host ?user ?pass ?port ~db ?socket ?flags ()

module Mdb_repr = struct
  type t = Field.value

  let view = function
    | `Bytes b -> `Blob (Bytes.to_string b)
    | `String s -> `Text s
    | `Float f -> `Float f
    | `Int i -> `Int i
    | `Null -> `Null
    | `Time t -> `Time (ptime_of_time_exn t)

  let repr : sqldata -> t = function
    | `Blob b -> `Bytes (Bytes.of_string b)
    | `Text s -> `String s
    | `Float f -> `Float f
    | `Int i -> `Int i
    | `Null -> `Null
    | `Time t -> `Time (time_of_ptime t)
end

module Mdb_encoding = Make(Mdb_repr)
include Mdb_encoding

type ('k, 'e, 'w, 'o) prepared = {
  stmt : ('k, 'e, 'w, 'o) stmt ;
  prepared : Stmt.t ;
}

let prepare ~table mdb stmt =
  let stmt_str = Format.asprintf "%a" (pp_stmt ~table) stmt in
  prepare mdb stmt_str  >>= function
  | Error (_, msg) -> failwith (stmt_str ^ ": " ^ msg)
  | Ok prepared -> return { stmt ; prepared }

let insert { stmt ; prepared } a =
  Stmt.execute prepared
    (Array.of_list (construct (encoding stmt) a)) >>|
  Result.map ~f:ignore

let select { stmt ; prepared } =
  let get encoding res () =
    Res.fetch (module Row.Array) res >>|
    Result.map ~f:begin function
      | None -> None
      | Some a ->
        let a = List.map ~f:Field.value (Array.to_list a) in
        match destruct (encoding stmt) a with
        | None -> invalid_arg "decoding error"
        | Some v -> Some v
    end in
  begin
    match where stmt, order_by stmt with
    | Some w, Some (g, _) ->
      let params =
        List.map ~f:Mdb_repr.repr (construct_expr w @ construct_expr g) in
      Stmt.execute prepared (Array.of_list params)
    | Some w, _ ->
      let params = List.map ~f:Mdb_repr.repr (construct_expr w) in
      Stmt.execute prepared (Array.of_list params)
    | _, Some (g, _) ->
      let params = List.map ~f:Mdb_repr.repr (construct_expr g) in
      Stmt.execute prepared (Array.of_list params)
    | None, None -> Stmt.execute prepared [||]
  end >>| Result.map ~f:(get encoding)

let select_exn stmt =
  select stmt >>| or_failwith

let fold stmt ~init ~f =
  select stmt >>= function
  | Error e -> return (Error e)
  | Ok next ->
    let rec inner a =
      next () >>= function
      | Error e -> return (Error e)
      | Ok None -> return (Ok a)
      | Ok (Some v) -> f a v >>= inner
    in
    inner init

let fold_exn stmt ~init ~f =
  fold stmt ~init ~f >>| or_failwith

(*---------------------------------------------------------------------------
   Copyright (c) 2019 Vincent Bernardoff

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
