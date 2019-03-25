(*---------------------------------------------------------------------------
   Copyright (c) 2019 Vincent Bernardoff. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Async

module Wait : Mariadb.Nonblocking.Wait
  with type 'a IO.future = 'a Deferred.t

include module type of Mariadb.Nonblocking.Make(Wait)

open Tysql

val or_failwith : 'a result -> 'a

val connect_url :
  ?socket:string -> ?flags:flag list -> Uri.t -> t result Deferred.t
(** user:pass@host:port/db *)

type ('kind, 'e, 'where, 'order_by) prepared

val prepare :
  table:string -> t -> ('k, 'e, 'w, 'o) stmt ->
  ('k, 'e, 'w, 'o) prepared Deferred.t

val insert :
  (insert, 'e, unit, unit) prepared -> 'e -> unit result Deferred.t

val select :
  (select, 'e, _, _) prepared ->
  (unit -> 'e option result Deferred.t) result Deferred.t

val select_exn :
  (select, 'a, _, _) prepared ->
  (unit -> 'a option result Deferred.t) Deferred.t

val fold :
  (select, 'a, _, _) prepared ->
  init:'c ->
  f:('c -> 'a -> 'c Deferred.t) -> 'c result Deferred.t

val fold_exn :
  (select, 'a, _, _) prepared ->
  init:'c ->
  f:('c -> 'a -> 'c Deferred.t) -> 'c Deferred.t

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
