(*---------------------------------------------------------------------------
   Copyright (c) 2019 Vincent Bernardoff. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

type _ typ =
  | Blob : string typ
  | Text : string typ
  | Float : float typ
  | Int : int typ
  | Time : Ptime.t typ

let blob = Blob
let text = Text
let float = Float
let int = Int
let time = Time

type _ encoding =
  | Tup : 'a column -> 'a encoding
  | Tups : 'a encoding * 'b encoding -> ('a * 'b) encoding
  | Conv : ('a -> 'b) * ('b -> 'a) * 'b encoding -> 'a encoding

and _ column =
  | Req : string * 'a typ -> 'a column
  | Opt : string * 'a typ -> 'a option column

let req name v = Req (name, v)
let opt name v = Opt (name, v)

let conv of_t to_t a = Conv (of_t, to_t, a)
let tup1 a = Tup a
let tup2 a b = Tups (Tup a, Tup b)
let tup3 a b c =
  conv
    (fun (a, b, c) -> (a, (b, c)))
    (fun (a, (b, c)) -> (a, b, c))
    (Tups (Tup a, Tups (Tup b, Tup c)))

let tup4 a b c d =
  conv
    (fun (a, b, c, d) -> ((a, b), (c, d)))
    (fun ((a, b), (c, d)) -> (a, b, c, d))
    (Tups (Tups (Tup a, Tup b), Tups (Tup c, Tup d)))

let merge_tups t1 t2 = Tups (t1, t2)

type sqldata = [
  | `Null
  | `Blob of string
  | `Text of string
  | `Float of float
  | `Int of int
  | `Time of Ptime.t
]

let construct_field (type a) (c : a column) (v : a) : sqldata =
  match c, v with
  | Req (_, Blob), b -> `Blob b
  | Req (_, Text), s -> `Text s
  | Req (_, Float), f -> `Float f
  | Req (_, Int), i -> `Int i
  | Req (_, Time), t -> `Time t
  | Opt (_, Blob), Some b -> `Blob b
  | Opt (_, Text), Some s -> `Text s
  | Opt (_, Float), Some f -> `Float f
  | Opt (_, Int), Some i -> `Int i
  | Opt (_, Time), Some t -> `Time t
  | _ -> `Null

let rec construct :
  type a. a encoding -> a -> sqldata list = fun encoding v ->
  match encoding, v with
  | Tup a, x -> [construct_field a x]
  | Tups (a, b), (x, y) -> construct a x @ construct b y
  | Conv (of_t, _, enc), _ -> construct enc (of_t v)

let destruct_column (type a) (column : a column) (field : sqldata) : a option =
  match column, field with
  | Req (_, Blob), `Blob s   -> Some s
  | Req (_, Text), `Text s   -> Some s
  | Req (_, Float), `Float f -> Some f
  | Req (_, Int), `Int i     -> Some i
  | Req (_, Time), `Time t   -> Some t
  | Opt _, `Null             -> Some None
  | Opt (_, Blob), `Blob s   -> Some (Some s)
  | Opt (_, Text), `Text s   -> Some (Some s)
  | Opt (_, Float), `Float f -> Some (Some f)
  | Opt (_, Int), `Int i     -> Some (Some i)
  | Opt (_, Time), `Time t   -> Some (Some t)
  | _ -> None

let rec destruct :
  type a. a encoding -> sqldata list -> int -> (a * int) option =
  fun encoding arr i ->
  match encoding with
  | Tup a -> begin
      match destruct_column a (List.nth arr i) with
      | None -> None
      | Some a -> Some (a, succ i)
    end
  | Tups (a, b) -> begin
      match destruct a arr i with
      | None -> None
      | Some (a, j) ->
        match destruct b arr j with
        | None -> None
        | Some (b, k) -> Some ((a, b), k)
    end
  | Conv (_, to_t, enc) ->
    match destruct enc arr i with
    | None -> None
    | Some (a, i) -> Some (to_t a, i)

let destruct encoding a =
  match destruct encoding a 0 with
  | None -> None
  | Some (a, _) -> Some a

module type REPR = sig
  type t

  val view : t -> sqldata
  val repr : sqldata -> t
end

module Make (R: REPR) = struct
  let construct e a = List.map R.repr (construct e a)
  let destruct e v = destruct e (List.map R.view v)
end

type _ expr =
  | Bind_param : unit expr
  | Column : 'a column -> 'a column expr
  | String : string -> string expr
  | True : bool expr
  | False : bool expr
  | Eq : 'a expr * 'b expr -> ('a * 'b) expr
  | Lt : 'a expr * 'b expr -> ('a * 'b) expr
  | Gt : 'a expr * 'b expr -> ('a * 'b) expr
  | Leq : 'a expr * 'b expr -> ('a * 'b) expr
  | Geq : 'a expr * 'b expr -> ('a * 'b) expr
  | Like : 'a expr * 'b expr -> ('a * 'b) expr
  | And : 'a expr * 'b expr -> ('a * 'b) expr
  | Or : 'a expr * 'b expr -> ('a * 'b) expr

let rec pp_expr : type a. Format.formatter -> a expr -> unit = fun ppf -> function
  | Bind_param -> Format.pp_print_char ppf '?'
  | String _ -> Format.pp_print_char ppf '?'
  | Column (Req (n, _)) -> Format.pp_print_string ppf n
  | Column (Opt (n, _)) -> Format.pp_print_string ppf n
  | True -> Format.pp_print_string ppf "true"
  | False -> Format.pp_print_string ppf "false"
  | Eq (a, b)  -> Format.fprintf ppf "(%a = %a)" pp_expr a pp_expr b
  | Lt (a, b) -> Format.fprintf ppf "(%a <= %a)" pp_expr a pp_expr b
  | Gt (a, b) -> Format.fprintf ppf "(%a >= %a)" pp_expr a pp_expr b
  | Leq (a, b) -> Format.fprintf ppf "(%a <= %a)" pp_expr a pp_expr b
  | Geq (a, b) -> Format.fprintf ppf "(%a >= %a)" pp_expr a pp_expr b
  | Like (a, b) -> Format.fprintf ppf "(%a like %a)" pp_expr a pp_expr b
  | And (a, b) -> Format.fprintf ppf "(%a and %a)" pp_expr a pp_expr b
  | Or (a, b) -> Format.fprintf ppf "(%a or %a)" pp_expr a pp_expr b

let eq a b = Eq (a, b)
let lt a b = Lt (a, b)
let gt a b = Gt (a, b)
let leq a b = Leq (a, b)
let geq a b = Geq (a, b)
let like a b = Like (a, b)
let union a b = Or (a, b)
let inter a b = And (a, b)
let true_expr = True
let false_expr = False
let string_expr s = String s
let column_expr c = Column c
let bind_param_expr = Bind_param

let rec construct_expr : type a. a expr -> sqldata list = function
  | Bind_param -> [ `Text "?" ]
  | Column _ -> []
  | String s -> [ `Text s ]
  | True -> []
  | False -> []
  | Eq (a, b) -> construct_pair a b
  | Lt (a, b) -> construct_pair a b
  | Gt (a, b) -> construct_pair a b
  | Leq (a, b) -> construct_pair a b
  | Geq (a, b) -> construct_pair a b
  | Like (a, b) -> construct_pair a b
  | And (a, b) -> construct_pair a b
  | Or (a, b) -> construct_pair a b

and construct_pair :
  type a b. a expr -> b expr -> sqldata list = fun c1 c2 ->
  let a = construct_expr c1 in
  let b = construct_expr c2 in
  a @ b

type insert
type select

type ('kind, 'e, 'w, 'o) stmt =
  | Insert : 'e encoding -> (insert, 'e, unit, unit) stmt
  | Select :
      'e encoding * 'w expr option * ('o expr * [`Asc|`Desc]) option ->
      (select, 'e, 'w, 'o) stmt

let encoding :
  type a b c d. (a, b, c, d) stmt -> b encoding = function
  | Insert e -> e
  | Select (e, _, _) -> e

let where (Select (_, w, _)) = w
let order_by (Select (_, _, o)) = o

let insert_stmt e = Insert e
let select_stmt ?where ?order_by e = Select (e, where, order_by)

let rec names : type a. a encoding -> string list = function
  | Tup (Req (n, _)) -> [n]
  | Tup (Opt (n, _)) -> [n]
  | Tups (a, b) -> names a @ names b
  | Conv (_, _, e) -> names e

let pp_sql_encoding mode ppf e =
  let names = match mode with
    | `Insert -> List.map (fun _ -> "?") (names e)
    | `Select -> names e in
  let open Format in
  pp_print_list
    ~pp_sep:(fun ppf () -> pp_print_string ppf ", ")
    pp_print_string ppf names

let pp_insert ppf e = pp_sql_encoding `Insert ppf e
let pp_select ppf e = pp_sql_encoding `Select ppf e

let pp_direction ppf = function
  | `Asc -> Format.pp_print_string ppf "asc"
  | `Desc -> Format.pp_print_string ppf "desc"

let pp_where ppf = function
  | None -> Format.pp_print_string ppf ""
  | Some e -> Format.fprintf ppf "where %a" pp_expr e

let pp_order_by ppf = function
  | None -> Format.pp_print_string ppf ""
  | Some (e, direction) ->
    Format.fprintf ppf "order by %a %a" pp_expr e pp_direction direction

let pp_stmt :
  type k e w o. table:string -> Format.formatter -> (k, e, w, o) stmt -> unit =
  fun ~table ppf stmt ->
  match stmt with
  | Insert e ->
    Format.fprintf ppf "insert into %s (%a) values (%a)"
      table pp_select e pp_insert e
  | Select (e, w, o) ->
    Format.fprintf ppf "select %a from %s %a %a"
      pp_select e table pp_where w pp_order_by o

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
