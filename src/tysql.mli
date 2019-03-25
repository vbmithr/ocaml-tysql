(*---------------------------------------------------------------------------
   Copyright (c) 2019 Vincent Bernardoff. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

type 'a typ

val blob : string typ
val text : string typ
val float  : float typ
val int    : int typ
val time   : Ptime.t typ

type 'a column

val req : string -> 'a typ -> 'a column
val opt : string -> 'a typ -> 'a option column

type 'a encoding

val tup1 : 'a column -> 'a encoding
val tup2 : 'a column -> 'b column -> ('a * 'b) encoding
val tup3 : 'a column -> 'b column -> 'c column -> ('a * 'b * 'c) encoding
val tup4 : 'a column -> 'b column -> 'c column -> 'd column -> ('a * 'b * 'c * 'd) encoding
val conv : ('a -> 'b) -> ('b -> 'a) -> 'b encoding -> 'a encoding
val merge_tups : 'a encoding -> 'b encoding -> ('a * 'b) encoding

type sqldata = [
  | `Null
  | `Blob of string
  | `Text of string
  | `Float of float
  | `Int of int
  | `Time of Ptime.t
]

val construct : 'a encoding -> 'a -> sqldata list
val destruct : 'a encoding -> sqldata list -> 'a option

module type REPR = sig
  type t

  val view : t -> sqldata
  val repr : sqldata -> t
end

module Make (R: REPR) : sig
  val construct : 'a encoding -> 'a -> R.t list
  val destruct : 'a encoding -> R.t list -> 'a option
end

type 'a expr
val eq : 'a expr -> 'b expr -> ('a * 'b) expr
val lt : 'a expr -> 'b expr -> ('a * 'b) expr
val gt : 'a expr -> 'b expr -> ('a * 'b) expr
val leq : 'a expr -> 'b expr -> ('a * 'b) expr
val geq : 'a expr -> 'b expr -> ('a * 'b) expr
val like : 'a expr -> 'b expr -> ('a * 'b) expr
val union : 'a expr -> 'b expr -> ('a * 'b) expr
val inter : 'a expr -> 'b expr -> ('a * 'b) expr
val true_expr : bool expr
val false_expr : bool expr
val string_expr : string -> string expr
val column_expr : 'a column -> 'a column expr
val bind_param_expr : unit expr

val construct_expr : 'a expr -> sqldata list

type insert
type select

type ('k, 'e, 'w, 'o) stmt

val pp_stmt :
  table:string -> Format.formatter -> (_, _, _, _) stmt -> unit

val encoding : (_, 'e, _, _) stmt -> 'e encoding
val where : (select, _, 'w, _) stmt -> 'w expr option
val order_by : (select, _, _, 'o) stmt -> ('o expr * [`Asc|`Desc]) option

val insert_stmt :
  'e encoding -> (insert, 'e, unit, unit) stmt

val select_stmt :
  ?where:'w expr ->
  ?order_by:('o expr * [`Asc | `Desc]) ->
  'e encoding -> (select, 'e, 'w, 'o) stmt

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
