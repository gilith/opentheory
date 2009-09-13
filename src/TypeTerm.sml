(* ========================================================================= *)
(* HIGHER ORDER LOGIC TYPES AND TERMS                                        *)
(* Copyright (c) 2009 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

structure TypeTerm :> TypeTerm =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* Type IDs.                                                                 *)
(* ------------------------------------------------------------------------- *)

type idTy = int;

(* ------------------------------------------------------------------------- *)
(* Term IDs.                                                                 *)
(* ------------------------------------------------------------------------- *)

type id = int;

(* ------------------------------------------------------------------------- *)
(* The mutually recursive datatype of higher order logic types and terms.    *)
(* ------------------------------------------------------------------------- *)

datatype ty =
    Ty of
      {id : idTy,
       ty : ty',
       sz : int}

and ty' =
    VarTy of Name.name
  | OpTy of Name.name * ty list * opTy

and opTy =
    UndefTy
  | DefTy of defOpTy

and defOpTy =
    DefOpTy of
      {pred : term,
       vars : Name.name list}

and var =
    VarV of Name.name * ty

and term =
    Term of
      {id : id,
       tm : term',
       sz : int,
       ty : ty}

and term' =
    Const of Name.name * ty * const
  | Var of var
  | Comb of term * term
  | Abs of var * term

and const =
    Undef
  | Def of defConst
  | AbsTy of defOpTy
  | RepTy of defOpTy

and defConst =
    DefConst of term;

end
