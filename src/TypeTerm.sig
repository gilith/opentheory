(* ========================================================================= *)
(* HIGHER ORDER LOGIC TYPES AND TERMS                                        *)
(* Copyright (c) 2009 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

signature TypeTerm =
sig

(* ------------------------------------------------------------------------- *)
(* Type IDs.                                                                 *)
(* ------------------------------------------------------------------------- *)

type idTy = int

(* ------------------------------------------------------------------------- *)
(* Term IDs.                                                                 *)
(* ------------------------------------------------------------------------- *)

type id = int

(* ------------------------------------------------------------------------- *)
(* An abstract interface to the mutually recursive datatype of higher order  *)
(* logic types and terms.                                                    *)
(* ------------------------------------------------------------------------- *)

type ty

type term

datatype defOpTy =
    DefOpTy of
      {pred : term,
       vars : Name.name list}

datatype opTy =
    UndefTy
  | DefTy of defOpTy

datatype ty' =
    VarTy of Name.name
  | OpTy of Name.name * ty list * opTy

datatype var =
    VarV of Name.name * ty

datatype defConst =
    DefConst of term

datatype const =
    Undef
  | Def of defConst
  | AbsTy of defOpTy
  | RepTy of defOpTy

datatype term' =
    Const of Name.name * ty * const
  | Var of var
  | Comb of term * term
  | Abs of var * term

end
