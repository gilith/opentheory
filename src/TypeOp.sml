(* ========================================================================= *)
(* HIGHER ORDER LOGIC TYPE OPERATORS                                         *)
(* Copyright (c) 2009 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

structure TypeOp :> TypeOp =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* A type of type operators.                                                 *)
(* ------------------------------------------------------------------------- *)

type typeOp = TypeTerm.opTy;

type typeOpData =
     {name : Name.name,
      prov : TypeTerm.provOpTy};

val mk = TypeTerm.OpTy;

fun dest (TypeTerm.OpTy data) = data;

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

val name = TypeTerm.nameOpTy;

val prov = TypeTerm.provOpTy;

fun mkUndef name =
    let
      val prov = TypeTerm.UndefProvOpTy
    in
      mk
        {name = name,
         prov = prov}
    end;

fun isUndef ot =
    case prov ot of
      TypeTerm.UndefProvOpTy => true
    | _ => false;

(* ------------------------------------------------------------------------- *)
(* A total order.                                                            *)
(* ------------------------------------------------------------------------- *)

val compare = TypeTerm.compareOpTy;

val equal = TypeTerm.equalOpTy;

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val pp = Print.ppMap name Name.pp;

val toString = Print.toString pp;

end

structure TypeOpOrdered =
struct type t = TypeOp.typeOp val compare = TypeOp.compare end

structure TypeOpSet = ElementSet (TypeOpOrdered)

structure TypeOpMap = KeyMap (TypeOpOrdered)
