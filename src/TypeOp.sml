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

fun ppWithShow show = Print.ppMap (Show.showName show o name) Name.pp;

val pp = ppWithShow Show.default;

val toString = Print.toString pp;

end

structure TypeOpOrdered =
struct type t = TypeOp.typeOp val compare = TypeOp.compare end

structure TypeOpMap = KeyMap (TypeOpOrdered)

structure TypeOpSet = ElementSet (TypeOpMap)
