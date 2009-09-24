(* ========================================================================= *)
(* HIGHER ORDER LOGIC CONSTANTS                                              *)
(* Copyright (c) 2009 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

structure Const :> Const =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* A type of constants.                                                      *)
(* ------------------------------------------------------------------------- *)

type const = TypeTerm.const;

type constData =
     {name : Name.name,
      prov : TypeTerm.provConst};

val mk = TypeTerm.Const;

fun dest (TypeTerm.Const data) = data;

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

val name = TypeTerm.nameConst;

val prov = TypeTerm.provConst;

fun mkUndef name =
    let
      val prov = TypeTerm.UndefProvConst
    in
      mk
        {name = name,
         prov = prov}
    end;

(* ------------------------------------------------------------------------- *)
(* A total order.                                                            *)
(* ------------------------------------------------------------------------- *)

val compare = TypeTerm.compareConst;

val equal = TypeTerm.equalConst;

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val pp = Print.ppMap name Name.pp;

val toString = Print.toString pp;

end

structure ConstOrdered =
struct type t = Const.const val compare = Const.compare end

structure ConstSet = ElementSet (ConstOrdered)

structure ConstMap = KeyMap (ConstOrdered)
