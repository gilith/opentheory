(* ========================================================================= *)
(* SUBSTITUTIONS FOR HIGHER ORDER LOGIC TYPES                                *)
(* Copyright (c) 2004 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

structure TypeSubst :> TypeSubst =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* Type substitution maps.                                                   *)
(* ------------------------------------------------------------------------- *)

type substMap = Type.ty NameMap.map;

val emptyMap : substMap = NameMap.new ();

val nullMap : substMap -> bool = NameMap.null;

val peekMap : substMap -> Name.name -> Type.ty option = NameMap.peek;

val insertMap : substMap -> Name.name * Type.ty -> substMap = NameMap.insert;

val fromListMap : (Name.name * Type.ty) list -> substMap = NameMap.fromList;

val normMap =
    let
      fun pred (n,ty) = not (Type.equalVar n ty)
    in
      NameMap.filter pred
    end;

(* ------------------------------------------------------------------------- *)
(* Type substitutions.                                                       *)
(* ------------------------------------------------------------------------- *)

datatype subst =
    Subst of
      {subMap : substMap,
       seen : Type.ty option IntMap.map};

fun mk subMap =
    let
      val subMap = normMap subMap
      val seen = IntMap.new ()
    in
      Subst
        {subMap = subMap,
         seen = seen}
    end;

val empty = mk emptyMap;

fun null (Subst {subMap,...}) = nullMap subMap;

(* ------------------------------------------------------------------------- *)
(* Applying substitutions: returns NONE for unchanged.                       *)
(* ------------------------------------------------------------------------- *)

fun substMap subMap =
    let
      fun subst ty seen =
          let
            val i = Type.id ty
          in
            case IntMap.peek seen i of
              SOME ty' => (ty',seen)
            | NONE =>
              let
                val (ty',seen) =
                    case Type.dest ty of
                      TypeTerm.VarTy' n => (peekMap subMap n, seen)
                    | TypeTerm.OpTy' (ot,tys) =>
                      let
                        val (tys',seen) = substList tys seen

                        val ty' =
                            case tys' of
                              SOME tys => SOME (Type.mkOp (ot,tys))
                            | NONE => NONE
                      in
                        (ty',seen)
                      end

                val seen = IntMap.insert seen (i,ty')
              in
                (ty',seen)
              end
          end

      and substList tys seen =
          case tys of
            [] => (NONE,seen)
          | ty :: tys =>
            let
              val (ty',seen) = subst ty seen
              val (tys',seen) = substList tys seen
              val result =
                  case tys' of
                    SOME tys => SOME (Option.getOpt (ty',ty) :: tys)
                  | NONE =>
                    case ty' of
                      NONE => NONE
                    | SOME ty => SOME (ty :: tys)
            in
              (result,seen)
            end
    in
      if nullMap subMap then (fn _ => fn seen => (NONE,seen)) else subst
    end;

fun sharingSubst ty sub =
    let
      val Subst {subMap,seen} = sub
      val (ty',seen) = substMap subMap ty seen
      val sub = Subst {subMap = subMap, seen = seen}
    in
      (ty',sub)
    end;

fun subst sub ty =
    let
      val Subst {subMap,seen} = sub
      val (ty',_) = substMap subMap ty seen
    in
      ty'
    end;

(* ------------------------------------------------------------------------- *)
(* Matching.                                                                 *)
(* ------------------------------------------------------------------------- *)

local
  fun rawMatch subMap tys =
      case tys of
        [] => subMap
      | (ty1,ty2) :: rest =>
        case Type.dest ty1 of
          TypeTerm.VarTy' n1 =>
          (case peekMap subMap n1 of
             NONE => insertMap subMap (n1,ty2)
           | SOME ty2' =>
             if Type.equal ty2 ty2' then rawMatch subMap rest
             else raise Error "TypeSubst.rawMatch: incompatible var substs")
        | TypeTerm.OpTy' (o1,l1) =>
          let
            val (o2,l2) = Type.destOp ty2
          in
            if TypeOp.equal o1 o2 then rawMatch subMap (zip l1 l2 @ rest)
            else raise Error "TypeSubst.rawMatch: different type operators"
          end;
in
  fun matchList' subMap tyl = rawMatch subMap tyl;

  fun matchList tyl = mk (rawMatch emptyMap tyl);

  fun match' subMap ty1 ty2 = rawMatch subMap [(ty1,ty2)];

  fun match ty1 ty2 = mk (rawMatch emptyMap [(ty1,ty2)]);
end;

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val ppMap =
    Print.ppMap NameMap.toList (Print.ppList (Print.ppPair Name.pp Type.pp));

val toStringMap = Print.toString ppMap;

fun pp (Subst {subMap,...}) = ppMap subMap;

val toString = Print.toString pp;

end
