(* ========================================================================= *)
(* SUBSTITUTIONS FOR HIGHER ORDER LOGIC TYPES                                *)
(* Copyright (c) 2004-2006 Joe Hurd, distributed under the GNU GPL version 2 *)
(* ========================================================================= *)

structure TypeSubst :> TypeSubst =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* Type substitutions.                                                       *)
(* ------------------------------------------------------------------------- *)

datatype subst = TypeSubst of Type.ty NameMap.map;

val empty = TypeSubst (NameMap.new ());

fun null (TypeSubst m) = NameMap.null m;

fun add n_ty (TypeSubst sub) = TypeSubst (NameMap.insert sub n_ty);

fun peek (TypeSubst sub) n = NameMap.peek sub n;

fun toList (TypeSubst sub) = NameMap.toList sub;

(* ------------------------------------------------------------------------- *)
(* Normalization removes identity substitutions v |-> v.                     *)
(* ------------------------------------------------------------------------- *)

fun norm (TypeSubst sub) =
    let
      fun f (n,ty,z) = if Type.equalVar n ty then z else NameMap.insert z (n,ty)

      val sub = NameMap.foldl f (NameMap.new ()) sub
    in
      TypeSubst sub
    end;

(* ------------------------------------------------------------------------- *)
(* Applying substitutions: returns NONE for unchanged.                       *)
(* ------------------------------------------------------------------------- *)

datatype sharingSubst =
    SharingSubst of
      {sub : subst,
       seen : Type.ty option IntMap.map};

fun newSharingSubst sub =
    let
      val sub = norm sub
      val seen = IntMap.new ()
    in
      SharingSubst
        {sub = sub,
         seen = seen}
    end;

fun rawSharingSubst sub =
    let
      fun rawSubst ty seen =
          let
            val i = Type.id ty
          in
            case IntMap.peek seen i of
              SOME ty' => (ty',seen)
            | NONE =>
              case Type.dest ty of
                Type.TypeVar n =>
                let
                  val TypeSubst m = sub
                  val ty' = NameMap.peek m n
                  val seen = IntMap.insert seen (i,ty')
                in
                  (ty',seen)
                end
              | Type.TypeOp (n,tys) =>
                let
                  val (tys',seen) = rawSubstList tys seen
                  val ty' =
                      case tys' of
                        SOME tys => SOME (Type.mkOp (n,tys))
                      | NONE => NONE
                  val seen = IntMap.insert seen (i,ty')
                in
                  (ty',seen)
                end
          end

      and rawSubstList [] seen = (NONE,seen)
        | rawSubstList (ty :: tys) seen =
          let
            val (ty',seen) = rawSubst ty seen
            val (tys',seen) = rawSubstList tys seen
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
      rawSubst
    end;

fun sharingSubst ty share =
    let
      val SharingSubst {sub,seen} = share
    in
      if null sub then (NONE,share)
      else
        let
          val (ty',seen) = rawSharingSubst sub ty seen
          val share = SharingSubst {sub = sub, seen = seen}
        in
          (ty',share)
        end
    end;

fun subst sub =
    let
      val share = newSharingSubst sub
    in
      fn ty =>
         let
           val (ty',_) = sharingSubst ty share
         in
           ty'
         end
    end;

(* ------------------------------------------------------------------------- *)
(* Matching.                                                                 *)
(* ------------------------------------------------------------------------- *)

local
  fun rawMatch sub [] = sub
    | rawMatch sub ((ty1,ty2) :: rest) =
      case Type.dest ty1 of
        Type.TypeVar n1 =>
        (case peek sub n1 of
           NONE => add (n1,ty2) sub
         | SOME ty2' =>
           if Type.equal ty2 ty2' then rawMatch sub rest
           else raise Error "incompatible variable substitutions")
      | Type.TypeOp (n1,l1) =>
        let
          val (n2,l2) = Type.destOp ty2
        in
          if Name.equal n1 n2 then rawMatch sub (zip l1 l2 @ rest)
          else raise Error "different type operators"
        end;
in
  fun matchList' sub tyl =
      rawMatch sub tyl
      handle Error err => raise Error ("TypeSubst.matchList': " ^ err);

  fun matchList tyl =
      rawMatch empty tyl
      handle Error err => raise Error ("TypeSubst.matchList: " ^ err);

  fun match' sub ty1 ty2 =
      rawMatch sub [(ty1,ty2)]
      handle Error err => raise Error ("TypeSubst.match': " ^ err);

  fun match ty1 ty2 =
      rawMatch empty [(ty1,ty2)]
      handle Error err => raise Error ("TypeSubst.match: " ^ err);
end;

end
