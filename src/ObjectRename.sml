(* ========================================================================= *)
(* RENAMING SYMBOLS IN OPENTHEORY OBJECTS                                    *)
(* Copyright (c) 2015 Joe Leslie-Hurd, distributed under the MIT license     *)
(* ========================================================================= *)

structure ObjectRename :> ObjectRename =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* Renaming symbols.                                                         *)
(* ------------------------------------------------------------------------- *)

fun renameTypeOp rename t =
    case rename (Symbol.TypeOp t) of
      NONE => NONE
    | SOME n =>
      let
(*OpenTheoryDebug
        val () =
            if TypeOp.isUndef t then ()
            else raise Bug "ObjectRename.renameTypeOp: defined type operator"
*)
      in
        SOME (TypeOp.mkUndef n)
      end;

fun renameType rename =
    let
      fun renameTy ty =
          case ty of
            TypeTerm.VarTy' _ => NONE
          | TypeTerm.OpTy' (t,tys) =>
            case renameTypeOp rename t of
              NONE => NONE
            | SOME t => SOME (Type.mkOp (t,tys))
    in
      TypeRewrite.new renameTy
    end;

fun renameConst rename c =
    case rename (Symbol.Const c) of
      NONE => NONE
    | SOME n =>
      let
(*OpenTheoryDebug
        val () =
            if Const.isUndef c then ()
            else raise Bug "ObjectRename.renameConst: defined constant"
*)
      in
        SOME (Const.mkUndef n)
      end;

fun renameTerm rename =
    let
      fun renameTm tm =
          case tm of
            TypeTerm.Var' _ => NONE
          | TypeTerm.App' _ => NONE
          | TypeTerm.Abs' _ => NONE
          | TypeTerm.Const' (c,ty) =>
            case renameConst rename c of
              NONE => NONE
            | SOME c => SOME (Term.mkConst (c,ty))
    in
      TermRewrite.new (renameType rename) renameTm
    end;

fun renameCommand rename gen cmd args =
    case cmd of
      Command.DefineConst =>
      let
        val c =
            case gen of
              [ObjectData.Const x, ObjectData.Thm _] => x
            | _ => raise Bug "ObjectRename.renameCommand.DefineConst: gen"
      in
        case rename (Symbol.Const c) of
          NONE => NONE
        | SOME n => raise Bug "not implemented"
      end
    | Command.DefineConstList => raise Bug "ObjectRename.renameCommand: not implemented"
    | Command.DefineTypeOp => raise Bug "ObjectRename.renameCommand: not implemented"
    | Command.DefineTypeOpLegacy => raise Bug "ObjectRename.renameCommand: not implemented"
(*OpenTheoryDebug
    | Command.Const => raise Bug "ObjectRename.renameCommand: Const"
    | Command.TypeOp => raise Bug "ObjectRename.renameCommand: TypeOp"
*)
    | _ => NONE;

(* ------------------------------------------------------------------------- *)
(* Object renaming: return NONE for unchanged.                               *)
(* ------------------------------------------------------------------------- *)

datatype rename =
    Rename of
      {tmRewr : TermRewrite.rewrite,
       rename : Symbol.symbol -> Name.name option,
       seen : Object.object option IntMap.map};

fun new rename =
    let
      val tmRewr = renameTerm rename
      and seen = IntMap.new ()
    in
      Rename
        {tmRewr = tmRewr,
         rename = rename,
         seen = seen}
    end;

val id = new (K NONE);

local
  fun renameObj rename =
      let
        fun renameCmd obj0 obj1 =
            let
              val gen =
                  case Object.provenance obj0 of
                    Object.Default =>
                    raise Bug "ObjectRename.renameObj.renameCmd: Default obj0"
                  | Object.Special {generated,...} => generated

              val (cmd,args) =
                  case Object.provenance obj1 of
                    Object.Default =>
                    raise Bug "ObjectRename.renameObj.renameCmdd: Default obj1"
                  | Object.Special {command,arguments,...} =>
                    (command,arguments)
            in
              renameCommand rename gen cmd args
            end

        fun preDescent obj (tmRewr,seen) =
            let
              val i = Object.id obj
            in
              case IntMap.peek seen i of
                SOME obj' =>
                {descend = false, result = (obj',(tmRewr,seen))}
              | NONE =>
                let
                  val Object.Object' {data,provenance} = Object.dest obj
                in
                  case provenance of
                    Object.Special _ =>
                    {descend = true, result = (NONE,(tmRewr,seen))}
                  | Object.Default =>
                    let
                      val (data',tmRewr) =
                          ObjectData.sharingRewrite data tmRewr

                      val obj' = Option.map Object.mkUnsavable data'

                      val seen = IntMap.insert seen (i,obj')
                    in
                      {descend = false, result = (obj',(tmRewr,seen))}
                    end
                end
            end

        fun postDescent obj0 obj1' (tmRewr,seen) =
            let
              val i = Object.id obj0

              val (unchanged,obj1) =
                  case obj1' of
                    NONE => (true,obj0)
                  | SOME obj => (false,obj)

              val (unchanged,obj2) =
                  case renameCmd obj0 obj1 of
                    NONE => (unchanged,obj1)
                  | SOME obj => (false,obj)

              val obj2' = if unchanged then NONE else SOME obj2

              val seen = IntMap.insert seen (i,obj2')
            in
              (obj2',(tmRewr,seen))
            end

        val savable = true
      in
        Object.maps
          {preDescent = preDescent,
           postDescent = postDescent,
           savable = savable}
      end;
in
  fun sharingRenameObject obj ren =
      let
        val Rename {tmRewr,rename,seen} = ren

        val (obj',(tmRewr,seen)) = renameObj rename obj (tmRewr,seen)

        val ren = Rename {tmRewr = tmRewr, rename = rename, seen = seen}
      in
        (obj',ren)
      end;
end;

fun renameObject rewr obj =
    let
      val (obj',_) = sharingRenameObject obj rewr
    in
      obj'
    end;

end
