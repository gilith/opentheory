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

fun renameObjectData rename =
    let
      fun renameData d rewr =
          case d of
            ObjectData.Num _ => (NONE,rewr)
          | ObjectData.Name _ => (NONE,rewr)
          | ObjectData.TypeOp t =>
            let
(*OpenTheoryDebug
              val () =
                  if TypeOp.isUndef t then ()
                  else raise Bug "ObjectRename.renameData.TypeOp: defined"
*)
              val t' = renameTypeOp rename t

              val d' = Option.map ObjectData.TypeOp t'
            in
              (d',rewr)
            end
          | ObjectData.Type ty =>
            let
              val (ty',rewr) = TermRewrite.sharingRewriteType ty rewr

              val d' = Option.map ObjectData.Type ty'
            in
              (d',rewr)
            end
          | ObjectData.Const c =>
            let
(*OpenTheoryDebug
              val () =
                  if Const.isUndef c then ()
                  else raise Bug "ObjectRename.renameData.Const: defined"
*)
              val c' = renameConst rename c

              val d' = Option.map ObjectData.Const c'
            in
              (d',rewr)
            end
          | ObjectData.Var v =>
            let
              val (v',rewr) = TermRewrite.sharingRewriteVar v rewr

              val d' = Option.map ObjectData.Var v'
            in
              (d',rewr)
            end
          | ObjectData.Term tm =>
            let
              val (tm',rewr) = TermRewrite.sharingRewriteTerm tm rewr

              val d' = Option.map ObjectData.Term tm'
            in
              (d',rewr)
            end
          | ObjectData.Thm th =>
            let
              val Thm.Thm {sequent = seq, axioms} = Thm.dest th
(*OpenTheoryDebug
              val () =
                  if SequentSet.equal axioms (SequentSet.singleton seq) then ()
                  else raise Bug "ObjectRename.renameData.Thm: not an axiom"
*)
              val (seq',rewr) = Sequent.sharingRewrite seq rewr

              val d' = Option.map (ObjectData.Thm o Thm.axiom) seq'
            in
              (d',rewr)
            end
          | ObjectData.List l =>
            let
              val (l',rewr) = TermRewrite.sharingRewriteList renameData l rewr

              val d' = Option.map ObjectData.List l'
            in
              (d',rewr)
            end
    in
      renameData
    end;

fun renameCommand rename gen cmd args =
    case cmd of
      Command.DefineConst =>
      let
        val c =
            case gen of
              [ObjectData.Const x, ObjectData.Thm _] => x
            | _ => raise Bug "ObjectRename.renameCommand.DefineConst.gen"
      in
        case rename (Symbol.Const c) of
          NONE => NONE
        | SOME n =>
          let
            val arg1 =
                case args of
                  [_,a1] => a1
                | _ => raise Bug "ObjectRename.renameCommand.DefineConst.args"

            val arg0 = Object.mkName n
          in
            SOME [arg0,arg1]
          end
      end
    | Command.DefineConstList =>
      let
        fun renameConstList dl =
            case dl of
              [] => []
            | d :: dl =>
              let
                val c =
                    case d of
                      ObjectData.Const c => c
                    | _ => raise Bug "ObjectRename.renameCommand.DefineConstList.renameConstList"
              in
                case (rename (Symbol.Const c), renameConstList dl) of
                  (NONE,[]) => []
                | (n,nl) => n :: nl
              end

        val cl =
            case gen of
              [ObjectData.List l, ObjectData.Thm _] => l
            | _ => raise Bug "ObjectRename.renameCommand.DefineConstList.gen"

        val nl = renameConstList cl
      in
        if List.null nl then NONE
        else
          let
            fun renameArg l obj =
                case l of
                  [] => obj
                | no :: l =>
                  let
                    val (objNV,obj) =
                        case Object.unMkCons obj of
                          NONE => raise Bug "ObjectRename.renameCommand.DefineConstList.mkListArg.obj"
                        | SOME x => x

                    val obj = renameArg l obj

                    val objNV =
                        case no of
                          NONE => objNV
                        | SOME n =>
                          case Object.unMkCons objNV of
                            NONE => raise Bug "ObjectRename.renameCommand.DefineConstList.mkListArg.objNV"
                          | SOME (_,objV) =>
                            let
                              val objN = Object.mkName n
                            in
                              Object.mkCons {savable = true} objN objV
                            end
                  in
                    Object.mkCons {savable = true} objNV obj
                  end

            val (arg0,arg1) =
                case args of
                  [a0,a1] => (a0,a1)
                | _ => raise Bug "ObjectRename.renameCommand.DefineConstList.args"

            val arg0 = renameArg nl arg0
          in
            SOME [arg0,arg1]
          end
      end
    | Command.DefineTypeOp =>
      let
        val (t,a,r) =
            case gen of
              [ObjectData.TypeOp t, ObjectData.Const a, ObjectData.Const r,
               ObjectData.Thm _, ObjectData.Thm _] => (t,a,r)
            | _ => raise Bug "ObjectRename.renameCommand.DefineTypeOp.gen"

        val nt = rename (Symbol.TypeOp t)
        and na = rename (Symbol.Const a)
        and nr = rename (Symbol.Const r)
      in
        case (nt,na,nr) of
          (NONE,NONE,NONE) => NONE
        | _ =>
          let
            val (arg0,arg1,arg2,arg3,arg4) =
                case args of
                  [a0,a1,a2,a3,a4] => (a0,a1,a2,a3,a4)
                | _ => raise Bug "ObjectRename.renameCommand.DefineTypeOp.args"

            val arg0 = case nt of SOME n => Object.mkName n | NONE => arg0
            and arg1 = case na of SOME n => Object.mkName n | NONE => arg1
            and arg2 = case nr of SOME n => Object.mkName n | NONE => arg2
          in
            SOME [arg0,arg1,arg2,arg3,arg4]
          end
      end
    | Command.DefineTypeOpLegacy =>
      let
        val (t,a,r) =
            case gen of
              [ObjectData.TypeOp t, ObjectData.Const a, ObjectData.Const r,
               ObjectData.Thm _, ObjectData.Thm _] => (t,a,r)
            | _ => raise Bug "ObjectRename.renameCommand.DefineTypeOpLegacy.gen"

        val nt = rename (Symbol.TypeOp t)
        and na = rename (Symbol.Const a)
        and nr = rename (Symbol.Const r)
      in
        case (nt,na,nr) of
          (NONE,NONE,NONE) => NONE
        | _ =>
          let
            val (arg0,arg1,arg2,arg3,arg4) =
                case args of
                  [a0,a1,a2,a3,a4] => (a0,a1,a2,a3,a4)
                | _ => raise Bug "ObjectRename.renameCommand.DefineTypeOpLegacy.args"

            val arg0 = case nt of SOME n => Object.mkName n | NONE => arg0
            and arg1 = case na of SOME n => Object.mkName n | NONE => arg1
            and arg2 = case nr of SOME n => Object.mkName n | NONE => arg2
          in
            SOME [arg0,arg1,arg2,arg3,arg4]
          end
      end
(*OpenTheoryDebug
    | Command.Const => raise Bug "ObjectRename.renameCommand.Const"
    | Command.TypeOp => raise Bug "ObjectRename.renameCommand.TypeOp"
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

              val (cmd,args,res) =
                  case Object.provenance obj1 of
                    Object.Default =>
                    raise Bug "ObjectRename.renameObj.renameCmd: Default obj1"
                  | Object.Special {command,arguments,result,...} =>
                    (command,arguments,result)
            in
              case renameCommand rename gen cmd args of
                NONE => NONE
              | SOME args =>
                let
                  val objs = Object.mkCommand {savable = true} cmd args
(*OpenTheoryDebug
                  val () =
                      if res < length objs then ()
                      else raise Bug "ObjectRename.renameObj.renameCmd: |objs|"
*)
                in
                  SOME (List.nth (objs,res))
                end
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
                      val (data',tmRewr) = renameObjectData rename data tmRewr

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
  fun sharingRename obj ren =
      let
        val Rename {tmRewr,rename,seen} = ren

        val (obj',(tmRewr,seen)) = renameObj rename obj (tmRewr,seen)

        val ren = Rename {tmRewr = tmRewr, rename = rename, seen = seen}
      in
        (obj',ren)
      end;
end;

fun rename rewr obj =
    let
      val (obj',_) = sharingRename obj rewr
    in
      obj'
    end;

end
