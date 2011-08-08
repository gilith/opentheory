(* ========================================================================= *)
(* GENERATING HASKELL PROJECTS FROM THEORIES                                 *)
(* Copyright (c) 2011 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

structure Haskell :> Haskell =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* A prefix marking theories that can be exported as Haskell packages.       *)
(* ------------------------------------------------------------------------- *)

val prefix = PackageName.haskellExport;

(* ------------------------------------------------------------------------- *)
(* A type of Haskell packages.                                               *)
(* ------------------------------------------------------------------------- *)

datatype data =
    Data of
      {name : TypeOp.typeOp,
       parameters : Name.name list,
       constructors : (Const.const * Type.ty list) list};

datatype newtype =
    Newtype of
      {name : TypeOp.typeOp,
       predicate : Term.term,
       abs : Const.const,
       rep : Const.const};

datatype value =
    Value of
      {name : Const.const,
       ty : Type.ty,
       equations : (Term.term list * Term.term) list};

datatype source =
    DataSource of data
  | NewtypeSource of newtype
  | ValueSource of value;

datatype module =
    Module of
      {namespace : Namespace.namespace,
       source : source list,
       submodules : module list};

datatype haskell =
     Haskell of
       {package : Package.package,
        source : module};

(* ------------------------------------------------------------------------- *)
(* Converting theorems into Haskell declarations.                            *)
(* ------------------------------------------------------------------------- *)

local
  fun destIndData tm =
      let
        val (p,t0) = Term.destForall tm

        fun destP t =
            let
              val (p',x) = Term.destApp t

              val () =
                  if Term.equalVar p p' then ()
                  else raise Error "bad p"
            in
              x
            end

        val (t1,t2) = Term.destImp t0

        val ty =
            let
              val (x,px) = Term.destForall t2

              val x' = destP px

              val () =
                  if Term.equalVar x x' then ()
                  else raise Error "bad x"
            in
              Var.typeOf x
            end

        fun destCon t =
            let
              val (vs,t3) = Term.stripForall t

              val t4 =
                  case total Term.destImp t3 of
                    NONE => t3
                  | SOME (t5,t6) =>
                    let
                      val vs = VarSet.fromList vs

                      fun check t =
                          if VarSet.member (Term.destVar (destP t)) vs then ()
                          else raise Error "bad step assumption"

                      val () = List.app check (Term.stripConj t5)
                    in
                      t6
                    end

              val t5 = destP t4

              val (t6,ts) = Term.stripApp t5

              val (c,_) = Term.destConst t6
            in
              (c, List.map Term.typeOf ts)
            end

        val (name,parms) =
            let
              val (name,parms) = Type.destOp ty

              val vs = List.map Type.destVar parms

              val () =
                  if NameSet.size (NameSet.fromList vs) = length vs then ()
                  else raise Error "duplicate type vars"
            in
              (name,vs)
            end

        val ts = Term.stripConj t1
      in
        Data
          {name = name,
           parameters = parms,
           constructors = List.map destCon ts}
      end
      handle Error err =>
        raise Error ("bad induction conjunct: " ^ err);

  fun destRecData tm =
      let
      in
        raise Error "not implemented"
      end
      handle Error err =>
        raise Error ("bad recursion conjunct: " ^ err);

  fun destCaseData tm =
      let
      in
        raise Error "not implemented"
      end
      handle Error err =>
        raise Error ("bad case conjunct: " ^ err);
in
  fun destData th =
      let
        val Sequent.Sequent {hyp,concl} = Thm.sequent th

        val () =
            if TermAlphaSet.null hyp then ()
            else raise Error "hypotheses"

        val (indData,recCaseData) = Term.destConj concl

        val (recData,caseData) = Term.destConj recCaseData

        val indData = destIndData indData
(***
        and recData = destRecData recData
        and caseData = destCaseData caseData
***)
      in
        indData
      end
      handle Error err =>
        raise Error ("bad data theorem: " ^ err);
end;

local
in
  fun destNewtype th =
      let
        val Sequent.Sequent {hyp,concl} = Thm.sequent th

        val () =
            if TermAlphaSet.null hyp then ()
            else raise Error "hypotheses"

        val (absRep,repAbs) = Term.destConj concl

        val (ty,abs,rep) =
            let
              val (a,t0) = Term.destForall absRep

              val ty = Var.typeOf a

              val (t1,t2) = Term.destEq t0

              val () =
                  if Term.equalVar a t2 then ()
                  else raise Error "bad rhs of absRep"

              val (abs,t3) = Term.destApp t1

              val (rep,t4) = Term.destApp t3

              val () =
                  if Term.equalVar a t4 then ()
                  else raise Error "bad var inside absRep"
            in
              (ty,abs,rep)
            end

        val (ty',pred,abs',rep') =
            let
              val (r,t0) = Term.destForall repAbs

              val (pred,t1) = Term.destEq t0

              val () =
                  let
                    val vs = Term.freeVars pred
                  in
                    if VarSet.subset vs (VarSet.singleton r) then ()
                    else raise Error "extra free vars in predicate"
                  end

              val (t2,t3) = Term.destEq t1

              val () =
                  if Term.equalVar r t3 then ()
                  else raise Error "bad rhs of repAbs"

              val (rep,t4) = Term.destApp t2

              val ty = Term.typeOf t4

              val (abs,t5) = Term.destApp t4

              val () =
                  if Term.equalVar r t5 then ()
                  else raise Error "bad var inside repAbs"
            in
              (ty,pred,abs,rep)
            end

        val () =
            if Type.equal ty ty' then ()
            else raise Error "different types in absRep and repAbs"

        val () =
            if Term.equal abs abs' then ()
            else raise Error "different abstractions in absRep and repAbs"

        val () =
            if Term.equal rep rep' then ()
            else raise Error "different representations in absRep and repAbs"

        val (name,_) = Type.destOp ty
        and (abs,_) = Term.destConst abs
        and (rep,_) = Term.destConst rep
      in
        Newtype
          {name = name,
           predicate = pred,
           abs = abs,
           rep = rep}
      end
      handle Error err =>
        raise Error ("bad newtype theorem: " ^ err);
end;

local
  fun destEqn tm =
      let
        val (_,tm) = Term.stripForall tm

        val (l,r) = Term.destEq tm

        val (f,a) = Term.stripApp l
      in
        ((f, length a), (a,r))
      end;
in
  fun destValue th =
      let
        val Sequent.Sequent {hyp,concl} = Thm.sequent th

        val () =
            if TermAlphaSet.null hyp then ()
            else raise Error "hypotheses"

        val (fns,eqns) = unzip (List.map destEqn (Term.stripConj concl))

        val (name,ty) =
            case fns of
              [] => raise Error "no equations"
            | (f,n) :: fns =>
              let
                fun eq (f',n') = Term.equal f f' andalso n = n'

                val () =
                    if List.all eq fns then ()
                    else raise Error "different const/arity"
              in
                Term.destConst f
              end
      in
        Value
          {name = name,
           ty = ty,
           equations = eqns}
      end
      handle Error err =>
        raise Error ("bad value theorem: " ^ err);
end;

fun destSource th =
    let
      val dataResult =
          Left (destData th)
          handle Error err => Right err

      val newtypeResult =
          Left (destNewtype th)
          handle Error err => Right err

      val valueResult =
          Left (destValue th)
          handle Error err => Right err
    in
      case (dataResult,newtypeResult,valueResult) of
        (Left x, Right _, Right _) => DataSource x
      | (Right _, Left x, Right _) => NewtypeSource x
      | (Right _, Right _, Left x) => ValueSource x
      | (Right e1, Right e2, Right e3) =>
        let
          val err =
              "bad source theorem:\n  " ^ e1 ^ "\n  " ^ e2 ^ "\n  " ^ e3 ^
              "\n" ^ Print.toString Thm.pp th
        in
          raise Error err
        end
      | _ => raise Bug "Haskell.destSource: ambiguous"
    end;

(* ------------------------------------------------------------------------- *)
(* Sorting Haskell declarations into a module hierarchy.                     *)
(* ------------------------------------------------------------------------- *)

fun nameData (Data {name,...}) = TypeOp.name name;

local
  fun addConstructor ((c,tys),sym) =
      let
        val sym = Symbol.addConst sym c

        val sym = Symbol.addTypeList sym tys;
      in
        sym
      end;
in
  fun symbolData d =
      let
        val Data {name, parameters = _, constructors = cons} = d

        val sym = Symbol.empty

        val sym = Symbol.addTypeOp sym name

        val sym = List.foldl addConstructor sym cons
      in
        sym
      end;
end;

fun symbolNewtype n =
    let
      val Newtype {name, predicate = pred, abs, rep} = n

      val sym = Symbol.empty

      val sym = Symbol.addTypeOp sym name

      val sym = Symbol.addTerm sym pred

      val sym = Symbol.addConst sym abs

      val sym = Symbol.addConst sym rep
    in
      sym
    end;

local
  fun addEquation ((args,tm),sym) =
      let
        val sym = Symbol.addTermList sym args

        val sym = Symbol.addTerm sym tm
      in
        sym
      end;
in
  fun symbolValue v =
      let
        val Value {name, ty, equations = eqns} = v

        val sym = Symbol.empty

        val sym = Symbol.addConst sym name

        val sym = Symbol.addType sym ty

        val sym = List.foldl addEquation sym eqns
      in
        sym
      end;
end;

fun nameNewtype (Newtype {name,...}) = TypeOp.name name;

fun nameValue (Value {name,...}) = Const.name name;

fun nameSource s =
    case s of
      DataSource x => nameData x
    | NewtypeSource x => nameNewtype x
    | ValueSource x => nameValue x;

fun namespaceSource s = Name.namespace (nameSource s);

fun symbolSource s =
    case s of
      DataSource x => symbolData x
    | NewtypeSource x => symbolNewtype x
    | ValueSource x => symbolValue x;

(***
local
  fun addSym s = (s, symbolSource s);
in
  fun sortSource sl =
      let
        val sl = List.map addSym sl
      in
        
      end;
end;
***)

local
  fun init src = (Namespace.toList (namespaceSource src), src);

  fun split ((ns,src),(acc,nsubs)) =
      case ns of
        [] => (src :: acc, nsubs)
      | n :: ns => (acc, (n,(ns,src)) :: nsubs);

  fun group nsubs =
      case nsubs of
        [] => []
      | (n,sub) :: nsubs =>
        let
          val (ns,nsubs) = List.partition (equal n o fst) nsubs
        in
          (n, sub :: List.map snd ns) :: group nsubs
        end;

  fun mkTree namespace nsource =
      let
        val (source,nsubs) = List.foldl split ([],[]) (rev nsource)

        val nsubs = group nsubs
      in
        Module
          {namespace = namespace,
           source = source,
           submodules = List.map (addTree namespace) nsubs}
      end

  and addTree namespace (n,nsource) =
      let
        val namespace = Namespace.append namespace (Namespace.fromString n)
      in
        mkTree namespace nsource
      end;
in
  fun mkModule source =
      let
        val namespace = Namespace.global
        and source = List.map init source
      in
        mkTree namespace source
      end;
end;

local
  fun exposed (module,acc) =
      let
        val Module {namespace,source,submodules} = module

        val acc =
            if List.null source then acc
            else NamespaceSet.add acc namespace
      in
        List.foldl exposed acc submodules
      end;
in
  fun exposedModule source = exposed (source,NamespaceSet.empty);
end;

(* ------------------------------------------------------------------------- *)
(* Converting a theory to a Haskell package.                                 *)
(* ------------------------------------------------------------------------- *)

local
  fun getTheory name thys =
      case Theory.peekTheory name thys of
        SOME thy => thy
      | NONE =>
        let
          val err = "missing " ^ PackageName.toString name ^ " block in theory"
        in
          raise Error err
        end;
in
  fun splitTheories thy =
      let
        val thys =
            case Theory.node thy of
              Theory.Package {theories,...} => theories
            | _ => raise Bug "Haskell.theories: not a package theory"

        val src = getTheory PackageName.srcHaskellExport thys
        and test = getTheory PackageName.testHaskellExport thys
      in
        {src = src,
         test = test}
      end;
end;

fun destSourceTheory src =
    let
      val art = Theory.article src

      val ths = ThmSet.toList (Thms.thms (Article.thms art))
    in
      List.map destSource ths
    end;

fun convert pkg thy =
    let
      val {src, test = _} = splitTheories thy

      val source = mkModule (destSourceTheory src)
    in
      Haskell
        {package = pkg,
         source = source}
    end;

(* ------------------------------------------------------------------------- *)
(* Exporting various OpenTheory names to Haskell.                            *)
(* ------------------------------------------------------------------------- *)

fun exportPackageName name =
    let
      val old = PackageName.haskellExport
      and new = PackageName.newHaskellExport
    in
      if PackageName.equal name old then new
      else
        case PackageName.destStrictPrefix old name of
          SOME n => PackageName.append new n
        | NONE =>
          let
            val err =
                "non-Haskell package name: " ^ PackageName.toString name
          in
            raise Error err
          end
    end;

local
  val haskellNamespace = Namespace.fromList ["Haskell"]
  and opentheoryNamespace = Namespace.fromList ["OpenTheory"];
in
  fun exportNamespace ns =
      case Namespace.rewrite (haskellNamespace,opentheoryNamespace) ns of
        SOME ns => ns
      | NONE => raise Error ("non-Haskell namespace: " ^ Namespace.toString ns);
end;

local
  val typeOpMapping =
      NameMap.fromList
        [(Name.boolTypeOp, Name.mkGlobal "bool")];

  val constMapping =
      NameMap.fromList
        [(Name.conjConst, Name.mkGlobal "&&")];

  fun exportName n =
      let
        val (ns,n) = Name.dest n

        val ns = exportNamespace ns
      in
        Name.mk (ns,n)
      end;
in
  fun exportTypeOpName n =
      case NameMap.peek typeOpMapping n of
        SOME n => n
      | NONE =>
        exportName n
        handle Error err =>
          let
            val err = "bad type operator name: " ^ Name.toString n ^ "\n" ^ err
          in
            raise Error err
          end;

  fun exportConstName n =
      case NameMap.peek constMapping n of
        SOME n => n
      | NONE =>
        exportName n
        handle Error err =>
          let
            val err = "bad constant name: " ^ Name.toString n ^ "\n" ^ err
          in
            raise Error err
          end;
end;

(* ------------------------------------------------------------------------- *)
(* Writing a Haskell package to disk.                                        *)
(* ------------------------------------------------------------------------- *)

fun ppPackageName name = PackageName.pp (exportPackageName name);

local
  fun relativeNamespace namespace ns =
      case Namespace.rewrite (namespace,Namespace.global) ns of
        SOME ns => ns
      | NONE => ns;

  fun relativeName namespace n =
      let
        val (ns,n) = Name.dest n

        val ns = relativeNamespace namespace ns
      in
        Name.mk (ns,n)
      end;

  fun exportRelativeNamespace namespace =
      let
        val namespace = exportNamespace namespace
      in
        fn ns => relativeNamespace namespace (exportNamespace ns)
      end;

  fun exportRelativeTypeOpName namespace =
      let
        val namespace = exportNamespace namespace
      in
        fn n => relativeName namespace (exportTypeOpName n)
      end;

  fun exportRelativeConstName namespace =
      let
        val namespace = exportNamespace namespace
      in
        fn n => relativeName namespace (exportConstName n)
      end;
in
  fun ppNamespace ns = Namespace.pp (exportNamespace ns);

  fun ppRelativeNamespace namespace ns =
      Namespace.pp (exportRelativeNamespace namespace ns);

  fun ppRelativeTypeOpName namespace n =
      Name.pp (exportRelativeTypeOpName namespace n);

  fun ppRelativeConstName namespace n =
      Name.pp (exportRelativeConstName namespace n);
end;

fun ppData ns data =
    let
      val Data {name, parameters = parms, constructors = cons} = data
    in
      Print.blockProgram Print.Inconsistent 2
        [Print.ppString "data ",
         ppRelativeTypeOpName ns (TypeOp.name name)]
    end;

fun ppNewtype ns x = Print.ppString "newtype";

fun ppValue ns x = Print.ppString "value";

fun ppSource ns s =
    case s of
      DataSource x => ppData ns x
    | NewtypeSource x => ppNewtype ns x
    | ValueSource x => ppValue ns x;

local
  fun ppSpaceSource ns s =
      Print.sequence
        (Print.sequence Print.addNewline Print.addNewline)
        (ppSource ns s);
in
  fun ppSourceList ns sl =
      case sl of
        [] => Print.skip
      | s :: sl =>
        Print.program (ppSource ns s :: List.map (ppSpaceSource ns) sl);
end;

fun ppTag (s,pp) =
    Print.blockProgram Print.Inconsistent 2
      [Print.ppString s,
       Print.ppString ": ",
       pp];

fun ppTags spps =
    case spps of
      [] => Print.skip
    | spp :: spps =>
      Print.blockProgram Print.Inconsistent 0
        (ppTag spp ::
         List.map (Print.sequence Print.addNewline o ppTag) spps);

local
  val ppVersion = PackageVersion.pp;
(***
  fun ppVersion version =
      let
        val today = Date.fromTimeLocal (Time.now ())

        val y = Date.year today
        and m = Date.month today
        and d = Date.day today

        val m =
            case m of
              Date.Jan => 1
            | Date.Feb => 2
            | Date.Mar => 3
            | Date.Apr => 4
            | Date.May => 5
            | Date.Jun => 6
            | Date.Jul => 7
            | Date.Aug => 8
            | Date.Sep => 9
            | Date.Oct => 10
            | Date.Nov => 11
            | Date.Dec => 12
      in
        Print.program
          [PackageVersion.pp version,
           Print.ppString ".",
           Print.ppInt y,
           Print.ppString ".",
           Print.ppInt m,
           Print.ppString ".",
           Print.ppInt d]
      end;
***)

  fun ppSection s pps =
      Print.blockProgram Print.Inconsistent 2
        [Print.ppString s,
         Print.addNewline,
         Print.program pps];

  val ppBuildDepends =
      [Print.ppString "base >= 4.0 && < 5.0",
       Print.ppString ",",
       Print.addNewline,
       Print.ppString "opentheory >= 1.0 && < 2.0"];

  fun ppExposedModules mods =
      case NamespaceSet.toList mods of
        [] => []
      | ns :: nss =>
        ppNamespace ns ::
        List.map (Print.sequence Print.addNewline o ppNamespace) nss;
in
  fun ppCabal (pkg,source) =
      let
        val name = Package.name pkg
        and version = Package.version pkg
        and {description} = Package.description pkg
        and {license} = Package.license pkg
        and {author} = Package.author pkg
        and mods = exposedModule source
      in
        Print.blockProgram Print.Inconsistent 0
          [ppTags
             [("Name", ppPackageName name),
              ("Version", ppVersion version),
              ("Description", Print.ppString description),
              ("License", Print.ppString license),
              ("License-file", Print.ppString "LICENSE"),
              ("Cabal-version", Print.ppString ">= 1.8.0.6"),
              ("Build-type", Print.ppString "Simple"),
              ("Author", Print.ppString author),
              ("Maintainer", Print.ppString author)],
           Print.addNewline,
           Print.addNewline,
           ppSection "Library"
             [ppSection "Build-depends:" ppBuildDepends,
              Print.addNewline,
              Print.addNewline,
              ppTag ("hs-source-dirs", Print.ppString "src"),
              Print.addNewline,
              Print.addNewline,
              ppTag ("ghc-options", Print.ppString "-Wall -Werror"),
              Print.addNewline,
              Print.addNewline,
              ppSection "Exposed-modules:" (ppExposedModules mods)]]
      end;
end;

local
  fun ppModuleDeclaration namespace =
      Print.blockProgram Print.Inconsistent 0
        [Print.ppString "module ",
         ppNamespace namespace,
         Print.addNewline,
         Print.ppString "where"];
in
  fun ppModule (pkg,namespace,source) =
      let
        val {description} = Package.description pkg
        and {license} = Package.license pkg
        and {author} = Package.author pkg
      in
        Print.blockProgram Print.Inconsistent 0
          [Print.ppString "{- |",
           Print.addNewline,
           ppTags
             [("Module", Print.ppString "$Header$"),
              ("Description", Print.ppString description),
              ("License", Print.ppString license)],
           Print.addNewline,
           Print.addNewline,
           ppTags
             [("Maintainer", Print.ppString author),
              ("Stability", Print.ppString "provisional"),
              ("Portability", Print.ppString "portable")],
           Print.addNewline,
           Print.addNewline,
           Print.ppString description,
           Print.addNewline,
           Print.ppString "-}",
           Print.addNewline,
           ppModuleDeclaration namespace,
           Print.addNewline,
           Print.addNewline,
           ppSourceList namespace source]
      end;
end;

fun mkSubDirectory {directory = dir} sub =
    let
      val dir = OS.Path.concat (dir,sub)

      val () = OS.FileSys.mkDir dir
    in
      {directory = dir}
    end;

fun outputCabal {directory = dir} pkg source =
    let
      val ss = Print.toStream ppCabal (pkg,source)

      val file =
          let
            val base = Print.toLine ppPackageName (Package.name pkg)

            val f = OS.Path.joinBaseExt {base = base, ext = SOME "cabal"}
          in
            OS.Path.joinDirFile {dir = dir, file = f}
          end

      val () = Stream.toTextFile {filename = file} ss
    in
      ()
    end;

local
  fun outputSrc pkg {directory = dir} sub namespace source =
      let
        val ss = Print.toStream ppModule (pkg,namespace,source)

        val file =
            let
              val f = OS.Path.joinBaseExt {base = sub, ext = SOME "hs"}
            in
              OS.Path.joinDirFile {dir = dir, file = f}
            end

        val () = Stream.toTextFile {filename = file} ss
      in
        ()
      end;

  fun outputMod pkg dir ns module =
      let
        val Module {namespace,source,submodules} = module

        val sub =
            let
              val pp =
                  if Namespace.isGlobal ns then ppNamespace
                  else ppRelativeNamespace ns
            in
              Print.toLine pp namespace
            end

        val () =
            if List.null source then ()
            else outputSrc pkg dir sub namespace source

        val () =
            if List.null submodules then ()
            else
              let
                val dir = mkSubDirectory dir sub
              in
                List.app (outputMod pkg dir namespace) submodules
              end
      in
        ()
      end;
in
  fun outputSource dir pkg module =
      let
        val Module {namespace,source,submodules} = module

        val () =
            if Namespace.isGlobal namespace then ()
            else raise Bug "Haskell.outputSource: not global namespace"

        val () =
            if List.null source then ()
            else raise Error "cannot export global definitions"

        val dir = mkSubDirectory dir "src"
      in
        List.app (outputMod pkg dir namespace) submodules
      end;
end;

fun toPackage haskell =
    let
      val Haskell {package,source} = haskell

      val dir =
          let
            val name = Print.toLine ppPackageName (Package.name package)

            val dir = {directory = OS.FileSys.getDir ()}
          in
            mkSubDirectory dir name
          end

      val () = outputCabal dir package source

      val () = outputSource dir package source
    in
      ()
    end;

(* ------------------------------------------------------------------------- *)
(* Export a theory to a Haskell package.                                     *)
(* ------------------------------------------------------------------------- *)

fun export dir namever =
    let
      val info =
          case Directory.peek dir namever of
            SOME i => i
          | NONE =>
            let
              val err =
                  "theory " ^ PackageNameVersion.toString namever ^
                  " is not installed"
            in
              raise Error err
            end

      val pkg = PackageInfo.package info

      val importer = Directory.importer dir

      val graph = TheoryGraph.empty {savable = false}

      val imps = TheorySet.empty

      val int = Interpretation.natural

      val (_,thy) =
          TheoryGraph.importPackageInfo importer graph
            {imports = imps,
             interpretation = int,
             info = info}

      val haskell = convert pkg thy

      val () = toPackage haskell
    in
      ()
    end;

end
