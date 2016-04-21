(* ========================================================================= *)
(* PACKAGE THEORY GRAPHS                                                     *)
(* Copyright (c) 2010 Joe Leslie-Hurd, distributed under the MIT license     *)
(* ========================================================================= *)

structure PackageTheoryGraph :> PackageTheoryGraph =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* Topological sort of theory blocks to respect union blocks.                *)
(* ------------------------------------------------------------------------- *)

fun sortUnion thys =
    if PackageTheory.sortedUnion thys then thys
    else PackageTheory.sortUnion thys;

(* ------------------------------------------------------------------------- *)
(* Topological sort of theory blocks to respect import declarations.         *)
(* ------------------------------------------------------------------------- *)

fun sortImports thys =
    if PackageTheory.sortedImports thys then thys
    else PackageTheory.sortImports thys;

(* ------------------------------------------------------------------------- *)
(* Individual theory blocks.                                                 *)
(* ------------------------------------------------------------------------- *)

datatype vanilla =
    Vanilla of
      (TheoryGraph.graph * Theory.theory * Summary.summary) PackageNameMap.map;

val emptyVanilla = Vanilla (PackageNameMap.new ());

fun getVanilla (Vanilla vmap) name =
    case PackageNameMap.peek vmap name of
      SOME x => x
    | NONE => raise Bug "PackageTheoryGraph.getVanilla";

fun getNameTheoryVanilla vanilla name =
    let
      val (_,thy,_) = getVanilla vanilla name
    in
      TheoryName.mk (name,thy)
    end;

fun getMainNameTheoryVanilla vanilla =
    getNameTheoryVanilla vanilla PackageTheory.mainName;

fun addVanilla finder {directory} (theory, Vanilla vmap) =
    let
      val PackageTheory.Theory {name,node,...} = theory

      val graph = TheoryGraph.empty {savable = false}

      val imports = TheorySet.empty

      val int = Interpretation.natural

      val nodeImports = TheorySet.empty

      val (graph,thy) =
          TheoryGraph.importNode finder graph
            {directory = directory,
             imports = imports,
             interpretation = int,
             nodeImports = nodeImports,
             node = node}

      val art = Theory.article thy

      val ths = Article.thms art

      val sum = Summary.fromThms ths
    in
      Vanilla (PackageNameMap.insert vmap (name,(graph,thy,sum)))
    end;

fun fromListVanilla finder dir theories =
    List.foldl (addVanilla finder dir) emptyVanilla theories
(*OpenTheoryDebug
    handle Error err => raise Error ("PackageTheoryGraph.fromListVanilla: " ^ err);
*)

(* ------------------------------------------------------------------------- *)
(* Fixed point calculation of theory block definitions.                      *)
(* ------------------------------------------------------------------------- *)

datatype definitions = Definitions of SymbolTable.table PackageNameMap.map;

val emptyDefinitions = Definitions (PackageNameMap.new ());

fun getDefinitions (Definitions dmap) name =
    case PackageNameMap.peek dmap name of
      SOME defs => defs
    | NONE => SymbolTable.empty;

fun getListDefinitions definitions names =
    let
      val defsl = List.map (getDefinitions definitions) names
    in
      SymbolTable.unionList defsl
    end;

fun addDefinitions vanilla (theory,(changed,definitions)) =
    let
      val PackageTheory.Theory {name,imports,...} = theory

(*OpenTheoryTrace2
      val () = Print.trace PackageName.pp "PackageTheoryGraph.addDefinitions.name" name
*)
      val defs = getDefinitions definitions name

      val idefs = getListDefinitions definitions imports

      val defs' =
          if PackageTheory.isUnion theory then idefs
          else
            let
              val (_,_,sum) = getVanilla vanilla name

              fun addTypeOp (ot,sym) =
                  case SymbolTable.peekTypeOp idefs (TypeOp.name ot) of
                    NONE => sym
                  | SOME ot => SymbolTable.addTypeOp sym ot

              fun addConst (c,sym) =
                  case SymbolTable.peekConst idefs (Const.name c) of
                    NONE => sym
                  | SOME c => SymbolTable.addConst sym c

              val provides = Summary.provides sum

              val {undefined = pinp, defined = pdef} =
                  SymbolTable.partitionUndef (Sequents.symbol provides)

              val pdef = TypeOpSet.foldl addTypeOp pdef (SymbolTable.typeOps pinp)

              val pdef = ConstSet.foldl addConst pdef (SymbolTable.consts pinp)
            in
              pdef
            end

(*OpenTheoryDebug
      val _ = TypeOpSet.subset (SymbolTable.typeOps defs) (SymbolTable.typeOps defs')
              orelse raise Bug "PackageTheoryGraph.addDefinitions: shrinking type op defs"

      val _ = ConstSet.subset (SymbolTable.consts defs) (SymbolTable.consts defs')
              orelse raise Bug "PackageTheoryGraph.addDefinitions: shrinking const defs"
*)
      val same =
          (TypeOpSet.size (SymbolTable.typeOps defs) =
           TypeOpSet.size (SymbolTable.typeOps defs')) andalso
          (ConstSet.size (SymbolTable.consts defs) =
           ConstSet.size (SymbolTable.consts defs'))
    in
      if same then (changed,definitions)
      else
        let
          val Definitions dmap = definitions

          val dmap = PackageNameMap.insert dmap (name,defs')

(*OpenTheoryTrace2
          val () = Print.trace SymbolTable.pp "PackageTheoryGraph.addDefinitions.defs'" defs'
*)
        in
          (true, Definitions dmap)
        end
    end;

fun fromListDefinitions vanilla theories =
    let
      fun pass defs =
          let
            val (changed,defs) =
                List.foldl (addDefinitions vanilla) (false,defs) theories
          in
            if changed then pass defs else defs
          end
    in
      pass emptyDefinitions
    end
(*OpenTheoryDebug
    handle Error err =>
      raise Error ("PackageTheoryGraph.fromListDefinitions: " ^ err);
*)

(* ------------------------------------------------------------------------- *)
(* Theory block summaries.                                                   *)
(* ------------------------------------------------------------------------- *)

datatype summary = Summary of Summary.summary PackageNameMap.map;

val emptySummary = Summary (PackageNameMap.new ());

fun getSummary (Summary smap) name =
    case PackageNameMap.peek smap name of
      SOME sum => sum
    | NONE => raise Bug "PackageTheoryGraph.getSummary";

fun addSummary vanilla definitions (theory,summary) =
    let
      val PackageTheory.Theory {name,imports,...} = theory
      and Summary smap = summary

(*OpenTheoryTrace2
      val () = Print.trace PackageName.pp "PackageTheoryGraph.addSummary.name" name
*)
      val sum =
          if PackageTheory.isUnion theory then
            let
              val sums = List.map (getSummary summary) imports

              val provs = List.map Summary.provides sums

              val prov = Sequents.unionList provs

              val sum' =
                  Summary.Summary'
                    {requires = prov,
                     provides = prov}
            in
              Summary.mk sum'
            end
          else
            let
              val (_,_,sum) = getVanilla vanilla name

              val idefs = getListDefinitions definitions imports

(*OpenTheoryTrace2
              val () = Print.trace SymbolTable.pp "PackageTheoryGraph.addSummary.idefs" idefs
*)
              val rewr = SymbolTable.inst idefs
            in
              Option.getOpt (Summary.rewrite rewr sum, sum)
            end

(*OpenTheoryTrace2
      val () = Print.trace Summary.pp "PackageTheoryGraph.addSummary.sum" sum
*)
    in
      Summary (PackageNameMap.insert smap (name,sum))
    end;

fun fromListSummary vanilla definitions theories =
    List.foldl (addSummary vanilla definitions) emptySummary theories
(*OpenTheoryDebug
    handle Error err => raise Error ("PackageTheoryGraph.fromListSummary: " ^ err);
*)

(* ------------------------------------------------------------------------- *)
(* Removing dead theory imports.                                             *)
(* ------------------------------------------------------------------------- *)

fun removeSequent req seqs =
    let
      val req' = SequentSet.difference req seqs

      val same = SequentSet.size req' = SequentSet.size req
    in
      (req',same)
    end;

fun removeSymbol (ots,cs) sym =
    let
      fun undefT t = not (SymbolTable.knownTypeOp sym (TypeOp.name t))

      fun undefC c = not (SymbolTable.knownConst sym (Const.name c))

      val ots' = TypeOpSet.filter undefT ots
      and cs' = ConstSet.filter undefC cs

      val same =
          TypeOpSet.size ots' = TypeOpSet.size ots andalso
          ConstSet.size cs' = ConstSet.size cs
    in
      ((ots',cs'),same)
    end;

fun removeDeadImportsTheory outputWarning vanilla definitions summary theory =
    let
      val PackageTheory.Theory {name,imports,node} = theory

      fun removeDefs inp imp = removeSymbol inp (getDefinitions definitions imp)

      fun addProv (imp,(acc,req,inp)) =
          let
            val (req,reqSame) =
                let
                  val prov = Summary.provides (getSummary summary imp)

                  val pseqs = Sequents.sequents prov
                in
                  removeSequent req pseqs
                end

            val (acc,inp) =
                if reqSame then (acc,inp)
                else
                  let
                    val acc = PackageNameSet.add acc imp

                    val (inp,_) = removeDefs inp imp
                  in
                    (acc,inp)
                  end
          in
            (acc,req,inp)
          end

      fun addDef (imp,(acc,inp)) =
          let
            val (inp,inpSame) = removeDefs inp imp

            val acc = if inpSame then acc else PackageNameSet.add acc imp
          in
            (acc,inp)
          end

      fun warnDead imp =
          if not outputWarning then ()
          else
            warn
              ("redundant import " ^ PackageName.toString imp ^
               " in theory block " ^ PackageName.toString name)

      val req =
          let
            val sum = getSummary summary name
          in
            Sequents.sequents (Summary.requires sum)
          end

      val inp =
          let
            val (_,_,sum) = getVanilla vanilla name

            val psym = Sequents.symbol (Summary.provides sum)

            val {undefined = pinp, defined = _} = SymbolTable.partitionUndef psym
          in
            (SymbolTable.typeOps pinp, SymbolTable.consts pinp)
          end

      val alive = PackageNameSet.empty

      val (alive,_,inp) = List.foldl addProv (alive,req,inp) imports

      val (alive,_) = List.foldl addDef (alive,inp) imports

      fun isAlive imp = PackageNameSet.member imp alive

      val (imports,dead) = List.partition isAlive imports

      val () = List.app warnDead dead
    in
      PackageTheory.Theory
        {name = name,
         imports = imports,
         node = node}
    end;

fun removeDeadImports outputWarning vanilla definitions summary theories =
    let
      val remove =
          removeDeadImportsTheory outputWarning vanilla definitions summary
    in
      List.map remove theories
    end;

(* ------------------------------------------------------------------------- *)
(* Removing dead theory blocks.                                              *)
(* ------------------------------------------------------------------------- *)

fun removeDeadBlocks outputWarning theories =
    let
      fun warnDead thy =
          if not outputWarning then ()
          else
            let
              val msg =
                  "redundant theory block " ^
                  PackageName.toString (PackageTheory.name thy)
            in
              warn msg
            end

      val idx = PackageTheory.fromListIndex theories

      fun ancs acc work =
          case work of
            [] => acc
          | n :: work =>
            if PackageNameSet.member n acc then ancs acc work
            else
              let
                val thy = PackageTheory.getIndex idx n

                val acc = PackageNameSet.add acc n

                val work = PackageTheory.imports thy @ work
              in
                ancs acc work
              end

      val alive = ancs PackageNameSet.empty [PackageTheory.mainName]

      fun isAlive thy = PackageNameSet.member (PackageTheory.name thy) alive

      val (theories,dead) = List.partition isAlive theories

      val () = List.app warnDead dead
    in
      theories
    end;

(* ------------------------------------------------------------------------- *)
(* Remove dead theory blocks and import declarations.                        *)
(* ------------------------------------------------------------------------- *)

fun removeDead {finder,directory,outputWarning,theories} =
    let
(*OpenTheoryDebug
      val () =
          if PackageTheory.sortedUnion theories then ()
          else raise Bug "PackageTheoryGraph.removeDead"
*)
      val vanilla = fromListVanilla finder {directory = directory} theories

      val definitions = fromListDefinitions vanilla theories

      val summary = fromListSummary vanilla definitions theories

      val theories =
          removeDeadImports outputWarning vanilla definitions summary theories

      val theories = removeDeadBlocks outputWarning theories
    in
      theories
    end
(*OpenTheoryDebug
    handle Error err => raise Error ("PackageTheoryGraph.removeDead: " ^ err);
*)

(* ------------------------------------------------------------------------- *)
(* Add checksums to package include theory blocks.                           *)
(* ------------------------------------------------------------------------- *)

fun addChecksums {finder,theories} =
    let
      fun add namever chko =
          if Option.isSome chko then NONE
          else
            case PackageFinder.find finder namever NONE of
              SOME pkg => SOME (namever, SOME (Package.checksum pkg))
            | NONE => raise Bug "PackageTheoryGraph.addChecksums"
    in
      case PackageTheory.updateIncludes add theories of
        SOME theories => theories
      | NONE => theories
    end;

(* ------------------------------------------------------------------------- *)
(* Clean up theory blocks (addChecksums o removeDead o sortImports).         *)
(* ------------------------------------------------------------------------- *)

fun clean {finder,directory,outputWarning,theories} =
    let
      val theories = sortImports theories

      val theories =
          removeDead
            {finder = finder,
             directory = directory,
             outputWarning = outputWarning,
             theories = theories}

      val theories =
          addChecksums
            {finder = finder,
             theories = theories}
     in
       theories
     end;

end
