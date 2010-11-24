(* ========================================================================= *)
(* UNWIND MUTUALLY RECURSIVE THEORY PACKAGES                                 *)
(* Copyright (c) 2010 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

structure Dagify :> Dagify =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* Individual theory blocks.                                                 *)
(* ------------------------------------------------------------------------- *)

datatype vanilla =
    Vanilla of
      (Graph.graph * Theory.theory * Summary.summary) PackageBaseMap.map;

val emptyVanilla = Vanilla (PackageBaseMap.new ());

fun getVanilla (Vanilla vmap) name =
    case PackageBaseMap.peek vmap name of
      SOME x => x
    | NONE => raise Bug "Dagify.getVanilla";

fun getNameTheoryVanilla vanilla name =
    let
      val (_,thy,_) = getVanilla vanilla name
    in
      NameTheory.mk (name,thy)
    end;

fun getMainNameTheoryVanilla vanilla =
    getNameTheoryVanilla vanilla PackageTheory.mainName;

fun addVanilla importer {directory} (theory, Vanilla vmap) =
    let
      val PackageTheory.Theory {name,node,...} = theory

      val graph = Graph.empty {savable = false}

      val imports = TheorySet.empty

      val int = Interpretation.natural

      val (graph,thy) =
          Graph.importNode importer graph
            {directory = directory,
             imports = imports,
             interpretation = int,
             node = node}

      val art = Theory.article thy

      val ths = Article.thms art

      val sum = Summary.fromThms ths
    in
      Vanilla (PackageBaseMap.insert vmap (name,(graph,thy,sum)))
    end;

fun fromListVanilla importer dir theories =
    List.foldl (addVanilla importer dir) emptyVanilla theories
(*OpenTheoryDebug
    handle Error err => raise Error ("Dagify.fromListVanilla: " ^ err);
*)

(* ------------------------------------------------------------------------- *)
(* Fixed point calculation of theory block definitions.                      *)
(* ------------------------------------------------------------------------- *)

datatype definitions = Definitions of Symbol.symbol PackageBaseMap.map;

val emptyDefinitions = Definitions (PackageBaseMap.new ());

fun getDefinitions (Definitions dmap) name =
    case PackageBaseMap.peek dmap name of
      SOME defs => defs
    | NONE => Symbol.empty;

fun getListDefinitions definitions names =
    let
      val defsl = List.map (getDefinitions definitions) names
    in
      Symbol.unionList defsl
    end;

fun addDefinitions vanilla (theory,(changed,definitions)) =
    let
      val PackageTheory.Theory {name,imports,...} = theory

(*OpenTheoryTrace3
      val () = Print.trace PackageTheory.ppName "Dagify.addDefinitions.name" name
*)

      val defs = getDefinitions definitions name

      val idefs = getListDefinitions definitions imports

      val defs' =
          if PackageTheory.isUnion theory then idefs
          else
            let
              val (_,_,sum) = getVanilla vanilla name

              fun addTypeOp (ot,sym) =
                  case Symbol.peekTypeOp idefs (TypeOp.name ot) of
                    NONE => sym
                  | SOME ot => Symbol.addTypeOp sym ot

              fun addConst (c,sym) =
                  case Symbol.peekConst idefs (Const.name c) of
                    NONE => sym
                  | SOME c => Symbol.addConst sym c

              val provides = Summary.provides sum

              val {undefined = pinp, defined = pdef} =
                  Symbol.partitionUndef (Sequents.symbol provides)

              val pdef = TypeOpSet.foldl addTypeOp pdef (Symbol.typeOps pinp)

              val pdef = ConstSet.foldl addConst pdef (Symbol.consts pinp)
            in
              pdef
            end

(*OpenTheoryDebug
      val _ = TypeOpSet.subset (Symbol.typeOps defs) (Symbol.typeOps defs')
              orelse raise Bug "Dagify.addDefinitions: shrinking type op defs"

      val _ = ConstSet.subset (Symbol.consts defs) (Symbol.consts defs')
              orelse raise Bug "Dagify.addDefinitions: shrinking const defs"
*)

      val same =
          (TypeOpSet.size (Symbol.typeOps defs) =
           TypeOpSet.size (Symbol.typeOps defs')) andalso
          (ConstSet.size (Symbol.consts defs) =
           ConstSet.size (Symbol.consts defs'))
    in
      if same then (changed,definitions)
      else
        let
          val Definitions dmap = definitions

          val dmap = PackageBaseMap.insert dmap (name,defs')

(*OpenTheoryTrace3
          val () = Print.trace Symbol.pp "Dagify.addDefinitions.defs'" defs'
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
    handle Error err => raise Error ("Dagify.fromListDefinitions: " ^ err);
*)

(* ------------------------------------------------------------------------- *)
(* Theory block summaries.                                                   *)
(* ------------------------------------------------------------------------- *)

datatype summary = Summary of Summary.summary PackageBaseMap.map;

val emptySummary = Summary (PackageBaseMap.new ());

fun getSummary (Summary smap) name =
    case PackageBaseMap.peek smap name of
      SOME sum => sum
    | NONE => raise Bug "Dagify.getSummary";

fun addSummary vanilla definitions (theory,summary) =
    let
      val PackageTheory.Theory {name,imports,...} = theory
      and Summary smap = summary

(*OpenTheoryTrace3
      val () = Print.trace PackageTheory.ppName "Dagify.addSummary.name" name
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

(*OpenTheoryTrace3
              val () = Print.trace Symbol.pp "Dagify.addSummary.idefs" idefs
*)

              val rewr = Symbol.inst idefs
            in
              Option.getOpt (Summary.rewrite rewr sum, sum)
            end

(*OpenTheoryTrace3
      val () = Print.trace Summary.pp "Dagify.addSummary.sum" sum
*)
    in
      Summary (PackageBaseMap.insert smap (name,sum))
    end;

fun fromListSummary vanilla definitions theories =
    List.foldl (addSummary vanilla definitions) emptySummary theories
(*OpenTheoryDebug
    handle Error err => raise Error ("Dagify.fromListSummary: " ^ err);
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
      fun undefT t = not (Symbol.knownTypeOp sym (TypeOp.name t))

      fun undefC c = not (Symbol.knownConst sym (Const.name c))

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
                    val acc = PackageBaseSet.add acc imp

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

            val acc = if inpSame then acc else PackageBaseSet.add acc imp
          in
            (acc,inp)
          end

      fun warnDead imp =
          if not outputWarning then ()
          else
            warn
              ("redundant import " ^ PackageTheory.toStringName imp ^
               " in theory block " ^ PackageTheory.toStringName name)

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

            val {undefined = pinp, defined = _} = Symbol.partitionUndef psym
          in
            (Symbol.typeOps pinp, Symbol.consts pinp)
          end

      val alive = PackageBaseSet.empty

      val (alive,_,inp) = List.foldl addProv (alive,req,inp) imports

      val (alive,_) = List.foldl addDef (alive,inp) imports

      fun isAlive imp = PackageBaseSet.member imp alive

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
      fun remove thy =
          removeDeadImportsTheory outputWarning vanilla definitions summary thy
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
            warn
              ("redundant theory block " ^
               PackageTheory.toStringName (PackageTheory.name thy))

      val idx = PackageTheory.fromListIndex theories

      fun ancs acc work =
          case work of
            [] => acc
          | n :: work =>
            if PackageBaseSet.member n acc then ancs acc work
            else
              let
                val thy = PackageTheory.getIndex idx n

                val acc = PackageBaseSet.add acc n

                val work = PackageTheory.imports thy @ work
              in
                ancs acc work
              end

      val alive = ancs PackageBaseSet.empty [PackageTheory.mainName]

      fun isAlive thy = PackageBaseSet.member (PackageTheory.name thy) alive

      val (theories,dead) = List.partition isAlive theories

      val () = List.app warnDead dead
    in
      theories
    end;

(* ------------------------------------------------------------------------- *)
(* Remove dead theory imports and blocks.                                    *)
(* ------------------------------------------------------------------------- *)

datatype theories =
    Theories of
      {importer : Graph.importer,
       directory : string,
       theories : PackageTheory.theory list,
       vanilla : vanilla,
       definitions : definitions};

fun removeDead outputWarning {importer,directory,theories} =
    let
      val dir = {directory = directory}

      val theories' = PackageTheory.sortUnion theories

      val vanilla = fromListVanilla importer dir theories

      val definitions = fromListDefinitions vanilla theories'

      val summary = fromListSummary vanilla definitions theories'

      (* Remove redundant imports and theory blocks *)

      val theories =
          removeDeadImports outputWarning vanilla definitions summary theories

      val theories = removeDeadBlocks outputWarning theories
    in
      Theories
        {importer = importer,
         directory = directory,
         theories = theories,
         vanilla = vanilla,
         definitions = definitions}
    end
(*OpenTheoryDebug
    handle Error err => raise Error ("Dagify.removeDead: " ^ err);
*)

val mk = removeDead true;

fun theories (Theories {theories = x, ...}) = x;

(* ------------------------------------------------------------------------- *)
(* Visible primitive theories of theory blocks.                              *)
(* ------------------------------------------------------------------------- *)

datatype visible =
    Visible of
      {name : NameTheory.nameTheory,
       prov : SequentSet.set,
       defs : Symbol.symbol} list PackageBaseMap.map;

val emptyVisible = Visible (PackageBaseMap.new ());

fun getVisible (Visible vmap) name =
    case PackageBaseMap.peek vmap name of
      SOME vs => vs
    | NONE => raise Bug "Dagify.getVisible";

fun getListVisible visible names =
    let
      val vsl = List.map (getVisible visible) names
    in
      List.concat vsl
    end;

fun addVisible vanilla definitions (theory,visible) =
    let
      val PackageTheory.Theory {name,imports,...} = theory
      and Visible vmap = visible

(*OpenTheoryTrace3
      val () = Print.trace PackageTheory.ppName "Dagify.addVisible.name" name
*)

      val vs =
          if PackageTheory.isUnion theory then getListVisible visible imports
          else
            let
              fun mkVs thy rewr =
                  let
                    val art = Theory.article thy

                    val ths = Article.thms art

                    val prov = Sequents.fromThms ths

                    val (prov',rewr) = Sequents.sharingRewrite prov rewr

                    val prov = Option.getOpt (prov',prov)

                    val {undefined = _, defined = defs} =
                        Symbol.partitionUndef (Sequents.symbol prov)

                    val prov = Sequents.sequents prov

                    val vs =
                        {name = NameTheory.mk (name,thy),
                         prov = prov,
                         defs = defs}
                  in
                    (vs,rewr)
                  end

              val (_,thy,_) = getVanilla vanilla name

              val idefs = getListDefinitions definitions imports

              val rewr = Symbol.inst idefs

              val (vs,_) = maps mkVs (Graph.visiblePrimitives thy) rewr
            in
              vs
            end
    in
      Visible (PackageBaseMap.insert vmap (name,vs))
    end;

fun fromListVisible vanilla definitions theories =
    List.foldl (addVisible vanilla definitions) emptyVisible theories
(*OpenTheoryDebug
    handle Error err => raise Error ("Dagify.fromListVisible: " ^ err);
*)

(* ------------------------------------------------------------------------- *)
(* Visible primitive theories of named theories.                             *)
(* ------------------------------------------------------------------------- *)

datatype generate =
    Generate of NameTheorySet.set NameTheoryMap.map;

val emptyGenerate = Generate (NameTheoryMap.new ());

fun getGenerate (Generate gmap) nt =
    case NameTheoryMap.peek gmap nt of
      SOME nts => nts
    | NONE =>
      raise Bug ("Dagify.getGenerate: " ^ Print.toString NameTheory.pp nt);

local
  fun add generate (nt,acc) =
      NameTheorySet.union acc (getGenerate generate nt);
in
  fun getListGenerate generate nts =
      List.foldl (add generate) NameTheorySet.empty nts;

  fun getSetGenerate generate nts =
      NameTheorySet.foldl (add generate) NameTheorySet.empty nts;
end;

fun insertGenerate (Generate gmap) (nt,nts) =
    let
(*OpenTheoryDebug
      val () = if not (NameTheoryMap.inDomain nt gmap) then ()
               else raise Bug "Dagify.insertGenerate"
*)
    in
      Generate (NameTheoryMap.insert gmap (nt,nts))
    end;

fun foldlGenerate f b (Generate gmap) = NameTheoryMap.foldl f b gmap;

fun findlGenerate p (Generate gmap) = NameTheoryMap.findl p gmap;

fun ppGenerate (Generate gmap) =
    NameTheoryMap.pp
      (Print.ppMap NameTheorySet.toList (Print.ppList NameTheory.pp)) gmap;

fun addTheoryGenerate vanilla (theory,generate) =
    let
      val PackageTheory.Theory {name,imports,...} = theory

(*OpenTheoryTrace3
      val () = Print.trace PackageTheory.ppName "Dagify.addGenerate.name" name
*)
    in
      if PackageTheory.isUnion theory then
        let
          fun mkNT n = getNameTheoryVanilla vanilla n

          val nt = mkNT name

          val gens = getListGenerate generate (List.map mkNT imports)
        in
          insertGenerate generate (nt,gens)
        end
      else
        let
          fun mkNT thy = NameTheory.mk (name,thy)

          fun getGens generate thy = getGenerate generate (mkNT thy)

          fun getListGens generate thys =
              getListGenerate generate (List.map mkNT thys)

          val (graph,main,_) = getVanilla vanilla name

(*OpenTheoryTrace3
          val () = Print.trace pp "Dagify.addGenerate.graph" graph
*)

          val primThys = Graph.primitives main

          fun addThy (thy,generate) =
              let
                val nt = mkNT thy

(*OpenTheoryTrace3
                val () = Print.trace NameTheory.pp
                           "Dagify.addGenerate.addThy.nt" nt
*)

                val gens =
                    case Theory.node thy of
                      Theory.Article _ => NameTheorySet.empty
                    | Theory.Package {theories,...} =>
                      let
                        val main = Theory.mainTheory theories
                      in
                        getGens generate main
                      end
                    | Theory.Union => getListGens generate (Theory.imports thy)

                val gens =
                    if not (TheorySet.member thy primThys) then gens
                    else NameTheorySet.add gens nt
              in
                insertGenerate generate (nt,gens)
              end
        in
          TheorySet.foldl addThy generate (Graph.theories graph)
        end
    end;

fun fromTheoryListGenerate vanilla theories =
    let
      val generate =
          List.foldl (addTheoryGenerate vanilla) emptyGenerate theories

(*OpenTheoryTrace3
      val () = Print.trace ppGenerate
                 "Dagify.fromTheoryListGenerate.generate" generate
*)
    in
      generate
    end
(*OpenTheoryDebug
    handle Error err => raise Error ("Dagify.fromTheoryListGenerate: " ^ err);
*)

(* ------------------------------------------------------------------------- *)
(* Primitive dependencies of named theories.                                 *)
(* ------------------------------------------------------------------------- *)

datatype dependency =
    Dependency of NameTheorySet.set NameTheoryMap.map;

val emptyDependency = Dependency (NameTheoryMap.new ());

fun getDependency (Dependency dmap) nt =
    case NameTheoryMap.peek dmap nt of
      SOME nts => nts
    | NONE =>
      raise Bug ("Dagify.getDependency: " ^ Print.toString NameTheory.pp nt);

local
  fun add dependency (nt,acc) =
      NameTheorySet.union acc (getDependency dependency nt);
in
  fun getListDependency dependency nts =
      List.foldl (add dependency) NameTheorySet.empty nts;

  fun getSetDependency dependency nts =
      NameTheorySet.foldl (add dependency) NameTheorySet.empty nts;
end;

fun insertDependency (Dependency dmap) (nt,nts) =
    let
(*OpenTheoryDebug
      val () = if not (NameTheoryMap.inDomain nt dmap) then ()
               else raise Bug "Dagify.insertDependency"
*)
    in
      Dependency (NameTheoryMap.insert dmap (nt,nts))
    end;

fun updateDependency (Dependency dmap) (nt,nts) =
    let
(*OpenTheoryDebug
      val () = if NameTheoryMap.inDomain nt dmap then ()
               else raise Bug "Dagify.updateDependency"
*)
    in
      Dependency (NameTheoryMap.insert dmap (nt,nts))
    end;

fun foldlDependency f b (Dependency dmap) = NameTheoryMap.foldl f b dmap;

fun findlDependency p (Dependency dmap) = NameTheoryMap.findl p dmap;

local
  fun isRefl (nt,nts) = NameTheorySet.member nt nts;
in
  val findlReflexiveDependency = findlDependency isRefl;
end;

fun ppDependency (Dependency dmap) =
    NameTheoryMap.pp
      (Print.ppMap NameTheorySet.toList (Print.ppList NameTheory.pp)) dmap;

fun addTheoryDependency vanilla definitions visible generate
      (theory,dependency) =
    let
      val PackageTheory.Theory {name,imports,...} = theory

(*OpenTheoryTrace3
      val () = Print.trace PackageTheory.ppName "Dagify.addDependency.name" name
*)
    in
      if PackageTheory.isUnion theory then
        let
          fun mkNT n = getNameTheoryVanilla vanilla n

          val nt = mkNT name

          val deps = getListGenerate generate (List.map mkNT imports)
        in
          insertDependency dependency (nt,deps)
        end
      else
        let
          fun mkNT thy = NameTheory.mk (name,thy)

          fun getListGens generate thys =
              getListGenerate generate (List.map mkNT thys)

          fun getDeps dependency thy =
              getDependency dependency (mkNT thy)

          fun getListDeps dependency thys =
              getListDependency dependency (List.map mkNT thys)

          val visImps = getListVisible visible imports

          fun addVisThy (thy,(dependency,rewr)) =
              let
                val nt = mkNT thy

(*OpenTheoryTrace3
                val () = Print.trace NameTheory.pp
                           "Dagify.addDependency.addVisThy.nt" nt
*)

                val (deps,rewr) =
                    case Theory.node thy of
                      Theory.Article _ =>
                      let
                        fun pass2 ({name,prov,defs},(deps,acc)) =
                            if NameTheorySet.member name deps then (deps,acc)
                            else
                              let
                                val (req,inp) = acc

                                val (req,same) = removeSequent req prov
                              in
                                if same then (deps,acc)
                                else
                                  let
                                    val deps = NameTheorySet.add deps name

                                    val (inp,_) = removeSymbol inp defs
                                  in
                                    (deps,(req,inp))
                                  end
                              end

                        fun pass3 ({name,prov,defs},(deps,inp)) =
                            if NameTheorySet.member name deps then (deps,inp)
                            else
                              let
                                val (inp,same) = removeSymbol inp defs
                              in
                                if same then (deps,inp)
                                else
                                  let
                                    val deps = NameTheorySet.add deps name
                                  in
                                    (deps,inp)
                                  end
                              end

                        val deps = getListDeps dependency (Theory.imports thy)

                        fun pass1 ({name,prov,defs},acc) =
                            if not (NameTheorySet.member name deps) then acc
                            else
                              let
                                val (req,inp) = acc

                                val (req,_) = removeSequent req prov

                                val (inp,_) = removeSymbol inp defs
                              in
                                (req,inp)
                              end

                        val sum =
                            let
                              val art = Theory.article thy

                              val ths = Article.thms art
                            in
                              Summary.fromThms ths
                            end

                        val (req,rewr) =
                            let
                              val req = Sequents.sequents (Summary.requires sum)

                              val (req',rewr) =
                                  SequentSet.sharingRewrite req rewr

                              val req = Option.getOpt (req',req)
                            in
                              (req,rewr)
                            end

                        val inp =
                            let
                              val prov = Sequents.symbol (Summary.provides sum)

                              val {undefined = inp, defined = _} =
                                  Symbol.partitionUndef prov
                            in
                              (Symbol.typeOps inp, Symbol.consts inp)
                            end

                        val (req,inp) =
                            List.foldl pass1 (req,inp) visImps

                        val (deps,(req,inp)) =
                            List.foldl pass2 (deps,(req,inp)) visImps

                        val (deps,inp) =
                            List.foldl pass3 (deps,inp) visImps
                      in
                        (deps,rewr)
                      end
                    | Theory.Package {theories,...} =>
                      let
                        val main = Theory.mainTheory theories

                        val deps = getDeps dependency main
                      in
                        (deps,rewr)
                      end
                    | Theory.Union =>
                      let
                        val deps = getListDeps dependency (Theory.imports thy)
                      in
                        (deps,rewr)
                      end

                val dependency = insertDependency dependency (nt,deps)
              in
                (dependency,rewr)
              end

          fun addGenThy (thy,dependency) =
              let
                val nt = mkNT thy

(*OpenTheoryTrace3
                val () = Print.trace NameTheory.pp
                           "Dagify.addDependency.addGenThy.nt" nt
*)

                val deps = getDeps dependency thy

                val gens = getListGens generate (Theory.imports thy)

                val deps = NameTheorySet.union deps gens
              in
                updateDependency dependency (nt,deps)
              end

          val (graph,_,_) = getVanilla vanilla name

(*OpenTheoryTrace3
          val () = Print.trace pp "Dagify.addDependency.graph" graph
*)

          val rewr = Symbol.inst (getListDefinitions definitions imports)

          val thys = Graph.theories graph

          val (dependency,_) =
              TheorySet.foldl addVisThy (dependency,rewr) thys

          val dependency =
              TheorySet.foldl addGenThy dependency thys
        in
          dependency
        end
    end;

fun fromTheoryListDependency vanilla definitions visible generate theories =
    let
      val dependency =
          List.foldl
            (addTheoryDependency vanilla definitions visible generate)
            emptyDependency theories

(*OpenTheoryTrace3
      val () = Print.trace ppDependency
                 "Dagify.fromTheoryListDependency.dependency" dependency
*)
    in
      dependency
    end
(*OpenTheoryDebug
    handle Error err => raise Error ("Dagify.fromTheoryListDependency: " ^ err);
*)

local
  fun iterate (nt,nts,(dependency,changed)) =
      let
        val nts' = getSetDependency dependency nts

        val nts' = NameTheorySet.union nts nts'

        val same = NameTheorySet.size nts' = NameTheorySet.size nts

        val dependency =
            if same then dependency
            else updateDependency dependency (nt,nts')

        val changed = changed orelse not same
      in
        (dependency,changed)
      end;

  fun fixedPoint dependency =
      let
        val (dependency,changed) =
            foldlDependency iterate (dependency,false) dependency
      in
        if changed then fixedPoint dependency else dependency
      end;
in
  fun transitiveClosureDependency dependency =
      let
        val dependency' = fixedPoint dependency

(*OpenTheoryTrace3
        val () = Print.trace ppDependency
                   "Dagify.transitiveClosureDependency.dependency" dependency'
*)
      in
        dependency'
      end
(*OpenTheoryDebug
      handle Error err =>
        raise Error ("Dagify.transitiveClosureDependency: " ^ err);
*)
end;

fun reportCycleDependency dependency nt =
    let
      val n = NameTheory.name nt
    in
      raise Error ("theory block cycle including " ^
                   PackageTheory.toStringName n)
    end;

(* ------------------------------------------------------------------------- *)
(* Making install plans.                                                     *)
(* ------------------------------------------------------------------------- *)

fun addExportableDependency vanilla generate dependency expanded exported
      (theory,(n,acc)) =
    let
      val PackageTheory.Theory {name,imports,...} = theory

      fun mkNT thy = NameTheory.mk (name,thy)

      fun addExp stack (thy,(seen,acc)) =
          if TheorySet.member thy seen then (seen,acc)
          else
            let
              val seen = TheorySet.add seen thy

              val nt = mkNT thy

              val gens = getGenerate generate nt
            in
              if NameTheorySet.subset gens exported then (seen,acc)
              else
                let
                  val acc =
                      if NameTheorySet.member nt expanded then acc
                      else
                        let
                          val deps = getDependency dependency nt

                          val exportable = NameTheorySet.subset deps exported
                        in
                          if not exportable then acc
                          else (n,nt,stack) :: acc
                        end

                  val (seen,acc) =
                      List.foldl (addExp stack) (seen,acc) (Theory.imports thy)
                in
                  case Theory.node thy of
                    Theory.Package {theories,...} =>
                    let
                      val stack = nt :: stack

                      val main = Theory.mainTheory theories
                    in
                      addExp stack (main,(seen,acc))
                    end
                  | _ => (seen,acc)
                end
            end

      val stack = []
      and seen = TheorySet.empty

      val (_,main,_) = getVanilla vanilla name

      val (_,acc) = addExp stack (main,(seen,acc))

      val n = n + 1
    in
      (n,acc)
    end;

fun exportablePlan vanilla generate dependency expanded exported =
    let
      fun addExpTheory (theory,(n,exp)) =
          let
            val PackageTheory.Theory {name,imports,...} = theory

            fun mkNT thy = NameTheory.mk (name,thy)

            fun addExpThy stack (thy,(seen,exp)) =
                if TheorySet.member thy seen then (seen,exp)
                else
                  let
                    val seen = TheorySet.add seen thy

                    val nt = mkNT thy

                    val gens = getGenerate generate nt
                  in
                    if NameTheorySet.subset gens exported then (seen,exp)
                    else
                      let
                        val exp =
                            if NameTheorySet.member nt expanded then exp
                            else
                              let
                                val deps = getDependency dependency nt

                                val exportable =
                                    NameTheorySet.subset deps exported
                              in
                                if not exportable then exp
                                else (nt,stack,n) :: exp
                              end

                        val imps = Theory.imports thy

                        val (seen,exp) =
                            List.foldl (addExpThy stack) (seen,exp) imps
                      in
                        case Theory.node thy of
                          Theory.Package {theories,...} =>
                          let
                            val stack = nt :: stack

                            val main = Theory.mainTheory theories
                          in
                            addExpThy stack (main,(seen,exp))
                          end
                        | _ => (seen,exp)
                      end
                  end

            val stack = []
            and seen = TheorySet.empty

            val (_,main,_) = getVanilla vanilla name

            val (_,exp) = addExpThy stack (main,(seen,exp))

            val n = n + 1
          in
            (n,exp)
          end
    in
      fn theories =>
         let
           val (_,exp) = List.foldl addExpTheory (0,[]) theories
         in
           exp
         end
    end;

fun addUnionsPlan vanilla dependency expanded exported =
    let
      fun isNew news nt = NameTheorySet.member nt news

      fun isExp nt = NameTheorySet.member nt exported

      fun addTheory (theory,news_plan) =
          let
            val PackageTheory.Theory {name,imports,...} = theory
          in
            if PackageTheory.isUnion theory then
              let
                fun mkNT n = getNameTheoryVanilla vanilla n

                val (news,plan) = news_plan

                val nt = mkNT name

                val imps = List.map mkNT imports

                val deps = getDependency dependency nt

                val ok =
                    not (NameTheorySet.member nt news) andalso
                    List.exists (isNew news) imps andalso
                    NameTheorySet.all isExp deps
              in
                if not ok then news_plan
                else
                  let
                    val news = NameTheorySet.add news nt

                    val plan = (nt,[]) :: plan
                  in
                    (news,plan)
                  end
              end
            else
              let
                fun mkNT thy = NameTheory.mk (name,thy)

                fun addThy stack (thy,(seen,news,plan)) =
                    if TheorySet.member thy seen then (seen,news,plan)
                    else
                      let
                        val seen = TheorySet.add seen thy

                        val nt = mkNT thy

                        val imps = Theory.imports thy

                        val (seen,news,plan) =
                            List.foldl (addThy stack) (seen,news,plan) imps
                      in
                        case Theory.node thy of
                          Theory.Article _ => (seen,news,plan)
                        | Theory.Package {theories,...} =>
                          if not (NameTheorySet.member nt expanded) then
                            (seen,news,plan)
                          else
                            let
                              val stack = nt :: stack

                              val main = Theory.mainTheory theories

                              val (seen,news,plan) =
                                  addThy stack (main,(seen,news,plan))

                              val main = mkNT main

                              val ok =
                                  NameTheorySet.member main news andalso
                                  not (NameTheorySet.member nt news)

                              val news =
                                  if not ok then news
                                  else NameTheorySet.add news nt
                            in
                              (seen,news,plan)
                            end
                        | Theory.Union =>
                          let
                            val imps = List.map mkNT imps

                            val deps = getDependency dependency nt

                            val ok =
                                not (NameTheorySet.member nt news) andalso
                                List.exists (isNew news) imps andalso
                                NameTheorySet.all isExp deps
                          in
                            if not ok then (seen,news,plan)
                            else
                              let
                                val news = NameTheorySet.add news nt

                                val plan = (nt,stack) :: plan
                              in
                                (seen,news,plan)
                              end
                          end
                      end

                val stack = []
                and seen = TheorySet.empty
                and (news,plan) = news_plan

                val (_,main,_) = getVanilla vanilla name

                val (_,news,plan) = addThy stack (main,(seen,news,plan))
              in
                (news,plan)
              end
          end

      fun pass news plan theories =
          let
            val (news',plan) = List.foldl addTheory (news,plan) theories

            val changed = NameTheorySet.size news' > NameTheorySet.size news
          in
            if changed then pass news' plan theories else plan
          end
    in
      pass
    end;

val ppPlan =
    Print.ppList (Print.ppPair NameTheory.pp (Print.ppList NameTheory.pp));

local
  fun unexpanded expanded nt = not (NameTheorySet.member nt expanded);

  fun score expanded (nt,stack,n) =
      let
        val d = List.length (List.filter (unexpanded expanded) stack)
      in
        ((d,n),(nt,stack))
      end;

  fun compare (((d1,n1),_),((d2,n2),_)) =
      case Int.compare (d1,d2) of
        LESS => LESS
      | EQUAL => Int.compare (n1,n2)
      | GREATER => GREATER;
in
  fun fromTheoryListPlan vanilla generate dependency theories =
      let
        fun mkPlan expanded exported plan =
            let
              val exp =
                  exportablePlan vanilla generate dependency
                    expanded exported theories
            in
              if null exp then rev plan
              else
                let
                  val exp = List.map (score expanded) exp

                  val ((_,(nt,stack)),_) = minimum compare exp

                  val expanded = NameTheorySet.addList expanded stack

                  val gens = getGenerate generate nt

                  val exported = NameTheorySet.union exported gens

                  val plan = (nt,stack) :: plan

                  val plan =
                      addUnionsPlan vanilla dependency expanded exported
                        (NameTheorySet.singleton nt) plan theories
                in
                  mkPlan expanded exported plan
                end
            end

        val expanded = NameTheorySet.empty
        and exported = NameTheorySet.empty
        and plan = []

        val plan = mkPlan expanded exported plan

(*OpenTheoryTrace3
        val () = Print.trace ppPlan "Dagify.fromTheoryListPlan.plan" plan
*)
      in
        plan
      end
(*OpenTheoryDebug
      handle Error err =>
        raise Error ("Dagify.fromTheoryListPlan: " ^ err);
*)
end;

local
  fun mkNameSeg nt t =
      let
        val thy = NameTheory.theory nt

        val n =
            case Theory.node thy of
              Theory.Package {theories,...} =>
              let
                fun isT (_,t') = Theory.equal t t'
              in
                case List.find isT theories of
                  SOME (n,_) => n
                | NONE => raise Error "mkName.mkSeg: where is t?"
              end
            | _ => raise Error "mkName.mkSeg: not a package"
      in
        (n,thy)
      end;

  fun mkName nt stack names =
      let
        val t = NameTheory.theory nt

        val (ns,_) = maps mkNameSeg stack t

        val ns = List.filter (not o PackageTheory.isMainName) (rev ns)

        val n = NameTheory.name nt

        val n = PackageBase.concat (n :: ns)
      in
        PackageTheory.mkName {avoid = names} n
      end;

  fun mkImports generate dependency nt namel exported =
      let
        fun addImp (imp,acc) =
            let
              val n = NameTheoryMap.get exported imp
            in
              PackageBaseSet.add acc n
            end

        val imps =
            if NameTheory.isUnion nt then getGenerate generate nt
            else getDependency dependency nt

        val imps = NameTheorySet.foldl addImp PackageBaseSet.empty imps

        fun isImp n = PackageBaseSet.member n imps
      in
        rev (List.filter isImp namel)
      end;

  fun mkNode nt =
      Theory.toPackageTheoryNode (Theory.node (NameTheory.theory nt));

  fun addExported generate nt name =
      let
        fun addGen (gen,exported) = NameTheoryMap.insert exported (gen,name)

        val gens = getGenerate generate nt
      in
        fn exported => NameTheorySet.foldl addGen exported gens
      end;
in
  fun toTheoryListPlan vanilla generate dependency =
      let
        fun mkTheory (nt,stack) (namel,names,exported) =
          let
            val name = mkName nt stack names

            val imports = mkImports generate dependency nt namel exported

            val node = mkNode nt

            val theory =
                PackageTheory.Theory
                  {name = name,
                   imports = imports,
                   node = node}

            val namel = name :: namel

            val names = PackageBaseSet.add names name

            val exported = addExported generate nt name exported
          in
            (theory,(namel,names,exported))
          end

      val namel = []
      and names = PackageBaseSet.empty
      and exported = NameTheoryMap.new ()
    in
      fn plan =>
         let
           val (theories,state) = maps mkTheory plan (namel,names,exported)
         in
           if List.exists PackageTheory.isMain theories then theories
           else
             let
               val main = getMainNameTheoryVanilla vanilla

               val (theory,_) = mkTheory (main,[]) state
             in
               theories @ [theory]
             end
         end
    end
(*OpenTheoryDebug
    handle Error err =>
      raise Error ("Dagify.toTheoryListPlan: " ^ err);
*)
end;

(* ------------------------------------------------------------------------- *)
(* Unwind mutually recursive theory packages.                                *)
(* ------------------------------------------------------------------------- *)

fun unwind theoryInfo =
    let
      val Theories
            {importer,
             directory,
             theories,
             vanilla,
             definitions} = theoryInfo

      (* Precisely compute dependencies between theory blocks *)

      val theories' = PackageTheory.sortUnion theories

      val visible = fromListVisible vanilla definitions theories'

      val generate = fromTheoryListGenerate vanilla theories'

      val dependency =
          fromTheoryListDependency
            vanilla definitions visible generate theories'

      (* Untangle any theory block cycles *)

      val () =
          let
            val dep = transitiveClosureDependency dependency
          in
            case findlReflexiveDependency dep of
              NONE => ()
            | SOME (nt,_) => reportCycleDependency dependency nt
          end

      val plan = fromTheoryListPlan vanilla generate dependency theories

      val theories = toTheoryListPlan vanilla generate dependency plan

(*OpenTheoryTrace3
      val () =
          Print.trace PackageTheory.ppList "Dagify.unwind.theories" theories
*)

      val info =
          {importer = importer,
           directory = directory,
           theories = theories}
    in
      removeDead false info
    end
(*OpenTheoryDebug
    handle Error err => raise Error ("Dagify.unwind: " ^ err);
*)

end
