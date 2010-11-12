(* ========================================================================= *)
(* HIGHER ORDER LOGIC THEORY GRAPHS                                          *)
(* Copyright (c) 2009 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

structure Graph :> Graph =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* Ancestor theories.                                                        *)
(* ------------------------------------------------------------------------- *)

fun parents thy = TheorySet.fromList (Theory.imports thy);

local
  fun ancsThy acc thy thys =
      if TheorySet.member thy acc then ancsList acc thys
      else ancsPar (TheorySet.add acc thy) thy thys

  and ancsPar acc thy thys =
      ancsList acc (Theory.imports thy @ thys)

  and ancsList acc thys =
      case thys of
        [] => acc
      | thy :: thys => ancsThy acc thy thys;
in
  fun ancestors thy = ancsPar TheorySet.empty thy [];
end;

local
  fun primsList acc thys = List.foldl primsThy acc thys

  and primsThy (thy,acc) =
      if Theory.isPrimitive thy then TheorySet.add acc thy
      else primsNode acc (Theory.node thy)

  and primsNode acc node =
      case node of
        Theory.Article _ => raise Bug "Graph.primitives: Article"
      | Theory.Package {theories,...} => primsList acc theories
      | Theory.Union => acc;
in
  fun primitives thy =
      let
(*OpenTheoryDebug
        val _ = not (Theory.isUnion thy) orelse
                raise Bug "Graph.primitives: Union"
*)
      in
        primsThy (thy,TheorySet.empty)
      end;
end;

local
  fun primsList acc thys = List.foldl primsThy acc thys

  and primsThy (thy,acc) =
      if Theory.isPrimitive thy then TheorySet.add acc thy
      else
        let
          val Theory.Theory' {imports,node,...} = Theory.dest thy
        in
          case node of
            Theory.Article _ => raise Bug "Graph.visiblePrimitives: Article"
          | Theory.Package {main,...} => primsThy (main,acc)
          | Theory.Union => primsList acc imports
        end;
in
  fun visiblePrimitives thy =
      let
(*OpenTheoryDebug
        val _ = not (Theory.isUnion thy) orelse
                raise Bug "Graph.visiblePrimitives: Union"
*)
      in
        primsThy (thy,TheorySet.empty)
      end;
end;

(* ------------------------------------------------------------------------- *)
(* Theory environments.                                                      *)
(* ------------------------------------------------------------------------- *)

datatype environment =
    Environment of
      {named : Theory.theory PackageBaseMap.map,
       imported : Theory.theory list};

val emptyEnvironment =
    let
      val named = PackageBaseMap.new ()
      and imported = []
    in
      Environment
        {named = named,
         imported = imported}
    end;

fun peekEnvironment (Environment {named,...}) name =
    PackageBaseMap.peek named name;

fun insertEnvironment env (name,thy) =
    let
      val Environment {named,imported} = env

      val () =
          if not (PackageBaseMap.inDomain name named) then ()
          else raise Error ("duplicate theory name: " ^
                            PackageBase.toString name)

      val named = PackageBaseMap.insert named (name,thy)

      val imported = thy :: imported
    in
      Environment
        {named = named,
         imported = imported}
    end;

fun theoriesEnvironment (Environment {imported,...}) = rev imported;

fun mainEnvironment (Environment {named,...}) =
    case PackageBaseMap.peek named PackageTheory.mainName of
      SOME thy => thy
    | NONE => raise Error "no main theory";

(***
(* ------------------------------------------------------------------------- *)
(* Packaging theories.                                                       *)
(* ------------------------------------------------------------------------- *)

fun packageTheory {expand} =
    let
      fun convert pkg thy (avoid,cache,theories) =
          case TheoryMap.peek cache thy of
            SOME name => (name,(avoid,cache,theories))
          | NONE =>
            let
              val (name,(avoid,cache,theories)) =
                  convert' pkg thy (avoid,cache,theories)

              val cache = TheoryMap.insert cache (thy,name)
            in
              (name,(avoid,cache,theories))
            end

      and convert' pkg thy (avoid,cache,theories) =
          if expand thy then
            case Theory.node thy of
              Theory.Package {package,main,...} =>
              let
                val pkg = PackageName.base package
              in
                convert pkg main (avoid,cache,theories)
              end
            | _ => raise Error "cannot expand a non-Package node"
          else
            let
              val imports = Theory.imports thy

              val (imports,(avoid,cache,theories)) =
                  maps (convert pkg) imports (avoid,cache,theories)

              val pkg =
                  case Theory.node thy of
                    Theory.Package {package,...} => PackageName.base package
                  | _ => pkg

              val name = PackageTheory.mkName {avoid = avoid} pkg

              val avoid = PackageBaseSet.add avoid name

              val node =
                  case Theory.node thy of
                    Theory.Article {interpretation,filename} =>
                    PackageTheory.Article
                      {interpretation = interpretation,
                       filename = filename}
                  | Theory.Package {interpretation,package,...} =>
                    PackageTheory.Package
                      {interpretation = interpretation,
                       package = package}
                  | Theory.Union =>
                    PackageTheory.Union

              val theory =
                  PackageTheory.Theory
                    {name = name,
                     imports = imports,
                     node = node}

              val theories = theory :: theories
            in
              (name,(avoid,cache,theories))
            end

      val pkg = PackageTheory.mainName

      val avoid : PackageBaseSet.set = PackageBaseSet.singleton pkg

      val cache : PackageTheory.name TheoryMap.map = TheoryMap.new ()

      val theories : PackageTheory.theory list = []
    in
      fn thy =>
         let
           val (name',(_,cache,theories)) =
               convert' pkg thy (avoid,cache,theories)

(*OpenTheoryTrace3
           val () = Print.trace (TheoryMap.pp PackageTheory.ppName)
                      "Graph.packageTheory" cache
*)

           val theories =
               case theories of
                 [] => raise Error "no theories compiled"
               | theory :: theories =>
                 let
                   val PackageTheory.Theory {name,imports,node} = theory

(*OpenTheoryDebug
                   val _ = PackageBase.equal name name' orelse
                           raise Error "wrong name of compiled theory"
*)

                   val theory =
                       PackageTheory.Theory
                         {name = pkg,
                          imports = imports,
                          node = node}
                 in
                   theory :: theories
                 end

           val theories = rev theories
         in
           theories
         end
    end
(*OpenTheoryDebug
    handle Error err => raise Error ("Graph.packageTheory: " ^ err);
*)
***)

(* ------------------------------------------------------------------------- *)
(* A type of theory graphs.                                                  *)
(* ------------------------------------------------------------------------- *)

datatype graph =
    Graph of
      {savable : bool,
       theories : TheorySet.set,
       packages : TheorySet.set PackageNameMap.map};

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

fun empty {savable} =
    let
      val theories = TheorySet.empty

      val packages = PackageNameMap.new ()
    in
      Graph
        {savable = savable,
         theories = theories,
         packages = packages}
    end;

fun savable (Graph {savable = x, ...}) = x;

fun theories (Graph {theories = x, ...}) = x;

fun member thy graph = TheorySet.member thy (theories graph);

(* ------------------------------------------------------------------------- *)
(* Looking up theories by package name.                                      *)
(* ------------------------------------------------------------------------- *)

fun lookupPackages packages package =
    Option.getOpt (PackageNameMap.peek packages package, TheorySet.empty);

fun lookup (Graph {packages,...}) package =
    lookupPackages packages package;

(* ------------------------------------------------------------------------- *)
(* Adding theories.                                                          *)
(* ------------------------------------------------------------------------- *)

fun add graph thy =
    let
(*OpenTheoryDebug
      val thys = parents thy

      val _ = TheorySet.all (fn i => member i graph) thys orelse
              raise Bug "Graph.add: parent theory not in graph"
*)

      val Graph {savable,theories,packages} = graph

(*OpenTheoryDebug
      val sav = Article.savable (Theory.article thy)

      val _ = sav orelse not savable orelse
              raise Bug "Graph.add: adding unsavable theory to savable graph"
*)

      val theories = TheorySet.add theories thy

      val packages =
          case Theory.destPackage thy of
            NONE => packages
          | SOME p =>
            let
              val s = lookupPackages packages p

              val s = TheorySet.add s thy
            in
              PackageNameMap.insert packages (p,s)
            end
    in
      Graph
        {savable = savable,
         theories = theories,
         packages = packages}
    end;

(* ------------------------------------------------------------------------- *)
(* Finding matching theories.                                                *)
(* ------------------------------------------------------------------------- *)

datatype specification =
    Specification of
      {imports : TheorySet.set,
       interpretation : Interpretation.interpretation,
       name : PackageName.name}

fun match graph spec =
    let
      val Specification {imports = imp, interpretation = int, name} = spec

      fun matchImp thy =
          let
            val imp' = TheorySet.fromList (Theory.imports thy)
          in
            TheorySet.equal imp imp'
          end

      fun matchInt thy =
          case Theory.node thy of
            Theory.Package {interpretation = int', ...} =>
            Interpretation.equal int int'
          | _ => raise Bug "Graph.match.matchInt: theory not a Package"

      fun matchThy thy =
          matchImp thy andalso
          matchInt thy
    in
      TheorySet.filter matchThy (lookup graph name)
    end;

(* ------------------------------------------------------------------------- *)
(* An importer is used to import theory packages into a graph.               *)
(* ------------------------------------------------------------------------- *)

datatype importer =
    Importer of (graph -> specification -> graph * Theory.theory);

fun applyImporter (Importer f) graph spec = f graph spec;

(* ------------------------------------------------------------------------- *)
(* Importing theory packages.                                                *)
(* ------------------------------------------------------------------------- *)

fun importNode importer graph info =
    let
      val {directory,imports,interpretation,node} = info
    in
      case node of
          PackageTheory.Article {interpretation = int, filename = f} =>
          let
            val savable = savable graph

            val import = TheorySet.toArticle imports

            val interpretation = Interpretation.compose int interpretation

            val filename = OS.Path.concat (directory,f)

            val node =
                Theory.Article
                  {interpretation = interpretation,
                   filename = filename}

            val article =
                Article.fromTextFile
                  {savable = savable,
                   import = import,
                   interpretation = interpretation,
                   filename = filename}

            val imports = TheorySet.toList imports

            val thy' =
                Theory.Theory'
                  {imports = imports,
                   node = node,
                   article = article}

            val thy = Theory.mk thy'
          in
            (graph,thy)
          end
        | PackageTheory.Package {interpretation = int, package = name} =>
          let
            val interpretation = Interpretation.compose int interpretation

            val spec =
                Specification
                  {imports = imports,
                   interpretation = interpretation,
                   name = name}
          in
            applyImporter importer graph spec
          end
        | PackageTheory.Union =>
          let
            val node = Theory.Union

            val article = TheorySet.toArticle imports

            val imports = TheorySet.toList imports

            val thy' =
                Theory.Theory'
                  {imports = imports,
                   node = node,
                   article = article}

            val thy = Theory.mk thy'
          in
            (graph,thy)
          end
    end;

fun importTheory importer graph env info =
    let
      val {directory,imports,interpretation,theory} = info

      val PackageTheory.Theory {name, imports = imps, node} = theory

      fun addImp (imp,acc) =
          case peekEnvironment env imp of
            SOME thy => TheorySet.add acc thy
          | NONE => raise Error ("unknown theory import: " ^
                                 PackageTheory.toStringName imp)

      val imports = List.foldl addImp imports (PackageTheory.imports theory)

      val info =
          {directory = directory,
           imports = imports,
           interpretation = interpretation,
           node = node}

      val (graph,thy) = importNode importer graph info

      val env = insertEnvironment env (name,thy)
    in
      (graph,env,thy)
    end;

fun importTheories importer graph info =
    let
      val {directory,imports,interpretation,theories} = info

      fun impThy (theory,(graph,env)) =
          let
            val info =
                {directory = directory,
                 imports = imports,
                 interpretation = interpretation,
                 theory = theory}

            val (graph,env,_) = importTheory importer graph env info
          in
            (graph,env)
          end

      val env = emptyEnvironment
    in
      List.foldl impThy (graph,env) theories
    end;

fun importPackage importer graph info =
    let
      val {directory, imports, interpretation, name, package = pkg} = info

      val theories = Package.theories pkg

      val info =
          {directory = directory,
           imports = imports,
           interpretation = interpretation,
           theories = theories}

      val (graph,env) = importTheories importer graph info

      val main = mainEnvironment env

      val node =
          Theory.Package
            {interpretation = interpretation,
             package = name,
             theories = theoriesEnvironment env,
             main = main}

      val article = Theory.article main

      val imports = TheorySet.toList imports

      val thy' =
          Theory.Theory'
              {imports = imports,
               node = node,
               article = article}

      val thy = Theory.mk thy'
    in
      (graph,thy)
    end;

fun importPackageInfo importer graph data =
    let
      val {imports,interpretation,info} = data

      val {directory} = PackageInfo.directory info
      and name = PackageInfo.name info
      and pkg = PackageInfo.package info

      val data =
          {directory = directory,
           imports = imports,
           interpretation = interpretation,
           name = name,
           package = pkg}
    in
      importPackage importer graph data
    end;

fun importPackageName finder graph spec =
    let
      val thys = match graph spec
    in
      if not (TheorySet.null thys) then (graph, TheorySet.pick thys)
      else
        let
          val importer = fromFinderImporter finder

          val Specification {imports,interpretation,name} = spec

          val info =
              case PackageFinder.find finder name of
                SOME i => i
              | NONE => raise Error ("couldn't find package " ^
                                     PackageName.toString name)

          val data =
              {imports = imports,
               interpretation = interpretation,
               info = info}
        in
          importPackageInfo importer graph data
          handle Error err =>
            raise Error ("while importing package " ^
                         PackageName.toString name ^ "\n" ^ err)
        end
    end

and fromFinderImporter finder = Importer (importPackageName finder);

(* ------------------------------------------------------------------------- *)
(* Linearize mutually recursive theory packages.                             *)
(* ------------------------------------------------------------------------- *)

(* Individual theory blocks *)

datatype vanilla =
    Vanilla of (graph * Theory.theory * Summary.summary) PackageBaseMap.map;

val emptyVanilla = Vanilla (PackageBaseMap.new ());

fun getVanilla (Vanilla vmap) name =
    case PackageBaseMap.peek vmap name of
      SOME x => x
    | NONE => raise Bug "Graph.getVanilla";

fun addVanilla importer {directory} (theory, Vanilla vmap) =
    let
      val PackageTheory.Theory {name,node,...} = theory

      val savable = PackageTheory.isArticleNode node

      val graph = empty {savable = savable}

      val imports = TheorySet.empty

      val int = Interpretation.natural

      val (graph,thy) =
          importNode importer graph
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
    handle Error err => raise Error ("Graph.fromListVanilla: " ^ err);
*)

(* Fixed point calculation of theory block definitions *)

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
      val () = Print.trace PackageTheory.ppName "Graph.addDefinitions.name" name
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
              orelse raise Bug "Graph.addDefinitions: shrinking type op defs"

      val _ = ConstSet.subset (Symbol.consts defs) (Symbol.consts defs')
              orelse raise Bug "Graph.addDefinitions: shrinking const defs"
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
          val () = Print.trace Symbol.pp "Graph.addDefinitions.defs'" defs'
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
    handle Error err => raise Error ("Graph.fromListDefinitions: " ^ err);
*)

(* Individual theory block summaries *)

datatype summary = Summary of Summary.summary PackageBaseMap.map;

val emptySummary = Summary (PackageBaseMap.new ());

fun getSummary (Summary smap) name =
    case PackageBaseMap.peek smap name of
      SOME sum => sum
    | NONE => raise Bug "Graph.getSummary";

fun addSummary vanilla definitions (theory,summary) =
    let
      val PackageTheory.Theory {name,imports,...} = theory
      and Summary smap = summary

(*OpenTheoryTrace3
      val () = Print.trace PackageTheory.ppName "Graph.addSummary.name" name
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

              val req = Sequents.sequents (Summary.requires sum)
              and prov = Sequents.sequents (Summary.provides sum)

              val idefs = getListDefinitions definitions imports

(*OpenTheoryTrace3
              val () = Print.trace Symbol.pp "Graph.addSummary.idefs" idefs
*)

              val rewr = Symbol.inst idefs

              val (req',rewr) = SequentSet.sharingRewrite req rewr

              val prov' = SequentSet.rewrite rewr prov

              val req = Sequents.fromSet (Option.getOpt (req',req))
              and prov = Sequents.fromSet (Option.getOpt (prov',prov))

              val sum' =
                  Summary.Summary'
                    {requires = req,
                     provides = prov}
            in
              Summary.mk sum'
            end

(*OpenTheoryTrace3
      val () = Print.trace Summary.pp "Graph.addSummary.sum" sum
*)
    in
      Summary (PackageBaseMap.insert smap (name,sum))
    end;

fun fromListSummary vanilla definitions theories =
    List.foldl (addSummary vanilla definitions) emptySummary theories
(*OpenTheoryDebug
    handle Error err => raise Error ("Graph.fromListSummary: " ^ err);
*)

(* Removing dead imports *)

fun removeDeadImportsTheory vanilla definitions summary theory =
    let
      val PackageTheory.Theory {name,imports,node} = theory

      fun removeDefs (ots,cs) imp =
          let
            val pdef = getDefinitions definitions imp

            fun undefT t = not (Symbol.knownTypeOp pdef (TypeOp.name t))

            fun undefC c = not (Symbol.knownConst pdef (Const.name c))

            val ots' = TypeOpSet.filter undefT ots
            and cs' = ConstSet.filter undefC cs

            val same =
                TypeOpSet.size ots' = TypeOpSet.size ots andalso
                ConstSet.size cs' = ConstSet.size cs
          in
            ((ots',cs'),same)
          end

      fun addProv (imp,(acc,req,inp)) =
          let
            val (req,reqSame) =
                let
                  val prov = Summary.provides (getSummary summary imp)

                  val pseqs = Sequents.sequents prov

                  val req' = SequentSet.difference req pseqs

                  val same = SequentSet.size req' = SequentSet.size req
                in
                  (req',same)
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

fun removeDeadImports vanilla definitions summary theories =
    List.map (removeDeadImportsTheory vanilla definitions summary) theories;

(* Removing dead imports *)

fun removeDeadBlocks theories =
    let
      fun warnDead thy =
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

(* Named theories *)

datatype nameTheory = NameTheory of PackageTheory.name * Theory.theory;

fun compareNameTheory (p1,p2) =
    let
      val NameTheory (n1,t1) = p1
      and NameTheory (n2,t2) = p2
    in
      case PackageBase.compare (n1,n2) of
        LESS => LESS
      | EQUAL => Theory.compare (t1,t2)
      | GREATER => GREATER
    end;

(* Named theory information *)

(***
datatype nameTheoryInfo =
    NameTheoryInfo of
      {primTheories : nameTheory Set.set,

    PrimTheorySummary of (primTheory,Summary.summary) Map.map;

val emptyPrimTheorySummary = PrimTheorySummary (Map.new comparePrimTheory);
***)

(* Putting it all together *)

fun linearizeTheories importer dir theories =
    let
      val vanilla = fromListVanilla importer dir theories

      val theories' = PackageTheory.sortUnion theories

      val definitions = fromListDefinitions vanilla theories'

      val summary = fromListSummary vanilla definitions theories'

      val theories = removeDeadImports vanilla definitions summary theories

      val theories = removeDeadBlocks theories
    in
      theories
    end
(*OpenTheoryDebug
    handle Error err => raise Error ("Graph.linearizeTheories: " ^ err);
*)

end
