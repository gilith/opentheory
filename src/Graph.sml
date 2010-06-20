(* ========================================================================= *)
(* HIGHER ORDER LOGIC THEORY GRAPHS                                          *)
(* Copyright (c) 2009 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

structure Graph :> Graph =
struct

open Useful;

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
          case Theory.package thy of
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

fun match graph spec =
    let
      val {imports = imp,
           interpretation = int,
           package = pkg} = spec

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
      TheorySet.filter matchThy (lookup graph pkg)
    end;

(* ------------------------------------------------------------------------- *)
(* Importing theory packages.                                                *)
(* ------------------------------------------------------------------------- *)

fun importTheory graph info =
    let
      val {simulations,
           finder,
           directory,
           imports,
           interpretation,
           environment,
           theory} = info

      fun addImp (imp,acc) =
          case PackageBaseMap.peek environment imp of
            SOME thy => TheorySet.add acc thy
          | NONE => raise Error ("unknown theory import: " ^
                                 PackageBase.toString imp)

      val imports = List.foldl addImp imports (PackageTheory.imports theory)

      val node = PackageTheory.node theory

      val info =
          {simulations = simulations,
           finder = finder,
           directory = directory,
           imports = imports,
           interpretation = interpretation,
           node = node}
    in
      importNode graph info
    end

and importNode graph info =
    let
      val {simulations,
           finder,
           directory,
           imports,
           interpretation,
           node} = info

      val (graph,node,article) =
          case node of
            PackageTheory.Article {interpretation = int, filename = f} =>
            let
              val savable = savable graph

              val known = TheorySet.toArticle imports

              val interpretation = Interpretation.compose int interpretation

              val filename = OS.Path.joinDirFile {dir = directory, file = f}

              val node =
                  Theory.Article
                    {interpretation = interpretation,
                     filename = filename}

              val article =
                  Article.fromTextFile
                    {savable = savable,
                     known = known,
                     simulations = simulations,
                     interpretation = interpretation,
                     filename = filename}
            in
              (graph,node,article)
            end
          | PackageTheory.Package {interpretation = int, package = pkg} =>
            let
              val interpretation = Interpretation.compose int interpretation

              val info =
                  {simulations = simulations,
                   finder = finder,
                   imports = imports,
                   interpretation = interpretation,
                   package = pkg}

              val (graph,theory) = importPackageName graph info

              val node =
                  Theory.Package
                    {interpretation = interpretation,
                     package = pkg,
                     theory = theory}

              val article = Theory.article theory
            in
              (graph,node,article)
            end
          | PackageTheory.Union =>
            let
              val node = Theory.Union

              val article = TheorySet.toArticle imports
            in
              (graph,node,article)
            end

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

and importPackageName graph info =
    let
      val {simulations,
           finder,
           imports,
           interpretation,
           package = pkg} = info

      val spec =
          {imports = imports,
           interpretation = interpretation,
           package = pkg}

      val thys = match graph spec
    in
      if not (TheorySet.null thys) then (graph, TheorySet.pick thys)
      else
        let
          val package =
              case PackageFinder.find finder pkg of
                SOME p => p
              | NONE =>
                raise Error ("couldn't find package " ^ PackageName.toString pkg)

          val info =
              {simulations = simulations,
               finder = finder,
               imports = imports,
               interpretation = interpretation,
               package = package}
        in
          importPackageInfo graph info
          handle Error err =>
            raise Error ("while importing package " ^
                         PackageName.toString pkg ^ "\n" ^ err)
        end
    end

and importPackageInfo graph info =
    let
      val {simulations,
           finder,
           imports,
           interpretation,
           package = pkg} = info

      val {directory} = PackageInfo.directory pkg

      val pkg = PackageInfo.package pkg

      val info =
          {simulations = simulations,
           finder = finder,
           directory = directory,
           imports = imports,
           interpretation = interpretation,
           package = pkg}
    in
      importPackage graph info
    end

and importPackage graph info =
    let
      val {simulations,
           finder,
           directory,
           imports,
           interpretation,
           package = pkg} = info

      val theories = Package.theories pkg

      val info =
          {simulations = simulations,
           finder = finder,
           directory = directory,
           imports = imports,
           interpretation = interpretation,
           theories = theories}

      val (graph,env) = importTheories graph info
    in
      case List.filter PackageTheory.isMain theories of
        [] => raise Error "no main theory"
      | [theory] =>
        let
          val name = PackageTheory.name theory

          val thy =
              case PackageBaseMap.peek env name of
                SOME thy => thy
              | NONE => raise Bug "main theory vanished"
        in
          (graph,thy)
        end
      | _ :: _ :: _ => raise Error "multiple main theories"
    end

and importTheories graph info =
    let
      val {simulations,
           finder,
           directory,
           imports,
           interpretation,
           theories} = info

      fun impThy (theory,(graph,env)) =
          let
            val info =
                {simulations = simulations,
                 finder = finder,
                 directory = directory,
                 imports = imports,
                 interpretation = interpretation,
                 environment = env,
                 theory = theory}

            val (graph,thy) = importTheory graph info

            val name = PackageTheory.name theory

            val _ = not (PackageBaseMap.inDomain name env) orelse
                    raise Error ("duplicate theory name: " ^
                                 PackageBase.toString name)

            val env = PackageBaseMap.insert env (name,thy)
          in
            (graph,env)
          end

      val env = PackageBaseMap.new ()
    in
      List.foldl impThy (graph,env) theories
    end;

(* ------------------------------------------------------------------------- *)
(* Compiling theories to package requirements.                               *)
(* ------------------------------------------------------------------------- *)

(***
local
  fun avoidName s n =
      let
        fun avoidNum i =
            let
              val ni = n ^ "-" ^ Int.toString i
            in
              if StringSet.member ni s then avoidNum (i + 1) else ni
            end
      in
        if StringSet.member n s then avoidNum 1 else n
      end;

  fun thyName thyImp thy =
      case TheoryMap.peek thyImp thy of
        NONE => raise Error "set of theory theories is not closed"
      | SOME imp => PackageRequire.name imp;

  fun add (thy,(avoid,thyImp)) =
      case Theory.package thy of
        NONE => raise Error "theory theory has no package"
      | SOME pkg =>
        let
          val name = avoidName avoid (PackageName.base pkg)

          val avoid = StringSet.add avoid name

          val imps = map (thyName thyImp) (Theory.imports thy)

          val int = Theory.interpretation thy

          val imp =
              PackageRequire.Require
                {name = name,
                 imports = imps,
                 interpretation = int,
                 package = pkg}

          val thyImp = TheoryMap.insert thyImp (thy,imp)
        in
          (avoid,thyImp)
        end;
in
  fun mkRequires thys =
      let
        val avoid = StringSet.empty
        and thyImp = TheoryMap.new ()

        val (_,thyImp) = TheorySet.foldl add (avoid,thyImp) thys
      in
        thyImp
      end
(*OpenTheoryDebug
      handle Error err => raise Error ("Graph.mkRequires: " ^ err);
*)
end;
***)

end
