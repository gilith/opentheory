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
  fun primsList acc thys = List.foldl primsNameThy acc thys

  and primsNameThy ((_,thy),acc) = primsThy acc thy

  and primsThy acc thy =
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
        primsThy TheorySet.empty thy
      end;
end;

local
  fun primsList seen acc thys = List.foldl primsThy (seen,acc) thys

  and primsThy (thy,(seen,acc)) =
      if TheorySet.member thy seen then (seen,acc)
      else
        let
          val seen = TheorySet.add seen thy
        in
          if Theory.isPrimitive thy then (seen, thy :: acc)
          else primsThy' seen acc (Theory.dest thy)
        end

  and primsThy' seen acc thy' =
      let
        val Theory.Theory' {imports,node,...} = thy'
      in
        case node of
          Theory.Article _ => raise Bug "Graph.visiblePrimitives: Article"
        | Theory.Package {theories,...} =>
          let
            val main = Theory.mainTheory theories
          in
            primsThy (main,(seen,acc))
          end
        | Theory.Union => primsList seen acc imports
      end;
in
  fun visiblePrimitives thy =
      let
(*OpenTheoryDebug
        val _ = not (Theory.isUnion thy) orelse
                raise Bug "Graph.visiblePrimitives: Union"
*)

        val (_,acc) = primsThy (thy,(TheorySet.empty,[]))
      in
        rev acc
      end;
end;

(* ------------------------------------------------------------------------- *)
(* Theory environments.                                                      *)
(* ------------------------------------------------------------------------- *)

datatype environment =
    Environment of
      {named : Theory.theory PackageBaseMap.map,
       imported : (PackageTheory.name * Theory.theory) list};

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

      val imported = (name,thy) :: imported
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

            val graph = add graph thy
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

            val graph = add graph thy
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

      val node =
          Theory.Package
            {interpretation = interpretation,
             package = name,
             theories = theoriesEnvironment env}

      val article = Theory.article (mainEnvironment env)

      val imports = TheorySet.toList imports

      val thy' =
          Theory.Theory'
              {imports = imports,
               node = node,
               article = article}

      val thy = Theory.mk thy'

      val graph = add graph thy
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
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

fun pp graph =
    Print.blockProgram Print.Consistent 0
      [Print.ppString "Graph{",
       Print.ppInt (TheorySet.size (theories graph)),
       Print.ppString "}"];

end
