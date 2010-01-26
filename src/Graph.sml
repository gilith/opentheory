(* ========================================================================= *)
(* HIGHER ORDER LOGIC THEORY GRAPHS                                          *)
(* Copyright (c) 2009 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

structure Graph :> Graph =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* A type of theory instances.                                               *)
(* ------------------------------------------------------------------------- *)

datatype graph =
    Graph of
      {instances : InstanceSet.set,
       packages : InstanceSet.set PackageNameMap.map};

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

val empty =
    let
      val instances = InstanceSet.empty

      val packages = PackageNameMap.new ()
    in
      Graph
        {instances = instances,
         packages = packages}
    end;

fun instances (Graph {instances = x, ...}) = x;

fun member inst graph = InstanceSet.member inst (instances graph);

(* ------------------------------------------------------------------------- *)
(* Ancestor instances.                                                       *)
(* ------------------------------------------------------------------------- *)

fun parentsList inst =
    let
      val imps = Instance.imports inst

      val thyImps = Instance.theoryImports inst
    in
      imps @ thyImps
    end;

fun parents inst = InstanceSet.fromList (parentsList inst);

local
  fun ancsInst acc inst insts =
      if InstanceSet.member inst acc then ancsList acc insts
      else ancsPar (InstanceSet.add acc inst) inst insts

  and ancsPar acc inst insts =
      ancsList acc (parentsList inst @ insts)

  and ancsList acc insts =
      case insts of
        [] => acc
      | inst :: insts => ancsInst acc inst insts;
in
  fun ancestors inst = ancsPar InstanceSet.empty inst [];
end;

(* ------------------------------------------------------------------------- *)
(* Looking up theory instances by package name.                              *)
(* ------------------------------------------------------------------------- *)

fun lookupPackages packages package =
    Option.getOpt (PackageNameMap.peek packages package, InstanceSet.empty);

fun lookup (Graph {packages,...}) package =
    lookupPackages packages package;

(* ------------------------------------------------------------------------- *)
(* Adding instances.                                                         *)
(* ------------------------------------------------------------------------- *)

fun add graph inst =
    let
(*OpenTheoryDebug
      val insts = parents inst

      val _ = InstanceSet.all (fn i => member i graph) insts orelse
              raise Bug "Graph.add: parent instance not in graph"
*)

      val Graph {instances,packages} = graph

      val instances = InstanceSet.add instances inst

      val packages =
          case Instance.package inst of
            NONE => packages
          | SOME p =>
            let
              val s = lookupPackages packages p

              val s = InstanceSet.add s inst
            in
              PackageNameMap.insert packages (p,s)
            end
    in
      Graph
        {instances = instances,
         packages = packages}
    end;

(* ------------------------------------------------------------------------- *)
(* Finding matching theory instances.                                        *)
(* ------------------------------------------------------------------------- *)

fun match graph spec =
    let
      val {savable = sav,
           importsAtLeast = req,
           interpretationEquivalentTo = int,
           package = pkg} = spec

      fun matchSav inst =
          not sav orelse
          let
            val art = Instance.article inst
          in
            Article.savable art
          end

      fun matchReq inst =
          let
            val req' = InstanceSet.importsInstance inst
          in
            InstanceSet.subset req req'
          end

      fun matchInt inst =
          let
            val int' = Instance.interpretation inst

            val sum = Instance.summary inst

            val prov = Summary.provides sum

            val sym = Context.symbols prov
          in
            Interpretation.restrictEqual sym int int'
          end

      fun matchInst inst =
          matchSav inst andalso
          matchReq inst andalso
          matchInt inst
    in
      InstanceSet.filter matchInst (lookup graph pkg)
    end;

(* ------------------------------------------------------------------------- *)
(* Importing theory packages.                                                *)
(* ------------------------------------------------------------------------- *)

fun importTheory graph info =
    let
      val {savable,
           imports = req,
           simulations,
           importToInstance,
           interpretation = int,
           package = pkg,
           directory = dir,
           theory = thy} = info

      val req = InstanceSet.toList req

      val inst =
          Instance.fromTheory
            {savable = savable,
             imports = req,
             simulations = simulations,
             importToInstance = importToInstance,
             interpretation = int,
             directory = dir,
             package = pkg,
             theory = thy}

      val graph = add graph inst
    in
      (graph,inst)
    end;

fun matchImportPackageName graph info =
    let
      val {finder,
           savable,
           simulations,
           importsAtLeast = req,
           interpretationEquivalentTo = int,
           package = pkg} = info

      val matchInfo =
          {savable = savable,
           importsAtLeast = req,
           interpretationEquivalentTo = int,
           package = pkg}

      val insts = match graph matchInfo
    in
      if not (InstanceSet.null insts) then (graph, InstanceSet.pick insts)
      else
        let
          val info =
              {finder = finder,
               savable = savable,
               simulations = simulations,
               imports = req,
               interpretation = int,
               package = pkg}
        in
          importPackageName graph info
        end
    end

and importPackageName graph info =
    let
      val {finder,
           savable,
           simulations,
           imports = req,
           interpretation = int,
           package = pkg} = info

      val pkg =
          case PackageFinder.find finder pkg of
            SOME p => p
          | NONE =>
            raise Error ("couldn't find package " ^ PackageName.toString pkg)

      val info =
          {finder = finder,
           savable = savable,
           simulations = simulations,
           imports = req,
           interpretation = int,
           package = pkg}
    in
      importPackage graph info
    end

and importPackage graph info =
    let
      val {finder,
           savable,
           simulations,
           imports = req,
           interpretation = int,
           package = pkg} = info

      val Package.Package {name,directory,contents,...} = pkg

      val info =
          {finder = finder,
           savable = savable,
           simulations = simulations,
           imports = req,
           interpretation = int,
           package = name,
           directory = directory,
           contents = contents}
    in
      importContents graph info
    end
    handle Error err =>
      let
        val {package = pkg, ...} = info
        val Package.Package {name,...} = pkg

        val err =
            case name of
              NONE =>
(*OpenTheoryDebug
              "while importing unnamed package:\n" ^
*)
              err
            | SOME n =>
              "while importing package " ^ PackageName.toString n ^ ":\n" ^ err

(*OpenTheoryDebug
        val err = "Graph.importPackage: " ^ err
*)
      in
        raise Error err
      end

and importContents graph info =
    let
      val {finder,
           savable,
           simulations,
           imports = req,
           interpretation = int,
           package = pkg,
           directory = dir,
           contents} = info

      val PackageContents.Contents {requires,theory,...} = contents

      fun getRequire reqInsts r =
          case StringMap.peek reqInsts r of
            SOME inst => inst
          | NONE => raise Error ("unknown require block name: " ^ r)

      fun importReq (require,(graph,reqInsts)) =
          let
            val reqToInst = getRequire reqInsts

            val info =
                {finder = finder,
                 savable = savable,
                 simulations = simulations,
                 imports = req,
                 interpretation = int,
                 requireNameToInstance = reqToInst,
                 require = require}

            val (graph,inst) = importRequire graph info

            val name = PackageRequire.name require

            val reqInsts = StringMap.insert reqInsts (name,inst)
          in
            (graph,reqInsts)
          end

      val (graph,reqInsts) =
          List.foldl importReq (graph, StringMap.new ()) requires

      val impToInst = getRequire reqInsts

      val info =
          {savable = savable,
           simulations = simulations,
           imports = req,
           interpretation = int,
           package = pkg,
           directory = dir,
           importToInstance = impToInst,
           theory = theory}
    in
      importTheory graph info
    end

and importRequire graph info =
    let
      val {finder,
           savable,
           simulations,
           imports = imps,
           interpretation = int,
           requireNameToInstance = reqToInst,
           require} = info

      val PackageRequire.Require
            {name = _,
             imports = reqs,
             interpretation = reqInt,
             package = pkg} = require

      val importsAtLeast =
          let
            fun add (r,s) = InstanceSet.add s (reqToInst r)
          in
            List.foldl add imps reqs
          end

      val interpretationEquivalentTo = Interpretation.compose reqInt int

      val info =
          {finder = finder,
           savable = savable,
           simulations = simulations,
           importsAtLeast = importsAtLeast,
           interpretationEquivalentTo = interpretationEquivalentTo,
           package = pkg}
    in
      matchImportPackageName graph info
    end;

(* ------------------------------------------------------------------------- *)
(* Compiling instances to package requirements.                              *)
(* ------------------------------------------------------------------------- *)

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

  fun instName instReq inst =
      case InstanceMap.peek instReq inst of
        NONE => raise Error "set of theory instances is not closed"
      | SOME req => PackageRequire.name req;

  fun add (inst,(avoid,instReq)) =
      case Instance.package inst of
        NONE => raise Error "theory instance has no package"
      | SOME pkg =>
        let
          val name = avoidName avoid (PackageName.base pkg)

          val avoid = StringSet.add avoid name

          val imps = map (instName instReq) (Instance.imports inst)

          val int = Instance.interpretation inst

          val req =
              PackageRequire.Require
                {name = name,
                 imports = imps,
                 interpretation = int,
                 package = pkg}

          val instReq = InstanceMap.insert instReq (inst,req)
        in
          (avoid,instReq)
        end;
in
  fun mkRequires insts =
      let
        val avoid = StringSet.empty
        and instReq = InstanceMap.new ()

        val (_,instReq) = InstanceSet.foldl add (avoid,instReq) insts
      in
        instReq
      end
(*OpenTheoryDebug
      handle Error err => raise Error ("Graph.mkRequires: " ^ err);
*)
end;

end
