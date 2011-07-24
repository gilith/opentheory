(* ========================================================================= *)
(* GENERATING HASKELL PROJECTS FROM THEORIES                                 *)
(* Copyright (c) 2011 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

structure Haskell :> Haskell =
struct

open Useful;

val prefix = PackageName.haskellExport;

fun getTheory name thys =
    case Theory.peekTheory name thys of
      SOME thy => thy
    | NONE =>
      let
        val err = "missing " ^ PackageName.toString name ^ " block in theory"
      in
        raise Error err
      end;

fun theories dir namever =
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

      val importer = Directory.importer dir

      val graph = Graph.empty {savable = false}

      val imps = TheorySet.empty

      val int = Interpretation.natural

      val (graph,thy) =
          Graph.importPackageInfo importer graph
            {imports = imps,
             interpretation = int,
             info = info}

      val thys =
          case Theory.node thy of
            Theory.Package {theories,...} => theories
          | _ => raise Bug "Haskell.theories: not a package theory"

      val src = getTheory PackageName.srcHaskellExport thys
    in
      {src = src}
    end;

fun export dir namever =
    let
      val {src} = theories dir namever
    in
      ()
    end;

end
