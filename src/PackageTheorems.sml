(* ========================================================================= *)
(* PACKAGE THEOREMS                                                          *)
(* Copyright (c) 2011 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

structure PackageTheorems :> PackageTheorems =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* Theorems filenames.                                                       *)
(* ------------------------------------------------------------------------- *)

fun mkFilename namever =
    Article.mkFilename {base = PackageNameVersion.toString namever};

fun destFilename file =
    case Article.destFilename file of
      NONE => NONE
    | SOME {base} => total PackageNameVersion.fromString base;

fun isFilename file = Option.isSome (destFilename file);

(* ------------------------------------------------------------------------- *)
(* A type of package theorems.                                               *)
(* ------------------------------------------------------------------------- *)

datatype theorems' =
    Theorems' of
      {package : PackageNameVersion.nameVersion,
       sequents : Sequents.sequents}

datatype theorems =
    Theorems of
      {theorems' : theorems',
       export : ObjectExport.export};

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

fun sequents' (Theorems' {sequents = x, ...}) = x;

fun mk ths' =
    let
      val Theorems' {package = nv, sequents = seqs} = ths'

      val n = PackageNameVersion.toGlobal nv

      val exp = ObjectExport.brand n seqs

      val seqs = Sequents.fromThms (ObjectExport.toThms exp)

      val ths' =
          Theorems'
            {package = nv,
             sequents = seqs}
    in
      Theorems
        {theorems' = ths',
         export = exp}
    end;

fun dest (Theorems {theorems' = x, ...}) = x;

fun sequents ths = sequents' (dest ths);

fun symbol ths = Sequents.symbol (sequents ths);

fun defined ths = SymbolTable.defined (symbol ths);

(* ------------------------------------------------------------------------- *)
(* Unsatisfied assumptions.                                                  *)
(* ------------------------------------------------------------------------- *)

local
  fun unionDefined thl =
      SOME (SymbolTable.unionList (List.map defined thl))
      handle Error err =>
        let
          val mesg = "requires contain " ^ err

          val () = warn mesg
        in
          NONE
        end;

  fun compatibleDefined thl = Option.isSome (unionDefined thl);

  fun satisfy (th,(unsat,rewr)) =
      let
        val Theorems' {package = pkg, sequents = seqs} = dest th

        val seqs = Sequents.sequents seqs

        val (seqs',rewr) = SequentSet.sharingRewrite seqs rewr

        val seqs = Option.getOpt (seqs',seqs)

        val unsat' = SequentSet.difference unsat seqs

        val () =
            if SequentSet.size unsat' < SequentSet.size unsat then ()
            else
              let
                val mesg =
                    "redundant requires: " ^ PackageNameVersion.toString pkg

                val () = warn mesg
              in
                ()
              end
      in
        (unsat',rewr)
      end;

  fun removeSatisfied thl asms =
      let
        val asms = SequentSet.difference asms SequentSet.standardAxioms

        val (unsat,_) = List.foldl satisfy (asms,TermRewrite.undef) thl
      in
        unsat
      end;
in
  fun unsatisfiedAssumptions thl =
      if not (compatibleDefined thl) then NONE
      else SOME (removeSatisfied thl);
end;

(* ------------------------------------------------------------------------- *)
(* Using different versions of required packages to satisfy all assumptions. *)
(* ------------------------------------------------------------------------- *)

datatype versions =
    Versions of
      {definedTypeOps : PackageName.name NameMap.map,
       definedConsts : PackageName.name NameMap.map,
       assumptions : PackageNameSet.set SequentMap.map};

(***
fun mkVersions thl =
***)

(* ------------------------------------------------------------------------- *)
(* Output formats.                                                           *)
(* ------------------------------------------------------------------------- *)

fun fromTextFile {package = nv, filename} =
    let
      val state =
          ObjectRead.initial
            {import = ObjectThms.new {savable = true},
             interpretation = Interpretation.natural,
             savable = true}

      val state = ObjectRead.executeTextFile {filename = filename} state

      val ths = ObjectRead.thms state

      val exp = ObjectThms.toExport ths

      val seqs = Sequents.fromThms (ObjectThms.thms ths)

      val ths' =
          Theorems'
            {package = nv,
             sequents = seqs}
    in
      Theorems
        {theorems' = ths',
         export = exp}
    end;

fun toTextFile {theorems,filename} =
    let
      val Theorems {theorems' = _, export = exp} = theorems

      val exp =
          case ObjectExport.compress exp of
            NONE => exp
          | SOME exp => exp

      val () = ObjectWrite.toTextFile {export = exp, filename = filename}
    in
      ()
    end;

end
