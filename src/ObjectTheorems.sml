(* ========================================================================= *)
(* BRANDED THEOREM OBJECTS                                                   *)
(* Copyright (c) 2012 Joe Leslie-Hurd, distributed under the MIT license     *)
(* ========================================================================= *)

structure ObjectTheorems :> ObjectTheorems =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* A type of theorems.                                                       *)
(* ------------------------------------------------------------------------- *)

datatype theorems =
    Theorems of
      {sequents : Sequents.sequents,
       export : ObjectExport.export};

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

fun mk brand seqs =
    let
      val exp = ObjectExport.brand brand seqs

      val seqs = Sequents.fromThms (ObjectExport.toThms exp)
    in
      Theorems
        {sequents = seqs,
         export = exp}
    end;

fun sequents (Theorems {sequents = x, ...}) = x;

fun symbol ths = Sequents.symbol (sequents ths);

fun partitionUndef ths = Sequents.partitionUndef (sequents ths);

fun undefined ths = Sequents.undefined (sequents ths);

fun defined ths = Sequents.defined (sequents ths);

fun existsUndefined ths = Sequents.existsUndefined (sequents ths);

fun existsDefined ths = Sequents.existsDefined (sequents ths);

fun allUndefined ths = Sequents.allUndefined (sequents ths);

fun allDefined ths = Sequents.allDefined (sequents ths);

(* ------------------------------------------------------------------------- *)
(* Output formats.                                                           *)
(* ------------------------------------------------------------------------- *)

fun fromTextFile {filename} =
    let
      val parameters =
          {import = ObjectThms.new {savable = true},
           interpretation = Interpretation.natural,
           savable = true}

      val state =
          ObjectRead.executeTextFile
            {parameters = parameters,
             filename = filename}

      val exp = ObjectRead.export state

      val ths = ObjectThms.fromExport exp

      val seqs = Sequents.fromThms (ObjectThms.thms ths)
    in
      Theorems
        {sequents = seqs,
         export = exp}
    end;

fun toTextFile {theorems,filename} =
    let
      val Theorems {sequents = _, export = exp} = theorems

      val exp =
          case ObjectExport.compress exp of
            NONE => exp
          | SOME exp => exp

      val () =
          ObjectWrite.toTextFile
            {version = ArticleVersion.readDefault,
             export = exp,
             filename = filename}
    in
      ()
    end;

end
