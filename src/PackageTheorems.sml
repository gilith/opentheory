(* ========================================================================= *)
(* PACKAGE THEOREMS                                                          *)
(* Copyright (c) 2011 Joe Hurd, distributed under the MIT license            *)
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

fun package' (Theorems' {package = x, ...}) = x;

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

fun package ths = package' (dest ths);

fun sequents ths = sequents' (dest ths);

fun symbol ths = Sequents.symbol (sequents ths);

fun undefined ths = Sequents.undefined (sequents ths);

fun defined ths = Sequents.defined (sequents ths);

fun existsUndefined ths = Sequents.existsUndefined (sequents ths);

fun existsDefined ths = Sequents.existsDefined (sequents ths);

fun allUndefined ths = Sequents.allUndefined (sequents ths);

fun allDefined ths = Sequents.allDefined (sequents ths);

(* ------------------------------------------------------------------------- *)
(* Theory contexts.                                                          *)
(* ------------------------------------------------------------------------- *)

(***
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
            (*** It's not redundant if it adds definitions ***)
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
***)

local
  val initialGrounded = SymbolTable.primitives
  and initialSatisfied = SequentSet.standardAxioms;

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

  fun mkSatisfy ((pkg,seq),(seqs,asms)) =
      let
        val seqs = SequentSet.union seqs seq

        val asms' = SequentSet.difference asms seqs

        val () =
            if SequentSet.size asms' < SequentSet.size asms then ()
            else
              let
                val mesg =
                    "redundant requires: " ^ PackageNameVersion.toString pkg

                val () = warn mesg
              in
                ()
              end
      in
        (seqs,asms')
      end;

  fun groundsInput inp sym =
      case sym of
        Symbol.TypeOp t => SymbolTable.knownTypeOp inp (TypeOp.name t)
      | Symbol.Const c => SymbolTable.knownConst inp (Const.name c);

  fun mkGrounded inp thl =
      case thl of
        [] => (initialGrounded,initialSatisfied,[],TermRewrite.undef)
      | th :: thl =>
        let
          val (defs,seqs,seqsl,rewr) = mkGrounded inp thl

          val def = defined th
          and seq = Sequents.sequents (sequents th)

          val defs = SymbolTable.union defs def

          val grounds =
              SymbolSet.exists (groundsInput inp) (SymbolTable.symbols def)

          val (seq',rewr) = SequentSet.sharingRewrite seq rewr

          val seq = Option.getOpt (seq',seq)
        in
          if grounds then (defs, SequentSet.union seq seqs, seqsl, rewr)
          else (defs, seqs, (package th, seq) :: seqsl, rewr)
        end;

  fun groundedInputTypeOp defs t =
      SymbolTable.knownTypeOp defs (TypeOp.name t);

  fun groundedInputConst defs c =
      SymbolTable.knownConst defs (Const.name c);

  fun satisfiedAssumption seqs seq = SequentSet.member seq seqs;

  fun mkContext inp asms thl =
      let
        val (defs,seqs,seqsl,_) = mkGrounded inp thl

        val asms = SequentSet.difference asms seqs

        val (seqs,_) = List.foldl mkSatisfy (seqs,asms) seqsl
      in
        Summary.Context
          {groundedInputTypeOp = groundedInputTypeOp defs,
           groundedInputConst = groundedInputConst defs,
           satisfiedAssumption = satisfiedAssumption seqs}
      end;
in
  (* This function must compute the correct context for the input thl; *)
  (* the inp and asms arguments may be used *only* for the purpose of *)
  (* generating warnings about redundant requirements. *)

  fun context (inp,asms) thl =
      if compatibleDefined thl then mkContext inp asms thl
      else Summary.NoContext;
end;

fun summaryContext sum =
    let
      val Summary.Summary' {requires = req, provides = prov} =
          Summary.dest sum

      val inp =
          SymbolTable.union
            (Sequents.undefined req) (Sequents.undefined prov)

      val asms = Sequents.sequents req
    in
      context (inp,asms)
    end;

fun packageSummaryContext sum =
    summaryContext (PackageSummary.summary sum);

(* ------------------------------------------------------------------------- *)
(* Testing different versions of required theories.                          *)
(* ------------------------------------------------------------------------- *)

datatype versions =
    Versions of
      {names : PackageNameSet.set,
       definedTypeOps : PackageName.name NameMap.map,
       definedConsts : PackageName.name NameMap.map,
       satisfiedBy : PackageNameSet.set SequentMap.map};

local
  fun destSequents seqs =
      let
        val sym = Sequents.symbol seqs
        and seqs = Sequents.sequents seqs

        val sym = SymbolTable.defined sym

        val seqs' = SequentSet.rewrite TermRewrite.undef seqs
      in
        (sym, Option.getOpt (seqs',seqs))
      end;

  fun destTheorems th =
      let
        val Theorems' {package = nv, sequents = seqs} = dest th

        val n = PackageNameVersion.name nv
        and (sym,seqs) = destSequents seqs
      in
        (n,sym,seqs)
      end;

  fun addTypeOps n =
      let
        fun add (ot,ots) =
            let
              val otn = TypeOp.name ot
            in
              case NameMap.peek ots otn of
                NONE => NameMap.insert ots (otn,n)
              | SOME n' =>
                if PackageName.equal n' n then ots
                else
                  let
                    val err =
                        "clashing type operator name " ^ Name.toString otn
                  in
                    raise Error err
                  end
            end
      in
        TypeOpSet.foldl add
      end;

  fun addConsts n =
      let
        fun add (c,cs) =
            let
              val cn = Const.name c
            in
              case NameMap.peek cs cn of
                NONE => NameMap.insert cs (cn,n)
              | SOME n' =>
                if PackageName.equal n' n then cs
                else
                  let
                    val err =
                        "clashing constant name " ^ Name.toString cn
                  in
                    raise Error err
                  end
            end
      in
        ConstSet.foldl add
      end;

  fun addSat n seqs (seq,ns) =
      if not (SequentSet.member seq seqs) then ns
      else PackageNameSet.add ns n;

  fun checkSat ns =
      if not (PackageNameSet.null ns) then ()
      else raise Error "unsatisfied assumption";

  fun deleteSat n seqs (seq,ns) =
      if not (PackageNameSet.member n ns) then ns
      else if SequentSet.member seq seqs then ns
      else
        let
          val ns = PackageNameSet.delete ns n

          val () = checkSat ns
        in
          ns
        end;

  fun add (th,(ns,ots,cs,sat)) =
      let
        val (n,sym,seqs) = destTheorems th

        val () =
            if not (PackageNameSet.member n ns) then ()
            else raise Error "duplicate required package name"

        val ns = PackageNameSet.add ns n
        and ots = addTypeOps n ots (SymbolTable.typeOps sym)
        and cs = addConsts n cs (SymbolTable.consts sym)
        and sat = SequentMap.map (addSat n seqs) sat
      in
        (ns,ots,cs,sat)
      end;
in
  fun mkVersions asms thl =
      let
        val ns = PackageNameSet.empty
        and ots = NameMap.new ()
        and cs = NameMap.new ()
        and sat = SequentSet.map (K PackageNameSet.empty) asms

        val (ns,ots,cs,sat) = List.foldl add (ns,ots,cs,sat) thl

        val () = SequentMap.app (checkSat o snd) sat
      in
        Versions
          {names = ns,
           definedTypeOps = ots,
           definedConsts = cs,
           satisfiedBy = sat}
      end;

  fun addVersion vs th =
      let
        val Versions
            {names = ns,
             definedTypeOps = ots,
             definedConsts = cs,
             satisfiedBy = sat} = vs

        val (n,sym,seqs) = destTheorems th

        val () =
            if PackageNameSet.member n ns then ()
            else raise Error "unknown required package name"

        val ots = addTypeOps n ots (SymbolTable.typeOps sym)
        and cs = addConsts n cs (SymbolTable.consts sym)
        and sat = SequentMap.map (deleteSat n seqs) sat
      in
        Versions
          {names = ns,
           definedTypeOps = ots,
           definedConsts = cs,
           satisfiedBy = sat}
      end;
end;

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
