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

fun partitionUndef ths = Sequents.partitionUndef (sequents ths);

fun undefined ths = Sequents.undefined (sequents ths);

fun defined ths = Sequents.defined (sequents ths);

fun existsUndefined ths = Sequents.existsUndefined (sequents ths);

fun existsDefined ths = Sequents.existsDefined (sequents ths);

fun allUndefined ths = Sequents.allUndefined (sequents ths);

fun allDefined ths = Sequents.allDefined (sequents ths);

(* ------------------------------------------------------------------------- *)
(* Theory contexts.                                                          *)
(* ------------------------------------------------------------------------- *)

local
  val initialGrounded = SymbolSet.primitives
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

  fun mkSatisfy ((pkg,undef,seq),((undefs,seqs),(ungr,asms))) =
      let
        val undefs = SymbolSet.union undefs undef
        and seqs = SequentSet.union seqs seq

        val ungr' = SymbolSet.difference ungr undefs
        and asms' = SequentSet.difference asms seqs

        val progress =
            SymbolSet.size ungr' < SymbolSet.size ungr orelse
            SequentSet.size asms' < SequentSet.size asms

        val () =
            if progress then ()
            else
              let
                val mesg =
                    "redundant requires: " ^ PackageNameVersion.toString pkg

                val () = warn mesg
              in
                ()
              end
      in
        ((undefs,seqs),(ungr',asms'))
      end;

  fun definesSomeInput inp =
      let
        fun check sym =
          case sym of
            Symbol.TypeOp t => SymbolTable.knownTypeOp inp (TypeOp.name t)
          | Symbol.Const c => SymbolTable.knownConst inp (Const.name c)
      in
        SymbolSet.exists check
      end;

  fun definesInput inp thl =
      case thl of
        [] => (initialGrounded,initialSatisfied,[],TermRewrite.undef)
      | th :: thl =>
        let
          val (undefs,seqs,seqsl,rewr) = definesInput inp thl

          val {defined = def, undefined = undef} = partitionUndef th
          and seq = Sequents.sequents (sequents th)

          val undef = SymbolTable.symbols undef

          val (seq',rewr) = SequentSet.sharingRewrite seq rewr

          val seq = Option.getOpt (seq',seq)
        in
          if definesSomeInput inp (SymbolTable.symbols def) then
            let
              val undefs = SymbolSet.union undefs undef
              and seqs = SequentSet.union seqs seq
            in
              (undefs,seqs,seqsl,rewr)
            end
          else
            let
              val seqsl = (package th, undef, seq) :: seqsl
            in
              (undefs,seqs,seqsl,rewr)
            end
        end;

  fun groundedInput undefs defs s =
      SymbolSet.member s undefs orelse
      case s of
        Symbol.TypeOp t => SymbolTable.knownTypeOp defs (TypeOp.name t)
      | Symbol.Const c => SymbolTable.knownConst defs (Const.name c);

  fun satisfiedAssumption seqs seq = SequentSet.member seq seqs;

  fun mkContext inp ungr asms thl defs =
      let
        val (undefs,seqs,seqsl,_) = definesInput inp thl

        val ungr = SymbolSet.difference ungr undefs
        and asms = SequentSet.difference asms seqs

        val ((undefs,seqs),_) =
            List.foldl mkSatisfy ((undefs,seqs),(ungr,asms)) seqsl
      in
        Summary.Context
          {groundedInput = groundedInput undefs defs,
           satisfiedAssumption = satisfiedAssumption seqs}
      end;
in
  (* This function must compute the correct context for the input theorems *)
  (* argument (thl); the summary argument (sum) may be used *only* for the *)
  (* purpose of generating warnings about redundant requirements. *)

  fun context sum thl =
      case unionDefined thl of
        NONE => Summary.NoContext
      | SOME defs =>
        let
          val Summary.Summary' {requires = req, provides = prov} =
              Summary.dest sum

          val ireq = Sequents.undefined req
          and iprov = Sequents.undefined prov

          val inp = SymbolTable.union ireq iprov

          val ungr =
              SymbolSet.difference
                (SymbolTable.symbols iprov) (SymbolTable.symbols ireq)

          val asms = Sequents.sequents req
        in
          mkContext inp ungr asms thl defs
        end;
end;

fun packageContext sum = context (PackageSummary.summary sum);

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
