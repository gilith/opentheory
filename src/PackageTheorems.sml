(* ========================================================================= *)
(* PACKAGE THEOREMS                                                          *)
(* Copyright (c) 2011 Joe Leslie-Hurd, distributed under the MIT license     *)
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
                val name = PackageNameVersion.name pkg

                val mesg = "redundant requires: " ^ PackageName.toString name

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
                (SymbolTable.symbols iprov)
                (SymbolTable.symbols ireq)

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
       defined :
         {typeOps : PackageName.name NameMap.map,
          consts : PackageName.name NameMap.map},
       grounded :
         {typeOps : PackageNameSet.set NameMap.map,
          consts : PackageNameSet.set NameMap.map},
       satisfied : PackageNameSet.set SequentMap.map};

local
  val initialGrounded = SymbolSet.primitives
  and initialSatisfied = SequentSet.standardAxioms;

  fun destSequents seqs =
      let
        val sym = Sequents.symbol seqs
        and seqs = Sequents.sequents seqs

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

  val mkDefined =
      let
        val emptyName : PackageName.name NameMap.map = NameMap.new ()
      in
        {typeOps = emptyName, consts = emptyName}
      end;

  val mkGrounded =
      let
        val emptyName : PackageNameSet.set NameMap.map = NameMap.new ()

        fun addName nm n = NameMap.insert nm (n, PackageNameSet.empty)

        fun add (s, {typeOps = ots, consts = cs}) =
            case s of
              Symbol.TypeOp ot =>
              let
                val ots = addName ots (TypeOp.name ot)
              in
                {typeOps = ots, consts = cs}
              end
            | Symbol.Const c =>
              let
                val cs = addName cs (Const.name c)
              in
                {typeOps = ots, consts = cs}
              end
      in
        SymbolSet.foldl add {typeOps = emptyName, consts = emptyName}
      end;

  val mkSatisfied = SequentSet.map (K PackageNameSet.empty);

  fun addDefined pkg =
      let
        fun addName kind nm n =
            case NameMap.peek nm n of
                NONE => NameMap.insert nm (n,pkg)
              | SOME pkg' =>
                if PackageName.equal pkg' pkg then nm
                else
                  let
                    val err =
                        "name clash in defined " ^ kind ^ "s: " ^
                        Name.toString n
                  in
                    raise Error err
                  end

        fun addSymbol (s, {typeOps = ots, consts = cs}) =
            case s of
              Symbol.TypeOp ot =>
              let
                val ots = addName "type operator" ots (TypeOp.name ot)
              in
                {typeOps = ots, consts = cs}
              end
            | Symbol.Const c =>
              let
                val cs = addName "constant" cs (Const.name c)
              in
                {typeOps = ots, consts = cs}
              end
      in
        fn def => fn sym =>
           let
             val dsym = SymbolTable.symbols (SymbolTable.defined sym)
           in
             SymbolSet.foldl addSymbol def dsym
           end
      end;

  fun addGrounded n gr sym =
      let
        fun addTypeOp (ot,ns) =
            if not (SymbolTable.knownTypeOp sym ot) then ns
            else PackageNameSet.add ns n

        fun addConst (c,ns) =
            if not (SymbolTable.knownConst sym c) then ns
            else PackageNameSet.add ns n

        val {typeOps = ots, consts = cs} = gr

        val ots = NameMap.map addTypeOp ots
        and cs = NameMap.map addConst cs
      in
        {typeOps = ots, consts = cs}
      end;

  fun addSatisfied n sat seqs =
      let
        fun add (seq,ns) =
            if not (SequentSet.member seq seqs) then ns
            else PackageNameSet.add ns n
      in
        SequentMap.map add sat
      end;

  fun addInitial (th,(ns,def,gr,sat)) =
      let
        val (n,sym,seqs) = destTheorems th

        val () =
            if not (PackageNameSet.member n ns) then ()
            else
              let
                val err =
                    "duplicate in required theories: " ^
                    PackageName.toString n
              in
                raise Error err
              end

        val ns = PackageNameSet.add ns n
        and def = addDefined n def sym
        and gr = addGrounded n gr sym
        and sat = addSatisfied n sat seqs
      in
        (ns,def,gr,sat)
      end;

  val checkInitialGrounded =
      let
        fun check kind gr =
            case NameMap.findl (PackageNameSet.null o snd) gr of
              NONE => ()
            | SOME (n,_) =>
              let
                val err = "ungrounded input " ^ kind ^ ":\n" ^ Name.toString n
              in
                raise Error err
              end
      in
        fn {typeOps = ots, consts = cs} =>
          let
            val () = check "type operator" ots
            and () = check "constant" cs
          in
            ()
          end
      end;

  fun checkInitialSatisfied sat =
      case SequentMap.findl (PackageNameSet.null o snd) sat of
        NONE => ()
      | SOME (asm,_) =>
        let
          val err = "unsatisfied assumption:\n" ^ Sequent.toString asm
        in
          raise Error err
        end;

  fun checkInitial gr sat =
      let
        val () = checkInitialGrounded gr
        and () = checkInitialSatisfied sat
      in
        ()
      end
      handle Error err =>
        raise Error ("required theories not sufficient:\n" ^ err);

  fun removeGrounded n gr sym =
      let
        fun removeTypeOp (ot,ns) =
            if not (PackageNameSet.member n ns) then ns
            else if SymbolTable.knownTypeOp sym ot then ns
            else
              let
                val ns = PackageNameSet.delete ns n
              in
                if not (PackageNameSet.null ns) then ns
                else raise Error "ungrounded input type operator"
              end

        fun removeConst (c,ns) =
            if not (PackageNameSet.member n ns) then ns
            else if SymbolTable.knownConst sym c then ns
            else
              let
                val ns = PackageNameSet.delete ns n
              in
                if not (PackageNameSet.null ns) then ns
                else raise Error "ungrounded input constant"
              end

        val {typeOps = ots, consts = cs} = gr

        val ots = NameMap.map removeTypeOp ots
        and cs = NameMap.map removeConst cs
      in
        {typeOps = ots, consts = cs}
      end;

  fun removeSatisfied n sat seqs =
      let
        fun remove (seq,ns) =
            if not (PackageNameSet.member n ns) then ns
            else if SequentSet.member seq seqs then ns
            else
              let
                val ns = PackageNameSet.delete ns n
              in
                if not (PackageNameSet.null ns) then ns
                else raise Error "unsatisfied assumption"
              end
      in
        SequentMap.map remove sat
      end;
in
  fun mkVersions sum thl =
      let
        val Summary.Summary' {requires = req, provides = prov} =
            Summary.dest sum

        val ungr =
            let
              val ireq = SymbolTable.symbols (Sequents.undefined req)
              and iprov = SymbolTable.symbols (Sequents.undefined prov)

              val gr = SymbolSet.union ireq initialGrounded
            in
              SymbolSet.difference iprov gr
            end

        val unsat =
            let
              val asms = Sequents.sequents req
            in
              SequentSet.difference asms initialSatisfied
            end

        val ns = PackageNameSet.empty
        and def = mkDefined
        and gr = mkGrounded ungr
        and sat = mkSatisfied unsat

        val (ns,def,gr,sat) = List.foldl addInitial (ns,def,gr,sat) thl

        val () = checkInitial gr sat
      in
        Versions
          {names = ns,
           defined = def,
           grounded = gr,
           satisfied = sat}
      end;

  fun addVersion vs th =
      let
        val Versions
              {names = ns,
               defined = def,
               grounded = gr,
               satisfied = sat} = vs

        val (n,sym,seqs) = destTheorems th

        val () =
            if PackageNameSet.member n ns then ()
            else raise Error "unknown required package name"

        val def = addDefined n def sym
        and gr = removeGrounded n gr sym
        and sat = removeSatisfied n sat seqs
      in
        Versions
          {names = ns,
           defined = def,
           grounded = gr,
           satisfied = sat}
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
