(* ========================================================================= *)
(* GENERATING HASKELL PROJECTS FROM THEORIES                                 *)
(* Copyright (c) 2011 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

structure Haskell :> Haskell =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* A prefix marking theories that can be exported as Haskell packages.       *)
(* ------------------------------------------------------------------------- *)

val prefix = PackageName.haskellExport;

(* ------------------------------------------------------------------------- *)
(* A type of Haskell packages.                                               *)
(* ------------------------------------------------------------------------- *)

datatype data =
    Data of
      {name : TypeOp.typeOp,
       constructors : (Const.const * Type.ty list) list};

datatype source =
    DataSource of data;

datatype haskell =
     Haskell of
       {nameVersion : PackageNameVersion.nameVersion,
        source : source list};

(* ------------------------------------------------------------------------- *)
(* Converting a theory to a Haskell package.                                 *)
(* ------------------------------------------------------------------------- *)

local
  fun getTheory name thys =
      case Theory.peekTheory name thys of
        SOME thy => thy
      | NONE =>
        let
          val err = "missing " ^ PackageName.toString name ^ " block in theory"
        in
          raise Error err
        end;
in
  fun splitTheories thy =
      let
        val thys =
            case Theory.node thy of
              Theory.Package {theories,...} => theories
            | _ => raise Bug "Haskell.theories: not a package theory"

        val src = getTheory PackageName.srcHaskellExport thys
      in
        {src = src}
      end;
end;

local
  fun destIndData tm =
      let
        val (p,t0) = Term.destForall tm

        fun destP t =
            let
              val (p',x) = Term.destApp t

              val () =
                  if Term.equalVar p p' then ()
                  else raise Error "bad p"
            in
              x
            end

        val (t1,t2) = Term.destImp t0

        val ty =
            let
              val (x,px) = Term.destForall t2

              val x' = destP px

              val () =
                  if Term.equalVar x x' then ()
                  else raise Error "bad x"
            in
              Var.typeOf x
            end

        fun destCon t =
            let
              val (vs,t0) = Term.stripForall t

              val t1 =
                  case total Term.destImp t0 of
                    NONE => t0
                  | SOME (t2,t3) =>
                    let
                      val vs = VarSet.fromList vs

                      fun check t =
                          if VarSet.member (Term.destVar (destP t)) vs then ()
                          else raise Error "bad step assumption"

                      val () = List.app check (Term.stripConj t2)
                    in
                      t3
                    end

              val (t2,ts) = Term.stripApp t1

              val (c,_) = Term.destConst t2
            in
              (c, List.map Term.typeOf ts)
            end

        val name =
            let
              val (name,parms) = Type.destOp ty

              val vs = List.map Type.destVar parms

              val () =
                  if NameSet.size (NameSet.fromList vs) = length vs then ()
                  else raise Error "duplicate type vars"
            in
              name
            end

        val ts = Term.stripConj t1
      in
        Data
          {name = name,
           constructors = List.map destCon ts}
      end
      handle Error err =>
        raise Error ("bad induction conjunct: " ^ err);

  fun destRecData tm =
      let
      in
        raise Error "not implemented"
      end
      handle Error err =>
        raise Error ("bad recursion conjunct: " ^ err);

  fun destCaseData tm =
      let
      in
        raise Error "not implemented"
      end
      handle Error err =>
        raise Error ("bad case conjunct: " ^ err);
in
  fun destData th =
      let
        val Sequent.Sequent {hyp,concl} = Thm.sequent th

        val () =
            if TermAlphaSet.null hyp then ()
            else raise Error "hypotheses"

        val (indData,recCaseData) = Term.destConj concl

        val (recData,caseData) = Term.destConj recCaseData

        val indData = destIndData indData
        and recData = destRecData recData
        and caseData = destCaseData caseData
      in
        raise Error "not implemented"
      end
      handle Error err =>
        raise Error ("bad data theorem: " ^ err);
end;

local
in
  fun destNewtype th =
      let
        val Sequent.Sequent {hyp,concl} = Thm.sequent th

        val () =
            if TermAlphaSet.null hyp then ()
            else raise Error "hypotheses"
      in
        raise Error "not implemented"
      end
      handle Error err =>
        raise Error ("bad newtype theorem: " ^ err);
end;

local
in
  fun destValue th =
      let
        val Sequent.Sequent {hyp,concl} = Thm.sequent th

        val () =
            if TermAlphaSet.null hyp then ()
            else raise Error "hypotheses"
      in
        raise Error "not implemented"
      end
      handle Error err =>
        raise Error ("bad value theorem: " ^ err);
end;

fun destSource th =
    let
      val dataResult =
          Left (destData th)
          handle Error err => Right err

      val newtypeResult =
          Left (destNewtype th)
          handle Error err => Right err

      val valueResult =
          Left (destValue th)
          handle Error err => Right err
    in
      case (dataResult,newtypeResult,valueResult) of
        (Left x, Right _, Right _) => DataSource x
      | (Right _, Left x, Right _) => raise Error "not implemented"
      | (Right _, Right _, Left x) => raise Error "not implemented"
      | (Right e1, Right e2, Right e3) =>
        let
          val err =
              "bad source theorem:\n  " ^ e1 ^ "\n  " ^ e2 ^ "\n  " ^ e3 ^
              "\n" ^ Print.toString Thm.pp th
        in
          raise Error err
        end
      | _ => raise Bug "Haskell.destSource: ambiguous"
    end;

fun destSourceTheory src =
    let
      val art = Theory.article src

      val ths = ThmSet.toList (Thms.thms (Article.thms art))
    in
      List.map destSource ths
    end;

fun convert namever thy =
    let
      val {src} = splitTheories thy

      val source = destSourceTheory src
    in
      Haskell
        {nameVersion = namever,
         source = source}
    end;

(* ------------------------------------------------------------------------- *)
(* Writing a Haskell package to disk.                                        *)
(* ------------------------------------------------------------------------- *)

fun toPackage h = ();

(* ------------------------------------------------------------------------- *)
(* Export a theory to a Haskell package.                                     *)
(* ------------------------------------------------------------------------- *)

fun export dir namever =
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

      val (_,thy) =
          Graph.importPackageInfo importer graph
            {imports = imps,
             interpretation = int,
             info = info}

      val haskell = convert namever thy

      val () = toPackage haskell
    in
      ()
    end;

end
