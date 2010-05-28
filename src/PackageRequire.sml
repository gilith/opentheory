(* ========================================================================= *)
(* REQUIRED THEORY PACKAGES                                                  *)
(* Copyright (c) 2009 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

structure PackageRequire :> PackageRequire =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* Constants.                                                                *)
(* ------------------------------------------------------------------------- *)

val requireKeywordString = "require";

(* ------------------------------------------------------------------------- *)
(* A type of required theory packages.                                       *)
(* ------------------------------------------------------------------------- *)

datatype require =
    Require of
      {name : PackageTheory.name,
       theory : PackageTheory.theory};

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

fun name (Require {name = x, ...}) = x;

fun theory (Require {theory = x, ...}) = x;

fun imports req = PackageTheory.imports (theory req);

fun body req = PackageTheory.body (theory req);

(* ------------------------------------------------------------------------- *)
(* Article dependencies.                                                     *)
(* ------------------------------------------------------------------------- *)

fun article req = PackageTheory.destArticle (theory req);

fun articles reqs = List.mapPartial article reqs;

(* ------------------------------------------------------------------------- *)
(* Package dependencies.                                                     *)
(* ------------------------------------------------------------------------- *)

fun package req = PackageTheory.destPackage (theory req);

fun packages reqs = List.mapPartial package reqs;

(* ------------------------------------------------------------------------- *)
(* Topological sort of requirements.                                         *)
(* ------------------------------------------------------------------------- *)

local
  fun toMap reql =
      let
        fun ins (req,(m,l)) =
            let
              val n = name req
              and rs = imports req

              val m = PackageBaseMap.insert m (n,(rs,req))

              val l = n :: l
            in
              (m,l)
            end

        val reqs_namel as (reqs,_) =
            List.foldl ins (PackageBaseMap.new (), []) reql

        fun check (n,(rs,_)) =
            case List.find (fn r => not (PackageBaseMap.inDomain r reqs)) rs of
              NONE => ()
            | SOME r =>
              raise Error ("require block \"" ^ PackageBase.toString n ^ "\" " ^
                           "imports unknown \"" ^ PackageBase.toString r ^ "\"")

        val () = PackageBaseMap.app check reqs
      in
        reqs_namel
      end;

  fun sortMap requires (dealt,dealtset) (stack,stackset) work =
      case work of
        [] =>
        (case stack of
           [] => rev dealt
         | (r,(req,work,stackset)) :: stack =>
           let
             val dealt = req :: dealt
             val dealtset = PackageBaseSet.add dealtset r
           in
             sortMap requires (dealt,dealtset) (stack,stackset) work
           end)
      | r :: work =>
        if PackageBaseSet.member r dealtset then
          sortMap requires (dealt,dealtset) (stack,stackset) work
        else if PackageBaseSet.member r stackset then
          let
            fun notR (r',_) = not (PackageBase.equal r' r)

            val l = map fst (takeWhile notR stack)
            val l = r :: rev (r :: l)
            val err = join " -> " (map PackageBase.toString l)
          in
            raise Error ("circular dependency:\n" ^ err)
          end
        else
          let
            val (rs,req) =
                case PackageBaseMap.peek requires r of
                  SOME rs_req => rs_req
                | NONE => raise Bug "PackageRequire.sort"

            val stack = (r,(req,work,stackset)) :: stack

            val stackset = PackageBaseSet.add stackset r

            val work = rs
          in
            sortMap requires (dealt,dealtset) (stack,stackset) work
          end;
in
  fun sort reqs =
      let
        val (reqs,work) = toMap reqs

        val dealt = []
        val dealtset = PackageBaseSet.empty

        val stack = []
        val stackset = PackageBaseSet.empty
      in
        sortMap reqs (dealt,dealtset) (stack,stackset) work
      end
(*OpenTheoryDebug
      handle Error err => raise Error ("PackageRequire.sort: " ^ err);
*)
end;

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val ppRequireKeyword = Print.addString requireKeywordString;

fun pp req =
    let
      val Require {name,theory} = req
    in
      Print.blockProgram Print.Consistent 0
        [ppRequireKeyword,
         Print.addString " ",
         PackageTheory.ppName name,
         Print.addString " ",
         PackageTheory.pp theory]
    end;

fun ppList reqs =
    case reqs of
      [] => Print.skip
    | req :: reqs =>
      let
        fun ppReq r = Print.program [Print.addNewline, Print.addNewline, pp r]
      in
        Print.blockProgram Print.Consistent 0
          (pp req :: map ppReq reqs)
      end;

(* ------------------------------------------------------------------------- *)
(* Parsing.                                                                  *)
(* ------------------------------------------------------------------------- *)

local
  infixr 9 >>++
  infixr 8 ++
  infixr 7 >>
  infixr 6 ||

  open Parse;

  val requireKeywordParser = exactString requireKeywordString;

  val requireSpaceParser =
      (requireKeywordParser ++ atLeastOneSpace ++
       PackageTheory.parserName ++
       PackageTheory.parser) >>
      (fn ((),((),(n,thy))) => Require {name = n, theory = thy});
in
  val parser = manySpace ++ requireSpaceParser >> snd;

  val parserList = manySpace ++ many requireSpaceParser >> snd;
end;

end
