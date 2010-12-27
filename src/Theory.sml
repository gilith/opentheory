(* ========================================================================= *)
(* HIGHER ORDER LOGIC THEORIES                                               *)
(* Copyright (c) 2009 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

structure Theory :> Theory =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* A type of theories.                                                       *)
(* ------------------------------------------------------------------------- *)

type id = int;

datatype theory =
    Theory of
      {id : id,
       theory : theory'}

and theory' =
    Theory' of
      {imports : theory list,
       node : node,
       article : Article.article}

and node =
    Article of
      {interpretation : Interpretation.interpretation,
       filename : string}
  | Package of
      {interpretation : Interpretation.interpretation,
       package : PackageNameVersion.nameVersion,
       theories : (PackageTheory.name * theory) list}
  | Union;

(* ------------------------------------------------------------------------- *)
(* Theory IDs.                                                             *)
(* ------------------------------------------------------------------------- *)

val newId : unit -> id =
    let
      val counter = ref 0
    in
      fn () =>
         let
           val ref count = counter
           val () = counter := count + 1
         in
           count
         end
    end;

fun id (Theory {id = x, ...}) = x;

fun equalId i thy = i = id thy;

fun compare (Theory {id = i1, ...}, Theory {id = i2, ...}) =
    Int.compare (i1,i2);

fun equal thy1 thy2 = (id thy1) = (id thy2);

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

fun mk thy' =
    let
(*OpenTheoryDebug
      val Theory' {node,article,...} = thy'

      val () =
          case node of
            Article _ => ()
          | _ =>
            if Inference.null (Article.inference article) then ()
            else raise Bug "Theory.mk: non-article with non-null inferences"
*)

      val id = newId ()
    in
      Theory
        {id = id,
         theory = thy'}
    end;

fun dest (Theory {theory = x, ...}) = x;

fun imports thy =
    let
      val Theory' {imports = x, ...} = dest thy
    in
      x
    end;

fun node thy =
    let
      val Theory' {node = x, ...} = dest thy
    in
      x
    end;

fun article thy =
    let
      val Theory' {article = x, ...} = dest thy
    in
      x
    end;

(* ------------------------------------------------------------------------- *)
(* Article theories.                                                         *)
(* ------------------------------------------------------------------------- *)

fun isArticleNode node =
    case node of
      Article _ => true
    | _ => false;

fun isArticle thy = isArticleNode (node thy);

(* ------------------------------------------------------------------------- *)
(* Package theories.                                                         *)
(* ------------------------------------------------------------------------- *)

fun destPackageNode node =
    case node of
      Package {package = pkg, ...} => SOME pkg
    | _ => NONE;

fun destPackage thy = destPackageNode (node thy);

fun isPackage thy = Option.isSome (destPackage thy);

fun isArticleTheory (_ : PackageTheory.name, thy) = isArticle thy;

fun existsArticleTheory theories = List.exists isArticleTheory theories;

fun isMainTheory (name, _ : theory) = PackageTheory.isMainName name;

fun mainTheory theories =
    case List.filter isMainTheory theories of
      [] => raise Error "Theory.mainTheory: no main theory"
    | [(_,thy)] => thy
    | _ :: _ :: _ => raise Error "Theory.mainTheory: multiple main theories";

(* ------------------------------------------------------------------------- *)
(* Union theories.                                                           *)
(* ------------------------------------------------------------------------- *)

fun isUnionNode node =
    case node of
      Union => true
    | _ => false;

fun isUnion thy = isUnionNode (node thy);

(* ------------------------------------------------------------------------- *)
(* Primitive theories cannot be expanded.                                    *)
(* ------------------------------------------------------------------------- *)

fun isPrimitiveNode node =
    case node of
      Article _ => true
    | Package {theories,...} => existsArticleTheory theories
    | Union => false;

fun isPrimitive thy = isPrimitiveNode (node thy);

(* ------------------------------------------------------------------------- *)
(* Creating PackageTheory nodes.                                             *)
(* ------------------------------------------------------------------------- *)

fun toPackageTheoryNode node =
    case node of
      Article {interpretation,filename} =>
      PackageTheory.Article
        {interpretation = interpretation,
         filename = filename}
    | Package {interpretation,package,...} =>
      PackageTheory.Package
        {interpretation = interpretation,
         package = package}
    | Union =>
      PackageTheory.Union;

(* ------------------------------------------------------------------------- *)
(* Theory summaries.                                                         *)
(* ------------------------------------------------------------------------- *)

fun summary thy =
    let
      val art = article thy

      val ths = Article.thms art
    in
      Summary.fromThms ths
    end;

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val ppId = Print.ppBracket "<" ">" Print.ppInt;

fun pp thy =
    Print.blockProgram Print.Consistent 0
      [Print.ppString "Theory",
       ppId (id thy)];

end

structure TheoryOrdered =
struct type t = Theory.theory val compare = Theory.compare end

structure TheoryMap =
struct

  local
    structure S = KeyMap (TheoryOrdered);
  in
    open S;
  end;

  fun pp ppX =
      let
        val ppTX = Print.ppOp2 " =>" Theory.pp ppX
      in
        fn m =>
          Print.blockProgram Print.Consistent 0
            [Print.ppString "TheoryMap",
             Print.ppList ppTX (toList m)]
      end;

end

structure TheorySet =
struct

  local
    structure S = ElementSet (TheoryMap);
  in
    open S;
  end;

  val inference =
      let
        fun add (thy,acc) =
            Inference.union acc (Article.inference (Theory.article thy))
      in
        foldl add Inference.empty
      end;

  val article =
      let
        fun add (thy,acc) = Article.union acc (Theory.article thy)
      in
        foldl add Article.empty
      end;

end
