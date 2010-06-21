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
       package : PackageName.name,
       theory : theory}
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

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

fun mk thy' =
    let
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
(* Package theories.                                                         *)
(* ------------------------------------------------------------------------- *)

fun packageNode node =
    case node of
      Package {package = pkg, ...} => SOME pkg
    | _ => NONE;

fun package thy = packageNode (node thy);

fun isPackage thy = Option.isSome (package thy);

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

fun pp thy =
    Print.blockProgram Print.Consistent 0
      [Print.addString "Theory<",
       Print.ppInt (id thy),
       Print.addString ">"];

end

structure TheoryOrdered =
struct type t = Theory.theory val compare = Theory.compare end

structure TheorySet =
struct

  local
    structure S = ElementSet (TheoryOrdered);
  in
    open S;
  end;

  val toArticle =
      let
        fun add (thy,acc) = Article.union acc (Theory.article thy)
      in
        foldl add Article.empty
      end;

end

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
            [Print.addString "TheoryMap",
             Print.ppList ppTX (toList m)]
      end;

end
