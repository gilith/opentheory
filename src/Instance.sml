(* ========================================================================= *)
(* HIGHER ORDER LOGIC THEORY INSTANCES                                       *)
(* Copyright (c) 2009 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

structure Instance :> Instance =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* A type of theory instances.                                               *)
(* ------------------------------------------------------------------------- *)

type id = int;

datatype instance =
    Instance of
      {id : id,
       instance : instance'}

and instance' =
    Instance' of
      {requires : instance list,
       interpretation : Interpretation.interpretation,
       package : PackageName.name option,
       theory : instance Theory.theory,
       article : Article.article};

(* ------------------------------------------------------------------------- *)
(* Instance IDs.                                                             *)
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

fun id (Instance {id = x, ...}) = x;

fun equalId i inst = i = id inst;

fun compare (Instance {id = i1, ...}, Instance {id = i2, ...}) =
    Int.compare (i1,i2);

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

fun mk inst' =
    let
      val id = newId ()
    in
      Instance
        {id = id,
         instance = inst'}
    end;

fun dest (Instance {instance = x, ...}) = x;

fun requires inst =
    let
      val Instance' {requires = x, ...} = dest inst
    in
      x
    end;

fun interpretation inst =
    let
      val Instance' {interpretation = x, ...} = dest inst
    in
      x
    end;

fun package inst =
    let
      val Instance' {package = x, ...} = dest inst
    in
      x
    end;

fun theory inst =
    let
      val Instance' {theory = x, ...} = dest inst
    in
      x
    end;

fun article inst =
    let
      val Instance' {article = x, ...} = dest inst
    in
      x
    end;

(* ------------------------------------------------------------------------- *)
(* Articles read by the instance theory.                                     *)
(* ------------------------------------------------------------------------- *)

fun theoryArticles inst =
    let
      val Instance' {interpretation = int, theory = thy, ...} = dest inst
    in
      Theory.articles int thy
    end;

(* ------------------------------------------------------------------------- *)
(* Instances imported by the theory.                                         *)
(* ------------------------------------------------------------------------- *)

fun theoryImports inst =
    let
      val Instance' {theory = thy, ...} = dest inst
    in
      Theory.imports thy
    end;

(* ------------------------------------------------------------------------- *)
(* Creating instances of theory packages.                                    *)
(* ------------------------------------------------------------------------- *)

fun fromTheory info =
    let
      val {savable,
           requires = req,
           simulations,
           importToInstance,
           interpretation = int,
           directory = dir,
           package = pkg,
           theory = thy} = info

      fun mkFilename {filename} =
          {filename = OS.Path.joinDirFile {dir = dir, file = filename}}

      fun mkThy thy =
          case thy of
            Theory.Local (t1,t2) => Theory.Local (mkThy t1, mkThy t2)
          | Theory.Sequence ts => Theory.Sequence (map mkThy ts)
          | Theory.Article f => Theory.Article (mkFilename f)
          | Theory.Interpret (i,t) => Theory.Interpret (i, mkThy t)
          | Theory.Import a => Theory.Import (importToInstance a)

      val thy = mkThy thy

      val known = Article.concat (map article req)

      val art =
          Theory.toArticle
            {savable = savable,
             known = known,
             simulations = simulations,
             importToArticle = article,
             interpretation = int,
             theory = thy}

      val instance' =
          Instance'
            {requires = req,
             interpretation = int,
             package = pkg,
             theory = thy,
             article = art}
    in
      mk instance'
    end;

end

structure InstanceOrdered =
struct type t = Instance.instance val compare = Instance.compare end

structure InstanceSet = ElementSet (InstanceOrdered)

structure InstanceMap = KeyMap (InstanceOrdered)
