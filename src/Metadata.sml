(* ========================================================================= *)
(* THEOREM METADATA                                                          *)
(* Copyright (c) 2014 Joe Leslie-Hurd, distributed under the MIT license     *)
(* ========================================================================= *)

structure Metadata :> Metadata =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* A type of metadata.                                                       *)
(* ------------------------------------------------------------------------- *)

datatype metadata = Metadata of metadata NameMap.map;

val empty = Metadata (NameMap.new ());

fun null (Metadata md) = NameMap.null md;

fun singleton n_md = Metadata (NameMap.singleton n_md);

local
  fun union1 ((_,md1),(_,md2)) =
      SOME (unionl md1 md2)

  and unionl (Metadata md1) (Metadata md2) =
      Metadata (NameMap.union union1 md1 md2);
in
  val union = merge;
end;

fun add md n_md = union md (singleton n_md);

fun toList (Metadata md) = NameMap.toList md;

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

local
  fun ppl md =
      case toList md of
        [] => Print.skip
      | n_md :: l =>
        Print.program (pp1 n_md :: List.map ppn l)

  and ppn n_md = Print.sequence Print.newline (pp1 n_md)

  and pp1 (n,md) =
      if null md then Name.pp n
      else
        Print.consistentBlock 2
          [Name.pp n,
           Print.ppChar #":",
           Print.break,
           ppl md];
in
  val pp = Print.ppBracket "{" "}" ppl;
end;

end
