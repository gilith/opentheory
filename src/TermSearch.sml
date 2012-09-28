(* ========================================================================= *)
(* SEARCHING FOR HIGHER ORDER LOGIC SUBTERMS                                 *)
(* Copyright (c) 2011 Joe Leslie-Hurd, distributed under the MIT license     *)
(* ========================================================================= *)

structure TermSearch :> TermSearch =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* Bottom-up term searching.                                                 *)
(* ------------------------------------------------------------------------- *)

datatype search =
    Search of
      {predicate : Term.term -> bool,
       leftToRight : bool,
       seen : Term.term option IntMap.map};

fun new {predicate,leftToRight} =
    let
      val seen = IntMap.new ()
    in
      Search
        {predicate = predicate,
         leftToRight = leftToRight,
         seen = seen}
    end;

fun leftToRight (Search {leftToRight = x, ...}) = x;

(* ------------------------------------------------------------------------- *)
(* The bottom-up traversal.                                                  *)
(* ------------------------------------------------------------------------- *)

fun searchTm pred lr tm seen =
    let
      val i = Term.id tm
    in
      case IntMap.peek seen i of
        SOME subtm => (subtm,seen)
      | NONE =>
        let
          val (subtm,seen) = searchTm' pred lr tm seen

          val subtm =
              if Option.isSome subtm then subtm
              else if pred tm then SOME tm
              else NONE

          val seen = IntMap.insert seen (i,subtm)
        in
          (subtm,seen)
        end
    end

and searchTm' pred lr tm seen =
    case Term.dest tm of
      TypeTerm.Const' _ => (NONE,seen)
    | TypeTerm.Var' _ => (NONE,seen)
    | TypeTerm.App' (f,a) =>
      if lr then
        let
          val subtm_seen as (subtm,seen) = searchTm pred lr f seen
        in
          if Option.isSome subtm then subtm_seen
          else searchTm pred lr a seen
        end
      else
        let
          val subtm_seen as (subtm,seen) = searchTm pred lr a seen
        in
          if Option.isSome subtm then subtm_seen
          else searchTm pred lr f seen
        end
    | TypeTerm.Abs' (_,b) => searchTm pred lr b seen;

(* ------------------------------------------------------------------------- *)
(* Searching for subterms.                                                   *)
(* ------------------------------------------------------------------------- *)

(* Terms *)

fun sharingSearchTerm tm search =
    let
      val Search {predicate,leftToRight,seen} = search

      val (subtm,seen') = searchTm predicate leftToRight tm seen

(*OpenTheoryDebug
      val () =
          if IntMap.size seen <= IntMap.size seen' then ()
          else raise Bug "TermSearch.sharingSearchTerm: shrinking vision"
*)

      val search =
          if IntMap.size seen' = IntMap.size seen then search
          else
            Search
              {predicate = predicate,
               leftToRight = leftToRight,
               seen = seen'}
    in
      (subtm,search)
    end;

fun searchTerm search tm =
    let
      val (subtm,_) = sharingSearchTerm tm search
    in
      subtm
    end;

(* Term lists *)

local
  fun searchList tms search =
      case tms of
        [] => (NONE,search)
      | tm :: tms => searchCons tm tms search

  and searchCons tm tms search =
      let
        val subtm_search as (subtm,search) = sharingSearchTerm tm search
      in
        if Option.isSome subtm then subtm_search
        else searchList tms search
      end;
in
  fun sharingSearchTermList tms search =
      if leftToRight search then searchList tms search
      else searchList (List.rev tms) search;
end;

fun searchTermList search tms =
    let
      val (subtm,_) = sharingSearchTermList tms search
    in
      subtm
    end;

(* Term sets *)

local
  fun searchIterOpt itero search =
      case itero of
        NONE => (NONE,search)
      | SOME iter => searchIter iter search

  and searchIter iter search =
      let
        val tm = TermAlphaSet.readIterator iter

        val subtm_search as (subtm,search) = sharingSearchTerm tm search
      in
        if Option.isSome subtm then subtm_search
        else searchIterOpt (TermAlphaSet.advanceIterator iter) search
      end;
in
  fun sharingSearchTermAlphaSet tms search =
      let
        val itero =
            if leftToRight search then TermAlphaSet.mkIterator tms
            else TermAlphaSet.mkRevIterator tms
      in
        searchIterOpt itero search
      end;
end;

fun searchTermAlphaSet search tms =
    let
      val (subtm,_) = sharingSearchTermAlphaSet tms search
    in
      subtm
    end;

end
