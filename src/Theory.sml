(* ========================================================================= *)
(* THEORIES OF HIGHER ORDER LOGIC                                            *)
(* Copyright (c) 2009 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

structure Theory :> Theory =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* Constants.                                                                *)
(* ------------------------------------------------------------------------- *)

val articleKeywordString = "article"
and closeBlockString = "}"
and importKeywordString = "import"
and inKeywordString = "in"
and interpretKeywordString = "interpret"
and localKeywordString = "local"
and openBlockString = "{"
and quoteString = "\""
and terminatorString = ";";

(* ------------------------------------------------------------------------- *)
(* A type of theory syntax.                                                  *)
(* ------------------------------------------------------------------------- *)

datatype 'a theory =
    Local of 'a theory * 'a theory
  | Sequence of 'a theory list
  | Article of {filename : string}
  | Interpret of Interpretation.interpretation * 'a theory
  | Import of 'a;

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

val empty = Sequence [];

fun append th1 th2 = Sequence [th1,th2];

fun map f =
    let
      fun mapf thy =
          case thy of
            Local (t1,t2) => Local (mapf t1, mapf t2)
          | Sequence ts => Sequence (List.map mapf ts)
          | Article f => Article f
          | Interpret (i,t) => Interpret (i, mapf t)
          | Import a => Import (f a)
    in
      mapf
    end;

(* ------------------------------------------------------------------------- *)
(* Articles read by the theory.                                              *)
(* ------------------------------------------------------------------------- *)

local
  fun extract acc thys =
      case thys of
        [] => acc
      | (int,thy) :: thys =>
        case thy of
          Local (thy1,thy2) =>
          let
            val thys = (int,thy1) :: (int,thy2) :: thys
          in
            extract acc thys
          end
        | Sequence ts =>
          let
            val thys = List.map (fn t => (int,t)) ts @ thys
          in
            extract acc thys
          end
        | Article f =>
          let
            val acc = (int,f) :: acc
          in
            extract acc thys
          end
        | Interpret (int',thy) =>
          let
            val int = Interpretation.compose int' int

            val thys = (int,thy) :: thys
          in
            extract acc thys
          end
        | Import _ => extract acc thys;
in
  fun articles int thy = extract [] [(int,thy)];
end;

(* ------------------------------------------------------------------------- *)
(* Imported theories.                                                        *)
(* ------------------------------------------------------------------------- *)

local
  fun extract acc thys =
      case thys of
        [] => acc
      | thy :: thys =>
        case thy of
          Local (thy1,thy2) =>
          let
            val thys = thy1 :: thy2 :: thys
          in
            extract acc thys
          end
        | Sequence ts =>
          let
            val thys = ts @ thys
          in
            extract acc thys
          end
        | Article _ => extract acc thys
        | Interpret (_,thy) =>
          let
            val thys = thy :: thys
          in
            extract acc thys
          end
        | Import a =>
          let
            val acc = a :: acc
          in
            extract acc thys
          end;
in
  fun imports thy = extract [] [thy];
end;

(* ------------------------------------------------------------------------- *)
(* Compiling theories to articles.                                           *)
(* ------------------------------------------------------------------------- *)

fun toArticle info =
    let
      val {savable,
           known = initialKnown,
           simulations,
           importToArticle,
           interpretation = initialInt,
           directory = dir,
           theory = initialThy} = info

      fun compile known int thy =
          case thy of
            Local (thy1,thy2) =>
            let
              val art1 = compile known int thy1
              val known = Article.append known art1
            in
              compile known int thy2
            end
          | Sequence thys =>
            let
              val (arts,_) = maps (compileAppend int) thys known
            in
              Article.concat arts
            end
          | Article {filename} =>
            let
              val filename = OS.Path.joinDirFile {dir = dir, file = filename}
            in
              Article.fromTextFile
                {savable = savable,
                 known = known,
                 simulations = simulations,
                 interpretation = int,
                 filename = filename}
            end
          | Interpret (pint,pthy) =>
            let
              val int = Interpretation.compose pint int
            in
              compile known int pthy
            end
          | Import imp =>
            importToArticle imp

      and compileAppend int thy known =
          let
            val art = compile known int thy

            val known = Article.append known art
          in
            (art,known)
          end
    in
      compile initialKnown initialInt initialThy
    end
    handle Error err => raise Error ("Theory.toArticle: " ^ err);

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val ppArticleKeyword = Print.addString articleKeywordString
and ppCloseBlock = Print.addString closeBlockString
and ppImportKeyword = Print.addString importKeywordString
and ppInKeyword = Print.addString inKeywordString
and ppInterpretKeyword = Print.addString interpretKeywordString
and ppLocalKeyword = Print.addString localKeywordString
and ppOpenBlock = Print.addString openBlockString
and ppQuote = Print.addString quoteString
and ppTerminator = Print.addString terminatorString;

fun ppBlock ppX x =
    Print.blockProgram Print.Consistent 0
      [Print.blockProgram Print.Consistent 2
         [ppOpenBlock,
          Print.addBreak 1,
          ppX x],
       Print.addBreak 1,
       ppCloseBlock];

fun ppQuotedFilename f =
    Print.program
      [ppQuote,
       Print.addString f,
       ppQuote];

fun pp ppImp =
    let
      val ppSpace = Print.addString " "

      fun ppThy thy =
          case thy of
            Local (thy1,thy2) =>
            Print.blockProgram Print.Consistent 0
              [ppLocalKeyword,
               ppSpace,
               ppThy thy1,
               ppSpace,
               ppInKeyword,
               ppSpaceThy thy2]
          | Sequence thys =>
            ppBlock ppList thys
          | Article {filename} =>
            Print.blockProgram Print.Consistent 2
              [ppArticleKeyword,
               Print.addBreak 1,
               ppQuotedFilename filename,
               ppTerminator]
          | Interpret (int,thy) =>
            Print.blockProgram Print.Consistent 0
              [ppInterpretKeyword,
               ppSpace,
               ppBlock Interpretation.pp int,
               ppSpace,
               ppInKeyword,
               ppSpaceThy thy]
          | Import imp =>
            Print.blockProgram Print.Consistent 2
              [ppImportKeyword,
               Print.addBreak 1,
               ppImp imp,
               ppTerminator]

      and ppSpaceThy thy = Print.sequence (Print.addBreak 1) (ppThy thy)

      and ppList thys =
          case thys of
            [] => Print.skip
          | thy :: thys => Print.program (ppThy thy :: List.map ppSpaceThy thys)
    in
      ppThy
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

  val articleKeywordParser = exactString articleKeywordString
  and closeBlockParser = exactString closeBlockString
  and importKeywordParser = exactString importKeywordString
  and inKeywordParser = exactString inKeywordString
  and interpretKeywordParser = exactString interpretKeywordString
  and localKeywordParser = exactString localKeywordString
  and openBlockParser = exactString openBlockString
  and quoteParser = exactString quoteString
  and terminatorParser = exactString terminatorString;

  val quotedFilenameParser =
      let
        fun isFilenameChar c = c <> #"\n" andalso c <> #"\""

        val filenameParser = atLeastOne (some isFilenameChar)
      in
        (quoteParser ++ filenameParser ++ quoteParser) >>
        (fn ((),(f,())) => {filename = implode f})
      end;

  fun theoryParser impParser inp =
      (localParser impParser ||
       sequenceParser impParser ||
       articleParser ||
       interpretParser impParser ||
       importParser impParser) inp

  and localParser impParser inp =
      ((localKeywordParser ++ atLeastOneSpace ++
        theoryParser impParser ++ manySpace ++
        inKeywordParser ++ atLeastOneSpace ++
        theoryParser impParser) >>
       (fn ((),((),(t1,((),((),((),t2)))))) => Local (t1,t2))) inp

  and sequenceParser impParser inp =
      ((openBlockParser ++ manySpace ++
        many (theorySpaceParser impParser) ++
        closeBlockParser) >>
       (fn ((),((),(ts,()))) => Sequence ts)) inp

  and articleParser inp =
      ((articleKeywordParser ++ manySpace ++
        quotedFilenameParser ++ manySpace ++ terminatorParser) >>
       (fn ((),((),(f,((),())))) => Article f)) inp

  and interpretParser impParser inp =
      ((interpretKeywordParser ++ manySpace ++
        openBlockParser ++ manySpace ++
        Interpretation.parser ++ manySpace ++
        closeBlockParser ++ manySpace ++
        inKeywordParser ++ atLeastOneSpace ++
        theoryParser impParser) >>
       (fn ((),((),((),((),(i,((),((),((),((),((),t)))))))))) =>
           Interpret (i,t))) inp

  and importParser impParser inp =
      ((importKeywordParser ++ atLeastOneSpace ++
        impParser ++ manySpace ++ terminatorParser) >>
       (fn ((),((),(i,((),())))) => Import i)) inp

  and theorySpaceParser impParser inp =
      (theoryParser impParser ++ manySpace >> fst) inp;
in
  fun parser impParser = manySpace ++ theorySpaceParser impParser >> snd;
end;

end
