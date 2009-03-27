(* ========================================================================= *)
(* ARTICLES OF PROOFS IN HIGHER ORDER LOGIC                                  *)
(* Copyright (c) 2004-2006 Joe Hurd, distributed under the GNU GPL version 2 *)
(* ========================================================================= *)

structure Article :> Article =
struct

open Useful Syntax Rule;

(* ------------------------------------------------------------------------- *)
(* Helper functions.                                                         *)
(* ------------------------------------------------------------------------- *)

fun revAppend [] s = s ()
  | revAppend (h :: t) s = revAppend t (K (Stream.Cons (h,s)));

fun revConcat strm =
    case strm of
      Stream.Nil => Stream.Nil
    | Stream.Cons (h,t) => revAppend h (revConcat o t);

(* ------------------------------------------------------------------------- *)
(* Simulating other theorem provers.                                         *)
(* ------------------------------------------------------------------------- *)

val simulations = HolLight.simulations;

fun simulate interpretation stack seq =
    case topCallStack stack of
      SOME (Object {object = Object.Ocall f, provenance = Pcall a, ...}) =>
      let
        val Object {object = a, ...} = a
      in
        case NameMap.peek simulations f of
          NONE => NONE
        | SOME sim =>
          let
            val r = sim interpretation seq a
            val ths = Object.thms r
          in
            case first (total (alpha seq)) ths of
              SOME th => SOME (th,[])
            | NONE =>
              let
                val ppOb = Print.ppOp2 " =" Print.ppString Object.pp
                val ppSeq = Print.ppOp2 " =" Print.ppString ppSequent
                val () = warn ("simulation failed: " ^ Name.toString f ^
                               "\n" ^ Print.toString ppOb ("input",a) ^
                               "\n" ^ Print.toString ppOb ("output",r) ^
                               "\n" ^ Print.toString ppSeq ("target",seq))
              in
                NONE
              end
          end
      end
    | _ => NONE;

(* ------------------------------------------------------------------------- *)
(* Executing commands.                                                       *)
(* ------------------------------------------------------------------------- *)

datatype state =
    State of
      {stack : stack,
       dict : dict,
       saved : saved};

val initialState =
    State
      {stack = emptyStack,
       dict = emptyDict,
       saved = emptySaved};

fun executeCommand savable known interpretation cmd state =
    let
      val State {stack,dict,saved} = state
    in
      case cmd of
      (* Errors *)

        Cop "error" =>
        let
          val ob = Object.Oerror
          and prov = Pnull
          and call = if savable then topCallStack stack else NONE
          val obj = mkObject (ob,prov,call)
          val stack = pushStack stack obj
        in
          State {stack = stack, dict = dict, saved = saved}
        end

      (* Numbers *)

      | Cnum i =>
        let
          val ob = Object.Onum i
          and prov = Pnull
          and call = if savable then topCallStack stack else NONE
          val obj = mkObject (ob,prov,call)
          val stack = pushStack stack obj
        in
          State {stack = stack, dict = dict, saved = saved}
        end

      (* Names *)

      | Cname n =>
        let
          val ob = Object.Oname n
          and prov = Pnull
          and call = if savable then topCallStack stack else NONE
          val obj = mkObject (ob,prov,call)
          val stack = pushStack stack obj
        in
          State {stack = stack, dict = dict, saved = saved}
        end

      (* Lists *)

      | Cop "nil" =>
        let
          val ob = Object.onil
          and prov = Pnull
          and call = if savable then topCallStack stack else NONE
          val obj = mkObject (ob,prov,call)
          val stack = pushStack stack obj
        in
          State {stack = stack, dict = dict, saved = saved}
        end

      | Cop "cons" =>
        let
          val (stack,
               objH as Object {object = obH, ...},
               objT as Object {object = obT, ...}) = pop2Stack stack
          val ob = Object.mkOcons (obH,obT)
          and prov =
              if containsThmsObject objH orelse containsThmsObject objT then
                Pcons (objH,objT)
              else
                Pnull
          and call = if savable then topCallStack stack else NONE
          val obj = mkObject (ob,prov,call)
          val stack = pushStack stack obj
        in
          State {stack = stack, dict = dict, saved = saved}
        end

      (* Types *)

      | Cop "type_var" =>
        let
          val (stack,
               Object {object = obN, ...}) = pop1Stack stack
          val ob = Object.mkOtypeVar obN
          and prov = Pnull
          and call = if savable then topCallStack stack else NONE
          val obj = mkObject (ob,prov,call)
          val stack = pushStack stack obj
        in
          State {stack = stack, dict = dict, saved = saved}
        end

      | Cop "type_op" =>
        let
          val (stack,
               Object {object = obN, ...},
               Object {object = obL, ...}) = pop2Stack stack
          val obN = Object.interpretType interpretation obN
          val ob = Object.mkOtypeOp (obN,obL)
          and prov = Pnull
          and call = if savable then topCallStack stack else NONE
          val obj = mkObject (ob,prov,call)
          val stack = pushStack stack obj
        in
          State {stack = stack, dict = dict, saved = saved}
        end

      (* Terms *)

      | Cop "var" =>
        let
          val (stack,
               Object {object = obN, ...},
               Object {object = obT, ...}) = pop2Stack stack
          val ob = Object.mkOtermVar (obN,obT)
          and prov = Pnull
          and call = if savable then topCallStack stack else NONE
          val obj = mkObject (ob,prov,call)
          val stack = pushStack stack obj
        in
          State {stack = stack, dict = dict, saved = saved}
        end

      | Cop "const" =>
        let
          val (stack,
               Object {object = obN, ...},
               Object {object = obT, ...}) = pop2Stack stack
          val obN = Object.interpretConst interpretation obN
          val ob = Object.mkOtermConst (obN,obT)
          and prov = Pnull
          and call = if savable then topCallStack stack else NONE
          val obj = mkObject (ob,prov,call)
          val stack = pushStack stack obj
        in
          State {stack = stack, dict = dict, saved = saved}
        end

      | Cop "comb" =>
        let
          val (stack,
               Object {object = obF, ...},
               Object {object = obA, ...}) = pop2Stack stack
          val ob = Object.mkOtermComb (obF,obA)
          and prov = Pnull
          and call = if savable then topCallStack stack else NONE
          val obj = mkObject (ob,prov,call)
          val stack = pushStack stack obj
        in
          State {stack = stack, dict = dict, saved = saved}
        end

      | Cop "abs" =>
        let
          val (stack,
               Object {object = obV, ...},
               Object {object = obB, ...}) = pop2Stack stack
          val ob = Object.mkOtermAbs (obV,obB)
          and prov = Pnull
          and call = if savable then topCallStack stack else NONE
          val obj = mkObject (ob,prov,call)
          val stack = pushStack stack obj
        in
          State {stack = stack, dict = dict, saved = saved}
        end

      (* Theorems *)

      | Cop "thm" =>
        let
          val (stack,
               Object {object = obH, ...},
               Object {object = obC, ...}) = pop2Stack stack

          val seq =
              {hyp = TermAlphaSet.fromList (Object.destOterms obH),
               concl = Object.destOterm obC}

          val (th,deps) =
              case findAlpha known seq of
                SOME th => (th,[])
              | NONE =>
                case searchSaved saved seq of
                  SOME th => (th,[])
                | NONE =>
                  case simulate interpretation stack seq of
                    SOME th_deps => th_deps
                  | NONE =>
                    case searchStack stack seq of
                      SOME th_deps => th_deps
                    | NONE =>
                      let
                        val th = Thm.axiom seq
(*OpenTheoryTrace1
                        val () = trace ("making new axiom in " ^
                                        topCallToStringStack stack ^ ":\n" ^
                                        thmToString th ^ "\n")
*)
                      in
                        (th,[])
                      end

          val ob = Object.Othm th
          and prov = Pthm (if savable then deps else [])
          and call = if savable then topCallStack stack else NONE
          val obj = mkObject (ob,prov,call)
          val stack = pushStack stack obj
        in
          State {stack = stack, dict = dict, saved = saved}
        end

      (* Function calls *)

      | Cop "call" =>
        let
          val (stack,
               objA as Object {object = obA, ...},
               Object {object = obN, ...}) = pop2Stack stack
          val _ = not (Object.isOcall obA) orelse
                  raise Error "cannot use an Ocall object as a call argument"
          val n = Object.destOname obN
          val n = Interpretation.interpretRule interpretation n
(*OpenTheoryTrace2
          val traceCall = null (callStack stack)
(*OpenTheoryTrace3
          val traceCall = true
*)
          val () = if not traceCall then ()
                   else trace ("call: " ^ Name.toString n ^ "\n" ^
                               "  stack = ["^Int.toString (sizeStack stack) ^
                               "], call stack = [" ^
                               Int.toString (length (callStack stack))^"]\n")
          val () = if not traceCall then ()
                   else Print.trace Object.pp "  input" obA
*)
          val ob = Object.Ocall n
          and prov = Pcall objA
          and call = if savable then topCallStack stack else NONE
          val obj = mkObject (ob,prov,call)
          val stack = pushStack stack obj
          val stack = pushStack stack objA
        in
          State {stack = stack, dict = dict, saved = saved}
        end

      | Cop "return" =>
        let
          val (stack,
               objR as Object {object = obR, provenance = provR, ...},
               Object {object = obN, ...}) = pop2Stack stack
          val _ = not (Object.isOcall obR) orelse
                  raise Error "cannot use an Ocall object as a return value"
          val n = Object.destOname obN
          val n = Interpretation.interpretRule interpretation n
          val (n',stack) = popCallStack stack
          val _ = Name.equal n' n orelse
                  raise Error ("call " ^ Name.toString n' ^
                               " matched by return " ^ Name.toString n)
(*OpenTheoryTrace2
          val traceReturn = null (callStack stack)
(*OpenTheoryTrace3
          val traceReturn = true
*)
          val () = if not traceReturn then ()
                   else trace ("return: " ^ Name.toString n ^ "\n" ^
                               "  stack = ["^Int.toString (sizeStack stack) ^
                               "], call stack = [" ^
                               Int.toString (length (callStack stack))^"]\n")
          val () = if not traceReturn then ()
                   else Print.trace Object.pp "  return" obR
*)
          val ob = obR
          and prov =
              if not (containsThmsObject objR) then Pnull
              else if savable then ObjectProv.Preturn objR
              else provR
          and call = if savable then topCallStack stack else NONE
          val obj = mkObject (ob,prov,call)
          val stack = pushStack stack obj
        in
          State {stack = stack, dict = dict, saved = saved}
        end

      (* Dictionary *)

      | Cop "def" =>
        let
          val (stack,
               Object {object = obI, ...}) = pop1Stack stack
          val objD as Object {object = obD, ...} = peekStack stack 0
          val _ = not (Object.isOcall obD) orelse
                  raise Error "cannot def an Ocall object"
          val i = Object.destOnum obI
          val dict = defDict dict (i,objD)
        in
          State {stack = stack, dict = dict, saved = saved}
        end

      | Cop "ref" =>
        let
          val (stack,
               Object {object = obI, ...}) = pop1Stack stack
          val i = Object.destOnum obI
          val objD as Object {object = obD, provenance = provD, ...} =
              refDict dict i
          val ob = obD
          and prov =
              if not (containsThmsObject objD) then Pnull
              else if savable then Pref objD
              else provD
          and call = if savable then topCallStack stack else NONE
          val obj = mkObject (ob,prov,call)
          val stack = pushStack stack obj
        in
          State {stack = stack, dict = dict, saved = saved}
        end

      | Cop "remove" =>
        let
          val (stack,
               Object {object = obI, ...}) = pop1Stack stack
          val i = Object.destOnum obI
          val (dict, objD as Object {object = obD, provenance = provD, ...}) =
              removeDict dict i
          val ob = obD
          and prov =
              if not (containsThmsObject objD) then Pnull
              else if savable then Pref objD
              else provD
          and call = if savable then topCallStack stack else NONE
          val obj = mkObject (ob,prov,call)
          val stack = pushStack stack obj
        in
          State {stack = stack, dict = dict, saved = saved}
        end

      (* General *)

      | Cop "pop" =>
        let
          val stack = popStack stack 1
        in
          State {stack = stack, dict = dict, saved = saved}
        end

      | Cop "dup" =>
        let
          val (stack,
               Object {object = obI, ...}) = pop1Stack stack
          val i = Object.destOnum obI
          val objD as Object {object = obD, provenance = provD, ...} =
              peekStack stack i
          val ob = obD
          and prov =
              if not (containsThmsObject objD) then Pnull
              else if savable then Pref objD
              else provD
          and call = if savable then topCallStack stack else NONE
          val _ = not (Object.isOcall ob) orelse
                  raise Error "cannot dup an Ocall object"
          val obj = mkObject (ob,prov,call)
          val stack = pushStack stack obj
        in
          State {stack = stack, dict = dict, saved = saved}
        end

      | Cop "save" =>
        let
          val (stack,
               objT) = pop1Stack stack
          val saved = addSaved saved objT
        in
          State {stack = stack, dict = dict, saved = saved}
        end

      (* Any other command is an error *)

      | Cop _ => raise Error "unknown command"
    end
    handle Error err => raise Error (commandToString cmd ^ ": " ^ err);

fun executeCommands savable known interpretation =
    let
      fun process (command,state) =
          executeCommand savable known interpretation command state
    in
      Stream.foldl process initialState
    end;

(* ------------------------------------------------------------------------- *)
(* Generating simple objects.                                                *)
(* ------------------------------------------------------------------------- *)

fun generateObject ob =
    case ob of
      Object.Oerror => (Cop "error", [])
    | Object.Onum i => (Cnum i, [])
    | Object.Oname n => (Cname n, [])
    | Object.Olist l =>
      (case l of
         [] => (Cop "nil", [])
       | h :: t => (Cop "cons", [h, Object.Olist t]))
    | Object.Otype ty =>
      (case Type.dest ty of
         Type.TypeVar n => (Cop "type_var", [Object.Oname n])
       | Type.TypeOp (n,tys) =>
         (Cop "type_op", [Object.Oname n, Object.mkOtypes tys]))
    | Object.Oterm tm =>
      (case Term.dest tm of
         Term.Const (n,ty) =>
         (Cop "const", [Object.Oname n, Object.Otype ty])
       | Term.Var (Var.Var (n,ty)) =>
         (Cop "var", [Object.Oname n, Object.Otype ty])
       | Term.Comb (f,a) =>
         (Cop "comb", [Object.Oterm f, Object.Oterm a])
       | Term.Abs (v,b) =>
         (Cop "abs", [Object.Oterm (Term.mkVar v), Object.Oterm b]))
    | Object.Othm th =>
      let
        val {hyp,concl} = sequent th
        val hyp = TermAlphaSet.toList hyp
      in
        (Cop "thm", [Object.mkOterms hyp, Object.Oterm concl])
      end
    | Object.Ocall _ => raise Bug "Article.generateObject: Ocall";

(* ------------------------------------------------------------------------- *)
(* Generating commands.                                                      *)
(* ------------------------------------------------------------------------- *)

datatype minDict =
    MinDict of
      {nextKey : int,
       refs : int ObjectMap.map,
       keys : int ObjectMap.map};

fun nullMinDict (MinDict {keys,...}) = ObjectMap.null keys;

local
  fun storable ob =
      case ob of
        Object.Oerror => false
      | Object.Onum _ => false
      | Object.Oname _ => false
      | Object.Olist l => not (null l)
      | Object.Otype _ => true
      | Object.Oterm _ => true
      | Object.Othm _ => true
      | Object.Ocall _ => raise Bug "Article.storable: Ocall";

  fun registerTop refs ob =
      let
        val p = ObjectMap.peek refs ob
        val known = Option.isSome p
        val k = Option.getOpt (p,0)
        val refs = ObjectMap.insert refs (ob, k + 1)
      in
        (known,refs)
      end;

  fun registerDeep refs [] = refs
    | registerDeep refs (ob :: obs) =
      if not (storable ob) then registerDeep refs obs
      else
        let
          val (known,refs) = registerTop refs ob
          val obs = if known then obs else snd (generateObject ob) @ obs
        in
          registerDeep refs obs
        end;

  fun register (obj,refs) =
      let
        val Object {object = ob, provenance = prov, ...} = obj
      in
        case prov of
          Pnull => registerDeep refs [ob]
        | Pcall _ => refs
        | ObjectProv.Preturn _ => refs
        | Pcons _ =>
          let
            val (known,refs) = registerTop refs ob
(*OpenTheoryDebug
            val _ =
                not known orelse
                let
                  val () = Print.trace (ppObject 1) "deja vu obj" obj
                  val k = ObjectMap.peek refs ob
                  val () = Print.trace (Print.ppOption Print.ppInt) "refs" k
                in
                  raise Bug "Article.register: Pcons"
                end
*)
          in
            refs
          end
        | Pref _ =>
          let
            val (known,refs) = registerTop refs ob
(*OpenTheoryDebug
            val _ = known orelse raise Bug "Article.register: Pref"
*)
          in
            refs
          end
        | Pthm _ => registerDeep refs [ob]
      end;
in
  fun newMinDict objs =
      let
        val nextKey = 1
        val refs = ObjectMap.new ()
        val refs = foldlObjectSet register refs objs
        val refs = ObjectMap.filter (fn (_,n) => n >= 2) refs
        val keys = ObjectMap.new ()
      in
        MinDict
          {nextKey = nextKey,
           refs = refs,
           keys = keys}
      end
      handle Error err =>
        raise Bug ("Article.newMinDict: " ^ err);
end;

fun alignCalls prevCall call cmds =
    case prevCall of
      NONE =>
      (case call of
         NONE => cmds
       | SOME _ => raise Bug "Article.alignCalls: top level to nested")
    | SOME (Object {id, object = ob, call = prevCall, ...}) =>
      let
        val aligned =
            case call of
              NONE => false
            | SOME (Object {id = id', ...}) => id = id'
      in
        if aligned then cmds
        else
          let
            val cmds =
                case ob of
                  Object.Ocall n =>
                  Cop "return" :: Cname n :: Cop "error" :: cmds
                | _ => raise Bug "Article.alignCalls: Ocall"
          in
            alignCalls prevCall call cmds
          end
      end;

local
  fun isKey (MinDict {keys,...}) ob = ObjectMap.inDomain ob keys;

  fun addKey dict cmds ob =
      let
        val MinDict {nextKey,refs,keys} = dict
(*OpenTheoryDebug
        val _ = not (ObjectMap.inDomain ob keys) orelse
                raise Bug "Article.addKey"
*)
      in
        case ObjectMap.peek refs ob of
          NONE => (dict,cmds)
        | SOME n =>
          let
            val key = nextKey
            val nextKey = nextKey + 1
            val keys = ObjectMap.insert keys (ob,key)
            val refs = ObjectMap.insert refs (ob, n - 1)
            val dict =
                MinDict {nextKey = nextKey, refs = refs, keys = keys}
            val cmds = [Cop "def", Cnum key] @ cmds
          in
            (dict,cmds)
          end
      end;

  fun useKey dict cmds ob =
      let
        val MinDict {nextKey,refs,keys} = dict
      in
        case ObjectMap.peek keys ob of
          NONE => raise Bug "Article.useKey"
        | SOME key =>
          let
            val cmds = Cnum key :: cmds
          in
            case ObjectMap.peek refs ob of
              NONE => raise Bug "generate"
            | SOME n =>
              if n = 1 then
                let
                  val refs = ObjectMap.delete refs ob
                  val keys = ObjectMap.delete keys ob
                  val dict =
                      MinDict {nextKey = nextKey, refs = refs, keys = keys}
                  val cmds = Cop "remove" :: cmds
                in
                  (dict,cmds)
                end
              else
                let
                  val refs = ObjectMap.insert refs (ob, n - 1)
                  val dict =
                      MinDict {nextKey = nextKey, refs = refs, keys = keys}
                  val cmds = Cop "ref" :: cmds
                in
                  (dict,cmds)
                end
          end
      end;

  fun generateDeep (ob,(dict,cmds)) =
      if isKey dict ob then useKey dict cmds ob
      else
        let
          val (cmd,pars) = generateObject ob
          val (dict,cmds) = foldl generateDeep (dict,cmds) pars
          val cmds = cmd :: cmds
        in
          addKey dict cmds ob
        end;
in
  fun generateMinDict stacksize prevCall dict obj =
      let
        val Object {object = ob, provenance = prov, call, ...} = obj
        val cmds = []
      in
        case prov of
          Pnull =>
          let
            val cmds = alignCalls prevCall call cmds
            val (dict,cmds) = generateDeep (ob,(dict,cmds))
            val stacksize = stacksize + (if Option.isSome call then 0 else 1)
          in
            (stacksize,call,dict,cmds)
          end
        | Pcall _ =>
          let
            val cmds = alignCalls prevCall call cmds
            val cmds =
                case ob of
                  Object.Ocall n => Cop "call" :: Cname n :: cmds
                | _ => raise Bug "Article.generateMinDict: Pcall"
            val call = SOME obj
          in
            (stacksize,call,dict,cmds)
          end
        | ObjectProv.Preturn (Object {call = rcall, ...}) =>
          let
            val cmds = alignCalls prevCall rcall cmds
            val cmds =
                case rcall of
                  SOME (Object {object = Object.Ocall n, ...}) =>
                  Cop "return" :: Cname n :: cmds
                | _ => raise Bug "Article.generateMinDict: Preturn"
          in
            (stacksize,call,dict,cmds)
          end
        | Pcons _ =>
          let
            val cmds = alignCalls prevCall call cmds
            val cmds = Cop "cons" :: cmds
            val (dict,cmds) = addKey dict cmds ob
            val stacksize = stacksize - (if Option.isSome call then 0 else 1)
          in
            (stacksize,call,dict,cmds)
          end
        | Pref _ =>
          let
            val cmds = alignCalls prevCall call cmds
            val (dict,cmds) = useKey dict cmds ob
            val stacksize = stacksize + (if Option.isSome call then 0 else 1)
          in
            (stacksize,call,dict,cmds)
          end
        | Pthm _ =>
          let
            val cmds = alignCalls prevCall call cmds
            val (dict,cmds) = generateDeep (ob,(dict,cmds))
            val stacksize = stacksize + (if Option.isSome call then 0 else 1)
          in
            (stacksize,call,dict,cmds)
          end
      end
      handle Error err =>
        raise Bug ("Article.generateMinDict: " ^ err);
end;

fun generate saved objs =
    let
      fun gen obj (stacksize,call,dict) =
          let
            val (stacksize,call,dict,cmds) =
                generateMinDict stacksize call dict obj

            val cmds =
                if not (memberObjectSet obj saved) then cmds
                else Cop "save" :: Cop "dup" :: Cnum 0 :: cmds
          in
            (cmds,(stacksize,call,dict))
          end

      fun finish (stacksize,call,dict) =
          let
(*OpenTheoryDebug
            val _ = nullMinDict dict orelse raise Error "nonempty dict"
*)
            val cmds = []
            val cmds = alignCalls call NONE cmds
            val cmds = funpow stacksize (cons (Cop "pop")) cmds
          in
            if null cmds then Stream.Nil else Stream.singleton cmds
          end

      val stacksize = 0
      val call = NONE
      val dict = newMinDict objs

      val strm = toStreamObjectSet objs
      val strm = Stream.maps gen finish (stacksize,call,dict) strm
    in
      revConcat strm
    end
    handle Error err =>
      raise Bug ("Article.generate: " ^ err);

(* ------------------------------------------------------------------------- *)
(* A type of proof articles.                                                 *)
(* ------------------------------------------------------------------------- *)

datatype article =
    Article of
      {thms : ThmSet.set,
       saved : objectSet option};

val empty =
    Article
      {thms = ThmSet.empty,
       saved = SOME emptyObjectSet};

fun append art1 art2 =
    let
      val Article {thms = thms1, saved = saved1} = art1
      and Article {thms = thms2, saved = saved2} = art2

      val thms = ThmSet.union thms1 thms2

      val saved =
          case (saved1,saved2) of
            (SOME s1, SOME s2) => SOME (unionObjectSet s1 s2)
          | _ => NONE
    in
      Article
        {thms = thms,
         saved = saved}
    end;

fun saved (Article {thms = x, ...}) = x;

fun prove article = findAlpha (saved article);

val summarize = Summary.fromThms o saved;

fun isSavable (Article {saved,...}) = Option.isSome saved;

(* ------------------------------------------------------------------------- *)
(* Input/Output.                                                             *)
(* ------------------------------------------------------------------------- *)

local
  (* Comment lines *)

  fun isComment l =
      case List.find (not o Char.isSpace) l of
        NONE => true
      | SOME #"#" => true
      | _ => false;
in
  fun executeTextFile {savable,known,interpretation,filename} =
      let
        (* Estimating parse error line numbers *)

        val lines = Stream.fromTextFile {filename = filename}

        val {chars,parseErrorLocation} = Parse.initialize {lines = lines}
      in
        (let
           (* The character stream *)

           val chars = Stream.filter (not o isComment) chars

           val chars = Parse.everything Parse.any chars

           (* The command stream *)

           val commands = Parse.everything spacedCommandParser chars
         in
           executeCommands savable known interpretation commands
         end
         handle Parse.NoParse => raise Error "parse error")
        handle Error err =>
          raise Error ("error in article file \"" ^ filename ^ "\" " ^
                       parseErrorLocation () ^ "\n" ^ err)
      end;
end;

fun fromTextFile {savable,known,interpretation,filename} =
    let
      val State {stack,dict,saved} =
          executeTextFile
            {savable = savable,
             known = known,
             filename = filename,
             interpretation = interpretation}

      val saved = theoremsSaved saved

      val saved =
          let
            val n = sizeStack stack
          in
            if n = 0 then saved
            else
              let
                val () = warn (Int.toString n ^ " object" ^
                               (if n = 1 then "" else "s") ^
                               " left on the stack by " ^ filename)

                val saved' = theoremsStack stack
                val saved' = unionTheorems saved saved'

                val n = sizeTheorems saved
                val n' = sizeTheorems saved' - n
              in
                if n = 0 then
                  let
                    val () =
                        if n' = 0 then ()
                        else
                          warn ("saving " ^ Int.toString n' ^ " theorem" ^
                                (if n' = 1 then "" else "s") ^
                                " left on the stack by " ^ filename)
                  in
                    saved'
                  end
                else
                  let
                    val () =
                        if n' = 0 then ()
                        else
                          warn (Int.toString n' ^ " unsaved theorem" ^
                                (if n' = 1 then "" else "s") ^
                                " left on the stack by " ^ filename)
                  in
                    saved
                  end
              end
          end

      val () =
          if sizeTheorems saved > 0 then ()
          else warn ("no theorems saved or left on the stack by " ^ filename)

      val () =
          let
            val n = sizeDict dict
          in
            if n = 0 then ()
            else
              warn (Int.toString n ^ " object" ^
                    (if n = 1 then "" else "s") ^
                    " left in the dictionary by " ^ filename)
          end

      val saved = toObjectSetTheorems saved

      val thms = toThmSetObjectSet saved

      val saved = if savable then SOME saved else NONE
    in
      Article
        {thms = thms,
         saved = saved}
    end
    handle Error err => raise Error ("Article.fromTextFile: " ^ err);

fun toTextFile {article,filename} =
    let
      val Article {saved,...} = article

      val saved =
          case saved of
            SOME s => toListObjectSet s
          | NONE => raise Error "unsavable"

      val (objs,saved) = reduceObject saved

      val saved = fromListObjectSet saved

(*OpenTheoryTrace3
      val () = Print.trace ppObjectSet "Article.toTextFile: objs" objs
      val () = Print.trace ppObjectSet "Article.toTextFile: saved" saved
*)

      val commands = generate saved objs

      val lines = Stream.map (fn c => commandToString c ^ "\n") commands
    in
      Stream.toTextFile {filename = filename} lines
    end
    handle Error err => raise Error ("Article.toTextFile: " ^ err);

end
