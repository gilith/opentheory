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

fun natFromString err s =
    case Int.fromString s of
      SOME i => i
    | NONE => raise Error err;

(* ------------------------------------------------------------------------- *)
(* Object IDs.                                                               *)
(* ------------------------------------------------------------------------- *)

type objectId = int;

val newObjectId : unit -> objectId =
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

(* ------------------------------------------------------------------------- *)
(* Object provenance.                                                        *)
(*                                                                           *)
(* Invariants *in order of priority*                                         *)
(*                                                                           *)
(* 1. The provenance of an Ocall object is the object that became the call   *)
(*    argument.                                                              *)
(*                                                                           *)
(* 2. Objects do not contain theorems iff they have provenance Pnull.        *)
(*    [Objects that do not contain theorems can be easily constructed.]      *)
(*                                                                           *)
(* 3. The provenance of a return value is the object that became the return  *)
(*    value.                                                                 *)
(*                                                                           *)
(* 4. If a theorem can be inferred from theorem-containing objects, then the *)
(*    provenance of the theorem is these objects.                            *)
(*    [This will happen most often when simulating an inference rule and     *)
(*    making use of the call argument - resulting in a singleton list.]      *)
(* ------------------------------------------------------------------------- *)

datatype object =
    Object of
      {id : objectId,
       object : Object.object,
       provenance : provenance,
       call : object option}

and provenance =
    Pnull
  | Pcall of object
  | Preturn of object
  | Pcons of object * object
  | Pref of object
  | Pthm of object list;

fun compareObject (Object {id = i1, ...}, Object {id = i2, ...}) =
    Int.compare (i1,i2);

fun object (Object {object = x, ...}) = x;

fun callObject (Object {call = x, ...}) = x;

local
  fun maps lr preF postF obj acc =
      case preF obj acc of
        Left obj => (obj,acc)
      | Right obj =>
        let
          val Object {id, object = ob, provenance = prov, call} = obj

          val (prov',call',acc) =
              if lr then
                let
                  val (prov',acc) = mapsProv lr preF postF prov acc
                  val (call',acc) =
                      Sharing.mapsOption (maps lr preF postF) call acc
                in
                  (prov',call',acc)
                end
              else
                let
                  val (call',acc) =
                      Sharing.mapsOption (maps lr preF postF) call acc
                  val (prov',acc) = mapsProv lr preF postF prov acc
                in
                  (prov',call',acc)
                end

          val obj =
              if Portable.pointerEqual (prov',prov) andalso
                 Portable.pointerEqual (call',call) then
                obj
              else
                Object {id = id, object = ob, provenance = prov', call = call'}

          val acc = postF obj acc
        in
          (obj,acc)
        end

  and mapsProv lr preF postF prov acc =
      case prov of
        Pnull => (prov,acc)
      | Pcall obj => mapsProv1 lr preF postF prov acc Pcall obj
      | Preturn obj => mapsProv1 lr preF postF prov acc Preturn obj
      | Pcons (objH,objT) =>
        let
          val (objH',objT',acc) =
              if lr then
                let
                  val (objH',acc) = maps lr preF postF objH acc
                  val (objT',acc) = maps lr preF postF objT acc
                in
                  (objH',objT',acc)
                end
              else
                let
                  val (objT',acc) = maps lr preF postF objT acc
                  val (objH',acc) = maps lr preF postF objH acc
                in
                  (objH',objT',acc)
                end

          val prov' =
              if Portable.pointerEqual (objH',objH) andalso
                 Portable.pointerEqual (objT',objT) then
                prov
              else
                Pcons (objH',objT')
        in
          (prov',acc)
        end
      | Pref obj => mapsProv1 lr preF postF prov acc Pref obj
      | Pthm objs =>
        let
          val (objs',acc) =
              (if lr then Sharing.maps else Sharing.revMaps)
                (maps lr preF postF) objs acc

          val prov' =
              if Portable.pointerEqual (objs',objs) then prov else Pthm objs'
        in
          (prov',acc)
        end

  and mapsProv1 lr preF postF prov acc con obj =
      let
        val (obj',acc) = maps lr preF postF obj acc
        val prov' = if Portable.pointerEqual (obj',obj) then prov else con obj'
      in
        (prov',acc)
      end;
in
  fun mapsObject preF postF obj acc = maps true preF postF obj acc;

  fun revMapsObject preF postF obj acc = maps false preF postF obj acc;
end;

fun callStackObject obj =
    case callObject obj of
      NONE => []
    | SOME c => c :: callStackObject c;

fun mkObject (object,provenance,call) =
    let
      val id = newObjectId ()
    in
      Object
        {id = id,
         object = object,
         provenance = provenance,
         call = call}
    end;

fun containsThmsObject obj =
    case obj of
      Object {object = Object.Ocall _, ...} =>
      raise Bug "Article.containsThmsObject: Ocall"
    | Object {provenance = Pnull, ...} => false
    | _ => true;

fun ppObject level obj =
    let
      val Object {id, object = ob, provenance = prov, call} = obj
      val level = level - 1
    in
      if level = ~1 then Print.ppInt id
      else
        Print.blockProgram Print.Consistent 2
          [Print.addString "Object",
           Print.addBreak 1,
           Print.record
             [("id", Print.ppInt id),
              ("object", Object.pp ob),
              ("provenance", ppProvenance level prov),
              ("call", Print.ppOption (ppObject level) call)]]
    end

and ppProvenance level prov =
    case prov of
      Pnull => Print.addString "Pnull"
    | Pcall obj =>
      Print.blockProgram Print.Consistent 2
        [Print.addString "Pcall",
         Print.addBreak 1,
         ppObject level obj]
    | Preturn obj =>
      Print.blockProgram Print.Consistent 2
        [Print.addString "Preturn",
         Print.addBreak 1,
         ppObject level obj]
    | Pcons objH_objT =>
      Print.blockProgram Print.Consistent 2
        [Print.addString "Pcons",
         Print.addBreak 1,
         Print.ppPair (ppObject level) (ppObject level) objH_objT]
    | Pref obj =>
      Print.blockProgram Print.Consistent 2
        [Print.addString "Pref",
         Print.addBreak 1,
         ppObject level obj]
    | Pthm objs =>
      Print.blockProgram Print.Consistent 2
        [Print.addString "Pthm",
         Print.addBreak 1,
         Print.ppList (ppObject level) objs];

(* ------------------------------------------------------------------------- *)
(* Object sets.                                                              *)
(* ------------------------------------------------------------------------- *)

datatype objectSet = ObjectSet of object Set.set;

val emptyObjectSet = ObjectSet (Set.empty compareObject);

fun nullObjectSet (ObjectSet set) = Set.null set;

fun sizeObjectSet (ObjectSet set) = Set.size set;

fun memberObjectSet obj (ObjectSet set) = Set.member obj set;

fun peekObjectSet (ObjectSet set) obj = Set.peek set obj;

fun addObjectSet (ObjectSet set) obj = ObjectSet (Set.add set obj);

fun addListObjectSet set objs =
    List.foldl (fn (obj,acc) => addObjectSet acc obj) set objs;

fun fromListObjectSet objs = addListObjectSet emptyObjectSet objs;

fun foldlObjectSet f b (ObjectSet set) = Set.foldl f b set;

local
  fun toStream NONE = Stream.Nil
    | toStream (SOME iter) =
      Stream.Cons (Set.readIterator iter, toStreamDelay iter)

  and toStreamDelay iter () = toStream (Set.advanceIterator iter)
in
  fun toStreamObjectSet (ObjectSet set) = toStream (Set.mkIterator set);
end;

val ppObjectSet =
    Print.ppBracket "{" "}" (Print.ppMap sizeObjectSet Print.ppInt);

(* ------------------------------------------------------------------------- *)
(* Reducing objects.                                                         *)
(* ------------------------------------------------------------------------- *)

local
  fun preReduce obj (reqd_refs as (reqd,refs)) =
      case peekObjectSet reqd obj of
        SOME obj => Left obj
      | NONE =>
        Right
        let
          val Object {id, object = ob, provenance = prov, call} = obj

          fun better rid =
              rid < id andalso
              case prov of
                Preturn (Object {id = rid', ...}) => rid < rid'
              | Pref (Object {id = rid', ...}) => rid < rid'
              | _ => true
        in
          case prov of
            Pnull => obj
          | Pcall _ => obj
          | _ =>
            case ObjectMap.peek refs ob of
              NONE => obj
            | SOME (robj as Object {id = rid, ...}) =>
              if not (better rid) then obj
              else
                Object
                  {id = id,
                   object = ob,
                   provenance = Pref robj,
                   call = call}
        end;

  fun postReduce (obj as Object {id, object = ob, ...}) (reqd,refs) =
      let
        val reqd = addObjectSet reqd obj

        val insertRefs =
            case ObjectMap.peek refs ob of
              NONE => true
            | SOME (Object {id = id', ...}) => id < id'

        val refs = if insertRefs then ObjectMap.insert refs (ob,obj) else refs
      in
        (reqd,refs)
      end;

  fun dependencies clean lr objs =
      let
        val reqd = emptyObjectSet
        val refs = ObjectMap.new ()
        val (objs',(reqd,_)) =
            if lr then
              Sharing.maps (mapsObject preReduce postReduce)
                objs (reqd,refs)
            else
              Sharing.revMaps (revMapsObject preReduce postReduce)
                objs (reqd,refs)

        val clean' = Portable.pointerEqual (objs',objs)

        val lr = not lr
      in
        if clean andalso clean' then (reqd,objs)
        else dependencies clean' lr objs'
      end;
in
  fun dependenciesObject objs =
      dependencies false true objs
      handle Error err =>
        raise Bug ("Article.dependenciesObjects: " ^ err);
end;

(* ------------------------------------------------------------------------- *)
(* Theorems in scope.                                                        *)
(* ------------------------------------------------------------------------- *)

datatype theorems =
    Theorems of (Thm.thm * object list) SequentMap.map;

val noTheorems = Theorems (SequentMap.new ());

fun sizeTheorems (Theorems thms) = SequentMap.size thms;

fun addTheorems thms th_deps =
    let
      val Theorems thmMap = thms
      and (th,_) = th_deps
    in
      Theorems (SequentMap.insert thmMap (sequent th, th_deps))
    end;

local
  fun add deps (th,thms) = addTheorems thms (th,deps);
in
  fun addObjectTheorems thms obj =
      List.foldl (add [obj]) thms (Object.thms (object obj));
end;

fun searchTheorems (Theorems thmMap) seq =
    case SequentMap.peek thmMap seq of
      NONE => NONE
    | SOME (th,deps) => SOME (alpha seq th, deps);

(* ------------------------------------------------------------------------- *)
(* Dictionaries.                                                             *)
(* ------------------------------------------------------------------------- *)

datatype dict = Dict of object IntMap.map;

val emptyDict = Dict (IntMap.new ());

fun sizeDict (Dict m) = IntMap.size m;

fun defDict (Dict dict) (key,obj) = Dict (IntMap.insert dict (key,obj));

fun refDict (Dict dict) key =
    case IntMap.peek dict key of
      SOME obj => obj
    | NONE => raise Error ("refDict: no entry for key " ^ Int.toString key);

fun removeDict (Dict dict) key =
    case IntMap.peek dict key of
      SOME obj => (Dict (IntMap.delete dict key), obj)
    | NONE => raise Error ("removeDict: no entry for key " ^ Int.toString key);

(* ------------------------------------------------------------------------- *)
(* Stacks.                                                                   *)
(* ------------------------------------------------------------------------- *)

datatype stack =
    Stack of
      {size : int,
       objects : object list,
       theorems : theorems list,
       call : (object * stack) option};

val emptyStack =
    Stack
      {size = 0,
       objects = [],
       theorems = [],
       call = NONE};

fun sizeStack (Stack {size = x, ...}) = x;

fun nullStack stack = sizeStack stack = 0;

fun frameSizeStack (Stack {size,call,...}) =
    size - (case call of NONE => 0 | SOME (_,stack) => sizeStack stack + 1);

fun objectStack (Stack {objects = x, ...}) = x;

fun theoremsStack (Stack {theorems,...}) =
    case theorems of
      [] => noTheorems
    | ths :: _ => ths;

fun pushStack stack obj =
    let
      val Stack {size,objects,theorems,call} = stack

      val size = size + 1

      val objects = obj :: objects

      val thms = addObjectTheorems (theoremsStack stack) obj

      val theorems = thms :: theorems

      val call = if Object.isOcall (object obj) then SOME (obj,stack) else call
    in
      Stack
        {size = size,
         objects = objects,
         theorems = theorems,
         call = call}
    end;

fun popStack stack n =
    if n > frameSizeStack stack then raise Error "Article.popStack: empty frame"
    else
      let
        val Stack {size,objects,theorems,call} = stack

        val size = size - n

        val objects = List.drop (objects,n)

        val theorems = List.drop (theorems,n)
      in
        Stack
          {size = size,
           objects = objects,
           theorems = theorems,
           call = call}
      end;

fun peekStack stack n =
    let
      val Stack {size,objects,...} = stack
    in
      if n >= size then raise Error "Article.peekStack: bad index"
      else List.nth (objects,n)
    end;

fun pop1Stack stack =
    (popStack stack 1,
     peekStack stack 0);

fun pop2Stack stack =
    (popStack stack 2,
     peekStack stack 1,
     peekStack stack 0);

fun popCallStack (Stack {call,...}) =
    case call of
      NONE => raise Error "Article.popCallStack: top level"
    | SOME (Object {object = ob, ...}, stack) => (Object.destOcall ob, stack);

fun topCallStack (Stack {call,...}) =
    case call of
      NONE => NONE
    | SOME (obj,_) => SOME obj;

fun topCallToStringStack stack =
    case topCallStack stack of
      NONE => "top level"
    | SOME obj =>
      case object obj of
        Object.Ocall n => Name.toString n
      | _ => raise Bug "Article.topCallToStringStack";

fun callStack stack =
    case topCallStack stack of
      NONE => []
    | SOME obj => obj :: callStackObject obj;

fun searchStack (Stack {theorems,...}) seq =
    case theorems of
      [] => NONE
    | thms :: _ => searchTheorems thms seq;

local
  fun push (obj,stack) = pushStack stack obj;
in
  fun appendStack stack1 stack2 =
      if nullStack stack1 then stack2
      else List.foldl push stack1 (rev (objectStack stack2));
end;

(* ------------------------------------------------------------------------- *)
(* Saved theorems.                                                           *)
(* ------------------------------------------------------------------------- *)

datatype saved = Saved of stack;

val emptySaved = Saved emptyStack;

fun appendSaved (Saved stack1) (Saved stack2) =
    Saved (appendStack stack1 stack2);

fun sizeSaved (Saved stack) = sizeStack stack;

fun addSaved (Saved stack) obj =
    if Object.isOthm (object obj) then Saved (pushStack stack obj)
    else raise Error "Object.addSaved: not an Othm object";

fun searchSaved (Saved stack) seq =
    case searchStack stack seq of
      SOME (th,_) => SOME th
    | NONE => NONE;

fun listSaved (Saved stack) = rev (objectStack stack);

val thmsSaved = ThmSet.fromList o map (Object.destOthm o object) o listSaved;

local
  fun add (obj,set) = ObjectMap.insert set (object obj, obj);
in
  fun toSetSaved (Saved stack) =
      List.foldl add (ObjectMap.new ()) (objectStack stack);
end;

local
  fun addSet (_,obj,set) = addObjectSet set obj;

  fun addStack (obj,stack) = pushStack stack obj;
in
  fun fromSetSaved set =
      let
        val objs = ObjectMap.foldl addSet emptyObjectSet set
        val stack = foldlObjectSet addStack emptyStack objs
      in
        Saved stack
      end;
end;

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
            val r = sim interpretation a
            val ths = Object.thms r
          in
            case first (total (alpha seq)) ths of
              SOME th => SOME (th,[])
            | NONE => NONE
          end
      end
    | _ => NONE;

(* ------------------------------------------------------------------------- *)
(* Commands.                                                                 *)
(* ------------------------------------------------------------------------- *)

datatype command =
    Cnum of int
  | Cname of Name.name
  | Cop of string;

fun ppCommand cmd =
    case cmd of
      Cnum i => Print.ppInt i
    | Cname n => Name.ppQuoted n
    | Cop c => Print.ppString c;

val commandToString = Print.toString ppCommand;

local
  infixr 9 >>++
  infixr 8 ++
  infixr 7 >>
  infixr 6 ||

  open Parse;

  fun isAlphaNum #"_" = true
    | isAlphaNum c = Char.isAlphaNum c;

  val space = many (some Char.isSpace) >> K ();

  val cnumParser =
      atLeastOne (some Char.isDigit) >>
      (Cnum o natFromString "bad number" o implode);

  val cnameParser = Name.quotedParser >> Cname;

  val tcommandParser =
      (some Char.isAlpha ++ many (some isAlphaNum)) >>
      (Cop o implode o op::);
in
  val commandParser = cnumParser || cnameParser || tcommandParser;

  val spacedCommandParser =
      (space ++ commandParser ++ space) >> (fn ((),(t,())) => [t]);
end;

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

fun executeCommand known interpretation cmd state =
    let
      val State {stack,dict,saved} = state
    in
      case cmd of
      (* Errors *)

        Cop "error" =>
        let
          val ob = Object.Oerror
          and prov = Pnull
          and call = topCallStack stack
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
          and call = topCallStack stack
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
          and call = topCallStack stack
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
          and call = topCallStack stack
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
          and call = topCallStack stack
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
          and call = topCallStack stack
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
          and call = topCallStack stack
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
          and call = topCallStack stack
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
          and call = topCallStack stack
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
          and call = topCallStack stack
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
          and call = topCallStack stack
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
          and prov = Pthm deps
          and call = topCallStack stack
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
(*OpenTheoryTrace1
          val traceCall = null (callStack stack)
(*OpenTheoryTrace2
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
          and call = topCallStack stack
          val obj = mkObject (ob,prov,call)
          val stack = pushStack stack obj
          val stack = pushStack stack objA
        in
          State {stack = stack, dict = dict, saved = saved}
        end

      | Cop "return" =>
        let
          val (stack,
               objR as Object {object = obR, ...},
               Object {object = obN, ...}) = pop2Stack stack
          val _ = not (Object.isOcall obR) orelse
                  raise Error "cannot use an Ocall object as a return value"
          val n = Object.destOname obN
          val n = Interpretation.interpretRule interpretation n
          val (n',stack) = popCallStack stack
          val _ = Name.equal n' n orelse
                  raise Error ("call " ^ Name.toString n' ^
                               " matched by return " ^ Name.toString n)
(*OpenTheoryTrace1
          val traceReturn = null (callStack stack)
(*OpenTheoryTrace2
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
          and prov = if containsThmsObject objR then Preturn objR else Pnull
          and call = topCallStack stack
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
          val objD as Object {object = obD, ...} = refDict dict i
          val ob = obD
          and prov = if containsThmsObject objD then Pref objD else Pnull
          and call = topCallStack stack
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
          val (dict, objD as Object {object = obD, ...}) = removeDict dict i
          val ob = obD
          and prov = if containsThmsObject objD then Pref objD else Pnull
          and call = topCallStack stack
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
          val objD = peekStack stack i
          val ob = object objD
          and prov = if containsThmsObject objD then Pref objD else Pnull
          and call = topCallStack stack
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

fun executeCommands known interpretation =
    let
      fun process (command,state) =
          executeCommand known interpretation command state
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
       | Term.Var (n,ty) =>
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
        | Preturn _ => refs
        | Pcons _ =>
          let
            val (known,refs) = registerTop refs ob
(*OpenTheoryDebug
            val _ =
                not known orelse
                let
                  val () = Print.trace (ppObject 1) "deja vu obj" obj
                  val k = Option.getOpt (ObjectMap.peek refs ob, 0)
                  val () = Print.trace Print.ppInt "refs" (k - 1)
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
in
  fun generateMinDict prevCall dict obj =
      let
        val Object {object = ob, provenance = prov, call, ...} = obj
        val cmds = []
      in
        case prov of
          Pnull =>
          let
            val cmds = alignCalls prevCall call cmds
            val (dict,cmds) = generateDeep (ob,(dict,cmds))
          in
            (call,dict,cmds)
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
            (call,dict,cmds)
          end
        | Preturn (Object {call = rcall, ...}) =>
          let
            val cmds = alignCalls prevCall rcall cmds
            val cmds =
                case rcall of
                  SOME (Object {object = Object.Ocall n, ...}) =>
                  Cop "return" :: Cname n :: cmds
                | _ => raise Bug "Article.generateMinDict: Preturn"
          in
            (call,dict,cmds)
          end
        | Pcons _ =>
          let
            val cmds = alignCalls prevCall call cmds
            val cmds = Cop "cons" :: cmds
            val (dict,cmds) = addKey dict cmds ob
          in
            (call,dict,cmds)
          end
        | Pref _ =>
          let
            val cmds = alignCalls prevCall call cmds
            val (dict,cmds) = useKey dict cmds ob
          in
            (call,dict,cmds)
          end
        | Pthm _ =>
          let
            val cmds = alignCalls prevCall call cmds
            val (dict,cmds) = generateDeep (ob,(dict,cmds))
          in
            (call,dict,cmds)
          end
      end
      handle Error err =>
        raise Bug ("Article.generateMinDict: " ^ err);
end;

fun generate saved objs =
    let
      fun gen obj (call,dict) =
          let
            val (call,dict,cmds) = generateMinDict call dict obj

            val cmds =
                if not (memberObjectSet obj saved) then cmds
                else Cop "save" :: Cop "dup" :: Cnum 0 :: cmds

            val cmds = rev cmds
          in
            (cmds,(call,dict))
          end

      val call = NONE
      val dict = newMinDict objs

      val strm = toStreamObjectSet objs
      val strm = Stream.maps gen (K Stream.Nil) (call,dict) strm
    in
      Stream.concat (Stream.map Stream.fromList strm)
    end
    handle Error err =>
      raise Bug ("Article.generate: " ^ err);

(* ------------------------------------------------------------------------- *)
(* A type of proof articles.                                                 *)
(* ------------------------------------------------------------------------- *)

datatype article =
    Article of
      {thms : ThmSet.set,
       saved : saved};

val empty =
    Article
      {thms = ThmSet.empty,
       saved = emptySaved};

fun append art1 art2 =
    let
      val Article {thms = thms1, saved = saved1} = art1
      and Article {thms = thms2, saved = saved2} = art2

      val thms = ThmSet.union thms1 thms2

      val saved = appendSaved saved1 saved2
    in
      Article
        {thms = thms,
         saved = saved}
    end;

fun saved (Article {thms = x, ...}) = x;

fun prove (Article {saved = x, ...}) seq = searchSaved x seq;

val summarize = Summary.fromThms o saved;

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
  fun executeTextFile {known,interpretation,filename} =
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
           executeCommands known interpretation commands
         end
         handle Parse.NoParse => raise Error "parse error")
        handle Error err =>
          raise Error ("error in article file \"" ^ filename ^ "\" " ^
                       parseErrorLocation () ^ "\n" ^ err)
      end;
end;

fun fromTextFile {known,interpretation,filename} =
    let
      val State {stack,dict,saved} =
          executeTextFile
            {known = known,
             filename = filename,
             interpretation = interpretation}

      val savedSet = toSetSaved saved

      val () =
          let
            val n = sizeSaved saved - ObjectMap.size savedSet
          in
            if n = 0 then ()
            else
              warn (Int.toString n ^ " saved theorem" ^
                    (if n = 1 then " is a duplicate" else "s are duplicates") ^
                    " in " ^ filename)
          end

      val savedSet =
          let
            val n = sizeStack stack
          in
            if n = 0 then savedSet
            else
              let
                val () =
                    warn (Int.toString n ^ " object" ^
                          (if n = 1 then "" else "s") ^
                          " left on the stack by " ^ filename)

                fun add (obj,(seen,ss)) =
                    let
                      val Object {id, object = ob, provenance = prov, ...} = obj
                    in
                      if IntSet.member id seen then (seen,ss)
                      else
                        let
                          val seen = IntSet.add seen id
                        in
                          case prov of
                            Pnull => (seen,ss)
                          | Pcall _ => (seen,ss)
                          | Preturn objR => add (objR,(seen,ss))
                          | Pcons (objH,objT) => add (objT, add (objH,(seen,ss)))
                          | Pref objR => add (objR,(seen,ss))
                          | Pthm _ =>
                            (seen,
                             if ObjectMap.inDomain ob savedSet then ss
                             else
                               case ObjectMap.peek ss ob of
                                 NONE => ObjectMap.insert ss (ob,obj)
                               | SOME (Object {id = id', ...}) =>
                                 if not (id < id') then ss
                                 else ObjectMap.insert ss (ob,obj))
                        end
                    end

                val (_,savedSet') =
                    List.foldl add (IntSet.empty,savedSet) (objectStack stack)

                val n = ObjectMap.size savedSet' - ObjectMap.size savedSet

                val () =
                    if n = 0 then ()
                    else
                      warn (Int.toString n ^ " unsaved theorem" ^
                            (if n = 1 then "" else "s") ^
                            " left on the stack by " ^ filename)
              in
                savedSet'
              end
          end

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

      val saved = fromSetSaved savedSet

      val thms = thmsSaved saved
    in
      Article
        {thms = thms,
         saved = saved}
    end
    handle Error err => raise Error ("Article.fromTextFile: " ^ err);

fun toTextFile {filename} article =
    let
      val Article {saved,...} = article
      val saved = listSaved saved
      val (objs,saved) = dependenciesObject saved
      val () = Print.trace ppObjectSet "objs" objs
      val saved = fromListObjectSet saved
      val () = Print.trace ppObjectSet "saved" saved
      val commands = generate saved objs
      val lines = Stream.map (fn c => commandToString c ^ "\n") commands
    in
      Stream.toTextFile {filename = filename} lines
    end
    handle Error err => raise Error ("Article.toTextFile: " ^ err);

end
