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
(*OpenTheoryTrace1
           val () = if count mod 1000 = 0 then trace "." else ()
*)
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

fun parentsProvenance prov =
    case prov of
      Pnull => []
    | Pcall obj => [obj]
    | Preturn obj => [obj]
    | Pcons (objH,objT) => [objH,objT]
    | Pref obj => [obj]
    | Pthm objs => objs;

fun parentsObject (Object {provenance = prov, call, ...}) =
    let
      val pars = parentsProvenance prov
    in
      case call of
        SOME obj => obj :: pars
      | NONE => pars
    end;

local
  infix ==

  val op== = Portable.pointerEqual;

  fun maps lr preF postF =
      let
        fun mapsObj obj acc =
            case preF obj acc of
              Left obj => (obj,acc)
            | Right obj =>
              let
                val Object {id, object = ob, provenance = prov, call} = obj

                val (prov',call',acc) =
                    if lr then
                      let
                        val (call',acc) = Sharing.mapsOption mapsObj call acc
                        val (prov',acc) = mapsProv prov acc
                      in
                        (prov',call',acc)
                      end
                    else
                      let
                        val (prov',acc) = mapsProv prov acc
                        val (call',acc) = Sharing.mapsOption mapsObj call acc
                      in
                        (prov',call',acc)
                      end

                val obj =
                    if prov' == prov andalso call' == call then obj
                    else
                      Object
                        {id = id,
                         object = ob,
                         provenance = prov',
                         call = call'}
              in
                postF obj acc
              end

        and mapsProv prov acc =
            case prov of
              Pnull => (prov,acc)
            | Pcall obj => mapsProv1 prov acc Pcall obj
            | Preturn obj => mapsProv1 prov acc Preturn obj
            | Pcons (objH,objT) =>
              let
                val (objH',objT',acc) =
                    if lr then
                      let
                        val (objH',acc) = mapsObj objH acc
                        val (objT',acc) = mapsObj objT acc
                      in
                        (objH',objT',acc)
                      end
                    else
                      let
                        val (objT',acc) = mapsObj objT acc
                        val (objH',acc) = mapsObj objH acc
                      in
                        (objH',objT',acc)
                      end

                val prov' =
                    if objH' == objH andalso objT' == objT then prov
                    else Pcons (objH',objT')
              in
                (prov',acc)
              end
            | Pref obj => mapsProv1 prov acc Pref obj
            | Pthm objs =>
              let
                val (objs',acc) =
                    (if lr then Sharing.maps else Sharing.revMaps)
                      mapsObj objs acc

                val prov' = if objs' == objs then prov else Pthm objs'
              in
                (prov',acc)
              end

        and mapsProv1 prov acc con obj =
            let
              val (obj',acc) = mapsObj obj acc
              val prov' = if obj' == obj then prov else con obj'
            in
              (prov',acc)
            end
      in
        mapsObj
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

fun unionObjectSet (ObjectSet set1) (ObjectSet set2) =
    ObjectSet (Set.union set1 set2);

fun addListObjectSet set objs =
    List.foldl (fn (obj,acc) => addObjectSet acc obj) set objs;

fun foldlObjectSet f b (ObjectSet set) = Set.foldl f b set;

fun foldrObjectSet f b (ObjectSet set) = Set.foldr f b set;

fun fromListObjectSet objs = addListObjectSet emptyObjectSet objs;

val toListObjectSet = foldrObjectSet op:: [];

local
  fun add (obj,set) = ThmSet.add set (Object.destOthm (object obj));
in
  fun toThmSetObjectSet set = foldlObjectSet add ThmSet.empty set;
end;

local
  fun toStream NONE = Stream.Nil
    | toStream (SOME iter) =
      Stream.Cons (Set.readIterator iter, toStreamDelay iter)

  and toStreamDelay iter () = toStream (Set.advanceIterator iter)
in
  fun toStreamObjectSet (ObjectSet set) = toStream (Set.mkIterator set);
end;

local
  fun ancs set [] = set
    | ancs set (obj :: objs) =
      if memberObjectSet obj set then ancs set objs
      else ancs (addObjectSet set obj) (parentsObject obj @ objs);
in
  fun ancestorsObject obj = ancs emptyObjectSet [obj];
end;

val ppObjectSet =
    Print.ppBracket "{" "}" (Print.ppMap sizeObjectSet Print.ppInt);

(* ------------------------------------------------------------------------- *)
(* Reducing objects.                                                         *)
(* ------------------------------------------------------------------------- *)

local
  fun improve refs obj =
      let
        val Object {id, object = ob, provenance = prov, call} = obj

        fun better rid =
            rid < id andalso
            case prov of
              Preturn (Object {id = rid', ...}) => rid < rid'
            | Pref (Object {id = rid', ...}) => rid < rid'
            | _ => true

        val obj' =
            case prov of
              Pnull => NONE
            | Pcall _ => NONE
            | _ =>
              case ObjectMap.peek refs ob of
                NONE => NONE
              | SOME (robj as Object {id = rid, ...}) =>
                if not (better rid) then NONE
                else
                  let
                    val prov = Pref robj

                    val obj =
                        Object
                          {id = id,
                           object = ob,
                           provenance = prov,
                           call = call}
                  in
                    SOME obj
                  end

(*OpenTheoryTrace4
        val ppImprovement = Print.ppOp2 " -->" (ppObject 1) (ppObject 1)
        val () =
            case obj' of
              SOME x => Print.trace ppImprovement "Article.improve" (obj,x)
            | NONE => ()
*)
      in
        obj'
      end;

  fun preReduce obj (reqd,refs) =
      case peekObjectSet reqd obj of
        SOME obj => Left obj
      | NONE => Right (Option.getOpt (improve refs obj, obj));

  fun postReduce obj (reqd,refs) =
      let
        val Object {id, object = ob, ...} = obj

        val reqd = addObjectSet reqd obj

        val insertRefs =
            case ObjectMap.peek refs ob of
              NONE => true
            | SOME (Object {id = id', ...}) => id < id'

        val refs = if insertRefs then ObjectMap.insert refs (ob,obj) else refs
      in
        (obj,(reqd,refs))
      end;

  val checkReduced =
      let
        fun check (obj,obs) =
            let
              val Object {id, object = ob, provenance = prov, call} = obj
            in
              case prov of
                Pnull => obs
              | Pcall _ => obs
              | Preturn _ => obs
              | _ =>
                case ObjectMap.peek obs ob of
                  NONE => ObjectMap.insert obs (ob,obj)
                | SOME obj' =>
                  let
                    val Object {id = id', ...} = obj'
                  in
                    case prov of
                      Pref (Object {id = rid, ...}) =>
                      if rid = id' then obs
                      else raise Error "does not reference initial instance"
                    | _ => raise Error "is not a reference"
                  end
                  handle Error err =>
                    let
                      val () = Print.trace (ppObject 1) "initial" obj'
                      val () = Print.trace (ppObject 1) "duplicate" obj
                      val () =
                          if memberObjectSet obj' (ancestorsObject obj) then
                            trace "duplicate depends on initial\n"
                          else
                            trace "duplicate does not depend on initial\n"
                    in
                      raise Error ("duplicate " ^ err)
                    end
            end
      in
        fn reqd =>
           let
             val acc = ObjectMap.new ()
             val acc = foldlObjectSet check acc reqd
           in
             ()
           end
      end;
in
  fun reduceObject objs =
      let
        val reqd = emptyObjectSet
        val refs = ObjectMap.new ()

        val (objs,(_,refs)) =
            Sharing.maps (mapsObject preReduce postReduce) objs (reqd,refs)

        val (objs,(reqd,refs')) =
            Sharing.maps (mapsObject preReduce postReduce) objs (reqd,refs)

(*OpenTheoryDebug
        val _ = Portable.pointerEqual (refs,refs') orelse
                raise Error "references changed"
        val () = checkReduced reqd
*)
      in
        (reqd,objs)
      end
      handle Error err =>
        raise Bug ("Article.reduceObject: " ^ err);
end;

(* ------------------------------------------------------------------------- *)
(* Theorems in scope.                                                        *)
(* ------------------------------------------------------------------------- *)

datatype theorems = Theorems of object SequentMap.map * IntSet.set;

val emptyTheorems = Theorems (SequentMap.new (), IntSet.empty);

fun sizeTheorems (Theorems (seqs,_)) = SequentMap.size seqs;

fun addTheorems thms obj =
    let
      val Theorems (seqs,ids) = thms
      and Object {id, object = ob, provenance = prov, ...} = obj
    in
      if IntSet.member id ids then thms
      else
        let
          val ids = IntSet.add ids id
        in
          case prov of
            Pnull => Theorems (seqs,ids)
          | Pcall _ => Theorems (seqs,ids)
          | Preturn objR => addTheorems (Theorems (seqs,ids)) objR
          | Pcons (objH,objT) =>
            let
              val thms = Theorems (seqs,ids)
              val thms = addTheorems thms objH
            in
              addTheorems thms objT
            end
          | Pref objR => addTheorems (Theorems (seqs,ids)) objR
          | Pthm _ =>
            let
              val seq = Syntax.sequent (Object.destOthm ob)

              val ins =
                  case SequentMap.peek seqs seq of
                    NONE => true
                  | SOME (Object {id = id', ...}) => id < id'

              val seqs = if ins then SequentMap.insert seqs (seq,obj) else seqs
            in
              Theorems (seqs,ids)
            end
        end
    end;

local
  fun choose (obj1,obj2) =
      let
        val Object {id = id1, ...} = obj1
        and Object {id = id2, ...} = obj2

        val obj = if id1 < id2 then obj1 else obj2
      in
        SOME obj
      end;
in
  fun unionTheorems thms1 thms2 =
      let
        val Theorems (seqs1,ids1) = thms1
        and Theorems (seqs2,ids2) = thms2

        val seqs = SequentMap.union choose seqs1 seqs2

        val ids = IntSet.union ids1 ids2
      in
        Theorems (seqs,ids)
      end;
end;

local
  fun add (_,obj,set) = addObjectSet set obj;
in
  fun toObjectSetTheorems (Theorems (seqs,_)) =
      SequentMap.foldl add emptyObjectSet seqs;
end;

fun searchTheorems (Theorems (seqs,_)) seq =
    case SequentMap.peek seqs seq of
      NONE => NONE
    | SOME obj =>
      let
        val th = alpha seq (Object.destOthm (object obj))
      in
        SOME (th,[obj])
      end;

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
      [] => emptyTheorems
    | ths :: _ => ths;

fun pushStack stack obj =
    let
      val Stack {size,objects,theorems,call} = stack

      val size = size + 1

      val objects = obj :: objects

      val thms = addTheorems (theoremsStack stack) obj

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

(* ------------------------------------------------------------------------- *)
(* Saved theorems.                                                           *)
(* ------------------------------------------------------------------------- *)

datatype saved = Saved of theorems;

val emptySaved = Saved emptyTheorems;

fun theoremsSaved (Saved thms) = thms;

fun unionSaved (Saved thms1) (Saved thms2) =
    Saved (unionTheorems thms1 thms2);

fun addSaved saved obj =
    let
      val Saved thms = saved
      and Object {object = ob, ...} = obj
    in
      case ob of
        Object.Othm th =>
        let
          val seq = sequent th
        in
          case searchTheorems thms seq of
            SOME _ =>
            let
              val () = warn ("saving duplicate theorem:\n" ^ thmToString th)
            in
              saved
            end
          | NONE =>
            let
              val thms = addTheorems thms obj
            in
              Saved thms
            end
        end
      | _ => raise Error "Article.addSaved: not an Othm object"
    end;

fun searchSaved (Saved thms) seq =
    case searchTheorems thms seq of
      SOME (th,_) => SOME th
    | NONE => NONE;

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
              else if savable then Preturn objR
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
        | Preturn _ => refs
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
        | Preturn (Object {call = rcall, ...}) =>
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

fun toTextFile {filename} article =
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
