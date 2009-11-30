(* ========================================================================= *)
(* READING OBJECTS FROM COMMANDS                                             *)
(* Copyright (c) 2004 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

structure ObjectRead :> ObjectRead =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* Building objects using data in another object.                            *)
(* ------------------------------------------------------------------------- *)

fun buildObject savable obj =
    let
      val thms = ObjectThms.singleton obj

      fun search th =
          case ObjectThms.search thms (Thm.sequent th) of
            SOME (_,objS) => ObjectProv.Ialpha objS
          | NONE => raise Error ("couldn't find theorem:\n" ^ Thm.toString th)
    in
      ObjectProv.build savable search
    end;

(* ------------------------------------------------------------------------- *)
(* A type of parameters for reading objects from commands.                   *)
(* ------------------------------------------------------------------------- *)

type parameters =
     {simulations : Simulation.simulations,
      known : ObjectThms.thms,
      interpretation : Interpretation.interpretation,
      savable : bool};

(* ------------------------------------------------------------------------- *)
(* A type of states for reading objects from commands.                       *)
(* ------------------------------------------------------------------------- *)

datatype state =
    State of
      {parameters : parameters,
       stack : ObjectStack.stack,
       dict : ObjectDict.dict,
       saved : ObjectThms.thms};

fun initial parameters =
    let
      val stack = ObjectStack.empty

      val dict = ObjectDict.empty

      val saved = ObjectThms.empty
    in
      State
        {parameters = parameters,
         stack = stack,
         dict = dict,
         saved = saved}
    end;

fun parameters (State {parameters = x, ...}) = x;

fun stack (State {stack = x, ...}) = x;

fun dict (State {dict = x, ...}) = x;

fun saved (State {saved = x, ...}) = x;

(* ------------------------------------------------------------------------- *)
(* Executing commands.                                                       *)
(* ------------------------------------------------------------------------- *)

fun execute cmd state =
    let
      val State {parameters,stack,dict,saved} = state
      val {simulations,known,interpretation,savable} = parameters
    in
      case cmd of
      (* Numbers *)

        Command.Num i =>
        let
          val obj = ObjectProv.mkNum i

          val stack = ObjectStack.push stack obj
        in
          State
            {parameters = parameters,
             stack = stack,
             dict = dict,
             saved = saved}
        end

      (* Names *)

      | Command.Name n =>
        let
          val obj = ObjectProv.mkName n

          val stack = ObjectStack.push stack obj
        in
          State
            {parameters = parameters,
             stack = stack,
             dict = dict,
             saved = saved}
        end

      (* Errors *)

      | Command.Error =>
        let
          val obj = ObjectProv.mkError ()

          val stack = ObjectStack.push stack obj
        in
          State
            {parameters = parameters,
             stack = stack,
             dict = dict,
             saved = saved}
        end

      (* Lists *)

      | Command.Nil =>
        let
          val obj = ObjectProv.mkNil ()

          val stack = ObjectStack.push stack obj
        in
          State
            {parameters = parameters,
             stack = stack,
             dict = dict,
             saved = saved}
        end

      | Command.Cons =>
        let
          val (stack,objH,objT) = ObjectStack.pop2 stack

          val obj = ObjectProv.mkCons objH objT

          val stack = ObjectStack.push stack obj
        in
          State
            {parameters = parameters,
             stack = stack,
             dict = dict,
             saved = saved}
        end

      (* Types *)

      | Command.TypeVar =>
        let
          val (stack,objN) = ObjectStack.pop1 stack

          val obj = ObjectProv.mkTypeVar objN

          val stack = ObjectStack.push stack obj
        in
          State
            {parameters = parameters,
             stack = stack,
             dict = dict,
             saved = saved}
        end

      | Command.TypeOp =>
        let
          val (stack,objN,objL) = ObjectStack.pop2 stack

          val n = Object.destOname (ObjectProv.object objN)
          val n = Interpretation.interpretTypeOp interpretation n

          val symbols =
              [ObjectThms.symbol known,
               ObjectThms.symbol saved,
               ObjectStack.symbol stack,
               ObjectStack.symbolSimulation stack]

          val ot = Symbol.mkTypeOp symbols n

          val obj = ObjectProv.mkTypeOp ot objL

          val stack = ObjectStack.push stack obj
        in
          State
            {parameters = parameters,
             stack = stack,
             dict = dict,
             saved = saved}
        end

      (* Terms *)

      | Command.Var =>
        let
          val (stack,objN,objT) = ObjectStack.pop2 stack

          val obj = ObjectProv.mkVar objN objT

          val stack = ObjectStack.push stack obj
        in
          State
            {parameters = parameters,
             stack = stack,
             dict = dict,
             saved = saved}
        end

      | Command.Const =>
        let
          val (stack,objN,objT) = ObjectStack.pop2 stack

          val n = Object.destOname (ObjectProv.object objN)
          val n = Interpretation.interpretConst interpretation n

          val symbols =
              [ObjectThms.symbol known,
               ObjectThms.symbol saved,
               ObjectStack.symbol stack,
               ObjectStack.symbolSimulation stack]

          val c = Symbol.mkConst symbols n

          val obj = ObjectProv.mkConst c objT

          val stack = ObjectStack.push stack obj
        in
          State
            {parameters = parameters,
             stack = stack,
             dict = dict,
             saved = saved}
        end

      | Command.App =>
        let
          val (stack,objF,objA) = ObjectStack.pop2 stack

          val obj = ObjectProv.mkApp objF objA

          val stack = ObjectStack.push stack obj
        in
          State
            {parameters = parameters,
             stack = stack,
             dict = dict,
             saved = saved}
        end

      | Command.Abs =>
        let
          val (stack,objV,objB) = ObjectStack.pop2 stack

          val obj = ObjectProv.mkAbs objV objB

          val stack = ObjectStack.push stack obj
        in
          State
            {parameters = parameters,
             stack = stack,
             dict = dict,
             saved = saved}
        end

      (* Theorems *)

      | Command.Thm =>
        let
          val (stack,objH,objC) = ObjectStack.pop2 stack

          val seq =
              let
                val obH = ObjectProv.object objH
                and obC = ObjectProv.object objC
              in
                Sequent.Sequent
                  {hyp = TermAlphaSet.fromList (Object.destOterms obH),
                   concl = Object.destOterm obC}
              end

          val (th,inf) =
              case ObjectThms.search saved seq of
                SOME (th,objS) => (th, ObjectProv.Ialpha objS)
              | NONE =>
                case ObjectStack.searchSimulation stack seq of
                  SOME (th,objS) => (th, ObjectProv.Isimulated objS)
                | NONE =>
                  case ObjectStack.search stack seq of
                    SOME (th,objS) => (th, ObjectProv.Ialpha objS)
                  | NONE =>
                    case ObjectThms.search known seq of
                      SOME (th,objS) => (th, ObjectProv.Ialpha objS)
                    | NONE =>
                      let
                        val th = Thm.axiom seq
(*OpenTheoryTrace1
                        val () = trace ("making new axiom in " ^
                                        ObjectStack.topCallToString stack ^
                                        ":\n" ^ Thm.toString th ^ "\n")
*)
                      in
                        (th,ObjectProv.Iaxiom)
                      end

          val obj = ObjectProv.mkThm {savable = savable} objH objC th inf

          val stack = ObjectStack.push stack obj
        in
          State
            {parameters = parameters,
             stack = stack,
             dict = dict,
             saved = saved}
        end

      (* Function calls *)

      | Command.Call =>
        let
          val (stack,objA,objN) = ObjectStack.pop2 stack

          val obA = ObjectProv.object objA
          and obN = ObjectProv.object objN

          val _ = not (Object.isOcall obA) orelse
                  raise Error "cannot use an Ocall object as a call argument"

          val n = Object.destOname obN

(*OpenTheoryTrace2
          val traceCall = null (ObjectStack.callStack stack)
(*OpenTheoryTrace3
          val traceCall = true
*)
          val () =
              if not traceCall then ()
              else
                trace
                  ("call: " ^ Name.toString n ^ "\n" ^ "  stack = [" ^
                   Int.toString (ObjectStack.size stack) ^ "], call stack = [" ^
                   Int.toString (length (ObjectStack.callStack stack)) ^ "]\n")

          val () = if not traceCall then ()
                   else Print.trace Object.pp "  input" obA
*)

          val (obA,objA,sims) =
              case Simulation.peek simulations n of
                NONE => (obA,objA,ThmSet.empty)
              | SOME (Simulation.Simulation sim) =>
                let
                  val ctxt =
                      Simulation.Context
                        {interpretation = interpretation,
                         input = obA}

                  val result =
                      sim ctxt
                      handle Error err =>
                        let
                          val ppOb = Print.ppOp2 " =" Print.ppString Object.pp
                        in
                          raise Error
                            ("simulation failed: " ^ Name.toString n ^
                             "\n" ^ Print.toString ppOb ("input",obA) ^
                             "\n" ^ err)
                        end

                  val Simulation.Result {input,thms} = result

                  val (obA,objA) =
                      case input of
                        NONE => (obA,objA)
                      | SOME obA =>
                        let
                          val objA = buildObject {savable = savable} objA obA
                        in
                          (obA,objA)
                        end
                in
                  (obA,objA,thms)
                end

          val obj = ObjectProv.mkCall n objA

          val stack = ObjectStack.push stack obj
          val stack = ObjectStack.push stack objA
          val stack = ObjectStack.addSimulation stack (sims,obj)
        in
          State
            {parameters = parameters,
             stack = stack,
             dict = dict,
             saved = saved}
        end

      | Command.Return =>
        let
          val (stack,objR,objN) = ObjectStack.pop2 stack

          val obR = ObjectProv.object objR
          and obN = ObjectProv.object objN

          val _ = not (Object.isOcall obR) orelse
                  raise Error "cannot use an Ocall object as a return value"

          val n = Object.destOname obN

          val (stack,n') = ObjectStack.popCall stack

          val _ = Name.equal n' n orelse
                  raise Error ("call " ^ Name.toString n' ^
                               " matched by return " ^ Name.toString n)
(*OpenTheoryTrace2
          val traceReturn = null (ObjectStack.callStack stack)
(*OpenTheoryTrace3
          val traceReturn = true
*)
          val () =
              if not traceReturn then ()
              else
                trace
                  ("return: " ^ Name.toString n ^ "\n" ^
                   "  stack = [" ^ Int.toString (ObjectStack.size stack) ^
                   "], call stack = [" ^
                   Int.toString (length (ObjectStack.callStack stack)) ^ "]\n")

          val () = if not traceReturn then ()
                   else Print.trace Object.pp "  return" obR
*)

          val obj = ObjectProv.mkReturn {savable = savable} objR

          val stack = ObjectStack.push stack obj
        in
          State
            {parameters = parameters,
             stack = stack,
             dict = dict,
             saved = saved}
        end

      (* Dictionary *)

      | Command.Def =>
        let
          val (stack,objI) = ObjectStack.pop1 stack
          val obI = ObjectProv.object objI

          val objD = ObjectStack.peek stack 0
          val obD = ObjectProv.object objD

          val _ = not (Object.isOcall obD) orelse
                  raise Error "cannot def an Ocall object"

          val i = Object.destOint obI

          val dict = ObjectDict.define dict (i,objD)
        in
          State
            {parameters = parameters,
             stack = stack,
             dict = dict,
             saved = saved}
        end

      | Command.Ref =>
        let
          val (stack,objI) = ObjectStack.pop1 stack

          val i = Object.destOint (ObjectProv.object objI)

          val objD = ObjectDict.refer dict i

          val obj = ObjectProv.mkRef {savable = savable} objD

          val stack = ObjectStack.push stack obj
        in
          State
            {parameters = parameters,
             stack = stack,
             dict = dict,
             saved = saved}
        end

      | Command.Remove =>
        let
          val (stack,objI) = ObjectStack.pop1 stack

          val i = Object.destOint (ObjectProv.object objI)

          val (dict,objD) = ObjectDict.remove dict i

          val obj = ObjectProv.mkRemove {savable = savable} objD

          val stack = ObjectStack.push stack obj
        in
          State
            {parameters = parameters,
             stack = stack,
             dict = dict,
             saved = saved}
        end

      (* General *)

      | Command.Pop =>
        let
          val stack = ObjectStack.pop stack 1
        in
          State
            {parameters = parameters,
             stack = stack,
             dict = dict,
             saved = saved}
        end

      | Command.Save =>
        let
          val objT = ObjectStack.peek stack 0

          val _ = Object.isOthm (ObjectProv.object objT) orelse
                  raise Error "can only save Othm objects"

          val saved = ObjectThms.add saved objT
        in
          State
            {parameters = parameters,
             stack = stack,
             dict = dict,
             saved = saved}
        end
    end
    handle Error err =>
      let
(*OpenTheoryDebug
        val State {stack,...} = state

        val ppStack =
            Print.ppMap
              (map ObjectProv.object o ObjectStack.objects)
              (Print.ppList Object.pp)

        val () = Print.trace ppStack "ObjectRead.execute: stack" stack
*)
        val err = "ObjectRead.execute " ^ Command.toString cmd ^ ": " ^ err
      in
        raise Error err
      end;

local
  fun process (cmd,state) = execute cmd state;
in
  fun executeStream strm state = Stream.foldl process state strm;
end;

(* ------------------------------------------------------------------------- *)
(* Executing text files.                                                     *)
(* ------------------------------------------------------------------------- *)

local
  (* Comment lines *)

  fun isComment l =
      case List.find (not o Char.isSpace) l of
        NONE => true
      | SOME #"#" => true
      | _ => false;
in
  fun executeTextFile {filename} state =
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

           val commands = Parse.everything Command.spacedParser chars
         in
           executeStream commands state
         end
         handle Parse.NoParse => raise Error "parse error")
        handle Error err =>
          raise Error ("error in file \"" ^ filename ^ "\" " ^
                       parseErrorLocation () ^ "\n" ^ err)
      end;
end;

end
