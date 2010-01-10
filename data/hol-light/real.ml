(* ========================================================================= *)
(* More basic properties of the reals.                                       *)
(*                                                                           *)
(*       John Harrison, University of Cambridge Computer Laboratory          *)
(*                                                                           *)
(*            (c) Copyright, University of Cambridge 1998                    *)
(*              (c) Copyright, John Harrison 1998-2007                       *)
(* ========================================================================= *)

(* ------------------------------------------------------------------------- *)
(* OpenTheory logging.                                                       *)
(* ------------------------------------------------------------------------- *)

logfile "real-thm";;

(* ------------------------------------------------------------------------- *)
(* Additional commutativity properties of the inclusion map.                 *)
(* ------------------------------------------------------------------------- *)

let REAL_OF_NUM_LT = log_lemma "REAL_OF_NUM_LT" (fun () -> prove
 (`!m n. &m < &n <=> m < n`,
  REWRITE_TAC[real_lt; GSYM NOT_LE; REAL_OF_NUM_LE]));;

let REAL_OF_NUM_GE = log_lemma "REAL_OF_NUM_GE" (fun () -> prove
 (`!m n. &m >= &n <=> m >= n`,
  REWRITE_TAC[GE; real_ge; REAL_OF_NUM_LE]));;

let REAL_OF_NUM_GT = log_lemma "REAL_OF_NUM_GT" (fun () -> prove
 (`!m n. &m > &n <=> m > n`,
  REWRITE_TAC[GT; real_gt; REAL_OF_NUM_LT]));;

let REAL_OF_NUM_MAX = log_lemma "REAL_OF_NUM_MAX" (fun () -> prove
 (`!m n. max (&m) (&n) = &(MAX m n)`,
  REWRITE_TAC[REAL_OF_NUM_LE; MAX; real_max; GSYM COND_RAND]));;

let REAL_OF_NUM_MIN = log_lemma "REAL_OF_NUM_MIN" (fun () -> prove
 (`!m n. min (&m) (&n) = &(MIN m n)`,
  REWRITE_TAC[REAL_OF_NUM_LE; MIN; real_min; GSYM COND_RAND]));;

let REAL_OF_NUM_SUC = log_lemma "REAL_OF_NUM_SUC" (fun () -> prove
 (`!n. &n + &1 = &(SUC n)`,
  REWRITE_TAC[ADD1; REAL_OF_NUM_ADD]));;

let REAL_OF_NUM_SUB = log_lemma "REAL_OF_NUM_SUB" (fun () -> prove
 (`!m n. m <= n ==> (&n - &m = &(n - m))`,
  REPEAT GEN_TAC THEN REWRITE_TAC[LE_EXISTS] THEN
  STRIP_TAC THEN ASM_REWRITE_TAC[ADD_SUB2] THEN
  REWRITE_TAC[GSYM REAL_OF_NUM_ADD] THEN
  ONCE_REWRITE_TAC[REAL_ADD_SYM] THEN
  REWRITE_TAC[real_sub; GSYM REAL_ADD_ASSOC] THEN
  MESON_TAC[REAL_ADD_LINV; REAL_ADD_SYM; REAL_ADD_LID]));;

(* ------------------------------------------------------------------------- *)
(* A few theorems we need to prove explicitly for later.                     *)
(* ------------------------------------------------------------------------- *)

let REAL_MUL_AC = log_lemma "REAL_MUL_AC" (fun () -> prove
 (`(m * n = n * m) /\
   ((m * n) * p = m * (n * p)) /\
   (m * (n * p) = n * (m * p))`,
  REWRITE_TAC[REAL_MUL_ASSOC; EQT_INTRO(SPEC_ALL REAL_MUL_SYM)] THEN
  AP_THM_TAC THEN AP_TERM_TAC THEN MATCH_ACCEPT_TAC REAL_MUL_SYM));;

let REAL_ADD_RDISTRIB = log_lemma "REAL_ADD_RDISTRIB" (fun () -> prove
 (`!x y z. (x + y) * z = x * z + y * z`,
  MESON_TAC[REAL_MUL_SYM; REAL_ADD_LDISTRIB]));;

let REAL_LT_LADD_IMP = log_lemma "REAL_LT_LADD_IMP" (fun () -> prove
 (`!x y z. y < z ==> x + y < x + z`,
  REPEAT GEN_TAC THEN CONV_TAC CONTRAPOS_CONV THEN
  REWRITE_TAC[real_lt] THEN
  DISCH_THEN(MP_TAC o MATCH_MP REAL_LE_LADD_IMP) THEN
  DISCH_THEN(MP_TAC o SPEC `--x`) THEN
  REWRITE_TAC[REAL_ADD_ASSOC; REAL_ADD_LINV; REAL_ADD_LID]));;

let REAL_LT_MUL = log_lemma "REAL_LT_MUL" (fun () -> prove
 (`!x y. &0 < x /\ &0 < y ==> &0 < x * y`,
  REPEAT GEN_TAC THEN REWRITE_TAC[REAL_LT_LE] THEN
  CONV_TAC(ONCE_DEPTH_CONV SYM_CONV) THEN
  STRIP_TAC THEN ASM_REWRITE_TAC[REAL_ENTIRE] THEN
  MATCH_MP_TAC REAL_LE_MUL THEN ASM_REWRITE_TAC[]));;

(* ------------------------------------------------------------------------- *)
(* Tactic version of REAL_ARITH.                                             *)
(* ------------------------------------------------------------------------- *)

let REAL_ARITH_TAC = CONV_TAC REAL_ARITH;;

(* ------------------------------------------------------------------------- *)
(* Prove all the linear theorems we can blow away automatically.             *)
(* ------------------------------------------------------------------------- *)

let REAL_EQ_ADD_LCANCEL_0 = log_lemma "REAL_EQ_ADD_LCANCEL_0" (fun () -> prove
 (`!x y. (x + y = x) <=> (y = &0)`,
  REAL_ARITH_TAC));;

let REAL_EQ_ADD_RCANCEL_0 = log_lemma "REAL_EQ_ADD_RCANCEL_0" (fun () -> prove
 (`!x y. (x + y = y) <=> (x = &0)`,
  REAL_ARITH_TAC));;

let REAL_LNEG_UNIQ = log_lemma "REAL_LNEG_UNIQ" (fun () -> prove
 (`!x y. (x + y = &0) <=> (x = --y)`,
  REAL_ARITH_TAC));;

let REAL_RNEG_UNIQ = log_lemma "REAL_RNEG_UNIQ" (fun () -> prove
 (`!x y. (x + y = &0) <=> (y = --x)`,
  REAL_ARITH_TAC));;

let REAL_NEG_LMUL = log_lemma "REAL_NEG_LMUL" (fun () -> prove
 (`!x y. --(x * y) = (--x) * y`,
  REAL_ARITH_TAC));;

let REAL_NEG_RMUL = log_lemma "REAL_NEG_RMUL" (fun () -> prove
 (`!x y. --(x * y) = x * (--y)`,
  REAL_ARITH_TAC));;

let REAL_NEGNEG = log_lemma "REAL_NEGNEG" (fun () -> prove
 (`!x. --(--x) = x`,
  REAL_ARITH_TAC));;

let REAL_NEG_MUL2 = log_lemma "REAL_NEG_MUL2" (fun () -> prove
 (`!x y. (--x) * (--y) = x * y`,
  REAL_ARITH_TAC));;

let REAL_LT_LADD = log_lemma "REAL_LT_LADD" (fun () -> prove
 (`!x y z. (x + y) < (x + z) <=> y < z`,
  REAL_ARITH_TAC));;

let REAL_LT_RADD = log_lemma "REAL_LT_RADD" (fun () -> prove
 (`!x y z. (x + z) < (y + z) <=> x < y`,
  REAL_ARITH_TAC));;

let REAL_LT_ANTISYM = log_lemma "REAL_LT_ANTISYM" (fun () -> prove
 (`!x y. ~(x < y /\ y < x)`,
  REAL_ARITH_TAC));;

let REAL_LT_GT = log_lemma "REAL_LT_GT" (fun () -> prove
 (`!x y. x < y ==> ~(y < x)`,
  REAL_ARITH_TAC));;

let REAL_NOT_EQ = log_lemma "REAL_NOT_EQ" (fun () -> prove
 (`!x y. ~(x = y) <=> x < y \/ y < x`,
  REAL_ARITH_TAC));;

let REAL_NOT_LE = log_lemma "REAL_NOT_LE" (fun () -> prove
 (`!x y. ~(x <= y) <=> y < x`,
  REAL_ARITH_TAC));;

let REAL_LET_ANTISYM = log_lemma "REAL_LET_ANTISYM" (fun () -> prove
 (`!x y. ~(x <= y /\ y < x)`,
  REAL_ARITH_TAC));;

let REAL_NEG_LT0 = log_lemma "REAL_NEG_LT0" (fun () -> prove
 (`!x. (--x) < &0 <=> &0 < x`,
  REAL_ARITH_TAC));;

let REAL_NEG_GT0 = log_lemma "REAL_NEG_GT0" (fun () -> prove
 (`!x. &0 < (--x) <=> x < &0`,
  REAL_ARITH_TAC));;

let REAL_NEG_LE0 = log_lemma "REAL_NEG_LE0" (fun () -> prove
 (`!x. (--x) <= &0 <=> &0 <= x`,
  REAL_ARITH_TAC));;

let REAL_NEG_GE0 = log_lemma "REAL_NEG_GE0" (fun () -> prove
 (`!x. &0 <= (--x) <=> x <= &0`,
  REAL_ARITH_TAC));;

let REAL_LT_TOTAL = log_lemma "REAL_LT_TOTAL" (fun () -> prove
 (`!x y. (x = y) \/ x < y \/ y < x`,
  REAL_ARITH_TAC));;

let REAL_LT_NEGTOTAL = log_lemma "REAL_LT_NEGTOTAL" (fun () -> prove
 (`!x. (x = &0) \/ (&0 < x) \/ (&0 < --x)`,
  REAL_ARITH_TAC));;

let REAL_LE_01 = log_lemma "REAL_LE_01" (fun () -> prove
 (`&0 <= &1`,
  REAL_ARITH_TAC));;

let REAL_LT_01 = log_lemma "REAL_LT_01" (fun () -> prove
 (`&0 < &1`,
  REAL_ARITH_TAC));;

let REAL_LE_LADD = log_lemma "REAL_LE_LADD" (fun () -> prove
 (`!x y z. (x + y) <= (x + z) <=> y <= z`,
  REAL_ARITH_TAC));;

let REAL_LE_RADD = log_lemma "REAL_LE_RADD" (fun () -> prove
 (`!x y z. (x + z) <= (y + z) <=> x <= y`,
  REAL_ARITH_TAC));;

let REAL_LT_ADD2 = log_lemma "REAL_LT_ADD2" (fun () -> prove
 (`!w x y z. w < x /\ y < z ==> (w + y) < (x + z)`,
  REAL_ARITH_TAC));;

let REAL_LE_ADD2 = log_lemma "REAL_LE_ADD2" (fun () -> prove
 (`!w x y z. w <= x /\ y <= z ==> (w + y) <= (x + z)`,
  REAL_ARITH_TAC));;

let REAL_LT_LNEG = log_lemma "REAL_LT_LNEG" (fun () -> prove
 (`!x y. --x < y <=> &0 < x + y`,
  REWRITE_TAC[real_lt; REAL_LE_RNEG; REAL_ADD_AC]));;

let REAL_LT_RNEG = log_lemma "REAL_LT_RNEG" (fun () -> prove
 (`!x y. x < --y <=> x + y < &0`,
  REWRITE_TAC[real_lt; REAL_LE_LNEG; REAL_ADD_AC]));;

let REAL_LT_ADDNEG = log_lemma "REAL_LT_ADDNEG" (fun () -> prove
 (`!x y z. y < (x + (--z)) <=> (y + z) < x`,
  REAL_ARITH_TAC));;

let REAL_LT_ADDNEG2 = log_lemma "REAL_LT_ADDNEG2" (fun () -> prove
 (`!x y z. (x + (--y)) < z <=> x < (z + y)`,
  REAL_ARITH_TAC));;

let REAL_LT_ADD1 = log_lemma "REAL_LT_ADD1" (fun () -> prove
 (`!x y. x <= y ==> x < (y + &1)`,
  REAL_ARITH_TAC));;

let REAL_SUB_ADD = log_lemma "REAL_SUB_ADD" (fun () -> prove
 (`!x y. (x - y) + y = x`,
  REAL_ARITH_TAC));;

let REAL_SUB_ADD2 = log_lemma "REAL_SUB_ADD2" (fun () -> prove
 (`!x y. y + (x - y) = x`,
  REAL_ARITH_TAC));;

let REAL_SUB_REFL = log_lemma "REAL_SUB_REFL" (fun () -> prove
 (`!x. x - x = &0`,
  REAL_ARITH_TAC));;

let REAL_LE_DOUBLE = log_lemma "REAL_LE_DOUBLE" (fun () -> prove
 (`!x. &0 <= x + x <=> &0 <= x`,
  REAL_ARITH_TAC));;

let REAL_LE_NEGL = log_lemma "REAL_LE_NEGL" (fun () -> prove
 (`!x. (--x <= x) <=> (&0 <= x)`,
  REAL_ARITH_TAC));;

let REAL_LE_NEGR = log_lemma "REAL_LE_NEGR" (fun () -> prove
 (`!x. (x <= --x) <=> (x <= &0)`,
  REAL_ARITH_TAC));;

let REAL_NEG_EQ_0 = log_lemma "REAL_NEG_EQ_0" (fun () -> prove
 (`!x. (--x = &0) <=> (x = &0)`,
  REAL_ARITH_TAC));;

let REAL_ADD_SUB = log_lemma "REAL_ADD_SUB" (fun () -> prove
 (`!x y. (x + y) - x = y`,
  REAL_ARITH_TAC));;

let REAL_NEG_EQ = log_lemma "REAL_NEG_EQ" (fun () -> prove
 (`!x y. (--x = y) <=> (x = --y)`,
  REAL_ARITH_TAC));;

let REAL_NEG_MINUS1 = log_lemma "REAL_NEG_MINUS1" (fun () -> prove
 (`!x. --x = (--(&1)) * x`,
  REAL_ARITH_TAC));;

let REAL_LT_IMP_NE = log_lemma "REAL_LT_IMP_NE" (fun () -> prove
 (`!x y. x < y ==> ~(x = y)`,
  REAL_ARITH_TAC));;

let REAL_LE_ADDR = log_lemma "REAL_LE_ADDR" (fun () -> prove
 (`!x y. x <= x + y <=> &0 <= y`,
  REAL_ARITH_TAC));;

let REAL_LE_ADDL = log_lemma "REAL_LE_ADDL" (fun () -> prove
 (`!x y. y <= x + y <=> &0 <= x`,
  REAL_ARITH_TAC));;

let REAL_LT_ADDR = log_lemma "REAL_LT_ADDR" (fun () -> prove
 (`!x y. x < x + y <=> &0 < y`,
  REAL_ARITH_TAC));;

let REAL_LT_ADDL = log_lemma "REAL_LT_ADDL" (fun () -> prove
 (`!x y. y < x + y <=> &0 < x`,
  REAL_ARITH_TAC));;

let REAL_SUB_SUB = log_lemma "REAL_SUB_SUB" (fun () -> prove
 (`!x y. (x - y) - x = --y`,
  REAL_ARITH_TAC));;

let REAL_LT_ADD_SUB = log_lemma "REAL_LT_ADD_SUB" (fun () -> prove
 (`!x y z. (x + y) < z <=> x < (z - y)`,
  REAL_ARITH_TAC));;

let REAL_LT_SUB_RADD = log_lemma "REAL_LT_SUB_RADD" (fun () -> prove
 (`!x y z. (x - y) < z <=> x < z + y`,
  REAL_ARITH_TAC));;

let REAL_LT_SUB_LADD = log_lemma "REAL_LT_SUB_LADD" (fun () -> prove
 (`!x y z. x < (y - z) <=> (x + z) < y`,
  REAL_ARITH_TAC));;

let REAL_LE_SUB_LADD = log_lemma "REAL_LE_SUB_LADD" (fun () -> prove
 (`!x y z. x <= (y - z) <=> (x + z) <= y`,
  REAL_ARITH_TAC));;

let REAL_LE_SUB_RADD = log_lemma "REAL_LE_SUB_RADD" (fun () -> prove
 (`!x y z. (x - y) <= z <=> x <= z + y`,
  REAL_ARITH_TAC));;

let REAL_LT_NEG = log_lemma "REAL_LT_NEG" (fun () -> prove
 (`!x y. --x < --y <=> y < x`,
  REAL_ARITH_TAC));;

let REAL_LE_NEG = log_lemma "REAL_LE_NEG" (fun () -> prove
 (`!x y. --x <= --y <=> y <= x`,
  REAL_ARITH_TAC));;

let REAL_ADD2_SUB2 = log_lemma "REAL_ADD2_SUB2" (fun () -> prove
 (`!a b c d. (a + b) - (c + d) = (a - c) + (b - d)`,
  REAL_ARITH_TAC));;

let REAL_SUB_LZERO = log_lemma "REAL_SUB_LZERO" (fun () -> prove
 (`!x. &0 - x = --x`,
  REAL_ARITH_TAC));;

let REAL_SUB_RZERO = log_lemma "REAL_SUB_RZERO" (fun () -> prove
 (`!x. x - &0 = x`,
  REAL_ARITH_TAC));;

let REAL_LET_ADD2 = log_lemma "REAL_LET_ADD2" (fun () -> prove
 (`!w x y z. w <= x /\ y < z ==> (w + y) < (x + z)`,
  REAL_ARITH_TAC));;

let REAL_LTE_ADD2 = log_lemma "REAL_LTE_ADD2" (fun () -> prove
 (`!w x y z. w < x /\ y <= z ==> w + y < x + z`,
  REAL_ARITH_TAC));;

let REAL_SUB_LNEG = log_lemma "REAL_SUB_LNEG" (fun () -> prove
 (`!x y. (--x) - y = --(x + y)`,
  REAL_ARITH_TAC));;

let REAL_SUB_RNEG = log_lemma "REAL_SUB_RNEG" (fun () -> prove
 (`!x y. x - (--y) = x + y`,
  REAL_ARITH_TAC));;

let REAL_SUB_NEG2 = log_lemma "REAL_SUB_NEG2" (fun () -> prove
 (`!x y. (--x) - (--y) = y - x`,
  REAL_ARITH_TAC));;

let REAL_SUB_TRIANGLE = log_lemma "REAL_SUB_TRIANGLE" (fun () -> prove
 (`!a b c. (a - b) + (b - c) = a - c`,
  REAL_ARITH_TAC));;

let REAL_EQ_SUB_LADD = log_lemma "REAL_EQ_SUB_LADD" (fun () -> prove
 (`!x y z. (x = y - z) <=> (x + z = y)`,
  REAL_ARITH_TAC));;

let REAL_EQ_SUB_RADD = log_lemma "REAL_EQ_SUB_RADD" (fun () -> prove
 (`!x y z. (x - y = z) <=> (x = z + y)`,
  REAL_ARITH_TAC));;

let REAL_SUB_SUB2 = log_lemma "REAL_SUB_SUB2" (fun () -> prove
 (`!x y. x - (x - y) = y`,
  REAL_ARITH_TAC));;

let REAL_ADD_SUB2 = log_lemma "REAL_ADD_SUB2" (fun () -> prove
 (`!x y. x - (x + y) = --y`,
  REAL_ARITH_TAC));;

let REAL_EQ_IMP_LE = log_lemma "REAL_EQ_IMP_LE" (fun () -> prove
 (`!x y. (x = y) ==> x <= y`,
  REAL_ARITH_TAC));;

let REAL_POS_NZ = log_lemma "REAL_POS_NZ" (fun () -> prove
 (`!x. &0 < x ==> ~(x = &0)`,
  REAL_ARITH_TAC));;

let REAL_DIFFSQ = log_lemma "REAL_DIFFSQ" (fun () -> prove
 (`!x y. (x + y) * (x - y) = (x * x) - (y * y)`,
  REAL_ARITH_TAC));;

let REAL_EQ_NEG2 = log_lemma "REAL_EQ_NEG2" (fun () -> prove
 (`!x y. (--x = --y) <=> (x = y)`,
  REAL_ARITH_TAC));;

let REAL_LT_NEG2 = log_lemma "REAL_LT_NEG2" (fun () -> prove
 (`!x y. --x < --y <=> y < x`,
  REAL_ARITH_TAC));;

let REAL_SUB_LDISTRIB = log_lemma "REAL_SUB_LDISTRIB" (fun () -> prove
 (`!x y z. x * (y - z) = x * y - x * z`,
  REAL_ARITH_TAC));;

let REAL_SUB_RDISTRIB = log_lemma "REAL_SUB_RDISTRIB" (fun () -> prove
 (`!x y z. (x - y) * z = x * z - y * z`,
  REAL_ARITH_TAC));;

(* ------------------------------------------------------------------------- *)
(* Theorems about "abs".                                                     *)
(* ------------------------------------------------------------------------- *)

let REAL_ABS_ZERO = log_lemma "REAL_ABS_ZERO" (fun () -> prove
 (`!x. (abs(x) = &0) <=> (x = &0)`,
  REAL_ARITH_TAC));;

let REAL_ABS_0 = log_lemma "REAL_ABS_0" (fun () -> prove
 (`abs(&0) = &0`,
  REAL_ARITH_TAC));;

let REAL_ABS_1 = log_lemma "REAL_ABS_1" (fun () -> prove
 (`abs(&1) = &1`,
  REAL_ARITH_TAC));;

let REAL_ABS_TRIANGLE = log_lemma "REAL_ABS_TRIANGLE" (fun () -> prove
 (`!x y. abs(x + y) <= abs(x) + abs(y)`,
  REAL_ARITH_TAC));;

let REAL_ABS_TRIANGLE_LE = log_lemma "REAL_ABS_TRIANGLE_LE" (fun () -> prove
 (`!x y z.abs(x) + abs(y - x) <= z ==> abs(y) <= z`,
  REAL_ARITH_TAC));;

let REAL_ABS_TRIANGLE_LT = log_lemma "REAL_ABS_TRIANGLE_LT" (fun () -> prove
 (`!x y z.abs(x) + abs(y - x) < z ==> abs(y) < z`,
  REAL_ARITH_TAC));;

let REAL_ABS_POS = log_lemma "REAL_ABS_POS" (fun () -> prove
 (`!x. &0 <= abs(x)`,
  REAL_ARITH_TAC));;

let REAL_ABS_SUB = log_lemma "REAL_ABS_SUB" (fun () -> prove
 (`!x y. abs(x - y) = abs(y - x)`,
  REAL_ARITH_TAC));;

let REAL_ABS_NZ = log_lemma "REAL_ABS_NZ" (fun () -> prove
 (`!x. ~(x = &0) <=> &0 < abs(x)`,
  REAL_ARITH_TAC));;

let REAL_ABS_ABS = log_lemma "REAL_ABS_ABS" (fun () -> prove
 (`!x. abs(abs(x)) = abs(x)`,
  REAL_ARITH_TAC));;

let REAL_ABS_LE = log_lemma "REAL_ABS_LE" (fun () -> prove
 (`!x. x <= abs(x)`,
  REAL_ARITH_TAC));;

let REAL_ABS_REFL = log_lemma "REAL_ABS_REFL" (fun () -> prove
 (`!x. (abs(x) = x) <=> &0 <= x`,
  REAL_ARITH_TAC));;

let REAL_ABS_BETWEEN = log_lemma "REAL_ABS_BETWEEN" (fun () -> prove
 (`!x y d. &0 < d /\ ((x - d) < y) /\ (y < (x + d)) <=> abs(y - x) < d`,
  REAL_ARITH_TAC));;

let REAL_ABS_BOUND = log_lemma "REAL_ABS_BOUND" (fun () -> prove
 (`!x y d. abs(x - y) < d ==> y < (x + d)`,
  REAL_ARITH_TAC));;

let REAL_ABS_STILLNZ = log_lemma "REAL_ABS_STILLNZ" (fun () -> prove
 (`!x y. abs(x - y) < abs(y) ==> ~(x = &0)`,
  REAL_ARITH_TAC));;

let REAL_ABS_CASES = log_lemma "REAL_ABS_CASES" (fun () -> prove
 (`!x. (x = &0) \/ &0 < abs(x)`,
  REAL_ARITH_TAC));;

let REAL_ABS_BETWEEN1 = log_lemma "REAL_ABS_BETWEEN1" (fun () -> prove
 (`!x y z. x < z /\ (abs(y - x)) < (z - x) ==> y < z`,
  REAL_ARITH_TAC));;

let REAL_ABS_SIGN = log_lemma "REAL_ABS_SIGN" (fun () -> prove
 (`!x y. abs(x - y) < y ==> &0 < x`,
  REAL_ARITH_TAC));;

let REAL_ABS_SIGN2 = log_lemma "REAL_ABS_SIGN2" (fun () -> prove
 (`!x y. abs(x - y) < --y ==> x < &0`,
  REAL_ARITH_TAC));;

let REAL_ABS_CIRCLE = log_lemma "REAL_ABS_CIRCLE" (fun () -> prove
 (`!x y h. abs(h) < (abs(y) - abs(x)) ==> abs(x + h) < abs(y)`,
  REAL_ARITH_TAC));;

let REAL_SUB_ABS = log_lemma "REAL_SUB_ABS" (fun () -> prove
 (`!x y. (abs(x) - abs(y)) <= abs(x - y)`,
  REAL_ARITH_TAC));;

let REAL_ABS_SUB_ABS = log_lemma "REAL_ABS_SUB_ABS" (fun () -> prove
 (`!x y. abs(abs(x) - abs(y)) <= abs(x - y)`,
  REAL_ARITH_TAC));;

let REAL_ABS_BETWEEN2 = log_lemma "REAL_ABS_BETWEEN2" (fun () -> prove
 (`!x0 x y0 y. x0 < y0 /\ &2 * abs(x - x0) < (y0 - x0) /\
                          &2 * abs(y - y0) < (y0 - x0)
        ==> x < y`,
  REAL_ARITH_TAC));;

let REAL_ABS_BOUNDS = log_lemma "REAL_ABS_BOUNDS" (fun () -> prove
 (`!x k. abs(x) <= k <=> --k <= x /\ x <= k`,
  REAL_ARITH_TAC));;

let REAL_BOUNDS_LE = log_lemma "REAL_BOUNDS_LE" (fun () -> prove
 (`!x k. --k <= x /\ x <= k <=> abs(x) <= k`,
  REAL_ARITH_TAC));;

let REAL_BOUNDS_LT = log_lemma "REAL_BOUNDS_LT" (fun () -> prove
 (`!x k. --k < x /\ x < k <=> abs(x) < k`,
  REAL_ARITH_TAC));;

(* ------------------------------------------------------------------------- *)
(* Theorems about max and min.                                               *)
(* ------------------------------------------------------------------------- *)

let REAL_MIN_MAX = log_lemma "REAL_MIN_MAX" (fun () -> prove
 (`!x y. min x y = --(max (--x) (--y))`,
  REAL_ARITH_TAC));;

let REAL_MAX_MIN = log_lemma "REAL_MAX_MIN" (fun () -> prove
 (`!x y. max x y = --(min (--x) (--y))`,
  REAL_ARITH_TAC));;

let REAL_MAX_MAX = log_lemma "REAL_MAX_MAX" (fun () -> prove
 (`!x y. x <= max x y /\ y <= max x y`,
  REAL_ARITH_TAC));;

let REAL_MIN_MIN = log_lemma "REAL_MIN_MIN" (fun () -> prove
 (`!x y. min x y <= x /\ min x y <= y`,
  REAL_ARITH_TAC));;

let REAL_MAX_SYM = log_lemma "REAL_MAX_SYM" (fun () -> prove
 (`!x y. max x y = max y x`,
  REAL_ARITH_TAC));;

let REAL_MIN_SYM = log_lemma "REAL_MIN_SYM" (fun () -> prove
 (`!x y. min x y = min y x`,
  REAL_ARITH_TAC));;

let REAL_LE_MAX = log_lemma "REAL_LE_MAX" (fun () -> prove
 (`!x y z. z <= max x y <=> z <= x \/ z <= y`,
  REAL_ARITH_TAC));;

let REAL_LE_MIN = log_lemma "REAL_LE_MIN" (fun () -> prove
 (`!x y z. z <= min x y <=> z <= x /\ z <= y`,
  REAL_ARITH_TAC));;

let REAL_LT_MAX = log_lemma "REAL_LT_MAX" (fun () -> prove
 (`!x y z. z < max x y <=> z < x \/ z < y`,
  REAL_ARITH_TAC));;

let REAL_LT_MIN = log_lemma "REAL_LT_MIN" (fun () -> prove
 (`!x y z. z < min x y <=> z < x /\ z < y`,
  REAL_ARITH_TAC));;

let REAL_MAX_LE = log_lemma "REAL_MAX_LE" (fun () -> prove
 (`!x y z. max x y <= z <=> x <= z /\ y <= z`,
  REAL_ARITH_TAC));;

let REAL_MIN_LE = log_lemma "REAL_MIN_LE" (fun () -> prove
 (`!x y z. min x y <= z <=> x <= z \/ y <= z`,
  REAL_ARITH_TAC));;

let REAL_MAX_LT = log_lemma "REAL_MAX_LT" (fun () -> prove
 (`!x y z. max x y < z <=> x < z /\ y < z`,
  REAL_ARITH_TAC));;

let REAL_MIN_LT = log_lemma "REAL_MIN_LT" (fun () -> prove
 (`!x y z. min x y < z <=> x < z \/ y < z`,
  REAL_ARITH_TAC));;

let REAL_MAX_ASSOC = log_lemma "REAL_MAX_ASSOC" (fun () -> prove
 (`!x y z. max x (max y z) = max (max x y) z`,
  REAL_ARITH_TAC));;

let REAL_MIN_ASSOC = log_lemma "REAL_MIN_ASSOC" (fun () -> prove
 (`!x y z. min x (min y z) = min (min x y) z`,
  REAL_ARITH_TAC));;

let REAL_MAX_ACI = log_lemma "REAL_MAX_ACI" (fun () -> prove
 (`(max x y = max y x) /\
   (max (max x y) z = max x (max y z)) /\
   (max x (max y z) = max y (max x z)) /\
   (max x x = x) /\
   (max x (max x y) = max x y)`,
  REAL_ARITH_TAC));;

let REAL_MIN_ACI = log_lemma "REAL_MIN_ACI" (fun () -> prove
 (`(min x y = min y x) /\
   (min (min x y) z = min x (min y z)) /\
   (min x (min y z) = min y (min x z)) /\
   (min x x = x) /\
   (min x (min x y) = min x y)`,
  REAL_ARITH_TAC));;

(* ------------------------------------------------------------------------- *)
(* To simplify backchaining, just as in the natural number case.             *)
(* ------------------------------------------------------------------------- *)

let REAL_LE_IMP =
  let pth = PURE_ONCE_REWRITE_RULE[IMP_CONJ] REAL_LE_TRANS in
  fun th -> GEN_ALL(MATCH_MP pth (SPEC_ALL th));;

let REAL_LET_IMP =
  let pth = PURE_ONCE_REWRITE_RULE[IMP_CONJ] REAL_LET_TRANS in
  fun th -> GEN_ALL(MATCH_MP pth (SPEC_ALL th));;

(* ------------------------------------------------------------------------- *)
(* Now a bit of nonlinear stuff.                                             *)
(* ------------------------------------------------------------------------- *)

let REAL_ABS_MUL = log_lemma "REAL_ABS_MUL" (fun () -> prove
 (`!x y. abs(x * y) = abs(x) * abs(y)`,
  REPEAT GEN_TAC THEN
  DISJ_CASES_TAC (SPEC `x:real` REAL_LE_NEGTOTAL) THENL
   [ALL_TAC;
    GEN_REWRITE_TAC (RAND_CONV o LAND_CONV) [GSYM REAL_ABS_NEG]] THEN
  (DISJ_CASES_TAC (SPEC `y:real` REAL_LE_NEGTOTAL) THENL
    [ALL_TAC;
     GEN_REWRITE_TAC (RAND_CONV o RAND_CONV) [GSYM REAL_ABS_NEG]]) THEN
  ASSUM_LIST(MP_TAC o MATCH_MP REAL_LE_MUL o end_itlist CONJ o rev) THEN
  REWRITE_TAC[REAL_MUL_LNEG; REAL_MUL_RNEG; REAL_NEG_NEG] THEN DISCH_TAC THENL
   [ALL_TAC;
    GEN_REWRITE_TAC LAND_CONV [GSYM REAL_ABS_NEG];
    GEN_REWRITE_TAC LAND_CONV [GSYM REAL_ABS_NEG];
    ALL_TAC] THEN
  ASM_REWRITE_TAC[real_abs; REAL_MUL_LNEG; REAL_MUL_RNEG; REAL_NEG_NEG]));;

let REAL_POW_LE = log_lemma "REAL_POW_LE" (fun () -> prove
 (`!x n. &0 <= x ==> &0 <= x pow n`,
  REPEAT STRIP_TAC THEN SPEC_TAC(`n:num`,`n:num`) THEN
  INDUCT_TAC THEN REWRITE_TAC[real_pow; REAL_POS] THEN
  MATCH_MP_TAC REAL_LE_MUL THEN ASM_REWRITE_TAC[]));;

let REAL_POW_LT = log_lemma "REAL_POW_LT" (fun () -> prove
 (`!x n. &0 < x ==> &0 < x pow n`,
  REPEAT STRIP_TAC THEN SPEC_TAC(`n:num`,`n:num`) THEN
  INDUCT_TAC THEN REWRITE_TAC[real_pow; REAL_LT_01] THEN
  MATCH_MP_TAC REAL_LT_MUL THEN ASM_REWRITE_TAC[]));;

let REAL_ABS_POW = log_lemma "REAL_ABS_POW" (fun () -> prove
 (`!x n. abs(x pow n) = abs(x) pow n`,
  GEN_TAC THEN INDUCT_TAC THEN
  ASM_REWRITE_TAC[real_pow; REAL_ABS_NUM; REAL_ABS_MUL]));;

let REAL_LE_LMUL = log_lemma "REAL_LE_LMUL" (fun () -> prove
 (`!x y z. &0 <= x /\ y <= z ==> x * y <= x * z`,
  ONCE_REWRITE_TAC[REAL_ARITH `x <= y <=> &0 <= y - x`] THEN
  REWRITE_TAC[GSYM REAL_SUB_LDISTRIB; REAL_SUB_RZERO; REAL_LE_MUL]));;

let REAL_LE_RMUL = log_lemma "REAL_LE_RMUL" (fun () -> prove
 (`!x y z. x <= y /\ &0 <= z ==> x * z <= y * z`,
  MESON_TAC[REAL_MUL_SYM; REAL_LE_LMUL]));;

let REAL_LT_LMUL = log_lemma "REAL_LT_LMUL" (fun () -> prove
 (`!x y z. &0 < x /\ y < z ==> x * y < x * z`,
  ONCE_REWRITE_TAC[REAL_ARITH `x < y <=> &0 < y - x`] THEN
  REWRITE_TAC[GSYM REAL_SUB_LDISTRIB; REAL_SUB_RZERO; REAL_LT_MUL]));;

let REAL_LT_RMUL = log_lemma "REAL_LT_RMUL" (fun () -> prove
 (`!x y z. x < y /\ &0 < z ==> x * z < y * z`,
  MESON_TAC[REAL_MUL_SYM; REAL_LT_LMUL]));;

let REAL_EQ_MUL_LCANCEL = log_lemma "REAL_EQ_MUL_LCANCEL" (fun () -> prove
 (`!x y z. (x * y = x * z) <=> (x = &0) \/ (y = z)`,
  REPEAT GEN_TAC THEN
  ONCE_REWRITE_TAC[REAL_ARITH `(x = y) <=> (x - y = &0)`] THEN
  REWRITE_TAC[GSYM REAL_SUB_LDISTRIB; REAL_ENTIRE; REAL_SUB_RZERO]));;

let REAL_EQ_MUL_RCANCEL = log_lemma "REAL_EQ_MUL_RCANCEL" (fun () -> prove
 (`!x y z. (x * z = y * z) <=> (x = y) \/ (z = &0)`,
  ONCE_REWRITE_TAC[REAL_MUL_SYM] THEN
  REWRITE_TAC[REAL_EQ_MUL_LCANCEL] THEN
  MESON_TAC[]));;

let REAL_MUL_LINV_UNIQ = log_lemma "REAL_MUL_LINV_UNIQ" (fun () -> prove
 (`!x y. (x * y = &1) ==> (inv(y) = x)`,
  REPEAT GEN_TAC THEN
  ASM_CASES_TAC `y = &0` THEN
  ASM_REWRITE_TAC[REAL_MUL_RZERO; REAL_OF_NUM_EQ; ARITH_EQ] THEN
  FIRST_ASSUM(SUBST1_TAC o SYM o MATCH_MP REAL_MUL_LINV) THEN
  ASM_REWRITE_TAC[REAL_EQ_MUL_RCANCEL] THEN
  DISCH_THEN(ACCEPT_TAC o SYM)));;

let REAL_MUL_RINV_UNIQ = log_lemma "REAL_MUL_RINV_UNIQ" (fun () -> prove
 (`!x y. (x * y = &1) ==> (inv(x) = y)`,
  ONCE_REWRITE_TAC[REAL_MUL_SYM] THEN
  MATCH_ACCEPT_TAC REAL_MUL_LINV_UNIQ));;

let REAL_INV_INV = log_lemma "REAL_INV_INV" (fun () -> prove
 (`!x. inv(inv x) = x`,
  GEN_TAC THEN ASM_CASES_TAC `x = &0` THEN
  ASM_REWRITE_TAC[REAL_INV_0] THEN
  MATCH_MP_TAC REAL_MUL_RINV_UNIQ THEN
  MATCH_MP_TAC REAL_MUL_LINV THEN
  ASM_REWRITE_TAC[]));;

let REAL_EQ_INV2 = log_lemma "REAL_EQ_INV2" (fun () -> prove
 (`!x y. inv(x) = inv(y) <=> x = y`,
  MESON_TAC[REAL_INV_INV]));;

let REAL_INV_EQ_0 = log_lemma "REAL_INV_EQ_0" (fun () -> prove
 (`!x. inv(x) = &0 <=> x = &0`,
  GEN_TAC THEN EQ_TAC THEN DISCH_TAC THEN ASM_REWRITE_TAC[REAL_INV_0] THEN
  ONCE_REWRITE_TAC[GSYM REAL_INV_INV] THEN ASM_REWRITE_TAC[REAL_INV_0]));;

let REAL_LT_INV = log_lemma "REAL_LT_INV" (fun () -> prove
 (`!x. &0 < x ==> &0 < inv(x)`,
  GEN_TAC THEN
  REPEAT_TCL DISJ_CASES_THEN ASSUME_TAC (SPEC `inv(x)` REAL_LT_NEGTOTAL) THEN
  ASM_REWRITE_TAC[] THENL
   [RULE_ASSUM_TAC(REWRITE_RULE[REAL_INV_EQ_0]) THEN ASM_REWRITE_TAC[];
    DISCH_TAC THEN SUBGOAL_THEN `&0 < --(inv x) * x` MP_TAC THENL
     [MATCH_MP_TAC REAL_LT_MUL THEN ASM_REWRITE_TAC[];
      REWRITE_TAC[REAL_MUL_LNEG]]] THEN
  SUBGOAL_THEN `inv(x) * x = &1` SUBST1_TAC THENL
   [MATCH_MP_TAC REAL_MUL_LINV THEN
    UNDISCH_TAC `&0 < x` THEN REAL_ARITH_TAC;
    REWRITE_TAC[REAL_LT_RNEG; REAL_ADD_LID; REAL_OF_NUM_LT; ARITH]]));;

let REAL_LT_INV_EQ = log_lemma "REAL_LT_INV_EQ" (fun () -> prove
 (`!x. &0 < inv x <=> &0 < x`,
  GEN_TAC THEN EQ_TAC THEN REWRITE_TAC[REAL_LT_INV] THEN
  GEN_REWRITE_TAC (funpow 2 RAND_CONV) [GSYM REAL_INV_INV] THEN
  REWRITE_TAC[REAL_LT_INV]));;

let REAL_INV_NEG = log_lemma "REAL_INV_NEG" (fun () -> prove
 (`!x. inv(--x) = --(inv x)`,
  GEN_TAC THEN ASM_CASES_TAC `x = &0` THEN
  ASM_REWRITE_TAC[REAL_NEG_0; REAL_INV_0] THEN
  MATCH_MP_TAC REAL_MUL_LINV_UNIQ THEN
  REWRITE_TAC[REAL_MUL_LNEG; REAL_MUL_RNEG; REAL_NEG_NEG] THEN
  MATCH_MP_TAC REAL_MUL_LINV THEN ASM_REWRITE_TAC[]));;

let REAL_LE_INV_EQ = log_lemma "REAL_LE_INV_EQ" (fun () -> prove
 (`!x. &0 <= inv x <=> &0 <= x`,
  REWRITE_TAC[REAL_LE_LT; REAL_LT_INV_EQ; REAL_INV_EQ_0] THEN
  MESON_TAC[REAL_INV_EQ_0]));;

let REAL_LE_INV = log_lemma "REAL_LE_INV" (fun () -> prove
 (`!x. &0 <= x ==> &0 <= inv(x)`,
  REWRITE_TAC[REAL_LE_INV_EQ]));;

let REAL_MUL_RINV = log_lemma "REAL_MUL_RINV" (fun () -> prove
 (`!x. ~(x = &0) ==> (x * inv(x) = &1)`,
  ONCE_REWRITE_TAC[REAL_MUL_SYM] THEN
  REWRITE_TAC[REAL_MUL_LINV]));;

let REAL_INV_1 = log_lemma "REAL_INV_1" (fun () -> prove
 (`inv(&1) = &1`,
  MATCH_MP_TAC REAL_MUL_RINV_UNIQ THEN
  REWRITE_TAC[REAL_MUL_LID]));;

let REAL_INV_EQ_1 = log_lemma "REAL_INV_EQ_1" (fun () -> prove
 (`!x. inv(x) = &1 <=> x = &1`,
  MESON_TAC[REAL_INV_INV; REAL_INV_1]));;

let REAL_DIV_1 = log_lemma "REAL_DIV_1" (fun () -> prove
 (`!x. x / &1 = x`,
  REWRITE_TAC[real_div; REAL_INV_1; REAL_MUL_RID]));;

let REAL_DIV_REFL = log_lemma "REAL_DIV_REFL" (fun () -> prove
 (`!x. ~(x = &0) ==> (x / x = &1)`,
  GEN_TAC THEN REWRITE_TAC[real_div; REAL_MUL_RINV]));;

let REAL_DIV_RMUL = log_lemma "REAL_DIV_RMUL" (fun () -> prove
 (`!x y. ~(y = &0) ==> ((x / y) * y = x)`,
  SIMP_TAC[real_div; GSYM REAL_MUL_ASSOC; REAL_MUL_LINV; REAL_MUL_RID]));;

let REAL_DIV_LMUL = log_lemma "REAL_DIV_LMUL" (fun () -> prove
 (`!x y. ~(y = &0) ==> (y * (x / y) = x)`,
  ONCE_REWRITE_TAC[REAL_MUL_SYM] THEN REWRITE_TAC[REAL_DIV_RMUL]));;

let REAL_ABS_INV = log_lemma "REAL_ABS_INV" (fun () -> prove
 (`!x. abs(inv x) = inv(abs x)`,
  GEN_TAC THEN CONV_TAC SYM_CONV THEN
  ASM_CASES_TAC `x = &0` THEN ASM_REWRITE_TAC[REAL_INV_0; REAL_ABS_0] THEN
  MATCH_MP_TAC REAL_MUL_RINV_UNIQ THEN
  REWRITE_TAC[GSYM REAL_ABS_MUL] THEN
  POP_ASSUM(SUBST1_TAC o MATCH_MP REAL_MUL_RINV) THEN
  REWRITE_TAC[REAL_ABS_1]));;

let REAL_ABS_DIV = log_lemma "REAL_ABS_DIV" (fun () -> prove
 (`!x y. abs(x / y) = abs(x) / abs(y)`,
  REWRITE_TAC[real_div; REAL_ABS_INV; REAL_ABS_MUL]));;

let REAL_INV_MUL = log_lemma "REAL_INV_MUL" (fun () -> prove
 (`!x y. inv(x * y) = inv(x) * inv(y)`,
  REPEAT GEN_TAC THEN
  MAP_EVERY ASM_CASES_TAC [`x = &0`; `y = &0`] THEN
  ASM_REWRITE_TAC[REAL_INV_0; REAL_MUL_LZERO; REAL_MUL_RZERO] THEN
  MATCH_MP_TAC REAL_MUL_LINV_UNIQ THEN
  ONCE_REWRITE_TAC[AC REAL_MUL_AC `(a * b) * (c * d) = (a * c) * (b * d)`] THEN
  EVERY_ASSUM(SUBST1_TAC o MATCH_MP REAL_MUL_LINV) THEN
  REWRITE_TAC[REAL_MUL_LID]));;

let REAL_INV_DIV = log_lemma "REAL_INV_DIV" (fun () -> prove
 (`!x y. inv(x / y) = y / x`,
  REWRITE_TAC[real_div; REAL_INV_INV; REAL_INV_MUL] THEN
  MATCH_ACCEPT_TAC REAL_MUL_SYM));;

let REAL_POW_MUL = log_lemma "REAL_POW_MUL" (fun () -> prove
 (`!x y n. (x * y) pow n = (x pow n) * (y pow n)`,
  GEN_TAC THEN GEN_TAC THEN INDUCT_TAC THEN
  ASM_REWRITE_TAC[real_pow; REAL_MUL_LID; REAL_MUL_AC]));;

let REAL_POW_INV = log_lemma "REAL_POW_INV" (fun () -> prove
 (`!x n. (inv x) pow n = inv(x pow n)`,
  GEN_TAC THEN INDUCT_TAC THEN
  ASM_REWRITE_TAC[real_pow; REAL_INV_1; REAL_INV_MUL]));;

let REAL_INV_POW = log_lemma "REAL_INV_POW" (fun () -> prove
 (`!x n. inv(x pow n) = (inv x) pow n`,
  REWRITE_TAC[REAL_POW_INV]));;

let REAL_POW_DIV = log_lemma "REAL_POW_DIV" (fun () -> prove
 (`!x y n. (x / y) pow n = (x pow n) / (y pow n)`,
  REWRITE_TAC[real_div; REAL_POW_MUL; REAL_POW_INV]));;

let REAL_POW_ADD = log_lemma "REAL_POW_ADD" (fun () -> prove
 (`!x m n. x pow (m + n) = x pow m * x pow n`,
  GEN_TAC THEN INDUCT_TAC THEN
  ASM_REWRITE_TAC[ADD_CLAUSES; real_pow; REAL_MUL_LID; REAL_MUL_ASSOC]));;

let REAL_POW_NZ = log_lemma "REAL_POW_NZ" (fun () -> prove
 (`!x n. ~(x = &0) ==> ~(x pow n = &0)`,
  GEN_TAC THEN INDUCT_TAC THEN
  REWRITE_TAC[real_pow; REAL_OF_NUM_EQ; ARITH] THEN
  ASM_MESON_TAC[REAL_ENTIRE]));;

let REAL_POW_SUB = log_lemma "REAL_POW_SUB" (fun () -> prove
 (`!x m n. ~(x = &0) /\ m <= n ==> (x pow (n - m) = x pow n / x pow m)`,
  REPEAT GEN_TAC THEN
  DISCH_THEN(CONJUNCTS_THEN2 ASSUME_TAC MP_TAC) THEN
  REWRITE_TAC[LE_EXISTS] THEN
  DISCH_THEN(CHOOSE_THEN SUBST1_TAC) THEN
  REWRITE_TAC[ADD_SUB2] THEN REWRITE_TAC[REAL_POW_ADD] THEN
  REWRITE_TAC[real_div] THEN ONCE_REWRITE_TAC[REAL_MUL_SYM] THEN
  GEN_REWRITE_TAC LAND_CONV [GSYM REAL_MUL_LID] THEN
  REWRITE_TAC[REAL_MUL_ASSOC] THEN AP_THM_TAC THEN AP_TERM_TAC THEN
  CONV_TAC SYM_CONV THEN MATCH_MP_TAC REAL_MUL_LINV THEN
  MATCH_MP_TAC REAL_POW_NZ THEN ASM_REWRITE_TAC[]));;

let REAL_LT_IMP_NZ = log_lemma "REAL_LT_IMP_NZ" (fun () -> prove
 (`!x. &0 < x ==> ~(x = &0)`,
  REAL_ARITH_TAC));;

let REAL_LT_LCANCEL_IMP = log_lemma "REAL_LT_LCANCEL_IMP" (fun () -> prove
 (`!x y z. &0 < x /\ x * y < x * z ==> y < z`,
  REPEAT GEN_TAC THEN
  DISCH_THEN(fun th -> ASSUME_TAC(CONJUNCT1 th) THEN MP_TAC th) THEN DISCH_THEN
   (MP_TAC o uncurry CONJ o (MATCH_MP REAL_LT_INV F_F I) o CONJ_PAIR) THEN
  DISCH_THEN(MP_TAC o MATCH_MP REAL_LT_LMUL) THEN
  POP_ASSUM(ASSUME_TAC o MATCH_MP REAL_MUL_LINV o MATCH_MP REAL_LT_IMP_NZ) THEN
  ASM_REWRITE_TAC[REAL_MUL_ASSOC; REAL_MUL_LID]));;

let REAL_LT_RCANCEL_IMP = log_lemma "REAL_LT_RCANCEL_IMP" (fun () -> prove
 (`!x y z. &0 < z /\ x * z < y * z ==> x < y`,
  ONCE_REWRITE_TAC[REAL_MUL_SYM] THEN REWRITE_TAC[REAL_LT_LCANCEL_IMP]));;

let REAL_LE_LCANCEL_IMP = log_lemma "REAL_LE_LCANCEL_IMP" (fun () -> prove
 (`!x y z. &0 < x /\ x * y <= x * z ==> y <= z`,
  REPEAT GEN_TAC THEN REWRITE_TAC[REAL_LE_LT; REAL_EQ_MUL_LCANCEL] THEN
  ASM_CASES_TAC `x = &0` THEN ASM_REWRITE_TAC[REAL_LT_REFL] THEN
  STRIP_TAC THEN ASM_REWRITE_TAC[] THEN DISJ1_TAC THEN
  MATCH_MP_TAC REAL_LT_LCANCEL_IMP THEN
  EXISTS_TAC `x:real` THEN ASM_REWRITE_TAC[]));;

let REAL_LE_RCANCEL_IMP = log_lemma "REAL_LE_RCANCEL_IMP" (fun () -> prove
 (`!x y z. &0 < z /\ x * z <= y * z ==> x <= y`,
  ONCE_REWRITE_TAC[REAL_MUL_SYM] THEN REWRITE_TAC[REAL_LE_LCANCEL_IMP]));;

let REAL_LE_RMUL_EQ = log_lemma "REAL_LE_RMUL_EQ" (fun () -> prove
 (`!x y z. &0 < z ==> (x * z <= y * z <=> x <= y)`,
  MESON_TAC[REAL_LE_RMUL; REAL_LE_RCANCEL_IMP; REAL_LT_IMP_LE]));;

let REAL_LE_LMUL_EQ = log_lemma "REAL_LE_LMUL_EQ" (fun () -> prove
 (`!x y z. &0 < z ==> (z * x <= z * y <=> x <= y)`,
  MESON_TAC[REAL_LE_RMUL_EQ; REAL_MUL_SYM]));;

let REAL_LT_RMUL_EQ = log_lemma "REAL_LT_RMUL_EQ" (fun () -> prove
 (`!x y z. &0 < z ==> (x * z < y * z <=> x < y)`,
  SIMP_TAC[GSYM REAL_NOT_LE; REAL_LE_RMUL_EQ]));;

let REAL_LT_LMUL_EQ = log_lemma "REAL_LT_LMUL_EQ" (fun () -> prove
 (`!x y z. &0 < z ==> (z * x < z * y <=> x < y)`,
  SIMP_TAC[GSYM REAL_NOT_LE; REAL_LE_LMUL_EQ]));;

let REAL_LE_MUL_EQ = log_lemma "REAL_LE_MUL_EQ" (fun () -> prove
 (`(!x y. &0 < x ==> (&0 <= x * y <=> &0 <= y)) /\
   (!x y. &0 < y ==> (&0 <= x * y <=> &0 <= x))`,
  MESON_TAC[REAL_LE_LMUL_EQ; REAL_LE_RMUL_EQ; REAL_MUL_LZERO; REAL_MUL_RZERO]));;

let REAL_LT_MUL_EQ = log_lemma "REAL_LT_MUL_EQ" (fun () -> prove
 (`(!x y. &0 < x ==> (&0 < x * y <=> &0 < y)) /\
   (!x y. &0 < y ==> (&0 < x * y <=> &0 < x))`,
  MESON_TAC[REAL_LT_LMUL_EQ; REAL_LT_RMUL_EQ; REAL_MUL_LZERO; REAL_MUL_RZERO]));;

let REAL_MUL_POS_LT = log_lemma "REAL_MUL_POS_LT" (fun () -> prove
 (`!x y. &0 < x * y <=> &0 < x /\ &0 < y \/ x < &0 /\ y < &0`,
  REPEAT STRIP_TAC THEN
  STRIP_ASSUME_TAC(SPEC `x:real` REAL_LT_NEGTOTAL) THEN
  STRIP_ASSUME_TAC(SPEC `y:real` REAL_LT_NEGTOTAL) THEN
  ASM_REWRITE_TAC[REAL_MUL_LZERO; REAL_MUL_RZERO; REAL_LT_REFL] THEN
  ASSUM_LIST(MP_TAC o MATCH_MP REAL_LT_MUL o end_itlist CONJ) THEN
  REPEAT(POP_ASSUM MP_TAC) THEN REAL_ARITH_TAC));;

let REAL_MUL_POS_LE = log_lemma "REAL_MUL_POS_LE" (fun () -> prove
 (`!x y. &0 <= x * y <=>
         x = &0 \/ y = &0 \/ &0 < x /\ &0 < y \/ x < &0 /\ y < &0`,
  REWRITE_TAC[REAL_ARITH `&0 <= x <=> x = &0 \/ &0 < x`] THEN
  REWRITE_TAC[REAL_MUL_POS_LT; REAL_ENTIRE] THEN REAL_ARITH_TAC));;

let REAL_LE_RDIV_EQ = log_lemma "REAL_LE_RDIV_EQ" (fun () -> prove
 (`!x y z. &0 < z ==> (x <= y / z <=> x * z <= y)`,
  REPEAT STRIP_TAC THEN
  FIRST_ASSUM(fun th ->
    GEN_REWRITE_TAC LAND_CONV [GSYM(MATCH_MP REAL_LE_RMUL_EQ th)]) THEN
  ASM_SIMP_TAC[real_div; GSYM REAL_MUL_ASSOC; REAL_MUL_LINV;
               REAL_MUL_RID; REAL_LT_IMP_NZ]));;

let REAL_LE_LDIV_EQ = log_lemma "REAL_LE_LDIV_EQ" (fun () -> prove
 (`!x y z. &0 < z ==> (x / z <= y <=> x <= y * z)`,
  REPEAT STRIP_TAC THEN
  FIRST_ASSUM(fun th ->
    GEN_REWRITE_TAC LAND_CONV [GSYM(MATCH_MP REAL_LE_RMUL_EQ th)]) THEN
  ASM_SIMP_TAC[real_div; GSYM REAL_MUL_ASSOC; REAL_MUL_LINV;
               REAL_MUL_RID; REAL_LT_IMP_NZ]));;

let REAL_LT_RDIV_EQ = log_lemma "REAL_LT_RDIV_EQ" (fun () -> prove
 (`!x y z. &0 < z ==> (x < y / z <=> x * z < y)`,
  SIMP_TAC[GSYM REAL_NOT_LE; REAL_LE_LDIV_EQ]));;

let REAL_LT_LDIV_EQ = log_lemma "REAL_LT_LDIV_EQ" (fun () -> prove
 (`!x y z. &0 < z ==> (x / z < y <=> x < y * z)`,
  SIMP_TAC[GSYM REAL_NOT_LE; REAL_LE_RDIV_EQ]));;

let REAL_EQ_RDIV_EQ = log_lemma "REAL_EQ_RDIV_EQ" (fun () -> prove
 (`!x y z. &0 < z ==> ((x = y / z) <=> (x * z = y))`,
  REWRITE_TAC[GSYM REAL_LE_ANTISYM] THEN
  SIMP_TAC[REAL_LE_RDIV_EQ; REAL_LE_LDIV_EQ]));;

let REAL_EQ_LDIV_EQ = log_lemma "REAL_EQ_LDIV_EQ" (fun () -> prove
 (`!x y z. &0 < z ==> ((x / z = y) <=> (x = y * z))`,
  REWRITE_TAC[GSYM REAL_LE_ANTISYM] THEN
  SIMP_TAC[REAL_LE_RDIV_EQ; REAL_LE_LDIV_EQ]));;

let REAL_LT_DIV2_EQ = log_lemma "REAL_LT_DIV2_EQ" (fun () -> prove
 (`!x y z. &0 < z ==> (x / z < y / z <=> x < y)`,
  SIMP_TAC[real_div; REAL_LT_RMUL_EQ; REAL_LT_INV_EQ]));;

let REAL_LE_DIV2_EQ = log_lemma "REAL_LE_DIV2_EQ" (fun () -> prove
 (`!x y z. &0 < z ==> (x / z <= y / z <=> x <= y)`,
  SIMP_TAC[real_div; REAL_LE_RMUL_EQ; REAL_LT_INV_EQ]));;

let REAL_MUL_2 = log_lemma "REAL_MUL_2" (fun () -> prove
 (`!x. &2 * x = x + x`,
  REAL_ARITH_TAC));;

let REAL_POW_EQ_0 = log_lemma "REAL_POW_EQ_0" (fun () -> prove
 (`!x n. (x pow n = &0) <=> (x = &0) /\ ~(n = 0)`,
  GEN_TAC THEN INDUCT_TAC THEN
  ASM_REWRITE_TAC[NOT_SUC; real_pow; REAL_ENTIRE] THENL
   [REAL_ARITH_TAC;
    CONV_TAC TAUT]));;

let REAL_LE_MUL2 = log_lemma "REAL_LE_MUL2" (fun () -> prove
 (`!w x y z. &0 <= w /\ w <= x /\ &0 <= y /\ y <= z
             ==> w * y <= x * z`,
  REPEAT STRIP_TAC THEN MATCH_MP_TAC REAL_LE_TRANS THEN
  EXISTS_TAC `w * z` THEN CONJ_TAC THENL
   [MATCH_MP_TAC REAL_LE_LMUL; MATCH_MP_TAC REAL_LE_RMUL] THEN
  ASM_REWRITE_TAC[] THEN
  MATCH_MP_TAC REAL_LE_TRANS THEN EXISTS_TAC `y:real` THEN
  ASM_REWRITE_TAC[]));;

let REAL_LT_MUL2 = log_lemma "REAL_LT_MUL2" (fun () -> prove
 (`!w x y z. &0 <= w /\ w < x /\ &0 <= y /\ y < z
             ==> w * y < x * z`,
  REPEAT STRIP_TAC THEN MATCH_MP_TAC REAL_LET_TRANS THEN
  EXISTS_TAC `w * z` THEN CONJ_TAC THENL
   [MATCH_MP_TAC REAL_LE_LMUL; MATCH_MP_TAC REAL_LT_RMUL] THEN
  ASM_REWRITE_TAC[] THENL
   [MATCH_MP_TAC REAL_LT_IMP_LE THEN ASM_REWRITE_TAC[];
    MATCH_MP_TAC REAL_LET_TRANS THEN EXISTS_TAC `y:real` THEN
    ASM_REWRITE_TAC[]]));;

let REAL_LT_SQUARE = log_lemma "REAL_LT_SQUARE" (fun () -> prove
 (`!x. (&0 < x * x) <=> ~(x = &0)`,
  GEN_TAC THEN REWRITE_TAC[REAL_LT_LE; REAL_LE_SQUARE] THEN
  GEN_REWRITE_TAC (LAND_CONV o RAND_CONV) [EQ_SYM_EQ] THEN
  REWRITE_TAC[REAL_ENTIRE]));;

let REAL_POW_1 = log_lemma "REAL_POW_1" (fun () -> prove
 (`!x. x pow 1 = x`,
  REWRITE_TAC[num_CONV `1`] THEN
  REWRITE_TAC[real_pow; REAL_MUL_RID]));;

let REAL_POW_ONE = log_lemma "REAL_POW_ONE" (fun () -> prove
 (`!n. &1 pow n = &1`,
  INDUCT_TAC THEN ASM_REWRITE_TAC[real_pow; REAL_MUL_LID]));;

let REAL_LT_INV2 = log_lemma "REAL_LT_INV2" (fun () -> prove
 (`!x y. &0 < x /\ x < y ==> inv(y) < inv(x)`,
  REPEAT STRIP_TAC THEN MATCH_MP_TAC REAL_LT_RCANCEL_IMP THEN
  EXISTS_TAC `x * y` THEN CONJ_TAC THENL
   [MATCH_MP_TAC REAL_LT_MUL THEN
    POP_ASSUM_LIST(MP_TAC o end_itlist CONJ) THEN REAL_ARITH_TAC;
    SUBGOAL_THEN `(inv x * x = &1) /\ (inv y * y = &1)` ASSUME_TAC THENL
     [CONJ_TAC THEN MATCH_MP_TAC REAL_MUL_LINV THEN
      POP_ASSUM_LIST(MP_TAC o end_itlist CONJ) THEN REAL_ARITH_TAC;
      ASM_REWRITE_TAC[REAL_MUL_ASSOC; REAL_MUL_LID] THEN
      GEN_REWRITE_TAC (LAND_CONV o LAND_CONV) [REAL_MUL_SYM] THEN
      ASM_REWRITE_TAC[GSYM REAL_MUL_ASSOC; REAL_MUL_RID]]]));;

let REAL_LE_INV2 = log_lemma "REAL_LE_INV2" (fun () -> prove
 (`!x y. &0 < x /\ x <= y ==> inv(y) <= inv(x)`,
  REPEAT GEN_TAC THEN REWRITE_TAC[REAL_LE_LT] THEN
  ASM_CASES_TAC `x:real = y` THEN ASM_REWRITE_TAC[] THEN
  STRIP_TAC THEN DISJ1_TAC THEN MATCH_MP_TAC REAL_LT_INV2 THEN
  ASM_REWRITE_TAC[]));;

let REAL_INV_LE_1 = log_lemma "REAL_INV_LE_1" (fun () -> prove
 (`!x. &1 <= x ==> inv(x) <= &1`,
  REPEAT STRIP_TAC THEN ONCE_REWRITE_TAC[GSYM REAL_INV_1] THEN
  MATCH_MP_TAC REAL_LE_INV2 THEN ASM_REWRITE_TAC[REAL_LT_01]));;

let REAL_INV_1_LE = log_lemma "REAL_INV_1_LE" (fun () -> prove
 (`!x. &0 < x /\ x <= &1 ==> &1 <= inv(x)`,
  REPEAT STRIP_TAC THEN ONCE_REWRITE_TAC[GSYM REAL_INV_1] THEN
  MATCH_MP_TAC REAL_LE_INV2 THEN ASM_REWRITE_TAC[REAL_LT_01]));;

let REAL_INV_LT_1 = log_lemma "REAL_INV_LT_1" (fun () -> prove
 (`!x. &1 < x ==> inv(x) < &1`,
  REPEAT STRIP_TAC THEN ONCE_REWRITE_TAC[GSYM REAL_INV_1] THEN
  MATCH_MP_TAC REAL_LT_INV2 THEN ASM_REWRITE_TAC[REAL_LT_01]));;

let REAL_INV_1_LT = log_lemma "REAL_INV_1_LT" (fun () -> prove
 (`!x. &0 < x /\ x < &1 ==> &1 < inv(x)`,
  REPEAT STRIP_TAC THEN ONCE_REWRITE_TAC[GSYM REAL_INV_1] THEN
  MATCH_MP_TAC REAL_LT_INV2 THEN ASM_REWRITE_TAC[REAL_LT_01]));;

let REAL_SUB_INV = log_lemma "REAL_SUB_INV" (fun () -> prove
 (`!x y. ~(x = &0) /\ ~(y = &0) ==> (inv(x) - inv(y) = (y - x) / (x * y))`,
  REWRITE_TAC[real_div; REAL_SUB_RDISTRIB; REAL_INV_MUL] THEN
  SIMP_TAC[REAL_MUL_ASSOC; REAL_MUL_RINV; REAL_MUL_LID] THEN
  REWRITE_TAC[GSYM REAL_MUL_ASSOC] THEN REWRITE_TAC[GSYM real_div] THEN
  SIMP_TAC[REAL_DIV_LMUL]));;

let REAL_DOWN = log_lemma "REAL_DOWN" (fun () -> prove
 (`!d. &0 < d ==> ?e. &0 < e /\ e < d`,
  GEN_TAC THEN DISCH_TAC THEN EXISTS_TAC `d / &2` THEN
  ASSUME_TAC(REAL_ARITH `&0 < &2`) THEN
  ASSUME_TAC(MATCH_MP REAL_MUL_LINV (REAL_ARITH `~(&2 = &0)`)) THEN
  CONJ_TAC THEN MATCH_MP_TAC REAL_LT_RCANCEL_IMP THEN EXISTS_TAC `&2` THEN
  ASM_REWRITE_TAC[real_div; GSYM REAL_MUL_ASSOC; REAL_MUL_RID] THEN
  UNDISCH_TAC `&0 < d` THEN REAL_ARITH_TAC));;

let REAL_DOWN2 = log_lemma "REAL_DOWN2" (fun () -> prove
 (`!d1 d2. &0 < d1 /\ &0 < d2 ==> ?e. &0 < e /\ e < d1 /\ e < d2`,
  REPEAT GEN_TAC THEN STRIP_TAC THEN
  DISJ_CASES_TAC(SPECL [`d1:real`; `d2:real`] REAL_LE_TOTAL) THENL
   [MP_TAC(SPEC `d1:real` REAL_DOWN);
    MP_TAC(SPEC `d2:real` REAL_DOWN)] THEN
  ASM_REWRITE_TAC[] THEN
  DISCH_THEN(X_CHOOSE_THEN `e:real` STRIP_ASSUME_TAC) THEN
  EXISTS_TAC `e:real` THEN
  POP_ASSUM_LIST(MP_TAC o end_itlist CONJ) THEN
  REAL_ARITH_TAC));;

let REAL_POW_LE2 = log_lemma "REAL_POW_LE2" (fun () -> prove
 (`!n x y. &0 <= x /\ x <= y ==> x pow n <= y pow n`,
  INDUCT_TAC THEN REWRITE_TAC[real_pow; REAL_LE_REFL] THEN
  REPEAT STRIP_TAC THEN MATCH_MP_TAC REAL_LE_MUL2 THEN
  ASM_REWRITE_TAC[] THEN CONJ_TAC THENL
   [MATCH_MP_TAC REAL_POW_LE THEN ASM_REWRITE_TAC[];
    FIRST_ASSUM MATCH_MP_TAC THEN ASM_REWRITE_TAC[]]));;

let REAL_POW_LE_1 = log_lemma "REAL_POW_LE_1" (fun () -> prove
 (`!n x. &1 <= x ==> &1 <= x pow n`,
  REPEAT STRIP_TAC THEN
  MP_TAC(SPECL [`n:num`; `&1`; `x:real`] REAL_POW_LE2) THEN
  ASM_REWRITE_TAC[REAL_POW_ONE; REAL_POS]));;

let REAL_POW_1_LE = log_lemma "REAL_POW_1_LE" (fun () -> prove
 (`!n x. &0 <= x /\ x <= &1 ==> x pow n <= &1`,
  REPEAT STRIP_TAC THEN
  MP_TAC(SPECL [`n:num`; `x:real`; `&1`] REAL_POW_LE2) THEN
  ASM_REWRITE_TAC[REAL_POW_ONE]));;

let REAL_POW_MONO = log_lemma "REAL_POW_MONO" (fun () -> prove
 (`!m n x. &1 <= x /\ m <= n ==> x pow m <= x pow n`,
  REPEAT GEN_TAC THEN REWRITE_TAC[LE_EXISTS] THEN
  DISCH_THEN(CONJUNCTS_THEN2 ASSUME_TAC MP_TAC) THEN
  DISCH_THEN(X_CHOOSE_THEN `d:num` SUBST1_TAC) THEN
  REWRITE_TAC[REAL_POW_ADD] THEN
  GEN_REWRITE_TAC LAND_CONV [GSYM REAL_MUL_RID] THEN
  MATCH_MP_TAC REAL_LE_LMUL THEN CONJ_TAC THENL
   [MATCH_MP_TAC REAL_LE_TRANS THEN EXISTS_TAC `&1` THEN
    REWRITE_TAC[REAL_OF_NUM_LE; ARITH] THEN
    MATCH_MP_TAC REAL_POW_LE_1 THEN ASM_REWRITE_TAC[];
    MATCH_MP_TAC REAL_POW_LE_1 THEN ASM_REWRITE_TAC[]]));;

let REAL_POW_LT2 = log_lemma "REAL_POW_LT2" (fun () -> prove
 (`!n x y. ~(n = 0) /\ &0 <= x /\ x < y ==> x pow n < y pow n`,
  INDUCT_TAC THEN REWRITE_TAC[NOT_SUC; real_pow] THEN REPEAT STRIP_TAC THEN
  ASM_CASES_TAC `n = 0` THEN ASM_REWRITE_TAC[real_pow; REAL_MUL_RID] THEN
  MATCH_MP_TAC REAL_LT_MUL2 THEN ASM_REWRITE_TAC[] THEN CONJ_TAC THENL
   [MATCH_MP_TAC REAL_POW_LE THEN ASM_REWRITE_TAC[];
    FIRST_ASSUM MATCH_MP_TAC THEN ASM_REWRITE_TAC[]]));;

let REAL_POW_LT_1 = log_lemma "REAL_POW_LT_1" (fun () -> prove
 (`!n x. ~(n = 0) /\ &1 < x ==> &1 < x pow n`,
  REPEAT STRIP_TAC THEN
  MP_TAC(SPECL [`n:num`; `&1`; `x:real`] REAL_POW_LT2) THEN
  ASM_REWRITE_TAC[REAL_POW_ONE; REAL_POS]));;

let REAL_POW_1_LT = log_lemma "REAL_POW_1_LT" (fun () -> prove
 (`!n x. ~(n = 0) /\ &0 <= x /\ x < &1 ==> x pow n < &1`,
  REPEAT STRIP_TAC THEN
  MP_TAC(SPECL [`n:num`; `x:real`; `&1`] REAL_POW_LT2) THEN
  ASM_REWRITE_TAC[REAL_POW_ONE]));;

let REAL_POW_MONO_LT = log_lemma "REAL_POW_MONO_LT" (fun () -> prove
 (`!m n x. &1 < x /\ m < n ==> x pow m < x pow n`,
  REPEAT GEN_TAC THEN REWRITE_TAC[LT_EXISTS] THEN
  DISCH_THEN(CONJUNCTS_THEN2 ASSUME_TAC MP_TAC) THEN
  DISCH_THEN(CHOOSE_THEN SUBST_ALL_TAC) THEN
  REWRITE_TAC[REAL_POW_ADD] THEN
  GEN_REWRITE_TAC LAND_CONV [GSYM REAL_MUL_RID] THEN
  MATCH_MP_TAC REAL_LT_LMUL THEN CONJ_TAC THENL
   [MATCH_MP_TAC REAL_POW_LT THEN
    MATCH_MP_TAC REAL_LT_TRANS THEN EXISTS_TAC `&1` THEN
    ASM_REWRITE_TAC[REAL_OF_NUM_LT; ARITH];
    SPEC_TAC(`d:num`,`d:num`) THEN
    INDUCT_TAC THEN ONCE_REWRITE_TAC[real_pow] THENL
     [ASM_REWRITE_TAC[real_pow; REAL_MUL_RID]; ALL_TAC] THEN
    GEN_REWRITE_TAC LAND_CONV [GSYM REAL_MUL_LID] THEN
    MATCH_MP_TAC REAL_LT_MUL2 THEN
    ASM_REWRITE_TAC[REAL_OF_NUM_LE; ARITH]]));;

let REAL_POW_POW = log_lemma "REAL_POW_POW" (fun () -> prove
 (`!x m n. (x pow m) pow n = x pow (m * n)`,
  GEN_TAC THEN GEN_TAC THEN INDUCT_TAC THEN
  ASM_REWRITE_TAC[real_pow; MULT_CLAUSES; REAL_POW_ADD]));;

let REAL_EQ_RCANCEL_IMP = log_lemma "REAL_EQ_RCANCEL_IMP" (fun () -> prove
 (`!x y z. ~(z = &0) /\ (x * z = y * z) ==> (x = y)`,
  REPEAT GEN_TAC THEN ONCE_REWRITE_TAC[GSYM REAL_SUB_0] THEN
  REWRITE_TAC[REAL_SUB_RZERO; GSYM REAL_SUB_RDISTRIB; REAL_ENTIRE] THEN
  CONV_TAC TAUT));;

let REAL_EQ_LCANCEL_IMP = log_lemma "REAL_EQ_LCANCEL_IMP" (fun () -> prove
 (`!x y z. ~(z = &0) /\ (z * x = z * y) ==> (x = y)`,
  ONCE_REWRITE_TAC[REAL_MUL_SYM] THEN MATCH_ACCEPT_TAC REAL_EQ_RCANCEL_IMP));;

let REAL_LT_DIV = log_lemma "REAL_LT_DIV" (fun () -> prove
 (`!x y. &0 < x /\ &0 < y ==> &0 < x / y`,
  SIMP_TAC[REAL_LT_MUL; REAL_LT_INV_EQ; real_div]));;

let REAL_LE_DIV = log_lemma "REAL_LE_DIV" (fun () -> prove
 (`!x y. &0 <= x /\ &0 <= y ==> &0 <= x / y`,
  SIMP_TAC[REAL_LE_MUL; REAL_LE_INV_EQ; real_div]));;

let REAL_DIV_POW2 = log_lemma "REAL_DIV_POW2" (fun () -> prove
 (`!x m n. ~(x = &0)
           ==> (x pow m / x pow n = if n <= m then x pow (m - n)
                                    else inv(x pow (n - m)))`,
  REPEAT STRIP_TAC THEN COND_CASES_TAC THEN ASM_REWRITE_TAC[] THEN
  ASM_SIMP_TAC[REAL_POW_SUB] THEN
  GEN_REWRITE_TAC LAND_CONV [GSYM REAL_INV_INV] THEN
  AP_TERM_TAC THEN REWRITE_TAC[REAL_INV_DIV] THEN
  UNDISCH_TAC `~(n:num <= m)` THEN REWRITE_TAC[NOT_LE] THEN
  DISCH_THEN(MP_TAC o MATCH_MP LT_IMP_LE) THEN
  ASM_SIMP_TAC[REAL_POW_SUB]));;

let REAL_DIV_POW2_ALT = log_lemma "REAL_DIV_POW2_ALT" (fun () -> prove
 (`!x m n. ~(x = &0)
           ==> (x pow m / x pow n = if n < m then x pow (m - n)
                                    else inv(x pow (n - m)))`,
  REPEAT STRIP_TAC THEN
  GEN_REWRITE_TAC LAND_CONV [GSYM REAL_INV_INV] THEN
  ONCE_REWRITE_TAC[REAL_INV_DIV] THEN
  ASM_SIMP_TAC[GSYM NOT_LE; REAL_DIV_POW2] THEN
  ASM_CASES_TAC `m <= n:num` THEN
  ASM_REWRITE_TAC[REAL_INV_INV]));;

let REAL_LT_POW2 = log_lemma "REAL_LT_POW2" (fun () -> prove
 (`!n. &0 < &2 pow n`,
  SIMP_TAC[REAL_POW_LT; REAL_OF_NUM_LT; ARITH]));;

let REAL_LE_POW2 = log_lemma "REAL_LE_POW2" (fun () -> prove
 (`!n. &1 <= &2 pow n`,
  GEN_TAC THEN MATCH_MP_TAC REAL_LE_TRANS THEN EXISTS_TAC `&2 pow 0` THEN
  SIMP_TAC[REAL_POW_MONO; LE_0; REAL_OF_NUM_LE; ARITH] THEN
  REWRITE_TAC[real_pow; REAL_LE_REFL]));;

let REAL_POW2_ABS = log_lemma "REAL_POW2_ABS" (fun () -> prove
 (`!x. abs(x) pow 2 = x pow 2`,
  GEN_TAC THEN REWRITE_TAC[real_abs] THEN
  COND_CASES_TAC THEN ASM_REWRITE_TAC[REAL_POW_NEG; ARITH_EVEN]));;

let REAL_LE_SQUARE_ABS = log_lemma "REAL_LE_SQUARE_ABS" (fun () -> prove
 (`!x y. abs(x) <= abs(y) <=> x pow 2 <= y pow 2`,
  REPEAT GEN_TAC THEN ONCE_REWRITE_TAC[GSYM REAL_POW2_ABS] THEN
  MESON_TAC[REAL_POW_LE2; REAL_ABS_POS; NUM_EQ_CONV `2 = 0`;
            REAL_POW_LT2; REAL_NOT_LE]));;

let REAL_LT_SQUARE_ABS = log_lemma "REAL_LT_SQUARE_ABS" (fun () -> prove
 (`!x y. abs(x) < abs(y) <=> x pow 2 < y pow 2`,
  REWRITE_TAC[GSYM REAL_NOT_LE; REAL_LE_SQUARE_ABS]));;

let REAL_EQ_SQUARE_ABS = log_lemma "REAL_EQ_SQUARE_ABS" (fun () -> prove
 (`!x y. abs x = abs y <=> x pow 2 = y pow 2`,
  REWRITE_TAC[GSYM REAL_LE_ANTISYM; REAL_LE_SQUARE_ABS]));;

let REAL_SOS_EQ_0 = log_lemma "REAL_SOS_EQ_0" (fun () -> prove
 (`!x y. x pow 2 + y pow 2 = &0 <=> x = &0 /\ y = &0`,
  REPEAT GEN_TAC THEN EQ_TAC THEN
  SIMP_TAC[REAL_POW_2; REAL_MUL_LZERO; REAL_ADD_LID] THEN
  DISCH_THEN(MP_TAC o MATCH_MP (REAL_ARITH
   `x + y = &0 ==> &0 <= x /\ &0 <= y ==> x = &0 /\ y = &0`)) THEN
  REWRITE_TAC[REAL_LE_SQUARE; REAL_ENTIRE]));;

let REAL_POW_LE2_ODD = log_lemma "REAL_POW_LE2_ODD" (fun () -> prove
 (`!n x y. x <= y /\ ODD n ==> x pow n <= y pow n`,
  REPEAT STRIP_TAC THEN DISJ_CASES_TAC(SPEC `y:real` REAL_LE_NEGTOTAL) THENL
   [DISJ_CASES_TAC(SPEC `x:real` REAL_LE_NEGTOTAL) THEN
    ASM_SIMP_TAC[REAL_POW_LE2] THEN
    SUBGOAL_THEN `&0 <= --x pow n /\ &0 <= y pow n` MP_TAC THENL
     [ASM_SIMP_TAC[REAL_POW_LE];
      ASM_REWRITE_TAC[REAL_POW_NEG; GSYM NOT_ODD] THEN REAL_ARITH_TAC];
    SUBGOAL_THEN `--y pow n <= --x pow n` MP_TAC THENL
     [MATCH_MP_TAC REAL_POW_LE2;
      ASM_REWRITE_TAC[REAL_POW_NEG; GSYM NOT_ODD]] THEN
    REPEAT(POP_ASSUM MP_TAC) THEN REAL_ARITH_TAC]));;

let REAL_POW_ZERO = log_lemma "REAL_POW_ZERO" (fun () -> prove
 (`!n. &0 pow n = if n = 0 then &1 else &0`,
  INDUCT_TAC THEN REWRITE_TAC[real_pow; NOT_SUC; REAL_MUL_LZERO]));;

let REAL_POW_MONO_INV = log_lemma "REAL_POW_MONO_INV" (fun () -> prove
 (`!m n x. &0 <= x /\ x <= &1 /\ n <= m ==> x pow m <= x pow n`,
  REPEAT STRIP_TAC THEN ASM_CASES_TAC `x = &0` THENL
   [ASM_REWRITE_TAC[REAL_POW_ZERO] THEN
    REPEAT(COND_CASES_TAC THEN REWRITE_TAC[REAL_POS; REAL_LE_REFL]) THEN
    UNDISCH_TAC `n:num <= m` THEN ASM_REWRITE_TAC[LE];
    GEN_REWRITE_TAC BINOP_CONV [GSYM REAL_INV_INV] THEN
    MATCH_MP_TAC REAL_LE_INV2 THEN REWRITE_TAC[GSYM REAL_POW_INV] THEN
    CONJ_TAC THENL
     [MATCH_MP_TAC REAL_POW_LT THEN REWRITE_TAC[REAL_LT_INV_EQ];
      MATCH_MP_TAC REAL_POW_MONO THEN ASM_REWRITE_TAC[] THEN
      MATCH_MP_TAC REAL_INV_1_LE] THEN
    ASM_REWRITE_TAC[REAL_LT_LE]]));;

let REAL_POW_LE2_REV = log_lemma "REAL_POW_LE2_REV" (fun () -> prove
 (`!n x y. ~(n = 0) /\ &0 <= y /\ x pow n <= y pow n ==> x <= y`,
  MESON_TAC[REAL_POW_LT2; REAL_NOT_LE]));;

let REAL_POW_LT2_REV = log_lemma "REAL_POW_LT2_REV" (fun () -> prove
 (`!n x y. &0 <= y /\ x pow n < y pow n ==> x < y`,
  MESON_TAC[REAL_POW_LE2; REAL_NOT_LE]));;

let REAL_POW_EQ = log_lemma "REAL_POW_EQ" (fun () -> prove
 (`!n x y. ~(n = 0) /\ &0 <= x /\ &0 <= y /\ x pow n = y pow n ==> x = y`,
  REWRITE_TAC[GSYM REAL_LE_ANTISYM] THEN MESON_TAC[REAL_POW_LE2_REV]));;

let REAL_POW_EQ_ABS = log_lemma "REAL_POW_EQ_ABS" (fun () -> prove
 (`!n x y. ~(n = 0) /\ x pow n = y pow n ==> abs x = abs y`,
  REPEAT STRIP_TAC THEN MATCH_MP_TAC REAL_POW_EQ THEN EXISTS_TAC `n:num` THEN
  ASM_REWRITE_TAC[REAL_ABS_POS; GSYM REAL_ABS_POW]));;

let REAL_POW_EQ_1_IMP = log_lemma "REAL_POW_EQ_1_IMP" (fun () -> prove
 (`!x n. ~(n = 0) /\ x pow n = &1 ==> abs(x) = &1`,
  REPEAT STRIP_TAC THEN GEN_REWRITE_TAC RAND_CONV [GSYM REAL_ABS_NUM] THEN
  MATCH_MP_TAC REAL_POW_EQ_ABS THEN EXISTS_TAC `n:num` THEN
  ASM_REWRITE_TAC[REAL_POW_ONE]));;

let REAL_POW_EQ_1 = log_lemma "REAL_POW_EQ_1" (fun () -> prove
 (`!x n. x pow n = &1 <=> abs(x) = &1 /\ (x < &0 ==> EVEN(n)) \/ n = 0`,
  REPEAT GEN_TAC THEN
  ASM_CASES_TAC `n = 0` THEN ASM_REWRITE_TAC[real_pow] THEN
  ASM_CASES_TAC `abs(x) = &1` THENL
   [ALL_TAC; ASM_MESON_TAC[REAL_POW_EQ_1_IMP]] THEN
  ASM_REWRITE_TAC[] THEN
  FIRST_X_ASSUM(DISJ_CASES_THEN SUBST1_TAC o MATCH_MP (REAL_ARITH
   `abs x = a ==> x = a \/ x = --a`)) THEN
  ASM_REWRITE_TAC[REAL_POW_NEG; REAL_POW_ONE] THEN
  REPEAT COND_CASES_TAC THEN ASM_REWRITE_TAC[] THEN REAL_ARITH_TAC));;

(* ------------------------------------------------------------------------- *)
(* Useful "without loss of generality" lemmas.                               *)
(* ------------------------------------------------------------------------- *)

let REAL_WLOG_LE = log_lemma "REAL_WLOG_LE" (fun () -> prove
 (`(!x y. P x y <=> P y x) /\ (!x y. x <= y ==> P x y) ==> !x y. P x y`,
  MESON_TAC[REAL_LE_TOTAL]));;

let REAL_WLOG_LT = log_lemma "REAL_WLOG_LT" (fun () -> prove
 (`(!x. P x x) /\ (!x y. P x y <=> P y x) /\ (!x y. x < y ==> P x y)
   ==> !x y. P x y`,
  MESON_TAC[REAL_LT_TOTAL]));;

(* ------------------------------------------------------------------------- *)
(* Close out the logfile.                                                    *)
(* ------------------------------------------------------------------------- *)

logfile_end ();;
