new_type ("T",0);;

let t = `t : T -> T -> bool` in
new_constant ("cmp", type_of t);;

let CMP_TOTAL_ORDER = new_axiom (`total_order cmp`);;

let cmp_thms =
    CONJUNCTS
      (REWRITE_RULE
         [refl_rel_def;antisym_rel_def;trans_rel_def;total_rel_def;
          pre_order_def;partial_order_def;total_order_def]
       CMP_TOTAL_ORDER);;

let cmp_list_def = theorem (fun () -> define
    `cmp_list [] l2 = T /\
     cmp_list (CONS h1 t1) [] = F /\
     cmp_list (CONS h1 t1) (CONS h2 t2) =
       if cmp h1 h2 then if cmp h2 h1 then cmp_list t1 t2 else T else F`);;

let CMP_LIST_TOTAL_ORDER = theorem (fun () -> prove
    (`total_order cmp_list`,
     REWRITE_TAC [refl_rel_def;antisym_rel_def;trans_rel_def;total_rel_def;
                  pre_order_def;partial_order_def;total_order_def] THEN
     REPEAT CONJ_TAC THENL
     [(LIST_INDUCT_TAC THEN REWRITE_TAC [cmp_list_def]) THEN
      ASM_MESON_TAC cmp_thms;
      (LIST_INDUCT_TAC THEN REWRITE_TAC [cmp_list_def]) THEN
      (LIST_INDUCT_TAC THEN REWRITE_TAC [cmp_list_def]) THEN
      POP_ASSUM (K ALL_TAC) THEN
      (LIST_INDUCT_TAC THEN REWRITE_TAC [cmp_list_def]) THEN
      POP_ASSUM (K ALL_TAC) THEN
      (ASM_CASES_TAC `cmp h h'` THEN ASM_REWRITE_TAC []) THEN
      (ASM_CASES_TAC `cmp h' h''` THEN ASM_REWRITE_TAC []) THEN
      ASM_CASES_TAC `cmp h'' h` THENL
      [ASM_MESON_TAC cmp_thms;
       ASM_REWRITE_TAC [] THEN
       ASM_MESON_TAC cmp_thms];
      (LIST_INDUCT_TAC THEN LIST_INDUCT_TAC THEN REWRITE_TAC [cmp_list_def]) THEN
      POP_ASSUM (K ALL_TAC) THEN
      ASM_REWRITE_TAC [CONS_11] THEN
      ASM_MESON_TAC cmp_thms;
      (LIST_INDUCT_TAC THEN LIST_INDUCT_TAC THEN REWRITE_TAC [cmp_list_def]) THEN
      POP_ASSUM (K ALL_TAC) THEN
      ASM_MESON_TAC cmp_thms]));;
