new_type ("T",0);;

let t = `t : T -> T -> bool` in
new_constant ("le", type_of t);;

let LE_TOTAL_ORDER = new_axiom (`total_order le`);;

let le_thms =
    CONJUNCTS
      (REWRITE_RULE
         [refl_rel_def;antisym_rel_def;trans_rel_def;total_rel_def;
          pre_order_def;partial_order_def;total_order_def]
       LE_TOTAL_ORDER);;

let le_list_def = theorem (fun () -> define
    `le_list [] l2 = T /\
     le_list (CONS h1 t1) [] = F /\
     le_list (CONS h1 t1) (CONS h2 t2) =
       if le h1 h2 then if le h2 h1 then le_list t1 t2 else T else F`);;

let LE_LIST_TOTAL_ORDER = theorem (fun () -> prove
    (`total_order le_list`,
     REWRITE_TAC [refl_rel_def;antisym_rel_def;trans_rel_def;total_rel_def;
                  pre_order_def;partial_order_def;total_order_def] THEN
     REPEAT CONJ_TAC THENL
     [(LIST_INDUCT_TAC THEN REWRITE_TAC [le_list_def]) THEN
      ASM_MESON_TAC le_thms;
      (LIST_INDUCT_TAC THEN REWRITE_TAC [le_list_def]) THEN
      (LIST_INDUCT_TAC THEN REWRITE_TAC [le_list_def]) THEN
      POP_ASSUM (K ALL_TAC) THEN
      (LIST_INDUCT_TAC THEN REWRITE_TAC [le_list_def]) THEN
      POP_ASSUM (K ALL_TAC) THEN
      (ASM_CASES_TAC `le h h'` THEN ASM_REWRITE_TAC []) THEN
      (ASM_CASES_TAC `le h' h''` THEN ASM_REWRITE_TAC []) THEN
      ASM_CASES_TAC `le h'' h` THENL
      [ASM_MESON_TAC le_thms;
       ASM_REWRITE_TAC [] THEN
       ASM_MESON_TAC le_thms];
      (LIST_INDUCT_TAC THEN LIST_INDUCT_TAC THEN REWRITE_TAC [le_list_def]) THEN
      POP_ASSUM (K ALL_TAC) THEN
      ASM_REWRITE_TAC [CONS_11] THEN
      ASM_MESON_TAC le_thms;
      (LIST_INDUCT_TAC THEN LIST_INDUCT_TAC THEN REWRITE_TAC [le_list_def]) THEN
      POP_ASSUM (K ALL_TAC) THEN
      ASM_MESON_TAC le_thms]));;
