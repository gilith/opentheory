let refl_rel_def = define
  `refl_rel r = !x : 'a. r x x`;;

let antisym_rel_def = define
  `antisym_rel r = !x y : 'a. r x y /\ r y x ==> x = y`;;

let trans_rel_def = define
  `trans_rel r = !x y z : 'a. r x y /\ r y z ==> r x z`;;

let total_rel_def = define
  `total_rel r = !x y : 'a. r x y \/ r y x`;;

let pre_order_def = define
  `pre_order (r : 'a -> 'a -> bool) <=> refl_rel r /\ trans_rel r`;;

let partial_order_def = define
  `partial_order (r : 'a -> 'a -> bool) <=> pre_order r /\ antisym_rel r`;;

let total_order_def = define
  `total_order (r : 'a -> 'a -> bool) <=> partial_order r /\ total_rel r`;;
