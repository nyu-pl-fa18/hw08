open OUnit2
open Arithmetic
  
let n = 12345689123456789
let bn = BigNat.from_int n

let bn_test1 test_ctxt =
  assert_equal BigNat.(bn + bn |> to_int) (n + n) 

let bn_test2 test_ctxt =
  assert_equal BigNat.(bn * bn |> to_int) (n * n) 

let bn_test3 test_ctxt =
  assert_equal BigNat.(bn - one |> to_int) (n - 1) 

let bn_test4 test_ctxt =
  assert_equal BigNat.(bn - (bn - one) |> to_int) 1 

let bn_test5 test_ctxt =
  assert_equal BigNat.(bn - zero |> to_int) n 

let bn_test6 test_ctxt =
  assert_equal BigNat.(zero - zero |> to_int) 0 
    
let bn_test7 test_ctxt =
  assert_equal BigNat.(bn + bn - bn) bn 
    
let bn_test8 test_ctxt =
  assert_raises
    (Invalid_argument "result is negative")
    (fun () -> BigNat.(bn - one - bn |> to_int))
    
let bn_test9 test_ctxt =
  assert_equal BigNat.(bn * bn / bn) bn

let fac_test1 test_ctxt =
  assert_equal (BigNat.zero |> fac) BigNat.one

let fac_test2 test_ctxt =
  assert_equal (5 |> BigNat.from_int |> fac |> BigNat.to_int) 120
    
let fac_test3 test_ctxt =
  assert_equal
    (BigNat.from_int 100 |> fac |> BigNat.to_string)
    "93326215443944152681699238856266700490715968264381621468592963895217599993229915608941463976156518286253697920827223758251185210916864000000000000000000000000"
    
let digit_test1 test_ctxt =
  assert_equal (BigNat.zero |> digits) 1

let digit_test2 test_ctxt =
  assert_equal (BigNat.one |> digits) 1

let digit_test3 test_ctxt =
  assert_equal (10 |> BigNat.from_int |> digits) 2

let digit_test3 test_ctxt =
  assert_equal (333 |> BigNat.from_int |> fac |> digits) 698

let bi = BigInt.from_int n

let bi_test1 test_ctxt =
  assert_equal BigInt.(bi |> to_string) (string_of_int n)

let bi_test2 test_ctxt =
  assert_equal BigInt.(neg bi |> to_string) (string_of_int (-n))

let bi_test3 test_ctxt =
  assert_equal BigInt.(neg zero) BigInt.zero

let bi_test4 test_ctxt =
  assert_equal BigInt.(bi |> neg |> neg) bi

let bi_test5 test_ctxt =
  assert_equal BigInt.(bi + zero) bi

let bi_test6 test_ctxt =
  assert_equal BigInt.(zero + bi) bi
    
let bi_test7 test_ctxt =
  assert_equal BigInt.(bi + bi |> to_int) (n + n)

let bi_test8 test_ctxt =
  assert_equal BigInt.(neg bi + bi) BigInt.zero

let bi_test9 test_ctxt =
  assert_equal BigInt.(neg bi + neg bi |> to_int) (-n + -n)
    
let bi_test10 test_ctxt =
  assert_equal BigInt.(bi - zero) bi

let bi_test11 test_ctxt =
  assert_equal BigInt.(zero - bi) (BigInt.neg bi)
    
let bi_test12 test_ctxt =
  assert_equal BigInt.(bi - bi) BigInt.zero

let bi_test13 test_ctxt =
  assert_equal BigInt.(neg bi - bi |> to_int) (-n - n)

let bi_test14 test_ctxt =
  assert_equal BigInt.(bi - neg bi |> to_int) (n - -n)
    
let bi_test15 test_ctxt =
  assert_equal BigInt.(neg bi - neg bi) BigInt.zero

let bi_test16 test_ctxt =
  assert_equal BigInt.(bi * zero) BigInt.zero

let bi_test17 test_ctxt =
  assert_equal BigInt.(zero * bi) BigInt.zero

let bi_test18 test_ctxt =
  assert_equal BigInt.(zero * neg bi) BigInt.zero

let bi_test19 test_ctxt =
  assert_equal BigInt.(neg bi * zero) BigInt.zero

let bi_test20 test_ctxt =
  assert_equal BigInt.(neg bi * one |> BigInt.to_int) (-n)

let bi_test21 test_ctxt =
  assert_equal BigInt.(one * neg bi |> BigInt.to_int) (-n)

let bi_test22 test_ctxt =
  assert_equal BigInt.(bi * bi |> to_int) (n * n)

let bi_test23 test_ctxt =
  assert_equal BigInt.(bi * neg bi |> to_int) (n * -n)

let bi_test24 test_ctxt =
  assert_equal BigInt.(neg bi * bi |> to_int) (n * -n)

let bi_test25 test_ctxt =
  assert_equal BigInt.(neg bi * neg bi |> to_int) (-n * -n)

let bi_test26 test_ctxt =
  assert_raises Division_by_zero (fun () -> BigInt.(bi / zero))

let bi_test27 test_ctxt =
  assert_equal BigInt.(bi / one) bi

let bi_test28 test_ctxt =
  assert_equal BigInt.(bi / bi) BigInt.one

let bi_test29 test_ctxt =
  assert_equal BigInt.(bi / neg bi) BigInt.(neg one)

let bi_test30 test_ctxt =
  assert_equal BigInt.(neg bi / bi) BigInt.(neg one)

let bi_test31 test_ctxt =
  assert_equal BigInt.(neg bi / neg bi) BigInt.one

let bi_test32 test_ctxt =
  assert_equal BigInt.(one / neg bi) BigInt.zero

let bi_test33 test_ctxt =
  assert_equal BigInt.(one mod neg bi) BigInt.one

let bi_test34 test_ctxt =
  assert_equal BigInt.(neg one mod bi) BigInt.(neg one)

let bi_test35 test_ctxt =
  assert_equal BigInt.(neg one mod neg bi) BigInt.(neg one)

let bi_test36 test_ctxt =
  assert_equal BigInt.(bi mod bi) BigInt.zero

let bi_test37 test_ctxt =
  assert_equal BigInt.(bi mod neg bi) BigInt.zero

let bi_test38 test_ctxt =
  assert_equal BigInt.(neg bi mod neg bi) BigInt.zero
    
let bi_test39 test_ctxt =
  assert_equal BigInt.(neg bi mod bi) BigInt.zero

let bi_test40 test_ctxt =
  assert_equal BigInt.(from_int 2 ** from_int 3 |> to_int) 8

let bi_test41 test_ctxt =
  assert_equal BigInt.(from_int (-2) ** from_int 3 |> to_int) (-8)

let bi_test42 test_ctxt =
  assert_equal BigInt.(from_int (-2) ** from_int 4 |> to_int) 16

let bi_test43 test_ctxt =
  assert_equal BigInt.(from_int 2 ** zero) BigInt.one

let bi_test44 test_ctxt =
  assert_equal BigInt.(from_int (-2) ** zero) BigInt.one

let bi_test45 test_ctxt =
  assert_raises
    (Invalid_argument "negative exponent")
    (fun () -> BigInt.(from_int 2 ** from_int (-2)))

let bi_test46 test_ctxt =
  assert_equal BigInt.(from_int (-2) <= zero) true

let bi_test47 test_ctxt =
  assert_equal BigInt.(from_int (-2) <= from_int (-3)) false
    
let bi_test48 test_ctxt =
  assert_equal BigInt.(one <= one) true

let bi_test49 test_ctxt =
  assert_equal BigInt.(from_int (-2) <= from_int (-2)) true

let bi_test50 test_ctxt =
  assert_equal BigInt.(from_int 3 <= from_int 2) false

let bi_test51 test_ctxt =
  assert_equal BigInt.(neg bi <= bi) true

let bi_test52 test_ctxt =
  assert_equal BigInt.(bi <= neg bi) false

let bi_test53 test_ctxt =
  assert_equal BigInt.(from_int (-2) < zero) true

let bi_test54 test_ctxt =
  assert_equal BigInt.(from_int (-2) < from_int (-3)) false
    
let bi_test55 test_ctxt =
  assert_equal BigInt.(one < one) false

let bi_test56 test_ctxt =
  assert_equal BigInt.(from_int (-2) < from_int (-2)) false

let bi_test57 test_ctxt =
  assert_equal BigInt.(from_int 3 < from_int 2) false

let bi_test58 test_ctxt =
  assert_equal BigInt.(neg bi < bi) true

let bi_test59 test_ctxt =
  assert_equal BigInt.(bi < neg bi) false

let bi_test60 test_ctxt =
  assert_equal BigInt.(neg bi |> abs) bi
    
let bi_test61 test_ctxt =
  assert_equal BigInt.(bi |> abs) bi

let bi_test62 test_ctxt =
  assert_equal BigInt.(neg zero |> abs) BigInt.zero

let bi_test63 test_ctxt =
  assert_equal BigInt.(zero |> abs) BigInt.zero

let bi_test64 test_ctxt =
  assert_equal BigInt.(zero |> is_even) true

let bi_test65 test_ctxt =
  assert_equal BigInt.(bi |> is_even) false

let bi_test66 test_ctxt =
  assert_equal BigInt.(neg bi |> is_even) false

let bi_test67 test_ctxt =
  assert_equal BigInt.(neg (from_int 2) |> is_even) true
    
let bi_test68 test_ctxt =
  assert_equal BigInt.(bi * from_int 2 - bi - bi) BigInt.zero

    
let suite =
  "Arithmetic suite" >:::
  ["bn_test1" >:: bn_test1;
   "bn_test2" >:: bn_test2;
   "bn_test3" >:: bn_test3;
   "bn_test4" >:: bn_test4;
   "bn_test5" >:: bn_test5;
   "bn_test6" >:: bn_test5;
   "bn_test7" >:: bn_test5;
   "bn_test8" >:: bn_test5;
   "bn_test9" >:: bn_test5;
   "fac_test1" >:: fac_test1;
   "fac_test2" >:: fac_test2;
   "fac_test3" >:: fac_test3;
   "digit_test1" >:: digit_test1;
   "digit_test2" >:: digit_test2;
   "digit_test3" >:: digit_test3;
   "bi_test1" >:: bi_test1;
   "bi_test2" >:: bi_test2;
   "bi_test3" >:: bi_test3;
   "bi_test4" >:: bi_test4;
   "bi_test5" >:: bi_test5;
   "bi_test6" >:: bi_test6;
   "bi_test7" >:: bi_test7;
   "bi_test8" >:: bi_test8;
   "bi_test9" >:: bi_test9;
   "bi_test10" >:: bi_test10;
   "bi_test11" >:: bi_test11;
   "bi_test12" >:: bi_test12;
   "bi_test13" >:: bi_test13;
   "bi_test14" >:: bi_test14;
   "bi_test15" >:: bi_test15;
   "bi_test16" >:: bi_test16;
   "bi_test17" >:: bi_test17;
   "bi_test18" >:: bi_test18;
   "bi_test19" >:: bi_test19;
   "bi_test20" >:: bi_test20;
   "bi_test21" >:: bi_test21;
   "bi_test22" >:: bi_test22;
   "bi_test23" >:: bi_test23;
   "bi_test24" >:: bi_test24;
   "bi_test25" >:: bi_test25;
   "bi_test26" >:: bi_test26;
   "bi_test27" >:: bi_test27;
   "bi_test28" >:: bi_test28;
   "bi_test29" >:: bi_test29;
   "bi_test30" >:: bi_test30;
   "bi_test31" >:: bi_test31;
   "bi_test32" >:: bi_test32;
   "bi_test33" >:: bi_test33;
   "bi_test34" >:: bi_test34;
   "bi_test35" >:: bi_test35;
   "bi_test36" >:: bi_test36;
   "bi_test37" >:: bi_test37;
   "bi_test38" >:: bi_test38;
   "bi_test39" >:: bi_test39;
   "bi_test40" >:: bi_test40;
   "bi_test41" >:: bi_test41;
   "bi_test42" >:: bi_test42;
   "bi_test43" >:: bi_test43;
   "bi_test44" >:: bi_test44;
   "bi_test45" >:: bi_test45;
   "bi_test46" >:: bi_test46;
   "bi_test47" >:: bi_test47;
   "bi_test48" >:: bi_test48;
   "bi_test49" >:: bi_test49;
   "bi_test50" >:: bi_test50;
   "bi_test51" >:: bi_test51;
   "bi_test52" >:: bi_test52;
   "bi_test53" >:: bi_test53;
   "bi_test54" >:: bi_test54;
   "bi_test55" >:: bi_test55;
   "bi_test56" >:: bi_test56;
   "bi_test57" >:: bi_test57;
   "bi_test58" >:: bi_test58;
   "bi_test59" >:: bi_test59;
   "bi_test60" >:: bi_test60;
   "bi_test61" >:: bi_test61;
   "bi_test62" >:: bi_test62;
   "bi_test63" >:: bi_test63;
   "bi_test64" >:: bi_test64;
   "bi_test65" >:: bi_test65;
   "bi_test66" >:: bi_test66;
   "bi_test67" >:: bi_test67;
   "bi_test68" >:: bi_test68;
 ]

let () = run_test_tt_main suite

