(* Tests for midterm.ml *)

open OUnit2
open Midterm

let assert_raises_failure x =
  assert_bool ""
    (try 
       begin
          ignore (x ()); 
          false
       end
     with
       | Failure _ -> true
       | _ -> false)

let assert_raises_invalid_arg x =
  assert_bool ""
    (try 
       begin
          ignore (x ()); 
          false
       end
     with
       | Invalid_argument _ -> true
       | _ -> false)

(* Some sample trees. *)

let t1  = Node (2, 35, Leaf, Node (1, 42, Leaf, Leaf))
let t1' = Node (2, 42, Node (1, 35, Leaf, Leaf), Leaf)

let t2  = Node (2, 35, Node (1, 42, Leaf, Leaf), Leaf)
let t2' = Node (2, 42, Leaf, Node (1, 35, Leaf, Leaf))

let t3  = Node (3, 21, Leaf, Node (2, 35, Leaf, Node (1, 42, Leaf, Leaf)))
let t3' = Node (2, 35, Node (1, 21, Leaf, Leaf), Node (1, 42, Leaf, Leaf))

let t4  = Node (3, 21, Node (2, 35, Node (1, 42, Leaf, Leaf), Leaf), Leaf)
let t4' = Node (2, 35, Node (1, 42, Leaf, Leaf), Node (1, 21, Leaf, Leaf))

let tree0 = Leaf
let tree1 = Node (1, 6, Leaf, Leaf)
let tree2 = Node (2, 6, Node (1, 3, Leaf, Leaf), Leaf)
let tree3 =
  Node (2, 3, Node (1, 1, Leaf, Leaf), Node (1, 6, Leaf, Leaf))
let tree4 =
  Node (3, 3, Node (1, 1, Leaf, Leaf),
   Node (2, 6, Node (1, 4, Leaf, Leaf), Leaf))
let tree5 =
  Node (3, 3, Node (1, 1, Leaf, Leaf),
   Node (2, 5, Node (1, 4, Leaf, Leaf), Node (1, 6, Leaf, Leaf)))
let tree6 =
  Node (3, 5, Node (2, 3, Node (1, 1, Leaf, Leaf), Node (1, 4, Leaf, Leaf)),
   Node (2, 6, Leaf, Node (1, 10, Leaf, Leaf)))
let tree7 =
  Node (3, 5, Node (2, 3, Node (1, 1, Leaf, Leaf), Node (1, 4, Leaf, Leaf)),
   Node (2, 7, Node (1, 6, Leaf, Leaf), Node (1, 10, Leaf, Leaf)))
let tree8 =
  Node (4, 5, Node (2, 3, Node (1, 1, Leaf, Leaf), Node (1, 4, Leaf, Leaf)),
   Node (3, 7, Node (1, 6, Leaf, Leaf),
    Node (2, 10, Node (1, 8, Leaf, Leaf), Leaf)))
let tree9 =
  Node (4, 5, Node (2, 3, Node (1, 1, Leaf, Leaf), Node (1, 4, Leaf, Leaf)),
   Node (3, 7, Node (1, 6, Leaf, Leaf),
    Node (2, 9, Node (1, 8, Leaf, Leaf), Node (1, 10, Leaf, Leaf))))
let tree10 =
  Node (4, 5,
   Node (3, 3, Node (2, 1, Leaf, Node (1, 2, Leaf, Leaf)),
    Node (1, 4, Leaf, Leaf)),
   Node (3, 7, Node (1, 6, Leaf, Leaf),
    Node (2, 9, Node (1, 8, Leaf, Leaf), Node (1, 10, Leaf, Leaf))))

let all_tests = "all" >:::
[ 
  "Problem 2.1: group3 and avg3" >:: (fun c ->
      let rints = [19; 72; 89; 49; 38; 70; 84; 66; 25; 13; 
                   5; 74; 16; 47; 14; 63; 74; 20; 38; 96] in
        begin
          assert_equal (group3 []) [];
          assert_equal (group3 [1]) [];
          assert_equal (group3 [1; 2]) [];
          assert_equal (group3 [1; 2; 3]) [(1, 2, 3)];
          assert_equal (group3 [1; 2; 3; 4]) [(1, 2, 3); (2, 3, 4)];
          assert_equal (group3 [1; 2; 3; 4; 5]) [(1, 2, 3); (2, 3, 4); (3, 4, 5)];
          assert_equal (group3 rints) 
            [(19, 72, 89); (72, 89, 49); (89, 49, 38); (49, 38, 70); (38, 70, 84);
             (70, 84, 66); (84, 66, 25); (66, 25, 13); (25, 13, 5); (13, 5, 74);
             (5, 74, 16); (74, 16, 47); (16, 47, 14); (47, 14, 63); (14, 63, 74);
             (63, 74, 20); (74, 20, 38); (20, 38, 96)];

          assert_equal (avg3 []) [];
          assert_equal (avg3 [1]) [];
          assert_equal (avg3 [1; 2]) [];
          assert_equal (avg3 [1; 2; 3]) [2];
          assert_equal (avg3 [1; 2; 3; 4]) [2; 3];
          assert_equal (avg3 [1; 2; 3; 4; 5]) [2; 3; 4];
          assert_equal (avg3 rints) 
            [60; 70; 58; 52; 64; 73; 58; 34; 14; 30; 31; 45; 25; 41; 50; 52; 44; 51];
        end
  );

  "Problem 2.2: take" >:: (fun c ->
      assert_equal (take 0 []) ([], []);
      assert_equal (take 10 []) ([], []);
      assert_equal (take 3 [1;2;3;4;5;6;7;8]) ([1; 2; 3], [4; 5; 6; 7; 8]);
      assert_equal (take 0 [1;2;3;4;5;6;7;8]) ([], [1; 2; 3; 4; 5; 6; 7; 8]);
      assert_equal (take 100 [1;2;3;4;5;6;7;8]) ([1; 2; 3; 4; 5; 6; 7; 8], []);
      assert_raises_invalid_arg (fun _ -> take (-1) [1;2;3;4;5;6;7;8]);
  );

  "Problem 2.2: take2" >:: (fun c ->
     assert_equal (take2 3 [1;2;3;4;5;6;7;8;9;10]) 
       ([1;2;3], [4;5;6], [7;8;9;10]);
     assert_equal (take2 2 [1;2;3;4;5;6;7;8;9;10]) 
       ([1;2], [3;4], [5;6;7;8;9;10]);
     assert_equal (take2 1 [1;2;3;4;5;6;7;8;9;10]) 
       ([1], [2], [3;4;5;6;7;8;9;10]);
     assert_equal (take2 0 [1;2;3;4;5;6;7;8;9;10]) 
       ([], [], [1;2;3;4;5;6;7;8;9;10]);
     assert_raises_invalid_arg (fun _ -> take2 (-1) [1;2;3;4;5;6;7;8;9;10]);
     assert_equal (take2 100 [1;2;3;4;5;6;7;8;9;10]) 
       ([1;2;3;4;5;6;7;8;9;10], [], []);
     assert_equal (take2 8 [1;2;3;4;5;6;7;8;9;10]) 
       ([1;2;3;4;5;6;7;8], [9;10], []);
     assert_equal (take2 5 [1;2;3;4;5;6;7;8;9;10]) 
       ([1;2;3;4;5], [6;7;8;9;10], []);
  );

  "Problem 2.2: mergeN" >:: (fun c ->
     assert_equal (mergeN 1 [8;10;4;5;11;7;2;1;3;9;6])
       [8;10;4;5;7;11;1;2;3;9;6];
     assert_equal (mergeN 2 [8;10;4;5;7;11;1;2;3;9;6])
       [4;5;8;10;1;2;7;11;3;6;9];
     assert_equal (mergeN 4 [4;5;8;10;1;2;7;11;3;6;9])
       [1;2;4;5;7;8;10;11;3;6;9];
     assert_equal (mergeN 8 [1;2;4;5;7;8;10;11;3;6;9])
       [1;2;3;4;5;6;7;8;9;10;11];
     assert_equal (mergeN 1 []) [];
     assert_equal (mergeN 2 []) [];
     assert_equal (mergeN 4 []) [];
     assert_raises_invalid_arg (fun _ -> mergeN (-1) []);
     assert_raises_invalid_arg (fun _ -> mergeN 0 []);
  );

  "Problem 2.2: merge_sort" >:: (fun c ->
      assert_equal (merge_sort []) [];
      assert_equal (merge_sort [1]) [1];
      assert_equal (merge_sort [1;1]) [1;1];
      assert_equal (merge_sort [1;2]) [1;2];
      assert_equal (merge_sort [2;1]) [1;2];
      assert_equal (merge_sort [1;2;3;4;5]) [1;2;3;4;5];
      assert_equal (merge_sort [5;1;2;3;4]) [1;2;3;4;5];
      assert_equal (merge_sort [5;1;3;2;4]) [1;2;3;4;5];
      assert_equal (merge_sort [5;4;3;2;1]) [1;2;3;4;5];
      assert_equal (merge_sort [1;2;3;4;5;4;3;2;1]) [1;1;2;2;3;3;4;4;5];
      assert_equal (merge_sort [5;4;3;2;1;2;3;4;5]) [1;2;2;3;3;4;4;5;5];
      assert_equal (merge_sort [5;1;3;2;3;4;2;1;5]) [1;1;2;2;3;3;4;5;5];
      assert_equal (merge_sort [1;1;1;1;1;1;1]) [1;1;1;1;1;1;1];
  );

  "Problem 3.1: filteri and filteriv" >:: (fun c ->
      let every_nth n = filteri (fun i -> i mod n = 0) in
      let every_nth_and_even n lst = 
        filteriv (fun i v -> i mod n = 0 && v mod 2 = 0) lst in
      let ns = [38; 37; 51; 13; 12; 74; 35; 30; 12; 93; 
                14; 88; 62; 62; 80; 59; 78; 35; 70; 27] in
        begin
          assert_equal (every_nth 1 ns) ns;
          assert_equal (every_nth 2 ns) [38; 51; 12; 35; 12; 14; 62; 80; 78; 70];
          assert_equal (every_nth 3 ns) [38; 13; 35; 93; 62; 59; 70];
          assert_equal (every_nth 4 ns) [38; 12; 12; 62; 78];
          assert_equal (every_nth 5 ns) [38; 74; 14; 59];

          assert_equal (every_nth_and_even 1 ns) [38; 12; 74; 30; 12; 14; 88; 
                                                  62; 62; 80; 78; 70];
          assert_equal (every_nth_and_even 2 ns) [38; 12; 12; 14; 62; 80; 78; 70];
          assert_equal (every_nth_and_even 3 ns) [38; 62; 70];
          assert_equal (every_nth_and_even 4 ns) [38; 12; 12; 62; 78];
          assert_equal (every_nth_and_even 5 ns) [38; 74; 14];
        end
  );
  
  "Problem 3.2: bests_so_far" >:: (fun c ->
     let peaks = bests_so_far (>) in
     let valleys = bests_so_far (<) in
     let longest_strings_so_far =
         bests_so_far (fun s2 s1 -> String.length s2 > String.length s1) in
     let ns = [13; 75; 39; 18; 6; 8; 2; 38; 67; 37; 95; 68; 54; 1; 41; 64; 98; 7; 85] in
     let ss = ["to"; "be"; "or"; "not"; "to"; "be"; "that"; "is"; "the"; "question"] in
       begin
         assert_equal (peaks ns) [13; 75; 95; 98];
         assert_equal (valleys ns) [13; 6; 2; 1];
         assert_equal (longest_strings_so_far ss) ["to"; "not"; "that"; "question"];
         assert_raises_failure (fun _ -> longest_strings_so_far []);
       end
  );

  "Problem 3.3: continued fractions" >:: (fun c ->
     let check_within v v' tol = abs_float (v -. v') <= tol in
     let golden_ratio = (1.0 +. sqrt 5.0) /. 2.0 in
     let pi = 4.0 *. atan 1.0 in
     let e = exp 1.0 in
     let tol = 1.0e-8 in
       begin
         assert_bool "continued fractions, part 2: golden_ratio"
           (check_within 
              (golden_ratio_terms 0)
              1.0
              tol);
         assert_bool "continued fractions, part 2: golden_ratio"
           (check_within 
              (golden_ratio_terms 1)
              2.0
              tol);
         assert_bool "continued fractions, part 2: golden_ratio"
           (check_within 
              (golden_ratio_terms 2)
              1.5 
              tol);
         assert_bool "continued fractions, part 2: golden_ratio"
           (check_within 
              (golden_ratio_terms 5)
              1.625 
              tol);
         assert_bool "continued fractions, part 2: golden_ratio"
           (check_within 
              (golden_ratio_terms 10)
              1.6179775280898876
              tol);
         assert_bool "continued fractions, part 2: golden_ratio"
           (check_within 
              (golden_ratio_terms 40)
              golden_ratio
              tol);

         assert_bool "continued fractions, part 2: pi"
           (check_within 
              (pi_terms 0)
              0.0
              tol);
         assert_bool "continued fractions, part 2: pi"
           (check_within 
              (pi_terms 1)
              4.0
              tol);
         assert_bool "continued fractions, part 2: pi"
           (check_within 
              (pi_terms 2)
              3.0
              tol);
         assert_bool "continued fractions, part 2: pi"
           (check_within 
              (pi_terms 3)
              3.16666666666666696
              tol);
         assert_bool "continued fractions, part 2: pi"
           (check_within 
              (pi_terms 5)
              3.14234234234234222
              tol);
         assert_bool "continued fractions, part 2: pi"
           (check_within 
              (pi_terms 10)
              3.14159254044654
              tol);
         assert_bool "continued fractions, part 2: pi"
           (check_within 
              (pi_terms 25)
              pi
              tol);

         assert_bool "continued fractions, part 2: e"
           (check_within 
              (e_terms 0)
              2.0
              tol);
         assert_bool "continued fractions, part 2: e"
           (check_within 
              (e_terms 1)
              3.0
              tol);
         assert_bool "continued fractions, part 2: e"
           (check_within 
              (e_terms 2)
              2.66666666666666652
              tol);
         assert_bool "continued fractions, part 2: e"
           (check_within 
              (e_terms 3)
              2.72727272727272751
              tol);
         assert_bool "continued fractions, part 2: e"
           (check_within 
              (e_terms 5)
              2.7184466019417477
              tol);
         assert_bool "continued fractions, part 2: e"
           (check_within 
              (e_terms 10)
              2.71828182735187429
              tol);
         assert_bool "continued fractions, part 2: e"
           (check_within 
              (e_terms 20)
              e
              tol);

         
         assert_bool "continued fractions, part 3"
           (check_within 
              golden_ratio 
              (converge golden_ratio_terms 1.0e-12)
              tol);
         assert_bool "continued fractions, part 3"
           (check_within 
              pi
              (converge pi_terms 1.0e-12)
              tol);
         assert_bool "continued fractions, part 3"
           (check_within 
              e
              (converge e_terms 1.0e-12)
              tol);
       end
  );

  "Problem 4.1: search" >:: (fun c ->
    assert_bool "test0" (not (search 6 tree0));

    assert_bool "test1" (not (search 3 tree1));
    assert_bool "test1" (search 6 tree1);

    assert_bool "test2" (not (search 1 tree2));
    assert_bool "test2" (search 3 tree2);
    assert_bool "test2" (search 6 tree2);

    assert_bool "test3" (not (search 4 tree3));
    assert_bool "test3" (search 1 tree3);
    assert_bool "test3" (search 3 tree3);
    assert_bool "test3" (search 6 tree3);

    assert_bool "test4" (not (search 5 tree4));
    assert_bool "test4" (search 4 tree4);
    assert_bool "test4" (search 1 tree4);
    assert_bool "test4" (search 3 tree4);
    assert_bool "test4" (search 6 tree4);

    assert_bool "test5" (not (search 10 tree5));
    assert_bool "test5" (search 5 tree5);
    assert_bool "test5" (search 4 tree5);
    assert_bool "test5" (search 1 tree5);
    assert_bool "test5" (search 6 tree5);
    assert_bool "test5" (search 3 tree5);

    assert_bool "test6" (not (search 7 tree6));
    assert_bool "test6" (search 10 tree6);
    assert_bool "test6" (search 5 tree6);
    assert_bool "test6" (search 4 tree6);
    assert_bool "test6" (search 1 tree6);
    assert_bool "test6" (search 6 tree6);
    assert_bool "test6" (search 3 tree6);

    assert_bool "test7" (not (search 8 tree7));
    assert_bool "test7" (search 7 tree7);
    assert_bool "test7" (search 10 tree7);
    assert_bool "test7" (search 5 tree7);
    assert_bool "test7" (search 4 tree7);
    assert_bool "test7" (search 1 tree7);
    assert_bool "test7" (search 6 tree7);
    assert_bool "test7" (search 3 tree7);

    assert_bool "test8" (not (search 9 tree8));
    assert_bool "test8" (search 8 tree8);
    assert_bool "test8" (search 7 tree8);
    assert_bool "test8" (search 10 tree8);
    assert_bool "test8" (search 5 tree8);
    assert_bool "test8" (search 4 tree8);
    assert_bool "test8" (search 1 tree8);
    assert_bool "test8" (search 6 tree8);
    assert_bool "test8" (search 3 tree8);

    assert_bool "test9" (not (search 2 tree9));
    assert_bool "test9" (search 9 tree9);
    assert_bool "test9" (search 8 tree9);
    assert_bool "test9" (search 7 tree9);
    assert_bool "test9" (search 10 tree9);
    assert_bool "test9" (search 5 tree9);
    assert_bool "test9" (search 4 tree9);
    assert_bool "test9" (search 1 tree9);
    assert_bool "test9" (search 6 tree9);
    assert_bool "test9" (search 3 tree9);

    assert_bool "test10" (not (search 11 tree10));
    assert_bool "test10" (search 2 tree10);
    assert_bool "test10" (search 9 tree10);
    assert_bool "test10" (search 8 tree10);
    assert_bool "test10" (search 7 tree10);
    assert_bool "test10" (search 10 tree10);
    assert_bool "test10" (search 5 tree10);
    assert_bool "test10" (search 4 tree10);
    assert_bool "test10" (search 1 tree10);
    assert_bool "test10" (search 6 tree10);
    assert_bool "test10" (search 3 tree10);
  );

  "Problem 4.2: rotate" >:: (fun c -> 
    assert_equal (left_rotate t1) t1';
    assert_equal (right_rotate t2) t2';
    assert_equal (left_rotate t3) t3';
    assert_equal (right_rotate t4) t4';
  );

  "Problem 4.3: insert" >:: (fun c -> 
    assert_equal (insert 6 tree0) tree1;
    assert_equal (insert 3 tree1) tree2;
    assert_equal (insert 1 tree2) tree3;
    assert_equal (insert 4 tree3) tree4;
    assert_equal (insert 5 tree4) tree5;
    assert_equal (insert 10 tree5) tree6;
    assert_equal (insert 7 tree6) tree7;
    assert_equal (insert 8 tree7) tree8;
    assert_equal (insert 9 tree8) tree9;
    assert_equal (insert 2 tree9) tree10;
  );

]

let _ = run_test_tt_main all_tests

