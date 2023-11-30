open OUnit2

let rec last = function
    | [] -> None
    | [x] -> Some x
    | _ :: tl -> last tl
;;

let rec last_two = function
    | [] | [_]-> None
    | [x; y] -> Some (x, y)
    | _ :: tl -> last_two tl
;;

let rec at k = function 
    | [] -> None
    | hd :: tl -> if k = 1 then Some hd else at (k - 1) tl
;;

let length list =
    let rec aux c = function
            | [] -> c
            | _ :: tl -> aux (c + 1) tl
    in
    aux 0 list
;;

let reverse list = 
    let rec aux acc = function
        | [] -> acc
        | hd :: tl -> aux (hd :: acc) tl
    in
    aux [] list
;;

let is_palindrome list = 
    list = reverse list
;;

let test = "OCaml Problems Suite" >::: [
    "last empty" >:: (fun _ -> assert_equal None (last []));
    "last some" >:: (fun _ -> assert_equal (Some "d") (last ["a";"b";"c";"d";]));
    "last two empty" >:: (fun _ -> assert_equal None (last_two []));
    "last two some" >:: (fun _ -> assert_equal (Some ("c", "d")) (last_two ["a";"b";"c";"d";]));
    "at too small" >:: (fun _ -> assert_equal None (at 3 ["a"]));
    "at some" >:: (fun _ -> assert_equal (Some "c") (at 3 ["a";"b";"c";"d";]));
    "length empty" >:: (fun _ -> assert_equal 0 (length []));
    "length some" >:: (fun _ -> assert_equal 3 (length ["a";"b";"c";]));
    "reverse some" >:: (fun _ -> assert_equal ["c";"b";"a"] (reverse ["a";"b";"c";]));
    "is_palindrome empty" >:: (fun _ -> assert_equal true (is_palindrome []));
    "is_palindrome some" >:: (fun _ -> assert_equal true (is_palindrome ["x";"a";"m";"a";"x";]));
];;


let () = 
    run_test_tt_main test;;
    ()
;;
