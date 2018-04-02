type 'a ft_ref = {mutable data: 'a }

let return x =
    { data = x }

let get x =
    x.data

let set x y =
    x.data <- y

let bind x f =
    f x.data

let main () =
    let test = return 42 in
    print_int (get test);
    print_char '\n';
    set test 43;
    print_int (get test);
    print_char '\n';
    let new_test = bind test (fun x -> {data= (float_of_int x) /. 2.}) in
    print_float (get new_test)

let () = main ()
