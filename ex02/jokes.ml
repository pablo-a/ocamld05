let init_joke_array () =
    let arr = Array.make 5 "0" in
    arr.(0) <- "There are 3 types of people in this world: Those that are good at Math and those that are not.\n";
    arr.(1) <- "Two cows are standing in a field.\nOne cow says \"MOOOOO!\"\nThe other cow says pretty much the same thing.\n";
    arr.(2) <- "Why didn’t the astronaut come home to his wife?\nHe needed his space.\n";
    arr.(3) <- "I got fired from my job at the bank today.\nAn old lady came in and asked me to check her balance, so I pushed her over.\n";
    arr.(4) <- "What did Batman say to Robin before they got in the car?\n“Robin, get in the car.”\n";
    arr

let main () =
    Random.self_init ();
    let some_jokes = init_joke_array () in
    let len = Array.length some_jokes in
    let pick_one_joke = Random.int len in
    print_string (some_jokes.(pick_one_joke))

let () = main ()
