let eu_dist x y =
    let dimensions = Array.length x in
    let rec sum total i =
        if i = 0 then total
        else
            let diff = (x.(i) -. y.(i)) in
            let square_diff = diff *. diff in
            sum (total +. square_diff) (i-1)
    in
    sum 0. (dimensions-1)

let main () =
    let a = Array.make 5 0. in
    let b = Array.make 5 1. in
    let x = Array.make 10 0. in
    let y = Array.make 10 2. in
    print_float (eu_dist a b);
    print_char '\n';
    print_float (eu_dist x y);
    print_char '\n'

let () = main ()
