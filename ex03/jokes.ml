let print_random_joke joke_array =
    let len = Array.length joke_array in
    let nb = Random.int len in
    print_endline ((joke_array).(nb))

let main file_name =
    let joke_array = (ref (Array.make 0 "")) in
    begin try
        let fd = open_in file_name in
        while true do
            let joke = input_line fd in
            let fake_arr = Array.make 1 joke in
            joke_array := (Array.append !joke_array fake_arr);
        done
    with
    | End_of_file -> print_random_joke !joke_array
    | _ -> failwith "Some error occured while reading the input file"
    end


let () =
    match (Array.length Sys.argv) with
    | 2 -> Random.self_init (); main Sys.argv.(1)
    | _ -> print_endline "provide a command line argument : text file containing jokes"
