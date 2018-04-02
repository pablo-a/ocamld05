(* our strsplit function *)
let str_split str c =
    "asd"

(* prend une ligne en parametre et la parse pour renvoyer => (float Array, string) *)
let parse_line str =
    let splitted = String.split_on_char 'c' str in
    let lst_len = List.length splitted in
    let rec aggregate lst array_to_fill index =
        match lst with
        | [] -> array_to_fill
        | [last] -> array_to_fill
        | hd::tl -> begin
            array_to_fill.(index) <- float_of_string hd;
            aggregate tl array_to_fill (index+1)
        end
    in
    let float_array = aggregate splitted (Array.make (lst_len-1) 0.0) 0 in
    let class_char = List.nth splitted (lst_len-1) in
    (float_array, class_char)

(* read chaque ligne et renvoie une liste de lignes strucurees. *)
let examples_of_file file_path =
    let fd = open_in file_path in
    let rec read_lines fd =
        try
            let line = input_line fd in
            let set = parse_line line in
            set :: read_lines fd
        with
        | End_of_file -> []
        | _ -> failwith "Could not read from file."
    in
    read_lines fd


(* debug function *)
let print_float_array arr =
    let len = Array.length arr in
    let rec walk_array index =
        match index with
        | y when y = len -> ()
        | _ -> print_float (arr.(index)); print_string ", "; walk_array (index+1)

    in
    walk_array 0

(* debug function *)
let rec print_lst = function
    | [] -> ()
    | hd::tl -> begin
        match hd with
        | (float_array, class_model) -> print_float_array float_array; print_endline class_model; print_lst tl
    end

(* main de test *)
let main () =
    let lst = examples_of_file "ionosphere.train.csv" in
    print_lst lst

let () = main ()
