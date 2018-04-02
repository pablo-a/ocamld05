type radar = float array * string
type neighboor = {radar : radar; distance : float}

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


(* Euclidean distance between two points defined by a N-dimensional vector. *)
let eu_dist x_radar y_radar =
    match (x_radar, y_radar) with
    | ((x, _), (y, _)) -> begin
        let dimensions = Array.length x in
        let rec sum total i =
        if i = 0 then total
        else
        let diff = (x.(i) -. y.(i)) in
        let square_diff = diff *. diff in
        sum (total +. square_diff) (i-1)
        in
        sum 0. (dimensions-1)
    end


(* function that find the closest neighboor and return the same label as *)
(* classification for the current radar. *)
let one_nn train_list to_guess =
    let rec walk_on_set train_list to_guess current_best_neighboor =
        match train_list with
        | [] -> current_best_neighboor
        | hd::tl -> begin
            let current_distance = eu_dist to_guess hd in
            if current_distance < current_best_neighboor.distance then
                walk_on_set tl to_guess {radar = hd; distance = current_distance}
            else
                walk_on_set tl to_guess current_best_neighboor
        end
    in
    (* loop on every answers we have, and give first elem by default as answer. *)
    let default_neighboor = {radar=(List.hd train_list); distance=10000.} in
    let closest_neighboor = walk_on_set train_list to_guess default_neighboor in

    (* decompose closest object to get the label associated. *)
    match closest_neighboor with
    | {radar; _} -> begin
        match radar with
        | (features, label) -> label
    end

(* main de test *)
let main () =
    let train_set = examples_of_file "ionosphere.train.csv" in
    let test_set = examples_of_file "ionosphere.test.csv" in

    match test_set with
    | [] -> ()
    | hd::tl -> begin
            match hd with
            | (features, label) ->
                print_endline "This radar is labelled as :";
                print_endline label;
                print_endline "The function classified it as :";
                print_endline (one_nn train_set hd)
        end


let () = main ()
