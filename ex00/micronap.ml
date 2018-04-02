let my_sleep () = Unix.sleep 1

let main () =
    let nb = read_int_opt () in
    match nb with
    | Some(y) when y < 1 -> ()
    | None -> print_endline "Not a number"
    | Some(y) when y > 0 ->
            (for i = 1 to y do
                my_sleep ();
            done)
    | _ -> ()

(* compile with ocaml unix.cma micronap.ml *)
let () = main ()
