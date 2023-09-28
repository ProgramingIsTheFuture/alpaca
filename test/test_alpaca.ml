open Alpaca

let handle_home _ = response_of_string "Hello HOME page"
let () = new_app () |> router [ route "/" handle_home ] |> run
