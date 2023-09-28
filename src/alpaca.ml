type methods = [ `Get | `Post | `Patch | `Delete | `Put ]
type request = { methods : methods }
type response = string
type handler = request -> response

let of_methods = function
  | `Get -> "GET"
  | `Post -> "POST"
  | `Patch -> "PATCH"
  | `Delete -> "DELETE"
  | `Put -> "PUT"

let to_methods = function
  | "GET" -> `Get
  | "POST" -> `Post
  | "PATCH" -> `Patch
  | "DELETE" -> `Delete
  | "PUT" -> `Put
  | _ -> failwith "Method is not supported"

type route_configs = { methods : methods list }
type configs = { port : int; routes : (string * handler * route_configs) list }

let route ?(methods = [ `Get ]) path handler = (path, handler, { methods })
let new_app ?(port = 8000) () = { port; routes = [] }
let router lroutes app = { app with routes = lroutes @ app.routes }

let response_of_string s =
  Eio.traceln "%s" s;
  "HTTP/1.0 200 OK\n\nHello World"

let parse_headers headers =
  let l = List.hd headers in
  let main_headers = String.split_on_char ' ' l in
  let meth = List.nth main_headers 0 |> to_methods in
  let path = List.nth main_headers 1 in
  let http_version = List.nth main_headers 2 in
  let headers = List.tl headers in
  (meth, path, http_version, headers)

let handle_client ~configs flow _addr =
  Eio.traceln "Server: got connection from client";
  let c = Cstruct.create 1024 in
  let i = Eio.Flow.single_read flow c in
  let str = Cstruct.sub c 0 i |> Cstruct.to_string in
  let parse = String.split_on_char '\n' str in
  List.iter (Eio.traceln "%s") parse;
  let meth, path, _http_version, _headers = parse_headers parse in
  let route =
    List.find_opt
      (fun (p, _, m) -> List.mem meth m.methods && p = path)
      configs.routes
  in
  match route with
  | Some (_, h, _) ->
      let res = Cstruct.of_string (h { methods = meth }) in
      Eio.Flow.write flow [ res ]
  | None ->
      let res = Cstruct.of_string "HTTP/1.0 200 OK\n\nNot found!" in
      Eio.Flow.write flow [ res ]

let run_server ~configs socket =
  Eio.Net.run_server socket (handle_client ~configs)
    ~on_error:(Eio.traceln "Error handling connection: %a" Fmt.exn)

let run configs =
  Eio_main.run (fun env ->
      let net = Eio.Stdenv.net env in
      let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, configs.port) in
      Eio.Switch.run @@ fun sw ->
      let server = Eio.Net.listen net ~sw ~reuse_addr:true ~backlog:5 addr in
      run_server ~configs server)
