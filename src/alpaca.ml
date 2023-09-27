type request
type response
type handler = request -> response
type methods = [ `Get | `Post | `Patch | `Delete | `Put ]
type route_configs = { methods : methods list }

type configs = {
  port : int;
  routes : (string * handler * route_configs option) list;
}

let route ?(methods = [ `Get ]) path handler = (path, handler, { methods })
let new_app ?(port = 8000) () = { port; routes = [] }
let router lroutes app = { app with routes = lroutes @ app.routes }
let run _configs = Eio_main.run (fun _env -> assert false)
