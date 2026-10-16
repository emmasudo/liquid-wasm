open Liquid_wasm
open Types

(*udiv example
  (func $main
    i32.const 2
    i32.const 1
    call $udiv
    drop
  )
  (func $udiv (param i32) (param {v:i32 | v <> 0}) (result i32)
    local.get 0
    local.get 1
    i32.div_u
  )
*)

let main_type = { params = []; results = [] }
let udiv_type = { params = [ i32; nonzero_i32 ]; results = [ i32 ] }

let udiv =
  { ftype = 1l; locals = []; body = [ LocalGet 0l; LocalGet 1l; I32DivU ] }

let main =
  {
    ftype = 0l;
    locals = [];
    body = [ I32Const 2l; I32Const 1l; Call 1l; Drop ];
  }

let example = { types = [ main_type; udiv_type ]; funcs = [ main; udiv ] }

let () =
  Typing.check_module example;
  print_endline "module is well-typed"
