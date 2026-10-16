open Types

exception Invalid of string

type context = {
  types : functype list;
  funcs : functype list;
  locals : valtype list;
  labels : resulttype list;
  return : resulttype option;
}

let empty = { types = []; funcs = []; locals = []; labels = []; return = None }

let lookup what (xs : 'a list) (i : int32) : 'a =
  match List.nth_opt xs (Int32.to_int i) with
  | Some x -> x
  | None -> raise (Invalid (Printf.sprintf "unknown %s %ld" what i))

let context_of_module (m : module_) : context =
  {
    empty with
    types = m.types;
    funcs = List.map (fun (f : func) -> lookup "type" m.types f.ftype) m.funcs;
  }

let context_of_func (c : context) (f : func) : context =
  let ft = lookup "type" c.types f.ftype in
  let zero (b : numtype) = { base = b; refinement = Equal 0l } in
  {
    c with
    locals = ft.params @ List.map zero f.locals;
    labels = [ ft.results ];
    return = Some ft.results;
  }

(* TODO: MAKE SURE TO ADJUST DROP WHEN SUPPORTING MORE TYPES *)
let type_of_instr (c : context) (i : instr) : functype =
  match i with
  | I32Const n ->
      { params = []; results = [ { base = I32; refinement = Equal n } ] }
  | LocalGet x -> { params = []; results = [ lookup "local" c.locals x ] }
  | I32DivU -> { params = [ i32; nonzero_i32 ]; results = [ i32 ] }
  | Call x -> lookup "function" c.funcs x
  | Drop -> { params = [ i32 ]; results = [] }

let string_of_valtype (t : valtype) : string =
  let base = match t.base with I32 -> "i32" in
  match t.refinement with
  | True -> base
  | Equal n -> Printf.sprintf "{v:%s | v = %ld}" base n
  | NotEqual n -> Printf.sprintf "{v:%s | v <> %ld}" base n

let string_of_instr_type (t : functype) : string =
  let rt ts = "[" ^ String.concat " " (List.map string_of_valtype ts) ^ "]" in
  rt t.params ^ " -> " ^ rt t.results

let implies (p : predicate) (q : predicate) : bool =
  match (p, q) with
  | _, True -> true
  | Equal n, Equal m -> n = m
  | Equal n, NotEqual m -> n <> m
  | NotEqual n, NotEqual m -> n = m
  | (True | NotEqual _), Equal _ -> false
  | True, NotEqual _ -> false

let subtype (s : valtype) (t : valtype) : bool =
  s.base = t.base && implies s.refinement t.refinement

let check_sub (s : valtype) (t : valtype) : unit =
  if not (subtype s t) then
    raise
      (Invalid
         (Printf.sprintf "type mismatch: %s is not a subtype of %s"
            (string_of_valtype s) (string_of_valtype t)))

let check_instr (c : context) (stack : valtype list) (i : instr) : valtype list
    =
  let t = type_of_instr c i in
  let rec pop expected stack =
    match (expected, stack) with
    | [], rest -> rest
    | e :: es, s :: ss ->
        check_sub s e;
        pop es ss
    | _ :: _, [] -> raise (Invalid "operand stack underflow")
  in
  List.rev_append t.results (pop (List.rev t.params) stack)

let check_func (c : context) (f : func) : unit =
  let c = context_of_func c f in
  let results = Option.get c.return in
  let stack = List.fold_left (check_instr c) [] f.body in
  let final = List.rev stack in
  if List.length final <> List.length results then
    raise
      (Invalid
         (Printf.sprintf "expected %d result(s), body leaves %d"
            (List.length results) (List.length final)));
  List.iter2 check_sub final results

let check_module (m : module_) : unit =
  let c = context_of_module m in
  List.iter (check_func c) m.funcs
