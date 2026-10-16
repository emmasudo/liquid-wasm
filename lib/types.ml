type typeidx = int32
type funcidx = int32
type localidx = int32
type labelidx = int32

type instr =
  | I32Const of int32
  | LocalGet of localidx
  | I32DivU
  | Call of funcidx
  | Drop

type expr = instr list
type numtype = I32
type predicate = True | NotEqual of int32 | Equal of int32
type valtype = { base : numtype; refinement : predicate }

let i32 = { base = I32; refinement = True }
let nonzero_i32 = { base = I32; refinement = NotEqual 0l }

type resulttype = valtype list
type functype = { params : resulttype; results : resulttype }
type func = { ftype : typeidx; locals : numtype list; body : expr }
type module_ = { types : functype list; funcs : func list }
