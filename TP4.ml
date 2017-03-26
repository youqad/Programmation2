type 'a pred = ' a -> bool

let f p q = if p true = q true && p false = q false then true else false

let _ = f (function x -> true) (function x -> true)


type stream = int -> bool

let true_cst stream = true

let false_cst stream = false

let pred_13 stream = if stream 13 then true else false

exception Gotcha

let tester_pred pred =
  let stream_trap i = raise Gotcha in
  try
    pred stream_trap; true
  with Gotcha -> false

let _ = tester_pred true_cst

let _ = tester_pred pred_13


exception Got of int


let tester_fst_access pred =
  try
    let _ = pred (fun i -> raise (Got i)) in
    -1
  with Got(i) -> i

let _ = tester_fst_access true_cst

let _ = tester_fst_access pred_13

let undef_everywhere_stream i = raise (Got i)

let rec is_pred_true_on_all_ext partial_stream pred =
  try
    pred partial_stream
  with Got i -> is_pred_true_on_all_ext (fun j -> if i=j then true else partial_stream j) pred
         &&
         is_pred_true_on_all_ext (fun j -> if i=j then false else partial_stream j) pred


let is_pred_true pred =
    is_pred_true_on_all_ext
      undef_everywhere_stream
      pred

let busy_pred stream =
  let x = ref 0 in (
  for i=0 to 16 do
    x:= !x+2+if stream i then 1 else 0;
  done;
  stream !x || not (stream !x) )
