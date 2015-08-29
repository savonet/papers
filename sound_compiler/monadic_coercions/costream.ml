type 'a costream = ('a -> unit) -> unit

(* return *)
let r : 'a -> 'a costream =
  fun x k -> k x

(* functor *)
let f : ('a -> 'b) -> 'a costream -> 'b costream =
  fun f c k -> c (fun x -> k (f x))

(* bind *)
let b : ('a -> 'b costream) -> 'a costream -> 'b costream =
  fun f c k -> c (fun x -> f x k)

(* strength *)
let s : ('a costream * 'b costream) -> ('a * 'b) costream =
  fun (c,d) k -> c (fun x -> d (fun y -> k (x,y)))
