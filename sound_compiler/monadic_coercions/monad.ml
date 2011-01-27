module type M = sig
  type 'a t

  val r : 'a -> 'a t

  val b : ('a -> 'b t) -> 'a t -> 'b t
end

module M : M = struct
  type 'a t = unit

  let r _ = ()

  let b _ _ = ()
end

type 'a t = 'a M.t
let b = M.b
let r = M.r
(** Strength. *)
(* let s : 'a t * 'b t -> ('a * 'b) t = fun (x, y) -> Obj.magic () *)
let id x = x
let eta = r
let mu : 'a t t -> 'a t = fun x -> b id x

let ( * ) g f = fun x -> g (f x)

let lift_fun f = M.b (fun x -> (M.r (f x)))

let lift_fun f = M.b (M.r * f)

let lift_fun f x = M.b (fun x' -> M.r (f x')) x

let lift_fun' f = M.b f

let lift_fun2 f x y = M.b (fun x' -> M.b (fun y' -> M.r (f x' y')) y) x

let lift_fun2 f x y = M.b (fun x' -> M.b (M.r * (f x')) y) x

let lift_fun3 f x y z = M.b (fun x' -> M.b (fun y' -> M.b (fun z' -> M.r (f x' y' z')) z) y) x

let lift_fun3'' f x y z = M.b (fun x -> f x y z) x

(** Dummy implementation of feedback. *)
let feedback : ('a -> 'a t) -> 'a -> 'a t = fun _ x -> r x

let dt = 1

(** Derivation when s is static. *)
let derivate s =
  let d x y = (y - x) / dt in
  let f (x',d') =
    let x = s in
    r (x, d x' x)
  in
  b (fun (x,y) -> r y) (feedback f (0,0))

(** Derivation. *)
let derivate (s : int t) : int t =
  let d x y = (y - x) / dt in
  let d : int t -> int t -> int t = lift_fun2 d in
  let f (x',d') : (int * int) -> (int * int) t  =
    let x = s in
    r (x, d x' x)
  in
  (fun (x,y) -> r y) (feedback f (r 0, r 0))
