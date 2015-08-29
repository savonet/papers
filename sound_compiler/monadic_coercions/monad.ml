module type M = sig
  type 'a t

  val r : 'a -> 'a t

  val b : ('a -> 'b t) -> 'a t -> 'b t
end

(** The monad. *)
module M : M = struct
  type 'a t = unit

  let r _ = ()

  let b _ _ = ()
end

(** Derived operations for the monad. *)
module M = struct
  include M

  let lift_fun f = b (fun x -> r (f x))
end

(** Stream. *)
module S = struct
  type t = int M.t

  let incr = M.lift_fun (fun x -> x + 1)
end

let app (f:int->int) (x:int) = f x

let s =
  (* We want to lift app. *)
  let app f x =
    (* Get rid of the argument x. *)
    let app f = app f x in
    
  in
  app S.incr 0

let () = ()






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
