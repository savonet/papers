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

let ( * ) g f = fun x -> g (f x)

let lift_fun f = M.b (fun x -> (M.r (f x)))

let lift_fun f = M.b (M.r * f)

let lift_fun f x = M.b (fun x' -> M.r (f x')) x

let lift_fun' f = M.b f

let lift_fun2 f x y = M.b (fun x' -> M.b (fun y' -> M.r (f x' y')) y) x

let lift_fun2 f x y = M.b (fun x' -> M.b (M.r * (f x')) y) x

let lift_fun3 f x y z = M.b (fun x' -> M.b (fun y' -> M.b (fun z' -> M.r (f x' y' z')) z) y) x

let lift_fun3'' f x y z = M.b (fun x -> f x y z) x
