module type S = sig
  type _ t

  val (>>=): 'a t -> ('a -> 'b t) -> 'b t

  val return : 'a -> 'a t
end

module type EXTENSION = sig
  type _ t

  include S with type 'a t := 'a t
  
  val join : 'a t t -> 'a t
  
  val sequence : 'a t list -> 'a list t
end

module Extend (M : S) : (EXTENSION with type 'a t := 'a M.t) = struct
  include M

  let join mma = mma >>= fun ma -> ma

  let sequence lma =
    let f ma mla =
      mla >>= fun la ->
      ma >>= fun a ->
      return (a::la) in
        List.fold_right f lma (return [])
end

(* Functor comes for free *)

module type FS = sig
  type _ t
  val map : 'a t -> ('a -> 'b) -> 'b t
end

module Functor (M : S) : (FS with type 'a t := 'a M.t) = struct
  type 'a t = 'a M.t

  let map ma f =
    let open M in
      ma >>= fun a -> return (f a)
end
