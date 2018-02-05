module type S = sig
  type _ t
  type state

  module Monad_instance : Monad_class.S with type 'a t = 'a t

  module Monad: module type of Monad_class.Extend(Monad_instance)

  module Functor: module type of Monad_class.Functor(Monad_instance)

  val run : 'a t -> state -> 'a * state

  val get : state t

  val set : state -> unit t
end

module Make_state (ST : sig type t end) : (S with type state = ST.t) = struct
  type state = ST.t

  module Run = struct
    type 'a t = state -> 'a * state
  end

  type 'a t = 'a Run.t

  let run ma s = ma s

  let get = fun s -> (s, s)

  let set s = fun _ -> ((), s)

  module Monad_instance = struct
      type 'a t = 'a Run.t

      let return a = fun s -> (a, s)

      let (>>=) ma f =
        fun s -> let (a, ns) = ma s in f a ns
  end

  module Monad = Monad_class.Extend(Monad_instance)

  module Functor = Monad_class.Functor(Monad_instance)
end

