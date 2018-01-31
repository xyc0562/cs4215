module Monad_instance : (Monad_class.S with type 'a t = 'a option) = struct

  type 'a t = 'a option

  let (>>=) ma f = match ma with
    | None -> None
    | Some a -> f a

  let return a = Some a
end

module Monad = Monad_class.Extend(Monad_instance)
