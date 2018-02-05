module Monad_instance : (Monad_class.S with type 'a t = 'a list) = struct
  type 'a t = 'a list
  
  let return x = [x]

  let (>>=) xs f = List.map f xs |>  List.flatten
end

module Monad = Monad_class.Extend(Monad_instance)

module Functor = Monad_class.Functor(Monad_instance)
