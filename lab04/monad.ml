module Monad_class = struct
  module type S = sig
    type _ t

    val (>>=): 'a t -> ('a -> 'b t) -> 'b t

    val return : 'a -> 'a t
  end
end

module List = struct
  module Monad_instance : (Monad_class.S with type 'a t = 'a list) = struct
    type 'a t = 'a list
    
    let return x = [x]

    let (>>=) ma f = List.map f ma |> List.flatten
  end
end

module Option = struct
  module Monad_instance : (Monad_class.S with type 'a t = 'a option) = struct
    type 'a t = 'a option

    let return x = Some x

    let (>>=) ma f = match ma with
      | None -> None
      | Some x -> f x
  end
end
