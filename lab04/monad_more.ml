module Monad_class = struct
  module type S = sig
    type _ t

    val (>>=): 'a t -> ('a -> 'b t) -> 'b t

    val return : 'a -> 'a t
  end

  module type EXTENSION = sig
    type _ t

    include S with type 'a t := 'a t

    val map : 'a t -> ('a -> 'b) -> 'b t

    val join : 'a t t -> 'a t

    val sequence : 'a t list -> 'a list t
  end

  module Extend (M : S) : (EXTENSION with type 'a t = 'a M.t) = struct
    include M

    let map ma f =
      ma >>= fun x -> f x |> return

    let join mma =
      mma >>= fun ma -> ma

    let sequence lma =
      let f ma ml =
        ma >>= fun x ->
        ml >>= fun l -> 
        return (x::l)
      in List.fold_right f lma (return [])
  end
end

module List = struct
  module Monad_instance : (Monad_class.S with type 'a t = 'a list) = struct
    type 'a t = 'a list
    
    let return x = [x]

    let (>>=) ma f = List.map f ma |> List.flatten
  end

  module Monad = Monad_class.Extend(Monad_instance)
end

module Option = struct
  module Monad_instance : (Monad_class.S with type 'a t = 'a option) = struct
    type 'a t = 'a option

    let return x = Some x

    let (>>=) ma f = match ma with
      | None -> None
      | Some x -> f x
  end

  module Monad = Monad_class.Extend(Monad_instance)
end
