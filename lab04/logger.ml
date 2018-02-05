module Logger_wrapper = struct
  include State.Make_state(struct type t = string list end)

  let log x = let open Monad in
    get >>= fun s -> set (x::s)
end
