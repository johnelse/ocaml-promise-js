type ('a, 'b) promise

type 'a resolve = 'a -> unit

type 'a reject = 'a -> unit

val is_supported : unit -> bool

val make : ('a resolve -> 'b reject -> unit) -> ('a, 'b) promise

val resolve : 'a -> ('a, 'b) promise

val reject : 'b -> ('a, 'b) promise

val catch : ('a, 'b) promise -> ('b -> 'a) -> ('a, 'b) promise

val then_1 : ('a, 'b) promise -> ('a -> 'c) -> ('c, 'b) promise

val then_2 : ('a, 'b) promise -> ('a -> 'c) -> ('b -> 'c)-> ('c, 'b) promise

val then_final : ('a, 'b) promise -> ('a -> unit) -> ('b -> unit)-> unit

val all : (('a, 'b) promise) array -> ('a array, 'b) promise

val race : (('a, 'b) promise) array -> ('a, 'b) promise
