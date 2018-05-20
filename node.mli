
module Value: sig
  type t

  module Type: sig
    type t = private [>
    | `Boolean
    | `External
    | `Function
    | `Null
    | `Number
    | `Object
    | `String
    | `Symbol
    | `Undefined
    ]
  end

  val typeof: t -> Type.t

  val of_int: int -> t
  val to_int: t -> int option
  val to_int_exn: t -> int

  val of_float: float -> t
  val to_float: t -> float option
  val to_float_exn: t -> float

  val of_string: string -> t
  val to_string: t -> string option
  val to_string_exn: t -> string

  val of_bool: bool -> t
  val to_bool: t -> bool option
  val to_bool_exn: t -> bool
end

module Codec: sig
  type 'a t
  val make: decode: (Value.t -> 'a option) -> encode: ('a -> Value.t) -> 'a t
  val encode: 'a -> 'a t -> Value.t
  val decode: Value.t -> 'a t -> 'a option
end

type ext = ..

module Function: sig
  type t

  val to_value: t -> Value.t
  val of_value: Value.t -> t option
  val of_value_exn: Value.t -> t
  val make: (Value.t list -> Value.t) -> t
  val call: Value.t list -> t -> Value.t
end

module Object: sig
  type t
  val make: unit -> t
  val of_value: Value.t -> t option
  val of_value_exn: Value.t -> t
  val to_value: t -> Value.t

  val set: string -> Value.t -> t -> t
  val set_f: string -> Function.t -> t -> t

  val get: string -> t -> Value.t
end

module Any: sig
  type t = private [>
    | `Boolean of bool
    | `Function of Function.t
    | `Null
    | `Number of float
    | `Object of Object.t
    | `String of string
    | `Undefined
  ]

  val to_value: t -> Value.t
  val of_value: Value.t -> t
end

module External: sig
  type t = ext

  val to_value: t -> Value.t
  val of_value: Value.t -> t option
  val of_value_exn: Value.t -> t
end

module Symbol: sig
  type t
  val make: string -> t
  val to_value: t -> Value.t
  val of_value: Value.t -> t option
  val of_value_exn: Value.t -> t
end

module Signature: sig
  type 'a t
  val (@@): 'b Codec.t -> 'a t -> ('b -> 'a) t
  val (@>): 'b Codec.t -> 'a Codec.t -> ('b -> 'a) t

  val float: float Codec.t
  val unit: unit Codec.t
  val int: int Codec.t
  val string: string Codec.t
  val bool: bool Codec.t
  val func: Function.t Codec.t
  val obj: Object.t Codec.t
  val value: Value.t Codec.t
  val any: Any.t Codec.t
  val ext: External.t Codec.t
  val symbol: Symbol.t Codec.t
  val with_default: 'a Codec.t -> 'a -> 'a Codec.t

  val wrap: 'a t -> 'a -> Function.t
end

val register: (Value.t -> Value.t) -> unit
val global: unit -> Value.t
val undefined: unit -> Value.t
val null: unit -> Value.t
val true_: unit -> Value.t
val false_: unit -> Value.t
val run: string -> Value.t

exception TypeError of string
exception JsError of Value.t