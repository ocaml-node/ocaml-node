type env

module EnvStack = struct
  let stack: env list ref = ref []

  let get_env () =
    match !stack with
    | [] -> failwith "there is no active env"
    | env::_ -> env

  let push_env env = stack := env :: !stack

  let pop_env () =
    match !stack with
    | [] -> failwith "there is no active env"
    | _::tl -> stack := tl
  ;;

  Callback.register "ocaml_node_get_env" get_env
end

type ext = ..

module LowLevel = struct
  type value
  type ref

  module Type = struct
    type t =
      | Undefined
      | Null
      | Boolean
      | Number
      | String
      | Symbol
      | Object
      | Function
      | External

    let to_string = function
    | Undefined -> "undefined"
    | Null -> "null"
    | Boolean -> "external"
    | Number -> "number"
    | String -> "string"
    | Symbol -> "symbol"
    | Object -> "object"
    | Function -> "function"
    | External -> "external"
  end

  type fn = env -> value array -> value

  external typeof: env -> value -> Type.t = "ocaml_node_value_typeof"
  external create_string_utf8: env -> string -> value = "ocaml_node_create_string_utf8"
  external get_value_string_utf8: env -> value -> string = "ocaml_node_get_value_string_utf8"
  external set_property: env -> value -> value -> value -> unit = "ocaml_node_set_property"
  external get_property: env -> value -> value -> value = "ocaml_node_get_property"
  external create_object: env -> value = "ocaml_node_create_object"
  external run_script: env -> value -> value = "ocaml_node_run_script"
  external throw: env -> value -> unit = "ocaml_node_throw"
  (* external create_error: env -> value -> value = "ocaml_node_create_error" *)
  external create_type_error: env -> value -> value = "ocaml_node_create_type_error"
  external get_global: env -> value = "ocaml_node_get_global"
  external get_null: env -> value = "ocaml_node_get_null"
  external get_undefined: env -> value = "ocaml_node_get_undefined"
  external get_boolean: env -> bool -> value = "ocaml_node_get_boolean"
  external create_double: env -> float -> value = "ocaml_node_create_double"
  external get_value_double: env -> value -> float = "ocaml_node_get_value_double"
  external get_and_clear_last_exception: env -> value = "ocaml_node_get_and_clear_last_exception"
  external is_exception_pending: env -> bool = "ocaml_node_is_exception_pending"
  external get_value_bool: env -> value -> bool = "ocaml_node_get_value_bool"
  external create_int: env -> int -> value = "ocaml_node_create_int"
  external get_value_int: env -> value -> int = "ocaml_node_get_value_int"
  external create_reference: env -> value -> ref = "ocaml_node_create_reference"
  external get_reference_value: env -> ref -> value = "ocaml_node_get_reference_value"
  external call_function: env -> value -> value -> value array -> value = "ocaml_node_call_function"
  external create_function: env -> string -> fn -> value = "ocaml_node_create_function"
  external create_external: env -> ext -> value = "ocaml_node_create_external"
  external get_value_external: env -> value -> ext = "ocaml_node_get_value_external"
  external create_symbol: env -> value -> value = "ocaml_node_create_symbol"
  external is_array: env -> value -> int = "ocaml_node_is_array"
  external create_array: env -> value = "ocaml_node_create_array"
  external get_array_length: env -> int = "ocaml_node_get_array_length"
  external get_element: env -> value -> int -> value = "ocaml_node_get_element"
  external set_element: env -> value -> int -> value -> unit = "ocaml_node_set_element"

  let init_cb = ref None

  let init: env -> value -> value = fun env value ->
    EnvStack.push_env env;

    match !init_cb with
    | None -> value
    | Some fn -> fn value
  ;;

  Callback.register "ocaml_node_init" init
end

exception TypeError of string

let to_exn = function
| Some v -> v
| None -> raise (TypeError "Unexpected type")

module Value = struct
  type t = LowLevel.ref

  module Type = struct
    type t = [
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

  (* TODO avoid creating wrappers *)
  let wrap v =
    let env = EnvStack.get_env () in
    let wrapper = LowLevel.create_object env in
    let key = LowLevel.create_string_utf8 env "v" in

    LowLevel.set_property env wrapper key v;
    LowLevel.create_reference (EnvStack.get_env ()) wrapper

  let unwrap v =
    let env = EnvStack.get_env () in
    let key = LowLevel.create_string_utf8 env "v" in
    let wrapper = LowLevel.get_reference_value (EnvStack.get_env ()) v in
    LowLevel.get_property env wrapper key

  let typeof v =
    match LowLevel.typeof (EnvStack.get_env ()) (unwrap v) with
    | Undefined -> `Undefined
    | Null -> `Null
    | Boolean -> `Boolean
    | Number -> `Number
    | String -> `String
    | Symbol -> `Symbol
    | Object -> `Object
    | Function -> `Function
    | External -> `External


  let of_int v = wrap (LowLevel.create_int (EnvStack.get_env ()) v)
  let to_int v =
    let v = unwrap v in
    let env = EnvStack.get_env () in

    match LowLevel.typeof env v with
    | Number -> Some (LowLevel.get_value_int env v)
    | _ -> None

  let to_int_exn v = to_exn (to_int v)

  let of_float v = wrap (LowLevel.create_double (EnvStack.get_env ()) v)
  let to_float v =
    let v = unwrap v in
    let env = EnvStack.get_env () in

    match LowLevel.typeof env v with
    | Number -> Some (LowLevel.get_value_double env v)
    | _ -> None
  let to_float_exn v = to_exn (to_float v)

  let of_bool v = wrap (LowLevel.get_boolean (EnvStack.get_env ()) v)
  let to_bool v =
    let v = unwrap v in
    let env = EnvStack.get_env () in

    match LowLevel.typeof env v with
    | Boolean -> Some (LowLevel.get_value_bool env v)
    | _ -> None
  let to_bool_exn v = to_exn (to_bool v)

  let of_string v = wrap (LowLevel.create_string_utf8 (EnvStack.get_env ()) v)
  let to_string v =
    let v = unwrap v in
    let env = EnvStack.get_env () in

    match LowLevel.typeof env v with
    | String -> Some (LowLevel.get_value_string_utf8 env v)
    | _ -> None
  let to_string_exn v = to_exn (to_string v)

end

let global () = Value.wrap (LowLevel.get_global (EnvStack.get_env ()))
let undefined () = Value.wrap (LowLevel.get_undefined (EnvStack.get_env ()))
let null () = Value.wrap (LowLevel.get_null (EnvStack.get_env ()))
let true_ () = Value.wrap (LowLevel.get_boolean (EnvStack.get_env ()) true)
let false_ () = Value.wrap (LowLevel.get_boolean (EnvStack.get_env ()) false)

module Codec = struct
  type 'a t = (Value.t -> 'a option) * ('a -> Value.t) * (unit -> 'a) option

  let make ~decode ~encode = (decode, encode, None)
  let make_with_default ~decode ~encode ~default = (decode, encode, Some default)
  let get_default (type a) (codec: a t) =
    let (_, _, default) = codec in
    match default with
    | None -> None
    | Some f -> Some (f ())

  let encode (type a) (v: a) (codec: a t) = let (_, encode, _) = codec in encode v
  let decode (type a) (v: Value.t) (codec: a t) = let (decode, _, _) = codec in decode v
end

exception JsError of Value.t

let wrap_exn exn =
  let env = EnvStack.get_env () in
  match exn with
  | TypeError msg -> Value.wrap (LowLevel.(create_type_error env (create_string_utf8 env msg)))
  | JsError v -> v
  | _ -> Value.wrap (LowLevel.(create_type_error env (create_string_utf8 env "OCaml exception")))
;;

module Function = struct
  type t = Value.t

  let to_value v = v
  let of_value v =
    match LowLevel.typeof (EnvStack.get_env ()) (Value.unwrap v) with
    | Function -> Some v
    | _ -> None
  let of_value_exn v = to_exn (of_value v)

  let make fn =
    let env = EnvStack.get_env () in
    let cb env args =
      EnvStack.push_env env;
      let args = Array.to_list args
        |> List.tl (* TODO handle receiver *)
        |> List.map Value.wrap in

      let result = try fn args with
      | _ as exn -> (
        let v = wrap_exn exn in
        LowLevel.throw env (Value.unwrap v);
        undefined ()
      ) in
      let result = Value.unwrap result in
      EnvStack.pop_env ();
      result
    in

    Value.wrap (LowLevel.create_function env "TODO" cb)

  let call args fn =
    let env = EnvStack.get_env () in
    let args = args |> List.map (Value.unwrap) |> Array.of_list in

    let result = LowLevel.call_function env (LowLevel.get_undefined env) (Value.unwrap fn) args in

    if (LowLevel.is_exception_pending env) then begin
      let exn = LowLevel.get_and_clear_last_exception env in
      raise (JsError (Value.wrap exn))
    end;

    Value.wrap result
end

module Object = struct
  type t = Value.t

  let make () = Value.wrap (LowLevel.create_object (EnvStack.get_env ()))

  let to_value v = v
  let of_value v =
    match LowLevel.typeof (EnvStack.get_env ()) (Value.unwrap v) with
    | Object
    | Function -> Some v
    | _ -> None

  let of_value_exn v = to_exn (of_value v)

  let set k v obj =
    let obj' = Value.unwrap obj in
    let env = EnvStack.get_env () in
    LowLevel.set_property env obj' (LowLevel.create_string_utf8 env k) (Value.unwrap v);
    obj

  let set_f k v fn = set k v (Function.to_value fn)

  let get k obj =
    let obj = Value.unwrap obj in
    let env = EnvStack.get_env () in
    Value.wrap(LowLevel.get_property env obj (LowLevel.create_string_utf8 env k))
end

module Any = struct
  type t = [
    | `Boolean of bool
    | `Function of Function.t
    | `Null
    | `Number of float
    | `Object of Object.t
    | `String of string
    | `Undefined
    | `Unsupported
  ]

  let to_value = function
  | `Boolean v -> Value.of_bool v
  | `Function v -> Function.to_value v
  | `Null -> null ()
  | `Number v -> Value.of_float v
  | `Object obj -> Object.to_value obj
  | `String v -> Value.of_string v
  | `Undefined -> undefined ()
  | _ -> failwith "Unsupported type"

  let of_value v =
    match Value.typeof v with
    | `Undefined -> `Undefined
    | `Null -> `Null
    | `Boolean -> `Boolean (Value.to_bool_exn v)
    | `Number -> `Number (Value.to_float_exn v)
    | `String -> `String (Value.to_string_exn v)
    | `Object -> `Object (Object.of_value_exn v)
    | `Function -> `Function (Function.of_value_exn v)
    | _ -> `Unsupported
end

module External = struct
  type t = ext

  let to_value v =
    Value.wrap (LowLevel.create_external (EnvStack.get_env ()) v)
    let of_value v =
      let env = EnvStack.get_env () in
      let v = Value.unwrap v in
      match LowLevel.typeof env v with
      | External -> Some (LowLevel.get_value_external env v)
      | _ -> None
    let of_value_exn v = to_exn (of_value v)

end

module Symbol = struct
  type t = Value.t
  let make desc =
    let env = EnvStack.get_env () in
    let desc = LowLevel.create_string_utf8 env desc in
    Value.wrap (LowLevel.create_symbol env desc)

  let to_value v = v
  let of_value v =
    match LowLevel.typeof (EnvStack.get_env ()) (Value.unwrap v) with
    | Symbol -> Some v
    | _ -> None

  let of_value_exn v = to_exn (of_value v)
end

module Signature = struct
    type _ t =
      | Ret: 'a Codec.t -> 'a t
      | Cons: 'b Codec.t * 'a t -> ('b -> 'a) t

    let (@@) a b = Cons(a, b)
    let (@>) a b  = Cons(a, (Ret b))

    let unit = Codec.make_with_default
      ~decode: (fun v ->
        match Value.typeof v with
        | `Undefined
        | `Null -> Some ()
        | _ -> None
      )
      ~encode: (fun _ -> undefined ())
      ~default: (fun () -> ())

    (* TODO handle size mismatch *)
    let rec apply : type a. (a t) -> a -> Value.t list -> Value.t = fun sign fn vals -> (
      match sign with
      | Ret codec -> Codec.encode fn codec
      | Cons (codec, tl) -> (
        match vals with
        | [] -> (match Codec.get_default codec with
          | None -> raise (TypeError "More arguments expected")
          | Some v -> apply tl (fn v) vals
        )
        | v::vals -> (
          let decoded = Codec.decode v codec in
          match decoded with
          | None -> raise (TypeError "Unepected argument")
          | Some v -> apply tl (fn v) vals
        )
      )
    )

    let float = Codec.make ~decode: Value.to_float ~encode: Value.of_float
    let int = Codec.make ~decode: Value.to_int ~encode: Value.of_int
    let string = Codec.make ~decode: Value.to_string ~encode: Value.of_string
    let bool = Codec.make ~decode: Value.to_bool ~encode: Value.of_bool
    let func = Codec.make ~decode: Function.of_value ~encode: Function.to_value
    let obj = Codec.make ~decode: Object.of_value ~encode: Object.to_value
    let value = Codec.make ~decode: (fun (v: Value.t) -> Some v) ~encode: (fun (v: Value.t) -> v)
    let any = Codec.make ~decode: (fun v -> Some (Any.of_value v)) ~encode: Any.to_value
    let ext = Codec.make ~decode: External.of_value ~encode: External.to_value
    let symbol = Codec.make ~decode: Symbol.of_value ~encode: Symbol.to_value

    let with_default codec default = Codec.make_with_default
      ~decode: (fun v ->
        let r = Codec.decode v codec in

        match r with
        | Some _ -> r
        | None -> (
          match Value.typeof v with
          | `Null
          | `Undefined -> Some default
          | _ -> None
        )
      )
      ~encode: (fun v -> Codec.encode v codec)
      ~default: (fun () -> default)

    let wrap sign fn = Function.make (fun args ->
      apply sign fn args
    )
  end


let register fn =
  LowLevel.init_cb := Some (fun value -> Value.(unwrap (fn (wrap value))))


let run script =
  let env = EnvStack.get_env () in
  Value.wrap (LowLevel.run_script env (LowLevel.create_string_utf8 env script))
