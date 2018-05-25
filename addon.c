#include "node_api.h"
#include <caml/callback.h>
#include <caml/alloc.h>
#include <stdio.h>
#include <caml/custom.h>
#include <caml/memory.h>

#define DEFAULT_OPS(name) static struct custom_operations name ## _ops =   \
{                                                                          \
  (char *)#name,                                                           \
  custom_finalize_default,                                                 \
  custom_compare_default,                                                  \
  custom_hash_default,                                                     \
  custom_serialize_default,                                                \
  custom_deserialize_default,                                              \
  custom_compare_ext_default                                               \
};

DEFAULT_OPS(napi_value)

#define Napi_value_val(v) (*((napi_value *) Data_custom_val(v)))

static value alloc_napi_value(napi_value napi_val) {
  value val = caml_alloc_custom(&napi_value_ops, sizeof (napi_value), 0, 1);
  Napi_value_val(val) = napi_val;
  return val;
}

#define Napi_env_val(v) (*((napi_env *) Data_custom_val(v)))

DEFAULT_OPS(napi_env)

#define Napi_ref_val(v) (*((napi_ref *) Data_custom_val(v)))

napi_env get_current_env() {
  static value* fn = NULL;

  if (fn == NULL) {
    fn = caml_named_value("ocaml_node_get_env");
  }
  
  return Napi_env_val(caml_callback(*fn, Val_unit));
}

static void finalize_ref(value val) {
  // napi_delete_reference(napi_env env, napi_ref ref);
  // TODO where do I get an env?
  // TODO call a callback into OCaml
}

static struct custom_operations napi_ref_ops =
{
  (char *)"napi_ref",
  finalize_ref,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default
};


static value alloc_napi_env(napi_env env) {
  value val = caml_alloc_custom(&napi_env_ops, sizeof (napi_env), 0, 1);
  Napi_env_val(val) = env;
  return val;
}

static value alloc_napi_ref(napi_ref ref) {
  value val = caml_alloc_custom(&napi_ref_ops, sizeof (napi_ref), 0, 1);
  Napi_ref_val(val) = ref;
  return val;
}

value ocaml_node_value_typeof(value env, value val) {
  CAMLparam2(env, val);
  napi_valuetype res;

  napi_typeof(Napi_env_val(env), Napi_value_val(val), &res);
  // TODO check status

  CAMLreturn(Val_int(res));
}

value ocaml_node_create_object(value env) {
  CAMLparam1(env);

  napi_value result;

  napi_create_object(Napi_env_val(env), &result);
  // TODO check status

  CAMLreturn(alloc_napi_value(result));
}

value ocaml_node_create_string_utf8(value env, value val) {
  CAMLparam2(env, val);

  napi_value result;

  napi_create_string_utf8(Napi_env_val(env),
                          String_val(val),
                          NAPI_AUTO_LENGTH,
                          &result);
                          

  // TODO check status

  CAMLreturn(alloc_napi_value(result));
}

value ocaml_node_get_value_string_utf8(value env, value val) {
  CAMLparam2(env, val);
  CAMLlocal1(r);

  size_t size;
  napi_get_value_string_utf8(Napi_env_val(env),
                             Napi_value_val(val),
                             NULL,
                             0,
                             &size);

  r = caml_alloc_string(size);

  napi_get_value_string_utf8(Napi_env_val(env),
                             Napi_value_val(val),
                             String_val(r),
                             size + 1,
                             &size);

  CAMLreturn(r);
}


value ocaml_node_set_property(value env, value obj, value key, value val) {
  CAMLparam4(env, obj, key, val);

  napi_set_property(Napi_env_val(env),
                    Napi_value_val(obj),
                    Napi_value_val(key),
                    Napi_value_val(val));

  CAMLreturn(Val_unit);
}

value ocaml_node_get_property(value env, value obj, value key) {
  CAMLparam3(env, obj, key);

  napi_value result;

  napi_get_property(Napi_env_val(env),
                    Napi_value_val(obj),
                    Napi_value_val(key),
                    &result);

  CAMLreturn(alloc_napi_value(result));
}

value ocaml_node_run_script(value env, value script) {
  CAMLparam2(env, script);

  napi_value result;

  napi_run_script(Napi_env_val(env),
                  Napi_value_val(script),
                  &result);

  CAMLreturn(alloc_napi_value(result));
}

value ocaml_node_throw(value env, value error) {
  CAMLparam2(env, error);
  napi_throw(Napi_env_val(env), Napi_value_val(error));
  CAMLreturn(Val_unit);
}

value ocaml_node_get_global(value env) {
  CAMLparam1(env);

  napi_value result;
  napi_get_global(Napi_env_val(env), &result);

  CAMLreturn(alloc_napi_value(result));
}

value ocaml_node_get_null(value env) {
  CAMLparam1(env);

  napi_value result;
  napi_get_null(Napi_env_val(env), &result);

  CAMLreturn(alloc_napi_value(result));
}

value ocaml_node_get_undefined(value env) {
  CAMLparam1(env);

  napi_value result;
  napi_get_undefined(Napi_env_val(env), &result);

  CAMLreturn(alloc_napi_value(result));
}

value ocaml_node_get_boolean(value env, value val) {
  CAMLparam2(env, val);

  napi_value result;
  napi_get_boolean(Napi_env_val(env), Bool_val(val), &result);

  CAMLreturn(alloc_napi_value(result));
}

value ocaml_node_create_double(value env, value val) {
  CAMLparam2(env, val);

  napi_value result;
  napi_create_double(Napi_env_val(env), Double_val(val), &result);

  CAMLreturn(alloc_napi_value(result));
}

value ocaml_node_get_value_double(value env, value val) {
  CAMLparam2(env, val);

  double result;
  napi_get_value_double(Napi_env_val(env), Napi_value_val(val), &result);
  CAMLreturn(caml_copy_double(result));
}

value ocaml_node_is_exception_pending(value env) {
  CAMLparam1(env);
  bool r;
  napi_is_exception_pending(Napi_env_val(env), &r);
  CAMLreturn(Val_bool(r));
}

value ocaml_node_get_and_clear_last_exception(value env) {
  CAMLparam1(env);

  napi_value result;

  napi_get_and_clear_last_exception(Napi_env_val(env), &result);
  CAMLreturn(alloc_napi_value(result));
}

value ocaml_node_get_value_bool(value env, value val) {
  CAMLparam2(env, val);

  bool result;
  napi_get_value_bool(Napi_env_val(env), Napi_value_val(val), &result);
  CAMLreturn(Val_bool(result));
}

value ocaml_node_create_int(value env, value val) {
  CAMLparam2(env, val);
  
  napi_value result;
  napi_create_int32(Napi_env_val(env), Int_val(val), &result); // TODO or int64?

  CAMLreturn(alloc_napi_value(result));
}

value ocaml_node_get_value_int(value env, value val) {
  CAMLparam2(env, val);

  int result;
  napi_get_value_int32(Napi_env_val(env), Napi_value_val(val), &result); // TODO or int64?
  CAMLreturn(Val_int(result));
}

value ocaml_node_create_reference(value env, value val) {
  CAMLparam2(env, val);

  napi_ref result;
  napi_create_reference(Napi_env_val(env), Napi_value_val(val), 1, &result);

  CAMLreturn(alloc_napi_ref(result));
}

value ocaml_node_get_reference_value(value env, value ref) {
  CAMLparam2(env, ref);

  napi_value result;
  napi_get_reference_value(Napi_env_val(env), Napi_ref_val(ref), &result);

  CAMLreturn(alloc_napi_value(result));
}

static void finalize_external(napi_env env, void* finalize_data, void* finalize_hint) {
  value val = (value) finalize_data;
  caml_remove_global_root(&val);
}

value ocaml_node_create_external(value env, value val) {
  CAMLparam2(env, val);

  napi_value result;
  napi_create_external(Napi_env_val(env), (void *)val, finalize_external, NULL, &result);

  caml_register_global_root(&val);

  CAMLreturn(alloc_napi_value(result));
}

value ocaml_node_get_value_external(value env, value val) {
  CAMLparam2(env, val);
  CAMLlocal1(result);

  napi_get_value_external(Napi_env_val(env), Napi_value_val(val), (void**)&result);
  CAMLreturn(result);
}

value ocaml_node_call_function(value env, value recv, value fn, value args) {
  CAMLparam4(env, recv, fn, args);

  int argc = Wosize_val(args);
  napi_value* argv = malloc(argc * sizeof(napi_value));

  for (int i = 0; i < argc; i++) {
    argv[i] = Napi_value_val(Field(args, i));
  }

  napi_value result;
  napi_call_function(Napi_env_val(env),
                     Napi_value_val(recv),
                     Napi_value_val(fn),
                     argc,
                     argv,
                     &result);

  free(argv);

  CAMLreturn(alloc_napi_value(result));
}

static napi_value function_callback(napi_env env, napi_callback_info info) {
  CAMLparam0 ();
  CAMLlocal2(fn, args);
  size_t argc = 0;

  napi_get_cb_info(env, info, &argc, NULL, NULL, NULL);

  napi_value* argv = (napi_value*) malloc(argc * sizeof(napi_value));

  napi_value thisArg;
  napi_get_cb_info(env, info, &argc, argv, &thisArg, (void **)&fn);

  args = caml_alloc(argc + 1, 0);

  Field(args, 0) = alloc_napi_value(thisArg);

  for (size_t i = 0; i < argc; i++) {
    Field(args, i + 1) = alloc_napi_value(argv[i]);
  }

  free(argv);

  return Napi_value_val(caml_callback2(fn, alloc_napi_env(env), args));
}

value ocaml_node_create_function(value env, value name, value fn) {
  CAMLparam3(env, name, fn);

  napi_value result;

  caml_register_global_root(&fn); // TODO finalize
  napi_create_function(Napi_env_val(env),
                       String_val(name),
                       NAPI_AUTO_LENGTH,
                       function_callback,
                       (void *) fn,
                       &result);

  CAMLreturn(alloc_napi_value(result));
}

value ocaml_node_create_error(value env, value msg) {
  CAMLparam2(env, msg);

  napi_value result;

  napi_create_error(Napi_env_val(env),
                    NULL,
                    Napi_value_val(msg),
                    &result);

  CAMLreturn(alloc_napi_value(result));
}

value ocaml_node_create_type_error(value env, value msg) {
  CAMLparam2(env, msg);

  napi_value result;

  napi_create_type_error(Napi_env_val(env),
                    NULL,
                    Napi_value_val(msg),
                    &result);

  CAMLreturn(alloc_napi_value(result));
}

value ocaml_node_create_symbol(value env, value desc) {
  CAMLparam2(env, desc);

  napi_value result;

  napi_create_symbol(Napi_env_val(env),
                     Napi_value_val(desc),
                     &result);

  CAMLreturn(alloc_napi_value(result));
}

value ocaml_node_is_array(value env, value val) {
  CAMLparam2(env, val);

  bool result;
  napi_is_array(Napi_env_val(env), Napi_value_val(val), &result);

  CAMLreturn(Val_bool(result));
}

value ocaml_node_create_array(value env) {
  CAMLparam1(env);

  napi_value result;
  napi_create_array(Napi_env_val(env), &result);

  CAMLreturn(alloc_napi_value(result));
}

value ocaml_node_get_array_length(value env, value val) {
  CAMLparam2(env, val);

  uint32_t result;

  napi_get_array_length(Napi_env_val(env),
                        Napi_value_val(val),
                        &result);

  CAMLreturn(Val_int(result));
}

value ocaml_node_get_element(value env, value val, value i) {
  CAMLparam3(env, val, i);

  napi_value result;
  napi_get_element(Napi_env_val(env),
                   Napi_value_val(val),
                   Int_val(i),
                   &result);

  CAMLreturn(alloc_napi_value(result));
}

value ocaml_node_set_element(value env, value val, value i, value v) {
  CAMLparam4(env, val, i, v);

  napi_set_element(Napi_env_val(env),
                    Napi_value_val(val),
                    Int_val(i),
                    Napi_value_val(v));

  CAMLreturn(Val_unit);
}

napi_value init(napi_env env, napi_value exports) {
  char *argv = NULL;
  caml_startup(&argv);

  // TODO GC
  value exp = caml_callback2(*caml_named_value("ocaml_node_init"), alloc_napi_env(env), alloc_napi_value(exports));
  return Napi_value_val(exp);
}

NAPI_MODULE(NODE_GYP_MODULE_NAME, init)
