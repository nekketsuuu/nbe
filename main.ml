(* Terms and semantics *)

type typ = Base | Fun of typ * typ
type id = string
type term =
  | Var of id
  | Lam of id * typ * term
  | App of term * term

let rec string_of_term e =
  match e with
  | Var(x) -> x
  | Lam(x, ty, e') -> "(\\" ^ x ^ ". " ^ string_of_term e' ^ ")"
  | App(e1, e2) -> (string_of_term e1) ^ " " ^ (string_of_term e2)

type semantics = 
  | MBase of term
  | MLam of (semantics -> semantics)

exception Undefined_variable

type environment = id -> semantics

let empty_env : environment = fun _ -> raise Undefined_variable

let extend_env name sem env =
  fun n -> if n = name then sem else env n

(* Term to semantics *)

exception Wrong_application

let rec eval e env =
  match e with
  | Var(name) -> env name
  | Lam(name, _, e') -> MLam(fun sem -> eval e' (extend_env name sem env))
  | App(e1, e2) ->
    let sem1 = eval e1 env in
    let sem2 = eval e2 env in
    begin
      match sem1 with
      | MBase(_) -> raise Wrong_application
      | MLam(f) -> f(sem2)
    end

(* Semantics to term *)

exception Illegal_type

let count = ref 0
let gensym () =
  let n = !count in
  count := !count + 1;
  "x" ^ (string_of_int n)

let rec reify sem ty =
  match sem with
  | MBase(e) -> e
  | MLam(f) ->
    begin
      match ty with
      | Base -> raise Illegal_type
      | Fun(ty1, ty2) ->
        let x = gensym () in
        Lam(x, ty1, reify (f @@ reflect (Var(x)) ty1) ty2)
    end
and reflect e ty =
  match ty with
  | Base -> MBase(e)
  | Fun(ty1, ty2) -> MLam(fun sem -> reflect (App(e, reify sem ty1)) ty2)

(* Normalization by evaluation *)

let normalize e ty = reify (eval e empty_env) ty

(* testing *)

type case = { input : term; ty : typ; expect : term; }
let cases : case list = [
  {
    input  = Lam("x", Base, App(Lam("y", Base, Var("y")), Var("x")));
    ty     = Fun(Base, Base);
    expect = Lam("x0", Base, Var("x0"))
  };
  {
    input  = Lam("y", Fun(Base, Base), App(Lam("x", Fun(Base, Base), Lam("y", Base, App(Var("x"), Var("y")))), Var("y")));
    ty     = Fun(Fun(Base, Base), Fun(Base, Base));
    expect = Lam("x1", Fun(Base, Base), Lam("x2", Base, App(Var("x1"), Var("x2"))))
  }
]

let rec test cases =
  let n = ref 1 in
  List.iter (test_one n) cases
and test_one n { input; ty; expect } =
  print_endline @@ "[Case " ^ string_of_int !n ^ "]";
  n := !n + 1;
  print_endline @@ "Input: " ^ string_of_term input;
  begin
    try
      let result = normalize input ty in
      if result = expect then
        print_endline @@ "Success: " ^ string_of_term expect
      else
        (print_endline @@ "Fail: expected " ^ string_of_term expect;
         print_endline @@ "      returned " ^ string_of_term result)
    with
    | e -> (print_endline @@ "Fail: expected " ^ string_of_term expect;
            print_endline @@ "      but failed with exception " ^ Printexc.to_string e)
  end;
  print_newline ()
