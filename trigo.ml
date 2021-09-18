(* TODO: exp, ln *)
type ref_function =
    (* toutes les fonctions de reference *)
    | Cos
    | Sin
;;

type ft =
    | Const of float
    | Var of char
    | Sum of (ft * ft)
    | Prod of (ft * ft)
    | Neg of ft
    | Pow of (ft * int)
    
    (* plus clair de faire intervenir des fonctions de reference*)
    | Ref of (ref_function * ft)
;;

let strf_ref f s = match f with
    | Cos -> "cos("^s^")"
    | Sin -> "sin("^s^")"


let eval_ref f x = match f with
| Cos -> cos x
| Sin -> sin x
;;


let rec strf (f: ft) :string = match f with
    | Const p -> string_of_float p
    | Var p -> String.make 1 p
    | Sum (u,v) -> (strf u)^"+"^(strf v)
    | Prod (u,v) -> (strf u)^"*"^(strf v)
    | Neg u -> "-"^(strf u)
    | Pow (u, n) -> (strf u)^"**"^(string_of_int n)
    | Ref (f, u) -> strf_ref f (strf u)
;;


let rec eval (f: ft) (affect: (char * float) list) :float = match f with
    | Var x -> List.assoc x affect
    | Const a -> a
    | Sum (a,b) -> (eval a affect) +. (eval b affect)
    | Prod (a,b) -> (eval a affect) *. (eval b affect)
    | Neg u ->  -. (eval u affect)
    | Pow (u, n) -> (eval u affect) ** (float_of_int n);
    (**| Quo (a,b) -> (eval a affect) /. (eval b affect)*)
    | Ref (f, u) -> eval_ref f (eval u affect)
;;

(* fonctions mutuellement recursives: derivate depend de derivate_ref et inversement*) 
let rec derivate (f: ft) (x: char) :ft = match f with
    | Var p when p=x -> Const 1.
    | Var _ | Const _ -> Const 0.
    | Sum (f,g) -> Sum ((derivate f x), (derivate g x))
    | Neg u -> Neg (derivate u x)
    | Prod (u,v) -> Sum (Prod ((derivate u x), v), (Prod ((derivate v x), u)))
    | Pow (u, n) -> Prod ((derivate u x), (Pow (u, (n-1))))
    (*| Quo (u,v) -> Quo (Diff (Prod ((derivate u x), v), (Prod ((derivate v x), u))), (Prod (v, v)))*)
    | Ref (f, u) -> derivate_ref f u x
and 
derivate_ref f u x = match f with
    | Cos -> Prod ((Neg (derivate u x)), (Ref (Sin, u)))
    | Sin -> Prod ((derivate u x), (Ref (Cos, u)))


let rec simplify (f: ft) :ft = match f with
    | Prod (u,v) when (u=Const 0. || v=Const 0.) -> Const 0.
    | Prod (u,v) when (u=Const 1.) -> v
    | Prod (u,v) when (v=Const 1.) -> u
    | Prod (u,v) when (u=v)        -> Pow (u, 2)
    | Prod (Pow (u, n), v) when (u=v) -> Pow (u, n+1)
    | Prod (u, Pow (v, n)) when (u=v) -> Pow (u, n+1)
    | Sum (u,v) when (u=Const 0.) -> v
    | Sum (u,v) when (v=Const 0.) -> u
    | Neg u when (u=Const 0.) -> Const 0.
    | _ -> f
;;

let f = Sum ((Ref (Cos, (Var 'x'))), (Prod ((Var 'x'), (Var 'y'))));;
let d = derivate f 'x';;
let s = simplify (Prod (Const 1., Var 'x'));;
let i = simplify d;;

print_endline (strf f);;
print_endline (strf d);;
print_endline (strf i);;
