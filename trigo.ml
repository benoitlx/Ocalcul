type ft =
    | Const of float
    | Var of string
    | Sum of (ft * ft)
    | Prod of (ft * ft)
    | Diff of (ft * ft)
    | Quo of (ft * ft)
    | Cos of ft
    | Sin of ft
;;

let rec strf (f: ft) :string = match f with
    | Const p -> string_of_float p
    | Var p -> p
    | Sum (u,v) -> (strf u)^"+"^(strf v)
    | Prod (u,v) -> (strf u)^"*"^(strf v)
    | Diff (u,v) -> (strf u)^"-"^(strf v)
    | Quo (u,v) -> (strf u)^"/"^(strf v)
    | Cos u -> "cos("^(strf u)^")"
    | Sin u -> "sin("^(strf u)^")"


let rec eval (f: ft) (affect: (string * float) list) :float = match f with
    | Var x -> List.assoc x affect
    | Const a -> a
    | Sum (a,b) -> (eval a affect) +. (eval b affect)
    | Prod (a,b) -> (eval a affect) *. (eval b affect)
    | Diff (a,b) -> (eval a affect) -. (eval b affect)
    | Quo (a,b) -> (eval a affect) /. (eval b affect)
    | Cos x -> cos (eval x affect)
    | Sin x -> sin (eval x affect)
;;

let rec derivate (f: ft) (x: string) :ft = match f with
    | Var p when p=x -> Const 1.
    | Var _ | Const _ -> Const 0.
    | Sum (f,g) -> Sum ((derivate f x), (derivate g x))
    | Diff (f,g) -> Diff ((derivate f x), (derivate g x))
    | Prod (u,v) -> Sum (Prod ((derivate u x), v), (Prod ((derivate v x), u)))
    | Quo (u,v) -> Quo (Diff (Prod ((derivate u x), v), (Prod ((derivate v x), u))), (Prod (v, v)))
    | Cos u -> Prod ((Diff ((Const 0.), (derivate u x))), (Sin u))
    | Sin u -> Prod ((derivate u x), (Cos u))
;;

let rec simplify (f: ft) :ft = match f with
    | Prod (u,v) -> 
            if u=(Const 0.) || v=(Const 0.) then Const 0. else
            if u=(Const 1.) then v else
            if v=(Const 1.) then u else
            Prod (u,v)
    | Sum (u,v) ->
            if u=(Const 0.) then v else
            if v=(Const 0.) then u else
            Sum (u,v)
    | Diff (u,v) ->
            if u=(Const 0.) then v else
            if v=(Const 0.) then u else
            Diff (u,v)
;;

let f = Sum ((Cos (Var "x")), (Prod ((Var "x"), (Var "y"))));;
let d = derivate f "x";;
let s = simplify (Prod (Const 1., Var "x"));;
let i = simplify d;;

print_endline (string_of_float (eval f [("x", 2.); ("y", 3.)]));;
print_endline (strf f);;
print_endline (strf d);;
print_endline (strf i);;
