namespace Interpreter

module Interpreter =

    type id = string
    type expr =
        | Var of id
        | Lam of id list*expr
        | App of expr*expr        
        | Bool of bool
        | Int of int
        | Cond of expr*expr*expr
        | Float of float
        | List of expr list
        | Let of id*expr*expr
        | LetRec of id*expr*expr
        | PrimitiveOperation of id
        | Op of id*int*expr list
        | Closure of expr*env
        | RClosure of expr*env*id
        | Prog of expr list
        | Print of expr
        | None
    and
        env = Map<id,expr>

    let ArgsNum = function
    | "&" -> 2
    | "|" -> 2
    | "!" -> 1

    | "+" -> 2
    | "-" -> 2
    | "*" -> 2
    | "%" -> 2
    | "/" -> 2

    | "=" -> 2
    | "!=" -> 2
    | ">" -> 2
    | "<" -> 2
    | ">=" -> 2
    | "<=" -> 2

    let Operation = function
    | "&" -> function 
        | [Bool(a);Bool(b)] -> Bool(a&&b)
    | "|" -> function 
        | [Bool(a);Bool(b)] -> Bool(a||b)
    | "not" -> function 
        | [Bool(a)] -> Bool(not a)
    | "+" -> function 
        | [Float(a);Float(b)] -> Float(a+b)
    | "-" -> function 
        | [Float(a);Float(b)] -> Float(a-b)
    | "*" -> function 
        | [Float(a);Float(b)] -> Float(a*b)
    | "/" -> function 
        | [Float(a);Float(b)] -> Float(a/b)
    | "%" -> function
        | [Float(a);Float(b)] -> Float(a % b)
    | ">" -> function 
        | [Float(a);Float(b)] -> Bool(a>b)
    | "<" -> function 
        | [Float(a);Float(b)] -> Bool(a<b)
    | ">=" -> function 
        | [Float(a);Float(b)] -> Bool(a>=b)
    | "<=" -> function 
        | [Float(a);Float(b)] -> Bool(a<=b)
    | "=" -> function 
        | [Float(a);Float(b)] -> Bool(a=b)
    | "!=" -> function 
        | [Float(a);Float(b)] -> Bool(a<>b)
        

    let rec eval exp env =
        let _ = printfn "eval %A where %A" exp env in
        match exp with
        | App(e1,e2) -> apply (eval e1 env) (eval e2 env)
        | Bool(n) -> Bool(n)
        | Int(n) -> Int(n)
        | Float(n) -> Float(n)
        | List(n) ->
            List(List.map(fun x -> eval x env) n)
        | Var(x) -> Map.find x env
        | PrimitiveOperation(f) -> Op(f, ArgsNum f, [])
        | Cond(cond, yes, no) ->
            if Bool(true) = eval cond env then eval yes env else eval no env
        | Op(id,n,el) -> Op(id,n,el)
        | Let(id, e1, e2) ->
            let r = eval e1 env in
                eval e2 (Map.add id r env)
        | LetRec(id, e1, e2) ->
            eval e2 (Map.add id (RClosure(e1, env, id)) env)
        | Lam(param,body) -> Closure(exp, env)
        | Closure(exp, env) -> exp
        | Prog(exp_list) -> 
            exp_list |> List.map(fun x -> eval x env) |> List.last
        | Print(x) -> 
            match x with
                | Float(n) -> 
                    printfn "%A" n
                    None
                | Bool(n) -> 
                    printfn "%A" n
                    None
                | _ -> 
                    printfn "the type does not support printing"
                    None
        | _ -> 
            printfn "invalid evaluation"
            exp
    and apply e1 e2 = 
        let _ = printfn "app (%A) (%A)" e1 e2 in
        match e1 with
        | Closure(Lam(param,body),env) -> 
            match e2 with
                |List(value_list) -> 
                    value_list
                    let env_add = List.zip param value_list |> Map.ofList
                    let new_env = Map.fold(fun acc key value -> Map.add key value acc) env env_add
                    eval body new_env
        | RClosure(Lam(param,body),env, id) -> 
            match e2 with
                |List(value_list) -> 
                    value_list
                    let env_add = List.zip param value_list |> Map.ofList
                    let new_env = Map.fold(fun acc key value -> Map.add key value acc) env env_add
                    eval body (Map.add id e1 new_env)
        | Op(id,n,args) ->
            if n=1 then (Operation id)(args@[e2])
            else Op(id, n-1, args@[e2])
        

        

