open FParsec
open System.IO
open Interpreter.Interpreter

module Parser = 
    let Parse, ParseRef = createParserForwardedToRef()

    let ParseFloat = pfloat .>> spaces |>> Float

    let ParseBool =
        pstring "true" <|> pstring "false"  .>> spaces |>> 
            function
                | "true" -> Bool(true)
                | "false" -> Bool(false)

    let Name =
        let isIdentifierFirstChar c = isLetter c || c = '_'
        let isIdentifierChar c = isLetter c || isDigit c || c = '_'

        many1Satisfy2L isIdentifierFirstChar isIdentifierChar "invalid variable or function name" .>> spaces |>> Var

    let Operator = choice[
                        pstring "&" |>> fun _ -> "&"
                        pstring "|" |>> fun _ -> "|"
                        pstring "not" |>> fun _ -> "not"
                        pstring "+" |>> fun _ -> "+"
                        pstring "-" |>> fun _ -> "-"
                        pstring "*" |>> fun _ -> "*"
                        pstring "/" |>> fun _ -> "/"
                        pstring "=" |>> fun _ -> "="
                        pstring "!=" |>> fun _ -> "!="
                        pstring ">" |>> fun _ -> ">"
                        pstring "<" |>> fun _ -> "<"
                        pstring ">=" |>> fun _ -> ">="
                        pstring "<=" |>> fun _ -> "<="
                        ]



    let Function =
        (pstring "fun" >>. spaces >>. Name) .>>. 
        (skipChar '(' >>. spaces  >>. many Name .>> skipChar ')') .>>. 
        (spaces >>. skipChar '{' >>. spaces >>. many Parse .>> spaces .>> skipChar '}' .>> spaces) .>>. 
        (many Parse .>> spaces) |>> 
            fun(((Var(func), params), body), expr_list) -> 
                // printfn "%A" expr_list
                let ids_extracted = 
                    params |> List.map(
                        function
                            |Var(x) -> x 
                )

                Let(func, Lam(ids_extracted, Prog(body)), Prog(expr_list))

    let FunctionRec =
        (pstring "rec fun" >>. spaces >>. Name) .>>. 
        (skipChar '(' >>. spaces  >>. many Name .>> skipChar ')') .>>. 
        (spaces >>. skipChar '{' >>. spaces >>. many Parse .>> spaces .>> skipChar '}' .>> spaces) .>>. 
        (many Parse .>> spaces) |>> 
            fun(((Var(func), params), body), expr_list) -> 
                // printfn "%A" expr_list
                let ids_extracted = 
                    params |> List.map(
                        function
                            |Var(x) -> x 
                )

                LetRec(func, Lam(ids_extracted, Prog(body)), Prog(expr_list))

    let Var = 
        pstring "var" >>. spaces >>. Name .>>. 
        (spaces >>. skipChar '=' >>. spaces >>. Parse) .>> spaces .>>. 
        many Parse .>> spaces|>> 
            fun ((Var(name), value), expr_list) -> Let(name, value, Prog(expr_list)) 

    let ParseList = 
        skipChar '[' >>. spaces >>. many Parse .>> spaces .>> skipChar ']' .>> spaces |>> List

    let FunCall =
        Name .>>. 
        (spaces >>. skipChar '(' >>. spaces >>. many Parse .>> spaces .>> skipChar ')' .>> spaces) |>>
            fun(func, arg) -> App(func, List(arg))

    let Operation = 
        (ParseFloat <|> attempt FunCall <|> Name <|> ParseBool) .>> spaces .>>. 
        Operator .>> spaces .>>. 
        (ParseFloat <|> attempt FunCall <|> Name <|> ParseBool) .>> spaces |>> 
            fun ((e1, op), e2) -> 
                App(App(PrimitiveOperation(op),  e1),  e2)



    let IndexList = 
        Name .>>.
        (spaces >>. skipChar '[' >>. spaces >>. pint32 .>> spaces .>> skipChar ']' .>> spaces) |>>
            fun(list, num) -> 
                App(App(PrimitiveOperation("[]"), list), Int(num))
    


    let Method = 
        Name .>> spaces .>>.
        (skipChar '.' >>. Name .>> spaces) .>>.
        (skipChar '(' >>. many Parse .>> spaces .>> skipChar ')' .>> spaces) |>>
            fun((list, Var(method)), args) -> 
                let acc = App(PrimitiveOperation(method), list)
                List.fold (fun acc x -> App(acc, x)) acc args 
            

    let Cond =
        (pstring "if" >>. spaces >>. Parse .>> spaces) .>>.
        (pstring "then" >>. spaces >>. skipChar '{' >>. spaces >>. many Parse .>> spaces .>> skipChar '}' .>> spaces) .>>.
        (pstring "else" >>. spaces >>. skipChar '{' >>. spaces >>. many Parse .>> spaces .>> skipChar '}' .>> spaces) |>>
        fun ((cond, res), alt) -> Cond(cond, Prog(res), Prog(alt))

    let ParsePrint =
        pstring "print" >>. spaces >>. skipChar '(' >>. spaces >>. many Parse .>> spaces .>> skipChar ')' .>> spaces |>> fun(arg) -> Print(Prog(arg))

    ParseRef := choice[FunctionRec; Function; ParsePrint; Cond; attempt Operation;  attempt FunCall; attempt IndexList; attempt Method; Var; ParseList; ParseFloat; ParseBool; Name;]

    let final = spaces >>. many Parse .>> eof |>> Prog

    let readFile filePath =
        try
            File.ReadAllText(filePath)
        with
        | :? FileNotFoundException ->
            failwithf "File not found: %s" filePath
        | ex ->
            failwithf "An error occurred while reading the file: %s" ex.Message




[<EntryPoint>]
let main argv =
    if argv.Length <> 1 then
        printfn "ERROR: please provide one argument - name of the file containing the code"
    else
        let filename = argv.[0]
        
        if File.Exists(filename) then
            let content = Parser.readFile filename
            match run Parser.final content with
            | Success(result,_,_) -> 
                eval result Map.empty
                ()
            | Failure(err,_,_) -> printfn "%A" err
        else
            printfn "ERROR: File '%s' does not exist." filename
    0