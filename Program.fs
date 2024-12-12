open System
open System.Collections.Generic

let tokenize (input: string) =
    let rec recHelper (chars: char list) acc =
        match chars with
        | [] -> List.rev acc
        | ' ' :: rest -> recHelper rest acc
        | '+' :: rest -> recHelper rest ("+" :: acc)
        | '-' :: rest -> recHelper rest ("-" :: acc)
        | '*' :: rest -> recHelper rest ("*" :: acc)
        | '/' :: rest -> recHelper rest ("/" :: acc)
        | '=' :: '=' :: rest -> recHelper rest ("==" :: acc)
        | '!' :: '=' :: rest -> recHelper rest ("!=" :: acc)
        | '>' :: '=' :: rest -> recHelper rest (">=" :: acc)
        | '<' :: '=' :: rest -> recHelper rest ("<=" :: acc)
        | '>' :: rest -> recHelper rest (">" :: acc)
        | '<' :: rest -> recHelper rest ("<" :: acc)
        | '=' :: rest -> recHelper rest ("=" :: acc)
        | ';' :: rest -> recHelper rest (";" :: acc)
        | x :: rest when Char.IsLetter x ->
            let var = chars |> List.takeWhile Char.IsLetter |> String.Concat
            let remaining = chars |> List.skipWhile Char.IsLetter
            recHelper remaining (var :: acc)
        | x :: rest when Char.IsDigit x ->
            let number = chars |> List.takeWhile Char.IsDigit |> String.Concat
            let remaining = chars |> List.skipWhile Char.IsDigit
            recHelper remaining (number :: acc)
        | _ -> failwith "Invalid character in input"

    recHelper (Seq.toList input) []

type Expression =
    | Number of int
    | Variable of string
    | BinaryOp of string * Expression * Expression

type Statement =
    | Assignment of string * Expression
    | If of Expression * Statement * Statement option
    | ExpressionStmt of Expression

let parse (tokens: string list) =
    let rec parsePrimary (tokens: string list) =
        match tokens with
        | [] -> failwith "Unexpected end of input in expression"
        | token :: rest ->
            match token with
            | t when Char.IsDigit t.[0] -> Number (int t), rest
            | t when Char.IsLetter t.[0] -> Variable t, rest
            | "(" ->
                let expr, remaining = parseExpression rest
                match remaining with
                | ")" :: rest -> expr, rest
                | _ -> failwith "Missing closing parenthesis"
            | _ -> failwith $"Unexpected token '{token}' in expression"

    and parseBinaryOp (left: Expression) (tokens: string list) =
        match tokens with
        | operator :: rest when operator = "+" || operator = "-" || operator = "*" || operator = "/" || operator = ">" || operator = "<" || operator = ">=" || operator = "<=" || operator = "==" || operator = "!=" ->
            let right, remaining = parsePrimary rest
            let combined = BinaryOp(operator, left, right)
            parseBinaryOp combined remaining
        | _ -> left, tokens

    and parseExpression (tokens: string list) =
        let primary, remaining = parsePrimary tokens
        parseBinaryOp primary remaining

    let rec parseStatement (tokens: string list) =
        match tokens with
        | [] -> failwith "Unexpected end of input in statement"
        | token :: rest ->
            match token with
            | t when Char.IsLetter t.[0] && t <> "if" ->
                match rest with
                | "=" :: rest ->
                    let expr, remaining = parseExpression rest
                    Assignment(t, expr), remaining
                | _ -> failwith "Expected '=' after variable"
            | "if" ->
                let condition, afterCondition = parseExpression rest
                match afterCondition with
                | "then" :: rest ->
                    let thenStmt, afterThen = parseStatement rest
                    match afterThen with
                    | "else" :: rest ->
                        let elseStmt, afterElse = parseStatement rest
                        If(condition, thenStmt, Some elseStmt), afterElse
                    | _ -> If(condition, thenStmt, None), afterThen
                | _ -> failwith "Expected 'then' after if condition"
            | _ ->
                let expr, remaining = parseExpression (token :: rest)
                ExpressionStmt expr, remaining

    let rec parseProgram (tokens: string list) =
        match tokens with
        | [] -> []
        | ";" :: rest -> parseProgram rest
        | _ ->
            let stmt, remaining = parseStatement tokens
            stmt :: parseProgram remaining

    parseProgram tokens

let evaluateImperative (program: Statement list) =
    let variables = Dictionary<string, int>()

    let rec evalExpression expr =
        match expr with
        | Number value -> value
        | Variable name ->
            if variables.ContainsKey(name) then
                variables.[name]
            else
                failwith $"Undefined variable '{name}'"
        | BinaryOp (op, left, right) ->
            let leftVal = evalExpression left
            let rightVal = evalExpression right
            match op with
            | "+" -> leftVal + rightVal
            | "-" -> leftVal - rightVal
            | "*" -> leftVal * rightVal
            | "/" ->
                if rightVal = 0 then failwith "Division by zero"
                else leftVal / rightVal
            | ">" -> if leftVal > rightVal then 1 else 0
            | "<" -> if leftVal < rightVal then 1 else 0
            | ">=" -> if leftVal >= rightVal then 1 else 0
            | "<=" -> if leftVal <= rightVal then 1 else 0
            | "==" -> if leftVal = rightVal then 1 else 0
            | "!=" -> if leftVal <> rightVal then 1 else 0
            | _ -> failwith $"Unknown operator '{op}'"

    let rec execStatement stmt =
        match stmt with
        | Assignment (name, expr) ->
            let value = evalExpression expr
            variables.[name] <- value
        | If (condition, thenStmt, elseStmtOpt) ->
            let condValue = evalExpression condition
            if condValue <> 0 then
                execStatement thenStmt
            else
                match elseStmtOpt with
                | Some elseStmt -> execStatement elseStmt
                | None -> ()
        | ExpressionStmt expr ->
            ignore (evalExpression expr)

    program |> List.iter execStatement
    variables


let evaluateFunctional (program: Statement list) =
    let rec evalExpression expr variables =
        match expr with
        | Number value->value
        | Variable name->
            match List.tryFind (fun (key, _) -> key = name) variables with
            | Some (_, value)->value
            | None -> failwith $"undefined variable '{name}'"
        | BinaryOp (op, left, right) ->
            let leftVal = evalExpression left variables
            let rightVal = evalExpression right variables
            match op with
            |"+"->leftVal + rightVal
            |"-"->leftVal - rightVal
            |"*"->leftVal * rightVal
            |"/"->
                if rightVal = 0 then failwith "division by zero"
                else leftVal / rightVal
            |">" ->if leftVal > rightVal then 1 else 0
            |"<" ->if leftVal < rightVal then 1 else 0
            |">=" ->if leftVal >= rightVal then 1 else 0
            |"<=" ->if leftVal <= rightVal then 1 else 0
            |"==" ->if leftVal = rightVal then 1 else 0
            |"!=" ->if leftVal <> rightVal then 1 else 0
            | _ ->failwith $"unknown operator '{op}'"

    let rec execStatement variables stmt =
        match stmt with
        | Assignment (name, expr)->
            let value = evalExpression expr variables
            (name, value) :: (List.filter (fun (key, _) -> key <> name) variables)
        | If (condition, thenStmt, elseStmtOpt) ->
            let condValue = evalExpression condition variables
            if condValue <> 0 then
                execStatement variables thenStmt
            else
                match elseStmtOpt with
                | Some elseStmt->execStatement variables elseStmt
                | None->variables
        | ExpressionStmt expr->
            ignore (evalExpression expr variables)
            variables

    let rec evalProgram variables stmts =
        match stmts with
        | [] -> variables
        | stmt :: rest->
            let newVars = execStatement variables stmt
            evalProgram newVars rest

    evalProgram [] program


let input = "x = 5 + 3; if x > 5 then y = 10 else y = 0"
let tokens = tokenize input

printfn "Tokens: %A" tokens
let ast = parse tokens
printfn "AST: %A" ast

let finalState = evaluateImperative ast
printfn $"Final State: {finalState}"

let finalStateFunctional = evaluateFunctional ast
printfn $"Final State (Functional): {finalStateFunctional}"