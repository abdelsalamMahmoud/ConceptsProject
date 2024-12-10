open System
open System.Collections.Generic

// Tokenizer: Converts input into tokens
let tokenize input =
    let rec recHelper chars acc =
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
        | x :: rest when Char.IsLetter x ->
            let var = List.takeWhile Char.IsLetter chars |> System.String.Concat
            let remaining = List.skipWhile Char.IsLetter chars
            recHelper remaining (var :: acc)
        | x :: rest when Char.IsDigit x ->
            let number = List.takeWhile Char.IsDigit chars |> System.String.Concat
            let remaining = List.skipWhile Char.IsDigit chars
            recHelper remaining (number :: acc)
        | _ -> failwith "Invalid character in input"

    recHelper (Seq.toList input) []

// Expression Types
type Expression =
    | Number of int
    | Variable of string
    | BinaryOp of string * Expression * Expression

// Statement Types
type Statement =
    | Assignment of string * Expression
    | If of Expression * Statement * Statement option
    | ExpressionStmt of Expression

// Parsing Functions
let parse tokens =
    let rec parseExpression tokens =
        match tokens with
        | [] -> failwith "Unexpected end of input in expression"
        | (token: string) :: rest ->
            match token with
            | t when System.Char.IsDigit t.[0] -> 
                let value = int t
                Number value, rest
            | t when System.Char.IsLetter t.[0] -> 
                Variable t, rest
            | "(" -> 
                let expr, remaining = parseExpression rest
                match remaining with
                | ")" :: rest -> expr, rest
                | _ -> failwith "Missing closing parenthesis"
            | _ -> failwith $"Unexpected token '{token}' in expression"

    let rec parseBinaryOp left tokens =
        match tokens with
        | [] -> left, []
        | operator :: rest when operator = "+" || operator = "-" || operator = "*" || operator = "/" -> 
            let right, remaining = parseExpression rest
            let combined = BinaryOp(operator, left, right)
            parseBinaryOp combined remaining
        | _ -> left, tokens

    let rec parseStatement tokens =
        match tokens with
        | [] -> failwith "Unexpected end of input in statement"
        | (token: string) :: rest ->
            match token with
            | t when System.Char.IsLetter t.[0] -> 
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
                | _ -> failwith "Expected 'then' in if statement"
            | _ ->
                let expr, remaining = parseExpression (token :: rest)
                ExpressionStmt expr, remaining

    let rec parseProgram tokens =
        match tokens with
        | [] -> []
        | _ ->
            let stmt, remaining = parseStatement tokens
            stmt :: parseProgram remaining

    parseProgram tokens

let evaluateImperative (program: Statement list) =
    // Mutable dictionary to store variable values
    let variables = Dictionary<string, int>()

    // Function to evaluate an expression
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
            | _ -> failwith $"Unknown operator '{op}'"

    // Function to execute a statement
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
            // Evaluate an expression statement but don't store the result
            ignore (evalExpression expr)

    // Iterate through the program and execute each statement
    program |> List.iter execStatement

    // Return the final state of variables
    variables

// Test input
let input = "x = 5 + 3 if x > 5 then y = x * 2 else y = 0"
let tokens = tokenize input
let ast = parse tokens
let finalState = evaluateImperative ast

// Print final state of variables
printfn $"Final State: {finalState}"