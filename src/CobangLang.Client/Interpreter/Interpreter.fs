module Interpreter

open AST
open System.Collections.Generic
open System

type Program = Statement list
type SoftwareState = {
    Variables: Dictionary<string, int>
    mutable StandardOutput: string
    mutable Return: int
}

type Signal = NormalSign | BreakSign | ReturnSign of int

let interpretAsync (program: Program) onOutput = async {
    let state = {
        Variables = Dictionary()
        StandardOutput = ""
        Return = 0
    }

    let rec exec program = async {
        match program with
        | [] -> return NormalSign 
        | VariableAssignment(var, expr) :: rest ->
            let value = evalExpr expr
            if state.Variables.ContainsKey(var) then
                state.Variables.[var] <- value
            else
                state.Variables.Add(var, value)
            return! exec rest

        | IfStatement(var, block) :: rest ->
            let! signal = 
                if state.Variables.[var] > 0 then
                    exec block
                else
                    async { return NormalSign }
                    
            match signal with
            | Signal.BreakSign -> return Signal.BreakSign
            | _ -> return! exec rest

        | WhileStatement(block) :: rest ->
            let mutable inside_continue = true
            while inside_continue do
                let! signal = exec block
                match signal with
                | Signal.BreakSign -> inside_continue <- false
                | _ -> ()
            return! exec rest

        | Sleep(seconds) :: rest ->
            do! Async.Sleep(seconds * 1000)
            return! exec rest

        | Output(var, isAsciiOutput) :: rest -> 
            let v = state.Variables.[var]
            if not isAsciiOutput then
                onOutput (sprintf "%d" v)
                state.StandardOutput <- state.StandardOutput + (sprintf "%d" v)
            else
                let ascil = v |> Convert.ToChar
                onOutput (sprintf "%c" ascil)
                state.StandardOutput <- state.StandardOutput + (sprintf "%c" ascil)
            return! exec rest

        | Return(var) :: _ ->
            state.Return <- state.Variables.[var]
            return Signal.ReturnSign state.Variables.[var]
        | Break :: _ ->
            return Signal.BreakSign
    }

    and evalExpr expr =
        match expr with
        | Number(n) -> n
        | Variable(var) -> state.Variables.[var]
        | BinaryOp(x, op, y) ->
            let x' = evalExpr x
            let y' = evalExpr y
            match op with
            | Add -> x' + y'
            | Sub -> x' - y'
            | Mul -> x' * y'
            | Div -> x' / y'

    do! exec program |> Async.Ignore
    return state
}