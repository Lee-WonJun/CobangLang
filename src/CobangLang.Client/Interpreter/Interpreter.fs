module Interpreter

open AST
open System.Collections.Generic
open System

type Program = Statement list
type SoftwareState = {
    Variables: Dictionary<string, int>
    mutable StandardOutput: string
}

type Signal = NormalSign | BreakSign | ReturnSign of int

let interpret (program: Program) =
    let state = {
        Variables = Dictionary()
        StandardOutput = ""
    }

    let rec exec program = 
        match program with
        | [] -> NormalSign 
        | VariableAssignment(var, expr) :: rest ->
            let value = evalExpr expr
            if state.Variables.ContainsKey(var) then
                state.Variables.[var] <- value
            else
                state.Variables.Add(var, value)
            exec rest

        | IfStatement(var, block) :: rest ->
            let signal = 
                if state.Variables.[var] > 0 then
                    exec block  // Return the signal from executing the block
                else
                    NormalSign
                    
            match signal with
            | Signal.BreakSign -> Signal.BreakSign  // Propagate break signal
            | _ -> exec rest

        | WhileStatement(block) :: rest ->
            let mutable inside_continue = true
            while inside_continue do
                exec block |> function 
                | Signal.BreakSign -> inside_continue <- false
                | _ -> ()
            exec rest

        | Sleep(seconds) :: rest ->
            System.Threading.Thread.Sleep(seconds * 1000)
            exec rest

        | Output(var, isAsciiOutput) :: rest -> 
            let v = state.Variables.[var]
            if not isAsciiOutput then
                state.StandardOutput <- state.StandardOutput + (sprintf "%d" v)
                printf "%d" v
            else
                let ascil = v |> Convert.ToChar
                state.StandardOutput <- state.StandardOutput + (sprintf "%c" ascil)
                printf "%c" ascil
            exec rest

        | Return(var) :: _ -> // 프로그램 완전 종료
             Signal.ReturnSign state.Variables.[var]
        | Break :: _ -> // While문 종료
            Signal.BreakSign


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

    exec program |> ignore
    state