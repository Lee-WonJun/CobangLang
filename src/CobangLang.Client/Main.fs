module CobangLang.Client.Main

open System.Net.Http
open Microsoft.AspNetCore.Components
open Elmish
open Bolero
open FParsec
open Parser
open Interpreter


// Import only the AST module
open AST

/// The Elmish application's model.
type Model =
    {
        code: string
        output: string
        error: string option
    }

let initModel =
    {
        code = ""
        output = ""
        error = None
    }

/// The Elmish application's update messages.
type Message =
    | SetCode of string
    | RunCode
    | ClearOutput
    | Error of exn
    | ClearError


let update (http: HttpClient) message model =
    match message with
    | SetCode code ->
        { model with code = code }, Cmd.none

    | RunCode ->
        try
            let output = 
                match runParserOnString program  () "cobang" model.code with
                | Success(result, _, _) ->
                    interpret result |> fun o -> o.StandardOutput
                | Failure(errorMsg, _, _) ->
                    printfn "Parsing failed with error: %s" errorMsg
                    "FAIL"

            { model with output = output }, Cmd.none
        with ex ->
            { model with error = Some ex.Message }, Cmd.none
            
    | ClearOutput ->
        { model with output = "" }, Cmd.none

    | Error exn ->
        { model with error = Some exn.Message }, Cmd.none
        
    | ClearError ->
        { model with error = None }, Cmd.none

type Main = Template<"wwwroot/main.html">

let view model dispatch =
    Main.Home()
        .Code(model.code, fun code -> dispatch (SetCode code))
        .RunCode(fun _ -> dispatch RunCode)
        .ClearOutput(fun _ -> dispatch ClearOutput)
        .Output(model.output)
        .Elt()

type MyApp() =
    inherit ProgramComponent<Model, Message>()

    [<Inject>]
    member val HttpClient = Unchecked.defaultof<HttpClient> with get, set

    override this.Program =
        let update = update this.HttpClient
        Program.mkProgram (fun _ -> initModel, Cmd.none) update view
        |> Program.withConsoleTrace
