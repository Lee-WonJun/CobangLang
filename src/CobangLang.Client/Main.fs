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
    }

let initModel =
    {
        code = ""
        output = ""
    }

/// The Elmish application's update messages.
type Message =
    | SetCode of string
    | RunCode
    | ClearOutput
    | WriteOutput of string


let update (http: HttpClient) message model =
    match message with
    | SetCode code ->
        { model with code = code }, Cmd.none

    | RunCode ->
        { model with output = "" }, Cmd.ofEffect (fun dispatch ->
            try
                match runParserOnString program () "cobang" model.code with
                | Success(result, _, _) ->
                    interpret result (fun x -> dispatch (WriteOutput x)) |> (fun o -> dispatch (WriteOutput (sprintf "return %d code" o.Return )))
                | Failure(errorMsg, _, _) ->
                    dispatch (WriteOutput (sprintf "Parsing failed: %s" errorMsg))
            with ex ->
                dispatch (WriteOutput ex.Message)
        )

    | ClearOutput ->
        { model with output = "" }, Cmd.none
    | WriteOutput str ->
        { model with output = model.output + str }, Cmd.none

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
