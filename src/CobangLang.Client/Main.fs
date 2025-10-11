module CobangLang.Client.Main

open System.Net.Http
open Microsoft.AspNetCore.Components
open Elmish
open Bolero
open FParsec
open Parser
open Interpreter
open Storage


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
        code = snd Storage.examples.[0]
        output = ""
    }

/// The Elmish application's update messages.
type Message =
    | SetCode of string
    | RunCode
    | ClearOutput
    | WriteOutput of string
    | LoadExample of int


let update (http: HttpClient) message model =
    match message with
    | SetCode code ->
        { model with code = code }, Cmd.none

    | LoadExample idx ->
        if idx >= 0 && idx < Storage.examples.Length then
            { model with code = snd Storage.examples.[idx] }, Cmd.none
        else
            model, Cmd.none

    | RunCode ->
        { model with output = "" }, Cmd.ofEffect (fun dispatch ->
            Async.StartImmediate(
                async {
                    try
                        match runParserOnString program () "cobang" model.code with
                        | Success(result, _, _) ->
                            let! state = interpretAsync result (fun x -> dispatch (WriteOutput x))
                            dispatch (WriteOutput (sprintf "\nreturn %d code" state.Return))
                        | Failure(errorMsg, _, _) ->
                            dispatch (WriteOutput (sprintf "Parsing failed: %s" errorMsg))
                    with ex ->
                        dispatch (WriteOutput ex.Message)
                }
            )
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
        .LoadExample(fun e -> dispatch (LoadExample (int (string e.Value))))
        .Elt()

type MyApp() =
    inherit ProgramComponent<Model, Message>()

    [<Inject>]
    member val HttpClient = Unchecked.defaultof<HttpClient> with get, set

    override this.Program =
        let update = update this.HttpClient
        Program.mkProgram (fun _ -> initModel, Cmd.none) update view
        |> Program.withConsoleTrace
