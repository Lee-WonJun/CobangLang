module CobangLang.Client.Main

open System
open System.Net.Http
open System.Net.Http.Json
open Microsoft.AspNetCore.Components
open Elmish
open Bolero
open Bolero.Html
open FParsec
open Parser
open Interpreter


// Import only the AST module
open AST

/// Routing endpoints definition.
type Page =
    | [<EndPoint "/">] Home

/// The Elmish application's model.
type Model =
    {
        page: Page
        code: string
        output: string
        error: string option
    }

let initModel =
    {
        page = Home
        code = ""
        output = ""
        error = None
    }

/// The Elmish application's update messages.
type Message =
    | SetPage of Page
    | SetCode of string
    | RunCode
    | ClearOutput
    | Error of exn
    | ClearError


let update (http: HttpClient) message model =
    match message with
    | SetPage page ->
        { model with page = page }, Cmd.none

    | SetCode code ->
        { model with code = code }, Cmd.none

    | RunCode ->
        try
            let output = 
                match runParserOnString program  () "kwanyulang" model.code with
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

/// Connects the routing system to the Elmish application.
let router = Router.infer SetPage (fun model -> model.page)

type Main = Template<"wwwroot/main.html">

let homePage model dispatch =
    Main.Home()
        .Code(
            textarea {
                attr.``class`` "textarea is-family-monospace"
                attr.rows 15
                attr.placeholder "Enter Cobang code here..."
                on.change (fun e -> dispatch (SetCode (e.Value.ToString())))
                text model.code
            }
        )
        .RunCode(fun _ -> dispatch RunCode)
        .ClearOutput(fun _ -> dispatch ClearOutput)
        .Output(
            pre {
                attr.``class`` "has-background-light p-4"
                attr.style "min-height: 300px; font-family: monospace; white-space: pre-wrap;"
                
                if String.IsNullOrEmpty model.output then
                    "// Output will appear here after running code"
                else
                    text model.output
            }
        )
        .Elt()

let menuItem (model: Model) (page: Page) (text: string) =
    Main.MenuItem()
        .Active(if model.page = page then "is-active" else "")
        .Url(router.Link page)
        .Text(text)
        .Elt()

let view model dispatch =
    Main()
        .Menu(
            menuItem model Home "Cobang Interpreter"
        )
        .Body(homePage model dispatch)
        .Error(
            cond model.error <| function
            | None -> empty()
            | Some err ->
                Main.ErrorNotification()
                    .Text(err)
                    .Hide(fun _ -> dispatch ClearError)
                    .Elt()
        )
        .Elt()

type MyApp() =
    inherit ProgramComponent<Model, Message>()

    override _.CssScope = CssScopes.MyApp

    [<Inject>]
    member val HttpClient = Unchecked.defaultof<HttpClient> with get, set

    override this.Program =
        let update = update this.HttpClient
        Program.mkProgram (fun _ -> initModel, Cmd.none) update view
        |> Program.withRouter router
