module App

open System
open Elmish
open Elmish.React
open Fable.React
open Feliz
open Feliz.CustomRouter

module PageA =
    type Model = unit
    type Msg = | NoOp
    let init () = (), Cmd.none
    let update msg state = state, Cmd.none
    let view state dispatch = Html.h1 "Hello Page A"

module PageB =
    type Model = unit
    type Msg = | NoOp
    let init () = (), Cmd.none
    let update msg state = state, Cmd.none
    let view state dispatch = Html.h1 "Hello Page B"

[<RequireQualifiedAccess>]
type Url =
    | Todos of Todos.Url
    | PageA
    | PageB
    | NotFound

let parseUrl (xs: string list) =
    match xs with
    | "todos" :: segments -> Url.Todos (Todos.parseUrl segments)
    | [ "page-a" ] -> Url.PageA
    | [ "page-b" ] -> Url.PageB
    | _ -> Url.NotFound

type Page =
    | Todos of Todos.Model
    | PageA of PageA.Model
    | PageB of PageB.Model
    | NotFound

type Msg =
    | UrlChanged of Url
    | TodosMsg of Todos.Msg
    | PageAMsg of PageA.Msg
    | PageBMsg of PageB.Msg

type InitProps =
    {
        AuthToken : string
    }

type State = {
        AuthToken : string
        CurrentUrl : Url
        CurrentPage : Page
    }

let init (props: InitProps) =
    let currentUrl = Router.currentPath() |> parseUrl
    
    let show page = { AuthToken = props.AuthToken; CurrentUrl = currentUrl; CurrentPage = page }
    
    match currentUrl with
    | Url.Todos todoUrl ->
        let page, cmd = Todos.init todoUrl
        show (Page.Todos page), Cmd.map TodosMsg cmd
    | Url.PageA ->
        let page, cmd = PageA.init ()
        show (Page.PageA page), Cmd.map PageAMsg cmd
    | Url.PageB ->
        let page, cmd = PageB.init ()
        show (Page.PageB page), Cmd.map PageBMsg cmd
    | Url.NotFound -> show (Page.NotFound), Cmd.none


let update msg state =
    match msg, state.CurrentPage with
    | TodosMsg subMsg, Todos subState ->
        let nextState, nextCmd = Todos.update subMsg subState
        { state with CurrentPage = Todos nextState }, Cmd.map PageAMsg nextCmd
    | PageAMsg subMsg, PageA subState ->
        let nextState, nextCmd = PageA.update subMsg subState
        { state with CurrentPage = PageA nextState }, Cmd.map PageAMsg nextCmd
    | PageBMsg subMsg, PageB subState ->
        let nextState, nextCmd = PageB.update subMsg subState
        { state with CurrentPage = PageB nextState }, Cmd.map PageBMsg nextCmd
    | UrlChanged nextUrl, _ ->
        let show page = { state with CurrentPage = page; CurrentUrl = nextUrl }
        
        match nextUrl with
        | Url.NotFound -> show Page.NotFound, Cmd.none
        | Url.PageA ->
            let pageA, pageACmd = PageA.init ()
            show (PageA pageA), Cmd.map PageAMsg pageACmd
        | Url.PageB ->
            let pageB, pageBCmd = PageB.init ()
            show (PageB pageB), Cmd.map PageBMsg pageBCmd
        | Url.Todos todoUrl ->
            let todos, todosCmd = Todos.init todoUrl
            show (Todos todos), Cmd.map TodosMsg todosCmd
            
    | _, _ ->
        // log a likely invalid transition
        state, Cmd.none

let view navigator model dispatch =
    React.router [
        router.navigator navigator
        router.pathMode
        router.onUrlChanged (parseUrl >> UrlChanged >> dispatch)
        router.children [
            match model.CurrentPage with
            | Todos subState -> Todos.view subState (TodosMsg >> dispatch)
            | PageA subState -> PageA.view subState (PageAMsg >> dispatch)
            | PageB subState -> PageB.view subState (PageBMsg >> dispatch)
            | NotFound -> Html.h1 "Page not found"
        ]
    ]

let appInit htmlId authToken (setRoute : string array -> bool -> unit) =
    let props: InitProps = {
        AuthToken = authToken
    }

    let navigator (segments, mode) =
        let skipLocationChange =
            match mode with
            | HistoryMode.ReplaceState -> true
            | _ -> false
        setRoute (Array.ofList segments) skipLocationChange

    Program.mkProgram init update (view navigator)
    |> Program.withReactSynchronous htmlId
    |> Program.withConsoleTrace
    |> Program.runWith props

let killApp domNode =
    ReactDom.unmountComponentAtNode domNode
