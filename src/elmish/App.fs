module App

open System
open Elmish
open Elmish.React
open Fable.React
open Feliz

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

type Route =
    | Todos
    | PageA
    | PageB
    | Unknown
    with
    static member fromStr str =
      match str with
      | "Todos" -> Todos
      | "PageA" -> PageA
      | "PageB" -> PageB
      | _ -> Unknown

type Page =
    | Todos of Todos.Model
    | PageA of PageA.Model
    | PageB of PageB.Model
    | NotFound

type Msg =
    | TodosMsg of Todos.Msg
    | PageAMsg of PageA.Msg
    | PageBMsg of PageB.Msg

type InitProps =
    {
        AuthToken : string
        Page : string
    }

type State = {
        AuthToken : string
        Page : Page
    }

let init (props: InitProps) =
    let page, cmd =
        match Route.fromStr props.Page with
        | Route.Todos ->
            let page, cmd = Todos.init ()
            Page.Todos page, Cmd.map TodosMsg cmd
        | Route.PageA ->
            let page, cmd = PageA.init ()
            Page.PageA page, Cmd.map PageAMsg cmd
        | Route.PageB ->
            let page, cmd = PageB.init ()
            Page.PageB page, Cmd.map PageBMsg cmd
        | Route.Unknown -> Page.NotFound, Cmd.none

    {
        AuthToken = props.AuthToken
        Page = page
    }, cmd

let update msg state =
    match msg, state.Page with
    | TodosMsg subMsg, Todos subState ->
        let nextState, nextCmd = Todos.update subMsg subState
        { state with Page = Todos nextState }, Cmd.map PageAMsg nextCmd
    | PageAMsg subMsg, PageA subState ->
        let nextState, nextCmd = PageA.update subMsg subState
        { state with Page = PageA nextState }, Cmd.map PageAMsg nextCmd
    | PageBMsg subMsg, PageB subState ->
        let nextState, nextCmd = PageB.update subMsg subState
        { state with Page = PageB nextState }, Cmd.map PageBMsg nextCmd
    | _, _ ->
        // log a likely invalid transition
        state, Cmd.none

let view model dispatch =
    match model.Page with
    | Todos subState -> Todos.view subState (TodosMsg >> dispatch)
    | PageA subState -> PageA.view subState (PageAMsg >> dispatch)
    | PageB subState -> PageB.view subState (PageBMsg >> dispatch)
    | NotFound -> Html.h1 "Page not found"

let appInit htmlId authToken page =
    let props: InitProps = {
        AuthToken = authToken
        Page = page
    }

    Program.mkProgram init update view
    |> Program.withReactSynchronous htmlId
    |> Program.withConsoleTrace
    |> Program.runWith props

let killApp domNode =
    ReactDom.unmountComponentAtNode domNode
