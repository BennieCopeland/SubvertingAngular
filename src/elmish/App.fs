module App

open Browser
open Elmish
open Elmish.React
open Fable.React
open Feliz

type State =
    {
        AuthToken : string
    }

let init props =
    props, Cmd.none

let update msg state =
    state, Cmd.none

let view model dispatch =
    Html.h1 "Hello Elmish"

let appInit htmlId authToken =
    let props = {
        AuthToken = authToken
    }

    Program.mkProgram init update view
    |> Program.withReactSynchronous htmlId
    |> Program.withConsoleTrace
    |> Program.runWith props

let killApp domNode =
    ReactDom.unmountComponentAtNode domNode
