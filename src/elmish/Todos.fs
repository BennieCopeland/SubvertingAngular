module Todos

open System
open Elmish
open Feliz

type Todo =
    {
        Title: string
        Completed: bool
    }

type Page =
    | Todos of Map<Guid, Todo>
    | Add of Todo
    | Edit of Guid * Todo
    | View of Todo

type Model =
    {
        TodoStore : Map<Guid, Todo>
        Page : Page
    }

type Msg =
    | AddClicked
    | EditClicked of Guid
    | ViewClicked of Guid
    | DeleteClicked of Guid
    | SetTitle of string
    | SetCompleted of bool
    | ReturnToList
    | SaveClicked

let init () =
    let todos =
        [
            Guid.NewGuid(), { Title = "Write a blog"; Completed = true }
            Guid.NewGuid(), { Title = "Update blog"; Completed = false }
        ]
        |> Map.ofList
    
    {
        TodoStore = todos
        Page = Todos todos
    }, Cmd.none

let update msg state =
    match msg, state.Page with
    | AddClicked, Page.Todos _ ->
        let todo = { Title = ""; Completed = false }
        { state with Page = Add todo }, Cmd.none
    | EditClicked todoId, Page.Todos _ ->
        let todo = Map.find todoId state.TodoStore
        { state with Page = Edit (todoId, todo) }, Cmd.none
    | ViewClicked todoId, Page.Todos _ ->
        let todo = Map.find todoId state.TodoStore
        { state with Page = View todo }, Cmd.none
    | DeleteClicked todoId, Page.Todos _ ->
        let todos = Map.remove todoId state.TodoStore
        { state with TodoStore = todos; Page = Todos todos }, Cmd.none
    | SetTitle title, Page.Add todo ->
        let todo = { todo with Title = title }
        { state with Page = Add todo }, Cmd.none
    | SetCompleted completed, Page.Add todo ->
        let todo = { todo with Completed = completed }
        { state with Page = Add todo }, Cmd.none
    | SetTitle title, Page.Edit (todoId, todo) ->
        let todo = { todo with Title = title }
        { state with Page = Edit (todoId, todo) }, Cmd.none
    | SetCompleted completed, Page.Edit (todoId, todo) ->
        let todo = { todo with Completed = completed }
        { state with Page = Edit (todoId, todo) }, Cmd.none
    | ReturnToList, Add _ | ReturnToList, Edit _ | ReturnToList, View _ ->
        { state with Page = Todos state.TodoStore }, Cmd.none
    | SaveClicked, Add todo ->
        let todos = Map.add (Guid.NewGuid()) todo state.TodoStore
        { state with TodoStore = todos; Page = Todos todos }, Cmd.none
    | SaveClicked, Edit (todoId, todo) ->
        let todos = Map.add todoId todo state.TodoStore
        { state with TodoStore = todos; Page = Todos todos }, Cmd.none
    | _, _ -> state, Cmd.none

let viewTodo todo dispatch =
    Html.div [
        Html.div [
            Html.label [
                prop.text "Title"
            ]
            Html.div [
                prop.text todo.Title
            ]
        ]
        Html.div [
            Html.label [
                prop.text "Completed?"
            ]
            Html.div [
                prop.text (if todo.Completed then "Yes" else "No")
            ]
        ]
        Html.button [
            prop.text "Back"
            prop.onClick (fun _ -> ReturnToList |> dispatch)
        ]
    ]

let editTodo todo dispatch =
    Html.div [
        Html.div [
            Html.label [
                prop.text "Title"
            ]
            Html.input [
                prop.valueOrDefault todo.Title
                prop.onTextChange (SetTitle >> dispatch)
            ]
        ]
        Html.div [
            Html.label [
                prop.text "Completed?"
            ]
            Html.input [
                prop.type'.checkbox
                prop.isChecked todo.Completed
                prop.onCheckedChange (SetCompleted >> dispatch)
            ]
        ]
        Html.button [
            prop.text "Save"
            prop.onClick (fun _ -> SaveClicked |> dispatch)
        ]
        Html.button [
            prop.text "Back"
            prop.onClick (fun _ -> ReturnToList |> dispatch)
        ]
    ]

let todoRow todoId todo dispatch =
    Html.tableRow [
        Html.tableCell [
            Html.text todo.Title
        ]
        Html.tableCell [
            Html.text (if todo.Completed then "Yes" else "No")
        ]
        Html.tableCell [
            Html.button [
                prop.onClick (fun _ -> EditClicked todoId |> dispatch)
                prop.text "Edit"
            ]
            Html.button [
                prop.onClick (fun _ -> ViewClicked todoId |> dispatch)
                prop.text "View"
            ]
        ]
    ]

let todosView todos dispatch =
    Html.table [
        prop.children [
            for todoId, todo in Map.toList todos ->
                todoRow todoId todo dispatch
        ]
    ]

let view state dispatch =
    match state.Page with
    | Todos todos ->
        todosView todos dispatch
    | Add todo -> editTodo todo dispatch
    | Edit (_, todo) -> editTodo todo dispatch
    | View todo -> viewTodo todo dispatch