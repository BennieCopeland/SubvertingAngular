module Todos

open System
open Elmish
open Feliz
open Feliz.Router

[<RequireQualifiedAccess>]
type Url =
    | List
    | Add
    | Edit of todoId:Guid
    | View of todoId:Guid
    | NotFound

let parseUrl = function
    | [] -> Url.List
    | [ "add" ] -> Url.Add
    | [ "edit"; Route.Guid todoId ] -> Url.Edit todoId
    | [ "view"; Route.Guid todoId ] -> Url.View todoId
    | _ -> Url.NotFound

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
    | NotFound

type Model =
    {
        CurrentUrl : Url
        CurrentPage : Page
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

let mutable todoStore =
    [
        Guid.NewGuid(), { Title = "Write a blog"; Completed = true }
        Guid.NewGuid(), { Title = "Update blog"; Completed = false }
    ]
    |> Map.ofList

let init url =
    let currentPage =
        match url with
        | Url.List ->
            let todos = todoStore
            Page.Todos todos
        | Url.Add ->
            let todo = { Title = ""; Completed = false }
            Page.Add todo
        | Url.Edit todoId ->
            let todo = Map.find todoId todoStore
            Page.Edit (todoId, todo)
        | Url.View todoId ->
            let todo = Map.find todoId todoStore
            Page.View todo
        | Url.NotFound ->
            Page.NotFound
    
    {
        CurrentUrl = url
        CurrentPage = currentPage
    }, Cmd.none

let update msg state =
    match msg, state.CurrentPage with
    | AddClicked, Page.Todos _ ->
        state, Cmd.navigatePath("todos", "add")
    | EditClicked todoId, Page.Todos _ ->
        state, Cmd.navigatePath("todos", "edit", todoId.ToString())
    | ViewClicked todoId, Page.Todos _ ->
        state, Cmd.navigatePath("todos", "view", todoId.ToString())
    | DeleteClicked todoId, Page.Todos _ ->
        todoStore <- Map.remove todoId todoStore

        { state with CurrentPage = Todos todoStore }, Cmd.none
    | SetTitle title, Page.Add todo ->
        let todo = { todo with Title = title }
        { state with CurrentPage = Add todo }, Cmd.none
    | SetCompleted completed, Page.Add todo ->
        let todo = { todo with Completed = completed }
        { state with CurrentPage = Add todo }, Cmd.none
    | SetTitle title, Page.Edit (todoId, todo) ->
        let todo = { todo with Title = title }
        { state with CurrentPage = Edit (todoId, todo) }, Cmd.none
    | SetCompleted completed, Page.Edit (todoId, todo) ->
        let todo = { todo with Completed = completed }
        { state with CurrentPage = Edit (todoId, todo) }, Cmd.none
    | ReturnToList, Add _ | ReturnToList, Edit _ | ReturnToList, View _ ->
        state, Cmd.navigatePath("todos")
    | SaveClicked, Add todo ->
        todoStore <- Map.add (Guid.NewGuid()) todo todoStore
        
        state, Cmd.navigatePath("todos")
    | SaveClicked, Edit (todoId, todo) ->
        todoStore <- Map.add todoId todo todoStore
        
        state, Cmd.navigatePath("todos")
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
            Html.button [
                prop.onClick (fun _ -> DeleteClicked todoId |> dispatch)
                prop.text "Delete"
            ]
        ]
    ]

let todosView todos dispatch =
    Html.div [
        Html.button [
            prop.onClick (fun _ -> AddClicked |> dispatch)
            prop.text "Add Todo"
        ]
        Html.table [
            prop.children [
                for todoId, todo in Map.toList todos ->
                    todoRow todoId todo dispatch
            ]
        ]
    ]

let view state dispatch =
    match state.CurrentPage with
    | Todos todos ->
        todosView todos dispatch
    | Add todo -> editTodo todo dispatch
    | Edit (_, todo) -> editTodo todo dispatch
    | View todo -> viewTodo todo dispatch
    | NotFound -> Html.h1 "Not Found"