module App

printf "hello world from fable-elmish-todo"


open Fable.Core
open Fable.Import
open Elmish

type Todo =
    { task: string
      completed: bool
      id: int 
      isBeingEditing: bool}

type VisableType =
    | ALL
    | COMPLETED
    | ACTIVE

type Model =
    { todos: Todo list
      input: string
      visableType: VisableType }

// model
let init _ =
    { todos =
        [ { task = "Taste Javascript"
            completed = true
            id = 0 
            isBeingEditing = false }
          { task = "Buy a unicorn"
            completed = false
            id = 1 
            isBeingEditing = false }]
      input = ""
      visableType = ALL 
     }


type Msg =
    | InputTask of string
    | NewTodo of string
    | UpdateVisableType of VisableType
    | Destroy of int
    | EnterEditMode of int
    | ExitEditMode of int
    | Editing of int * string
    | Toggle of int
    | ToggleAll


// update
let update (msg: Msg) (model: Model) =
    match msg with
    | InputTask task ->
        printf "creating new todo: %s" task
        { model with input = task }

    | NewTodo task ->
        { model with
            input = ""
            todos =
                model.todos
                @ [ { task = model.input
                      completed = false
                      id = model.todos.Length
                      isBeingEditing = false } ] }

    | UpdateVisableType vt -> { model with visableType = vt }
    | Destroy id -> 
        let filteredTodos = model.todos |> List.filter (fun todo -> todo.id <> id)
        { model with todos = filteredTodos }
    | EnterEditMode id -> 
        { model with todos = model.todos |> List.map (fun todo -> if todo.id = id then { todo with isBeingEditing = true } else todo ) }
    | Editing (id, task) ->
        { model with todos = model.todos |> List.map (fun todo -> if todo.id = id then { todo with isBeingEditing = true; task = task} else todo )}
    | ExitEditMode id ->
          { model with todos = model.todos |> List.map (fun todo -> if todo.id = id then { todo with isBeingEditing = false } else todo )}
    | Toggle id ->
          { model with todos = model.todos |> List.map (fun todo -> if todo.id = id then { todo with completed = not todo.completed } else todo )}
    | ToggleAll -> 
          { model with todos = model.todos |> List.map (fun todo -> { todo with completed = true })}

// view
open Fable.React
open Fable.React.Props
open Fable.Core.JsInterop
open Elmish.React

let footerView =
    footer [ ClassName "info" ] [
        p [] [
            str "Double-click to edit a todo"
        ]
        p [] [
            str "Template by "
            a [ Href "http://sindresorhus.com" ] [
                str "Sindre Sorhus"
            ]
        ]
        p [] [
            str "Created by "
            a [ Href "http://todomvc.com" ] [
                str "you"
            ]
        ]
        p [] [
            str "Part  of "
            a [ Href "http://todomvc.com" ] [
                str "TodoMVC"
            ]
        ]
    ]


let filterTodos (visableType: VisableType) (todos: Todo list) =
    match visableType with
    | ALL -> todos
    | COMPLETED -> todos |> List.filter (fun todo -> todo.completed)
    | ACTIVE ->
        todos
        |> List.filter (fun todo -> not todo.completed)


let viewTodoList (model: Model) dispatch =
    model.todos
    |> filterTodos model.visableType
    |> List.map (fun todo ->
        ul [ ClassName "todo-list" ] [
            li [ classList [("completed", todo.completed); ("editing", todo.isBeingEditing)] ] [
                
                div [ ClassName "view" ] [
                    input [ ClassName "toggle"
                            OnChange (fun _ -> todo.id |> Toggle |> dispatch)
                            Type "checkbox"
                            Checked todo.completed ];

                    label [ OnDoubleClick (fun _ -> todo.id |> EnterEditMode |> dispatch ) ] [ str todo.task; ];

                    button [ ClassName "destroy"; OnClick (fun _ -> (todo.id |> Destroy |> dispatch))] []
                ]

                let dispatchUpdateTodoMsg = fun task ->
                    Editing (todo.id, task) |> dispatch
                
                input [ ClassName "edit";
                        OnChange (fun evt -> !!evt.target?value |> dispatchUpdateTodoMsg)
                        OnKeyDown (fun evt -> match evt.key with 
                                                | "Enter" | "Escape" -> todo.id |> ExitEditMode |> dispatch 
                                                | _ -> ())
                        OnBlur (fun evt -> todo.id |> ExitEditMode |> dispatch)
                        Value todo.task ]
            ]
        ])

let viewInput (model: Model) dispatch =
    header [ ClassName "header" ] [
        h1 [] [ str "todos" ]
        input [ ClassName "new-todo"
                Placeholder "What needs to be done?"
                Value model.input
                AutoFocus true
                OnChange (fun e -> !!e.target?value |> InputTask |> dispatch)
                OnKeyDown (fun e ->
                    if e.key = "Enter" then
                        (model.input |> NewTodo |> dispatch)) ]
    ]

let viewStatus (model: Model) dispatch = 
            footer [ ClassName "footer" ] [
                span [ ClassName "todo-count" ] [
                    let unCompleted = model.todos |> List.filter (fun todo -> not todo.completed) |> List.length
                    strong [] [ ofInt unCompleted ]
                    str " item left"
                ]
                ul [ ClassName "filters" ] [
                    let filterClasses vt =
                        if model.visableType = vt then
                            "selected"
                        else
                            ""

                    let visableTypeChange vt = vt |> UpdateVisableType |> dispatch

                    li [] [
                        a [ ClassName(filterClasses ALL)
                            OnClick(fun _ -> visableTypeChange ALL) ] [
                            str "All"
                        ]
                    ]

                    li [] [
                        a [ ClassName(filterClasses ACTIVE)
                            OnClick(fun _ -> visableTypeChange ACTIVE) ] [
                            str "Active"
                        ]
                    ]

                    li [] [
                        a [ ClassName(filterClasses COMPLETED)
                            OnClick(fun _ -> visableTypeChange COMPLETED) ] [
                            str "Completed"
                        ]
                    ]

                ]
            ]

let view (model: Model) dispatch =

    div [ ClassName "todomvc-wrapper" ] [
        section [ ClassName "todoapp" ] [

            viewInput model dispatch

            section
                [ ClassName "main" ]
                ([ input [ ClassName "toggle-all"
                           Type "checkbox"
                           OnChange (fun _ ->
                                        printfn "clicked"
                                        dispatch ToggleAll)
                           Checked  (model.todos |> List.forall (fun todo -> todo.completed))
                         ]
                   label [] [ str "Mark all as complete" ]

                 ]
                 @ (viewTodoList model dispatch))

            viewStatus model dispatch
        ]
        footerView

    ]



open Elmish.Debug

Program.mkSimple init update view
|> Program.withReactBatched "app"
|> Program.withDebugger
|> Program.run
