namespace Feliz.CustomRouter

open Browser.Dom
open Browser.Types
open Elmish
open Fable.Core
open Fable.Core.JsInterop
open Feliz
open Feliz.UseListener
open System

type IUrlSearchParameters =
    abstract entries : unit -> seq<string array>

/// Determines whether the router will push a new entry to the history of modify the current one.
[<RequireQualifiedAccess>]
type HistoryMode =
    /// A new history will be added to the entries such that if the user clicks the back button,
    /// the previous page will be shown, this is the default bahavior of the router.
    | PushState = 1
    /// Only modifies the current history entry and does not add a new one to the history stack. Clicking the back button will *not* have the effect of retuning to the previous page.
    | ReplaceState = 2

/// Determines whether the router will use path or hash based routes
[<RequireQualifiedAccess>]
type RouteMode =
    | Hash = 1
    | Path = 2

[<RequireQualifiedAccess; System.ComponentModel.EditorBrowsable(System.ComponentModel.EditorBrowsableState.Never)>]
module Router =
    [<RequireQualifiedAccess>]
    module String =
        let (|Prefix|) (prefix: string) (str: string) =
            if str.StartsWith prefix then Some str
            else None

        let (|Suffix|) (suffix: string) (str: string) =
            if str.EndsWith suffix then Some str
            else None

        let inline split (sep: char) (str: string) =
            str.Split(sep)

    let inline hashPrefix str = "#/" + str
    let inline combine xs = String.concat "/" xs

    type RouterProps =
        abstract navigator: (string list * HistoryMode -> unit) option
        abstract onUrlChanged: (string list -> unit) option
        abstract application: ReactElement option
        abstract hashMode: RouteMode option

    [<Emit("encodeURIComponent($0)")>]
    let encodeURIComponent (value: string) : string = jsNative
    [<Emit("decodeURIComponent($0)")>]
    let decodeURIComponent (value: string) : string = jsNative

    let encodeQueryString queryStringPairs =
        queryStringPairs
        |> List.map (fun (key, value) ->
            String.concat "=" [ encodeURIComponent key; encodeURIComponent value ])
        |> String.concat "&"
        |> function
            | "" -> ""
            | pairs -> "?" + pairs

    let encodeQueryStringInts queryStringIntPairs =
        queryStringIntPairs
        |> List.map (fun (key, value: int) ->
            String.concat "=" [ encodeURIComponent key; unbox<string> value ])
        |> String.concat "&"
        |> function
            | "" -> ""
            | pairs -> "?" + pairs

    /// Safely returns tuple of list items without last one and last item
    let trySeparateLast xs =
        match xs |> List.rev with
        | [] -> None
        | [ single ] -> Some([], single)
        | list -> Some (list |> List.tail |> List.rev, list.Head)

    [<Emit("new URLSearchParams($0)")>]
    let createUrlSearchParams (queryString: string) : IUrlSearchParameters = jsNative

    [<Emit("window.navigator.userAgent")>]
    let navigatorUserAgent : string = jsNative

    type NavigationDetails =
        {
            Segments : string list
            HistoryMode : HistoryMode
        }
        
    let [<Literal>] customNavigationEventStarted = "CUSTOM_NAVIGATION_EVENT_STARTED"
    let [<Literal>] customNavigationEventFinished = "CUSTOM_NAVIGATION_EVENT_FINISHED"
    
    let private tokenizeUrl (path: string) =
        path
        |> String.split '/'
        |> List.ofArray
        |> List.collect (fun segment ->
            if String.IsNullOrWhiteSpace segment then []
            else
                let segment = segment.TrimEnd '#'

                match segment with
                | "?" -> []
                | String.Prefix "?" (Some _) -> [ segment ]
                | _ ->
                    match segment.Split [| '?' |] with
                    | [| value |] -> [ decodeURIComponent value ]
                    | [| value; "" |] -> [ decodeURIComponent value ]
                    | [| value; query |] -> [ decodeURIComponent value; "?" + query ]
                    | _ -> [])
    
    let private encodeUrlParts xs =
        xs
        |> List.map (fun (part: string) ->
            if part.Contains "?" || part.StartsWith "#" || part.StartsWith "/" then part
            else encodeURIComponent part)
        |> combine
    
    let requestNavigation xs mode =
        let details =
            jsOptions<CustomEventInit>(fun o ->
                o.detail <-
                    {
                        Segments = xs
                        HistoryMode = mode
                    })

        let e = Browser.Event.CustomEvent.Create(customNavigationEventStarted, details)

        window.dispatchEvent(e) |> ignore
    
    let performNavigation (mode: HistoryMode) encodedParts =
        if mode = HistoryMode.PushState
        then history.pushState ((), "", encodedParts)
        else history.replaceState((), "", encodedParts)

        let ev = document.createEvent("CustomEvent")

        ev.initEvent (customNavigationEventFinished, true, true)
        window.dispatchEvent ev |> ignore
    
    module PathRouting =
        let private (|AbsolutePath|RelativePath|EmptyPath|) xs =
            match xs with
            | head :: _ ->
                match head with
                | String.Prefix "/" (Some _) -> AbsolutePath
                | _ -> RelativePath
            | [] -> EmptyPath
        
        let private basePath () =
            match Option.ofObj (document.querySelector "base") with
            | Some e -> e.getAttribute "href"
            | None -> "/"
        
        let private fullPath () =
            window.location.pathname + window.location.search
        
        let private normalizeUrl =
            function
            | String.Prefix "/" (Some path) -> path
            | path -> "/" + path
        
        let private removeBaseSegments baseSegments allSegments =
            let rec f a b =
                match a, b with
                | headA :: tailA, headB :: tailB when headA = headB ->
                    f tailA tailB
                | _, _ -> b

            f baseSegments allSegments
        
        let private urlSegments (basePath: string) (path: string) =
            let baseSegments = tokenizeUrl basePath
            
            path
            |> tokenizeUrl
            |> removeBaseSegments baseSegments
      
        let private combineBasePath xs =
            let basePath = basePath ()
            
            let baseSegments = tokenizeUrl basePath
            
            match xs with
            | AbsolutePath -> xs
            | RelativePath -> baseSegments @ xs
            | EmptyPath -> baseSegments

        let currentPath () =
            (basePath(), fullPath())
            ||> urlSegments
                
        let encodeParts = combineBasePath >> encodeUrlParts >> normalizeUrl
        
    module HashRouting =
        let private normalizeUrl =
            function
            | String.Prefix "/" (Some path) -> "#" + path
            | String.Prefix "#/" (Some path) -> path
            | String.Prefix "#" (Some path) -> "#/" + path.Substring(1, path.Length - 1)
            | path -> "#/" + path
        
        let private urlSegments (path: string) =
            match path with
            | String.Prefix "#" (Some _) ->
                // remove the hash sign
                path.Substring(1, path.Length - 1)
            | String.Suffix "#" (Some _)
            | String.Suffix "#/" (Some _) -> ""
            | _ -> path
            |> tokenizeUrl
        
        let currentUrl () =
            window.location.hash
            |> urlSegments
        
        let encodeParts = encodeUrlParts >> normalizeUrl
    
    let router = React.memo(fun (input: RouterProps) ->
        let routeMode = Option.defaultValue RouteMode.Hash input.hashMode
        
        let onNavigate = React.useCallbackRef(fun (e: CustomEvent) ->
            let encodeParts =
                match routeMode with
                | RouteMode.Path -> PathRouting.encodeParts
                | _ -> HashRouting.encodeParts
            
            let defaultNavigator =
                fun (urlSegments, historyMode) ->
                    urlSegments
                    |> encodeParts
                    |> performNavigation historyMode
            
            let navigate = input.navigator |> Option.defaultValue defaultNavigator
            
            let detail = unbox<NavigationDetails> e.detail
            navigate (detail.Segments, detail.HistoryMode))
        
        let onChange = React.useCallbackRef(fun _ ->
            match routeMode with
            | RouteMode.Path -> PathRouting.currentPath ()
            | _ -> HashRouting.currentUrl ()
            |> Option.defaultValue ignore input.onUrlChanged
            )

        React.useWindowListener.on(customNavigationEventStarted, onNavigate)
        React.useWindowListener.on(customNavigationEventFinished, onChange)

        match input.application with
        | Some elem -> elem
        | None -> Html.none)

/// Defines a property for the `router` element
type IRouterProperty = interface end

[<AutoOpen>]
module ReactExtension =
    type React with
        /// Initializes the router as an element of the page and starts listening to URL changes.
        static member inline router (props: IRouterProperty list) =
            Router.router (unbox<Router.RouterProps> (createObj !!props))

[<Erase>]
type router =
    /// An event that is triggered when the URL in the address bar changes, whether by hand or programmatically using `Router.navigate(...)`.
    /// The event arguments are the parts of the URL, segmented into strings:
    ///
    /// `segment "#/" => [ ]`
    ///
    /// `segment "#/home" => [ "home" ]`
    ///
    /// `segment "#/home/settings" => [ "home"; "settings" ]`
    ///
    /// `segment "#/users/1" => [ "users"; "1" ]`
    ///
    /// `segment "#/users/1/details" => [ "users"; "1"; "details" ]`
    ///
    /// with query string parameters
    ///
    /// `segment "#/users?id=1" => [ "users"; "?id=1" ]`
    ///
    /// `segment "#/home/users?id=1" => [ "home"; "users"; "?id=1" ]`
    ///
    /// `segment "#/users?id=1&format=json" => [ "users"; "?id=1&format=json" ]`
    static member inline onUrlChanged (eventHandler: string list -> unit) : IRouterProperty = unbox ("onUrlChanged", eventHandler)

    /// The element that is rendered inside where the `router` is placed. Usually this contains the root application but it could also be part of another root element.
    ///
    /// It will keep listening for URL changes as long as the `router` is rendered on screen somewhere.
    static member inline children (element: ReactElement) : IRouterProperty = unbox ("application", element)

    /// The content that is rendered inside where the `router` is placed. Usually this contains the root application but it could also be part of another root element.
    ///
    /// It will keep listening for URL changes as long as the `router` is rendered on screen somewhere.
    static member inline children (elements: ReactElement list) : IRouterProperty = unbox ("application", React.fragment elements)

    /// Use # based routes (default)
    static member inline hashMode : IRouterProperty = unbox ("hashMode", RouteMode.Hash)

    /// Use full (HTML 5) based routes instead of # based.
    /// You have to be careful about which requests you want forwarded to the server and which ones should be handled locally.
    /// To keep the request local, you have to use the 'Router.navigate' function for all the URL transitions.
    static member inline pathMode : IRouterProperty = unbox ("hashMode", RouteMode.Path)
    
    static member inline navigator (eventHandler: string list * HistoryMode -> unit) : IRouterProperty = unbox ("navigator", eventHandler)

[<Erase>]
type Router =
    /// Parses the current URL of the page and returns the cleaned URL segments. This is default when working with hash URLs. When working with path-based URLs, use Router.currentPath() instead.
    static member inline currentUrl () =
        Router.HashRouting.currentUrl ()

    /// Parses the current URL of the page and returns the cleaned URL segments. This is default when working with path URLs. When working with hash-based (#) URLs, use Router.currentUrl() instead.
    static member inline currentPath () =
        Router.PathRouting.currentPath ()

    static member inline format([<ParamArray>] xs: string array) =
        Router.HashRouting.encodeParts (List.ofArray xs)

    static member inline format(xs: string list, queryString: (string * string) list) : string =
        xs
        |> Router.trySeparateLast
        |> Option.map (fun (r, l) -> Router.HashRouting.encodeParts (r @ [ l + Router.encodeQueryString queryString ]))
        |> Option.defaultWith (fun _ -> Router.HashRouting.encodeParts [ Router.encodeQueryString queryString ])

    static member inline format(segment: string, queryString: (string * string) list) : string =
        Router.HashRouting.encodeParts [ segment + Router.encodeQueryString queryString ]

    static member inline format(segment: string, queryString: (string * int) list) : string =
        Router.HashRouting.encodeParts [ segment + Router.encodeQueryStringInts queryString ]

    static member inline format(segment1: string, segment2: string, queryString: (string * string) list) : string =
        Router.HashRouting.encodeParts [ segment1; segment2 + Router.encodeQueryString queryString ]

    static member inline format(segment1: string, segment2: string, queryString: (string * int) list) : string =
        Router.HashRouting.encodeParts [ segment1; segment2 + Router.encodeQueryStringInts queryString ]

    static member inline format(segment1: string, segment2: string, segment3:int, queryString: (string * string) list) : string =
        Router.HashRouting.encodeParts [ segment1; segment2; unbox<string> segment3 + Router.encodeQueryString queryString ]

    static member inline format(segment1: string, segment2: string, segment3:int, queryString: (string * int) list) : string =
        Router.HashRouting.encodeParts [ segment1; segment2; unbox<string> segment3 + Router.encodeQueryStringInts queryString ]

    static member inline format(segment1: string, segment2: string, segment3:string, queryString: (string * string) list) : string =
        Router.HashRouting.encodeParts [ segment1; segment2; segment3 + Router.encodeQueryString queryString ]

    static member inline format(segment1: string, segment2: string, segment3:string, queryString: (string * int) list) : string =
        Router.HashRouting.encodeParts [ segment1; segment2; segment3 + Router.encodeQueryStringInts queryString ]

    static member inline format(segment1: string, segment2: int, segment3:string, queryString: (string * string) list) : string =
        Router.HashRouting.encodeParts [ segment1; unbox<string> segment2; segment3 + Router.encodeQueryString queryString ]

    static member inline format(segment1: string, segment2: int, segment3:string, queryString: (string * int) list) : string =
        Router.HashRouting.encodeParts [ segment1; unbox<string> segment2; segment3 + Router.encodeQueryStringInts queryString ]

    static member inline format(segment1: string, segment2: string, segment3:string, segment4: string, queryString: (string * string) list) : string =
        Router.HashRouting.encodeParts [ segment1; segment2; segment3; segment4 + Router.encodeQueryString queryString ]

    static member inline format(segment1: string, segment2: string, segment3:string, segment4: string, queryString: (string * int) list) : string =
        Router.HashRouting.encodeParts [ segment1; segment2; segment3; segment4 + Router.encodeQueryStringInts queryString ]

    static member inline format(segment1: string, segment2: string, segment3:string, segment4: string, segment5, queryString: (string * string) list) : string =
        Router.HashRouting.encodeParts [ segment1; segment2; segment3; segment4; segment5 + Router.encodeQueryString queryString ]

    static member inline format(segment1: string, segment2: string, segment3:string, segment4: string, segment5, queryString: (string * int) list) : string =
        Router.HashRouting.encodeParts [ segment1; segment2; segment3; segment4; segment5 + Router.encodeQueryStringInts queryString ]

    static member inline format(segment1: string, segment2: int, segment3:string, segment4: string, queryString: (string * string) list) : string =
        Router.HashRouting.encodeParts [ segment1; unbox<string> segment2; segment3; segment4 + Router.encodeQueryString queryString ]

    static member inline format(segment1: string, segment2: int, segment3:string, segment4: string, queryString: (string * int) list) : string =
        Router.HashRouting.encodeParts [ segment1; unbox<string> segment2; segment3; segment4 + Router.encodeQueryStringInts queryString ]

    static member inline format(segment1: string, segment2: int, segment3:int, segment4: string, queryString: (string * string) list) : string =
        Router.HashRouting.encodeParts [ segment1; unbox<string> segment2; unbox<string> segment3; segment4 + Router.encodeQueryString queryString ]

    static member inline format(segment1: string, segment2: int, segment3:int, segment4: string, queryString: (string * int) list) : string =
        Router.HashRouting.encodeParts [ segment1; unbox<string> segment2; unbox<string> segment3; segment4 + Router.encodeQueryStringInts queryString ]

    static member inline format(segment1: string, segment2: int, segment3:int, segment4: string, segment5: string, segment6: string, queryString: (string * string) list) : string =
        Router.HashRouting.encodeParts [ segment1; unbox<string> segment2; unbox<string> segment3; segment4; segment5; segment6 + Router.encodeQueryString queryString ]

    static member inline format(segment1: string, segment2: int, segment3:int, segment4: string, segment5: string, segment6: string, queryString: (string * int) list) : string =
        Router.HashRouting.encodeParts [ segment1; unbox<string> segment2; unbox<string> segment3; segment4; segment5; segment6 + Router.encodeQueryStringInts queryString ]

    static member inline format(segment1: string, segment2: int, segment3:int, segment4: int, segment5: string, queryString: (string * string) list) : string =
        Router.HashRouting.encodeParts [ segment1; unbox<string> segment2; unbox<string> segment3; unbox<string> segment4; segment5 + Router.encodeQueryString queryString ]

    static member inline format(segment1: string, segment2: int, segment3:int, segment4: int, segment5: string, queryString: (string * int) list) : string =
        Router.HashRouting.encodeParts [ segment1; unbox<string> segment2; unbox<string> segment3; unbox<string> segment4; segment5 + Router.encodeQueryStringInts queryString ]

    static member inline format(segment1: string, segment2: int, segment3:string, segment4: int, segment5: string, queryString: (string * string) list) : string =
        Router.HashRouting.encodeParts [ segment1; unbox<string> segment2; segment3; unbox<string> segment4; segment5 + Router.encodeQueryString queryString ]

    static member inline format(segment1: string, segment2: int, segment3:string, segment4: int, segment5: string, queryString: (string * int) list) : string =
        Router.HashRouting.encodeParts [ segment1; unbox<string> segment2; segment3; unbox<string> segment4; segment5 + Router.encodeQueryStringInts queryString ]

    static member inline format(segment1: string, segment2: int, segment3:string, segment4: string, segment5, queryString: (string * string) list) : string =
        Router.HashRouting.encodeParts [ segment1; unbox<string> segment2; segment3; segment4; segment5 + Router.encodeQueryString queryString ]

    static member inline format(segment1: string, segment2: int, segment3:string, segment4: string, segment5, queryString: (string * int) list) : string =
        Router.HashRouting.encodeParts [ segment1; unbox<string> segment2; segment3; segment4; segment5 + Router.encodeQueryStringInts queryString ]

    static member inline format(fullPath: string) : string =
        Router.HashRouting.encodeParts [ fullPath ]

    static member inline format(fullPath: string list) : string =
        Router.HashRouting.encodeParts fullPath

    static member inline format(segment: string, value: int) : string =
        Router.HashRouting.encodeParts [ segment; string value ]

    static member inline format(segment1: string, value1: int, value2: int) : string =
        Router.HashRouting.encodeParts [ segment1; string value1; string value2 ]

    static member inline format(segment1: string, segment2: string, value1: int) : string =
        Router.HashRouting.encodeParts [ segment1; segment2; string value1 ]

    static member inline format(segment1: string, value1: int, segment2: string) : string =
        Router.HashRouting.encodeParts [ segment1; string value1; segment2 ]

    static member inline format(segment1: string, value1: int, segment2: string, value2: int) : string =
        Router.HashRouting.encodeParts [ segment1; string value1; segment2; string value2 ]

    static member inline format(segment1: string, value1: int, segment2: string, value2: int, segment3: string) : string =
        Router.HashRouting.encodeParts [ segment1; string value1; segment2; string value2; segment3 ]

    static member inline format(segment1: string, value1: int, segment2: string, value2: int, segment3: string, segment4: string) : string =
        Router.HashRouting.encodeParts [ segment1; string value1; segment2; string value2; segment3; segment4 ]

    static member inline format(segment1: string, value1: int, segment2: string, value2: int, segment3: string, value3: int) : string =
        Router.HashRouting.encodeParts [ segment1; string value1; segment2; string value2; segment3; string value3 ]

    static member inline format(segment1: string, value1: int, value2: int, value3: int) : string =
        Router.HashRouting.encodeParts [ segment1; string value1; string value2; string value3 ]

    static member inline format(segment1: string, value1: int, value2: int, segment2: string) : string =
        Router.HashRouting.encodeParts [ segment1; string value1; string value2; segment2 ]


    static member inline navigate([<ParamArray>] xs: string array) =
        Router.requestNavigation (List.ofArray xs) HistoryMode.PushState

    static member inline navigate (xs: string list, queryString:(string * string) list) =
        xs
        |> Router.trySeparateLast
        |> Option.map (fun (r, l) -> Router.requestNavigation (r @ [ l + Router.encodeQueryString queryString ]) HistoryMode.PushState)
        |> Option.defaultWith (fun _ -> Router.requestNavigation [ Router.encodeQueryString queryString ] HistoryMode.PushState)

    static member inline navigate(segment: string, queryString: (string * string) list) =
        Router.requestNavigation [ segment + Router.encodeQueryString queryString ] HistoryMode.PushState

    static member inline navigate(segment: string, queryString: (string * string) list, mode: HistoryMode) =
        Router.requestNavigation [ segment + Router.encodeQueryString queryString ] mode

    static member inline navigate(segment: string, queryString: (string * int) list) =
        Router.requestNavigation [ segment + Router.encodeQueryStringInts queryString ] HistoryMode.PushState

    static member inline navigate(segment: string, queryString: (string * int) list, mode: HistoryMode) =
        Router.requestNavigation [ segment + Router.encodeQueryStringInts queryString ] mode

    static member inline navigate(segment1: string, segment2: string, queryString: (string * string) list) =
        Router.requestNavigation [ segment1; segment2 + Router.encodeQueryString queryString ] HistoryMode.PushState

    static member inline navigate(segment1: string, segment2: string, queryString: (string * string) list, mode: HistoryMode) =
        Router.requestNavigation [ segment1; segment2 + Router.encodeQueryString queryString ] mode

    static member inline navigate(segment1: string, segment2: string, queryString: (string * int) list) =
        Router.requestNavigation [ segment1; segment2 + Router.encodeQueryStringInts queryString ] HistoryMode.PushState

    static member inline navigate(segment1: string, segment2: string, queryString: (string * int) list, mode: HistoryMode) =
        Router.requestNavigation [ segment1; segment2 + Router.encodeQueryStringInts queryString ] mode

    static member inline navigate(segment1: string, segment2: string, segment3:int, queryString: (string * string) list) =
        Router.requestNavigation [ segment1; segment2; unbox<string> segment3 + Router.encodeQueryString queryString ] HistoryMode.PushState

    static member inline navigate(segment1: string, segment2: string, segment3:int, queryString: (string * string) list, mode: HistoryMode) =
        Router.requestNavigation [ segment1; segment2;string  segment3 + Router.encodeQueryString queryString ] mode

    static member inline navigate(segment1: string, segment2: string, segment3:int, queryString: (string * int) list) =
        Router.requestNavigation [ segment1; segment2; unbox<string> segment3 + Router.encodeQueryStringInts queryString ] HistoryMode.PushState

    static member inline navigate(segment1: string, segment2: string, segment3:int, queryString: (string * int) list, mode: HistoryMode) =
        Router.requestNavigation [ segment1; segment2; unbox<string> segment3 + Router.encodeQueryStringInts queryString ] mode

    static member inline navigate(segment1: string, segment2: string, segment3:string, queryString: (string * string) list) =
        Router.requestNavigation [ segment1; segment2; segment3 + Router.encodeQueryString queryString ] HistoryMode.PushState

    static member inline navigate(segment1: string, segment2: string, segment3:string, queryString: (string * string) list, mode: HistoryMode) =
        Router.requestNavigation [ segment1; segment2; segment3 + Router.encodeQueryString queryString ] mode

    static member inline navigate(segment1: string, segment2: string, segment3:string, queryString: (string * int) list) =
        Router.requestNavigation [ segment1; segment2; segment3 + Router.encodeQueryStringInts queryString ] HistoryMode.PushState

    static member inline navigate(segment1: string, segment2: string, segment3:string, queryString: (string * int) list, mode: HistoryMode) =
        Router.requestNavigation [ segment1; segment2; segment3 + Router.encodeQueryStringInts queryString ] mode

    static member inline navigate(segment1: string, segment2: int, segment3:string, queryString: (string * string) list) =
        Router.requestNavigation [ segment1; unbox<string> segment2; segment3 + Router.encodeQueryString queryString ] HistoryMode.PushState

    static member inline navigate(segment1: string, segment2: int, segment3:string, queryString: (string * string) list, mode: HistoryMode) =
        Router.requestNavigation [ segment1; unbox<string> segment2; segment3 + Router.encodeQueryString queryString ] mode

    static member inline navigate(segment1: string, segment2: int, segment3:string, queryString: (string * int) list) =
        Router.requestNavigation [ segment1; unbox<string> segment2; segment3 + Router.encodeQueryStringInts queryString ] HistoryMode.PushState

    static member inline navigate(segment1: string, segment2: int, segment3:string, queryString: (string * int) list, mode: HistoryMode) =
        Router.requestNavigation [ segment1; unbox<string> segment2; segment3 + Router.encodeQueryStringInts queryString ] mode

    static member inline navigate(segment1: string, segment2: string, segment3:string, segment4: string, queryString: (string * string) list) =
        Router.requestNavigation [ segment1; segment2; segment3; segment4 + Router.encodeQueryString queryString ] HistoryMode.PushState

    static member inline navigate(segment1: string, segment2: string, segment3:string, segment4: string, queryString: (string * string) list, mode: HistoryMode) =
        Router.requestNavigation [ segment1; segment2; segment3; segment4 + Router.encodeQueryString queryString ] mode

    static member inline navigate(segment1: string, segment2: string, segment3:string, segment4: string, queryString: (string * int) list) =
        Router.requestNavigation [ segment1; segment2; segment3; segment4 + Router.encodeQueryStringInts queryString ] HistoryMode.PushState

    static member inline navigate(segment1: string, segment2: string, segment3:string, segment4: string, queryString: (string * int) list, mode: HistoryMode) =
        Router.requestNavigation [ segment1; segment2; segment3; segment4 + Router.encodeQueryStringInts queryString ] mode

    static member inline navigate(segment1: string, segment2: string, segment3:string, segment4: string, segment5, queryString: (string * string) list) =
        Router.requestNavigation [ segment1; segment2; segment3; segment4; segment5 + Router.encodeQueryString queryString ] HistoryMode.PushState

    static member inline navigate(segment1: string, segment2: string, segment3:string, segment4: string, segment5, queryString: (string * string) list, mode: HistoryMode) =
        Router.requestNavigation [ segment1; segment2; segment3; segment4; segment5 + Router.encodeQueryString queryString ] mode

    static member inline navigate(segment1: string, segment2: string, segment3:string, segment4: string, segment5, queryString: (string * int) list) =
        Router.requestNavigation [ segment1; segment2; segment3; segment4; segment5 + Router.encodeQueryStringInts queryString ] HistoryMode.PushState

    static member inline navigate(segment1: string, segment2: string, segment3:string, segment4: string, segment5, queryString: (string * int) list, mode: HistoryMode) =
        Router.requestNavigation [ segment1; segment2; segment3; segment4; segment5 + Router.encodeQueryStringInts queryString ] mode

    static member inline navigate(segment1: string, segment2: int, segment3:string, segment4: string, queryString: (string * string) list) =
        Router.requestNavigation [ segment1; unbox<string> segment2; segment3; segment4 + Router.encodeQueryString queryString ] HistoryMode.PushState

    static member inline navigate(segment1: string, segment2: int, segment3:string, segment4: string, queryString: (string * string) list, mode: HistoryMode) =
        Router.requestNavigation [ segment1; unbox<string> segment2; segment3; segment4 + Router.encodeQueryString queryString ] mode

    static member inline navigate(segment1: string, segment2: int, segment3:string, segment4: string, queryString: (string * int) list) =
        Router.requestNavigation [ segment1; unbox<string> segment2; segment3; segment4 + Router.encodeQueryStringInts queryString ] HistoryMode.PushState

    static member inline navigate(segment1: string, segment2: int, segment3:string, segment4: string, queryString: (string * int) list, mode: HistoryMode) =
        Router.requestNavigation [ segment1; unbox<string> segment2; segment3; segment4 + Router.encodeQueryStringInts queryString ] mode

    static member inline navigate(segment1: string, segment2: int, segment3:int, segment4: string, queryString: (string * string) list) =
        Router.requestNavigation [ segment1; unbox<string> segment2; unbox<string> segment3; segment4 + Router.encodeQueryString queryString ] HistoryMode.PushState

    static member inline navigate(segment1: string, segment2: int, segment3:int, segment4: string, queryString: (string * string) list, mode: HistoryMode) =
        Router.requestNavigation [ segment1; unbox<string> segment2; unbox<string> segment3; segment4 + Router.encodeQueryString queryString ] mode

    static member inline navigate(segment1: string, segment2: int, segment3:int, segment4: string, queryString: (string * int) list) =
        Router.requestNavigation [ segment1; unbox<string> segment2; unbox<string> segment3; segment4 + Router.encodeQueryStringInts queryString ] HistoryMode.PushState

    static member inline navigate(segment1: string, segment2: int, segment3:int, segment4: string, queryString: (string * int) list, mode: HistoryMode) =
        Router.requestNavigation [ segment1; unbox<string> segment2; unbox<string> segment3; segment4 + Router.encodeQueryStringInts queryString ] mode

    static member inline navigate(segment1: string, segment2: int, segment3:int, segment4: string, segment5: string, segment6: string, queryString: (string * string) list) =
        Router.requestNavigation [ segment1; unbox<string> segment2; unbox<string> segment3; segment4; segment5; segment6 + Router.encodeQueryString queryString ] HistoryMode.PushState

    static member inline navigate(segment1: string, segment2: int, segment3:int, segment4: string, segment5: string, segment6: string, queryString: (string * string) list, mode: HistoryMode) =
        Router.requestNavigation [ segment1; unbox<string> segment2; unbox<string> segment3; segment4; segment5; segment6 + Router.encodeQueryString queryString ] mode

    static member inline navigate(segment1: string, segment2: int, segment3:int, segment4: string, segment5: string, segment6: string, queryString: (string * int) list) =
        Router.requestNavigation [ segment1; unbox<string> segment2; unbox<string> segment3; segment4; segment5; segment6 + Router.encodeQueryStringInts queryString ] HistoryMode.PushState

    static member inline navigate(segment1: string, segment2: int, segment3:int, segment4: string, segment5: string, segment6: string, queryString: (string * int) list, mode: HistoryMode) =
        Router.requestNavigation [ segment1; unbox<string> segment2; unbox<string> segment3; segment4; segment5; segment6 + Router.encodeQueryStringInts queryString ] mode

    static member inline navigate(segment1: string, segment2: int, segment3:int, segment4: int, segment5: string, queryString: (string * string) list) =
        Router.requestNavigation [ segment1; unbox<string> segment2; unbox<string> segment3; unbox<string> segment4; segment5 + Router.encodeQueryString queryString ] HistoryMode.PushState

    static member inline navigate(segment1: string, segment2: int, segment3:int, segment4: int, segment5: string, queryString: (string * string) list, mode: HistoryMode) =
        Router.requestNavigation [ segment1; unbox<string> segment2; unbox<string> segment3; unbox<string> segment4; segment5 + Router.encodeQueryString queryString ] mode

    static member inline navigate(segment1: string, segment2: int, segment3:int, segment4: int, segment5: string, queryString: (string * int) list) =
        Router.requestNavigation [ segment1; unbox<string> segment2; unbox<string> segment3; unbox<string> segment4; segment5 + Router.encodeQueryStringInts queryString ] HistoryMode.PushState

    static member inline navigate(segment1: string, segment2: int, segment3:int, segment4: int, segment5: string, queryString: (string * int) list, mode: HistoryMode) =
        Router.requestNavigation [ segment1; unbox<string> segment2; unbox<string> segment3; unbox<string> segment4; segment5 + Router.encodeQueryStringInts queryString ] mode

    static member inline navigate(segment1: string, segment2: int, segment3:string, segment4: int, segment5: string, queryString: (string * string) list) =
        Router.requestNavigation [ segment1; unbox<string> segment2; segment3; unbox<string> segment4; segment5 + Router.encodeQueryString queryString ] HistoryMode.PushState

    static member inline navigate(segment1: string, segment2: int, segment3:string, segment4: int, segment5: string, queryString: (string * string) list, mode: HistoryMode) =
        Router.requestNavigation [ segment1; unbox<string> segment2; segment3; unbox<string> segment4; segment5 + Router.encodeQueryString queryString ] mode

    static member inline navigate(segment1: string, segment2: int, segment3:string, segment4: int, segment5: string, queryString: (string * int) list) =
        Router.requestNavigation [ segment1; unbox<string> segment2; segment3; unbox<string> segment4; segment5 + Router.encodeQueryStringInts queryString ] HistoryMode.PushState

    static member inline navigate(segment1: string, segment2: int, segment3:string, segment4: int, segment5: string, queryString: (string * int) list, mode: HistoryMode) =
        Router.requestNavigation [ segment1; unbox<string> segment2; segment3; unbox<string> segment4; segment5 + Router.encodeQueryStringInts queryString ] mode

    static member inline navigate(segment1: string, segment2: int, segment3:string, segment4: string, segment5, queryString: (string * string) list) =
        Router.requestNavigation [ segment1; unbox<string> segment2; segment3; segment4; segment5 + Router.encodeQueryString queryString ] HistoryMode.PushState

    static member inline navigate(segment1: string, segment2: int, segment3:string, segment4: string, segment5, queryString: (string * string) list, mode: HistoryMode) =
        Router.requestNavigation [ segment1; unbox<string> segment2; segment3; segment4; segment5 + Router.encodeQueryString queryString ] mode

    static member inline navigate(segment1: string, segment2: int, segment3:string, segment4: string, segment5, queryString: (string * int) list) =
        Router.requestNavigation [ segment1; unbox<string> segment2; segment3; segment4; segment5 + Router.encodeQueryStringInts queryString ] HistoryMode.PushState

    static member inline navigate(segment1: string, segment2: int, segment3:string, segment4: string, segment5, queryString: (string * int) list, mode: HistoryMode) =
        Router.requestNavigation [ segment1; unbox<string> segment2; segment3; segment4; segment5 + Router.encodeQueryStringInts queryString ] mode

    static member inline navigate(fullPath: string) =
        Router.requestNavigation [ fullPath ] HistoryMode.PushState

    static member inline navigate(fullPath: string, mode: HistoryMode) =
        Router.requestNavigation [ fullPath ] mode

    static member inline navigate(segment: string, value: int) =
        Router.requestNavigation [ segment; string value ] HistoryMode.PushState

    static member inline navigate(segment: string, value: int, mode: HistoryMode) =
        Router.requestNavigation [ segment; string value ] mode

    static member inline navigate(segment1: string, value1: int, value2: int) =
        Router.requestNavigation [ segment1; string value1; string value2 ] HistoryMode.PushState

    static member inline navigate(segment1: string, value1: int, value2: int, mode: HistoryMode) =
        Router.requestNavigation [ segment1; string value1; string value2 ] mode

    static member inline navigate(segment1: string, segment2: string, value1: int) =
        Router.requestNavigation [ segment1; segment2; string value1 ] HistoryMode.PushState

    static member inline navigate(segment1: string, segment2: string, value1: int, mode: HistoryMode) =
        Router.requestNavigation [ segment1; segment2; string value1 ] mode

    static member inline navigate(segment1: string, value1: int, segment2: string) =
        Router.requestNavigation [ segment1; string value1; segment2 ] HistoryMode.PushState

    static member inline navigate(segment1: string, value1: int, segment2: string, mode: HistoryMode) =
        Router.requestNavigation [ segment1; string value1; segment2 ] mode

    static member inline navigate(segment1: string, value1: int, segment2: string, value2: int) =
        Router.requestNavigation [ segment1; string value1; segment2; string value2 ] HistoryMode.PushState

    static member inline navigate(segment1: string, value1: int, segment2: string, value2: int, mode: HistoryMode) =
        Router.requestNavigation [ segment1; string value1; segment2; string value2 ] mode

    static member inline navigate(segment1: string, value1: int, segment2: string, value2: int, segment3: string) =
        Router.requestNavigation [ segment1; string value1; segment2; string value2; segment3 ] HistoryMode.PushState

    static member inline navigate(segment1: string, value1: int, segment2: string, value2: int, segment3: string, mode: HistoryMode) =
        Router.requestNavigation [ segment1; string value1; segment2; string value2; segment3 ] mode

    static member inline navigate(segment1: string, value1: int, segment2: string, value2: int, segment3: string, segment4: string) =
        Router.requestNavigation [ segment1; string value1; segment2; string value2; segment3; segment4 ] HistoryMode.PushState

    static member inline navigate(segment1: string, value1: int, segment2: string, value2: int, segment3: string, segment4: string, mode: HistoryMode) =
        Router.requestNavigation [ segment1; string value1; segment2; string value2; segment3; segment4 ] mode

    static member inline navigate(segment1: string, value1: int, segment2: string, value2: int, segment3: string, value3: int) =
        Router.requestNavigation [ segment1; string value1; segment2; string value2; segment3; string value3 ] HistoryMode.PushState

    static member inline navigate(segment1: string, value1: int, segment2: string, value2: int, segment3: string, value3: int, mode: HistoryMode) =
        Router.requestNavigation [ segment1; string value1; segment2; string value2; segment3; string value3 ] mode

    static member inline navigate(segment1: string, value1: int, value2: int, value3: int) =
        Router.requestNavigation [ segment1; string value1; string value2; string value3 ] HistoryMode.PushState

    static member inline navigate(segment1: string, value1: int, value2: int, value3: int, mode: HistoryMode) =
        Router.requestNavigation [ segment1; string value1; string value2; string value3 ] mode

    static member inline navigate(segment1: string, value1: int, value2: int, segment2: string) =
        Router.requestNavigation [ segment1; string value1; string value2; segment2 ] HistoryMode.PushState

    static member inline navigate(segment1: string, value1: int, value2: int, segment2: string, mode: HistoryMode) =
        Router.requestNavigation [ segment1; string value1; string value2; segment2 ] mode

    static member inline navigateBack() = history.back()
    static member inline navigateBack(n: int) = history.go(-n)
    static member inline navigateForward(n: int) = history.go(n)

    static member inline formatPath([<ParamArray>] xs: string array) =
        Router.PathRouting.encodeParts (List.ofArray xs)

    static member inline formatPath(xs: string list, queryString: (string * string) list) : string =
        xs
        |> Router.trySeparateLast
        |> Option.map (fun (r, l) -> Router.PathRouting.encodeParts (r @ [ l + Router.encodeQueryString queryString ]))
        |> Option.defaultWith (fun _ -> Router.PathRouting.encodeParts [ Router.encodeQueryString queryString ])

    static member inline formatPath(segment: string, queryString: (string * string) list) : string =
        Router.PathRouting.encodeParts [ segment + Router.encodeQueryString queryString ]

    static member inline formatPath(segment: string, queryString: (string * int) list) : string =
        Router.PathRouting.encodeParts [ segment + Router.encodeQueryStringInts queryString ]

    static member inline formatPath(segment1: string, segment2: string, queryString: (string * string) list) : string =
        Router.PathRouting.encodeParts [ segment1; segment2 + Router.encodeQueryString queryString ]

    static member inline formatPath(segment1: string, segment2: string, queryString: (string * int) list) : string =
        Router.PathRouting.encodeParts [ segment1; segment2 + Router.encodeQueryStringInts queryString ]

    static member inline formatPath(segment1: string, segment2: string, segment3:int, queryString: (string * string) list) : string =
        Router.PathRouting.encodeParts [ segment1; segment2; unbox<string> segment3 + Router.encodeQueryString queryString ]

    static member inline formatPath(segment1: string, segment2: string, segment3:int, queryString: (string * int) list) : string =
        Router.PathRouting.encodeParts [ segment1; segment2; unbox<string> segment3 + Router.encodeQueryStringInts queryString ]

    static member inline formatPath(segment1: string, segment2: string, segment3:string, queryString: (string * string) list) : string =
        Router.PathRouting.encodeParts [ segment1; segment2; segment3 + Router.encodeQueryString queryString ]

    static member inline formatPath(segment1: string, segment2: string, segment3:string, queryString: (string * int) list) : string =
        Router.PathRouting.encodeParts [ segment1; segment2; segment3 + Router.encodeQueryStringInts queryString ]

    static member inline formatPath(segment1: string, segment2: int, segment3:string, queryString: (string * string) list) : string =
        Router.PathRouting.encodeParts [ segment1; unbox<string> segment2; segment3 + Router.encodeQueryString queryString ]

    static member inline formatPath(segment1: string, segment2: int, segment3:string, queryString: (string * int) list) : string =
        Router.PathRouting.encodeParts [ segment1; unbox<string> segment2; segment3 + Router.encodeQueryStringInts queryString ]

    static member inline formatPath(segment1: string, segment2: string, segment3:string, segment4: string, queryString: (string * string) list) : string =
        Router.PathRouting.encodeParts [ segment1; segment2; segment3; segment4 + Router.encodeQueryString queryString ]

    static member inline formatPath(segment1: string, segment2: string, segment3:string, segment4: string, queryString: (string * int) list) : string =
        Router.PathRouting.encodeParts [ segment1; segment2; segment3; segment4 + Router.encodeQueryStringInts queryString ]

    static member inline formatPath(segment1: string, segment2: string, segment3:string, segment4: string, segment5, queryString: (string * string) list) : string =
        Router.PathRouting.encodeParts [ segment1; segment2; segment3; segment4; segment5 + Router.encodeQueryString queryString ]

    static member inline formatPath(segment1: string, segment2: string, segment3:string, segment4: string, segment5, queryString: (string * int) list) : string =
        Router.PathRouting.encodeParts [ segment1; segment2; segment3; segment4; segment5 + Router.encodeQueryStringInts queryString ]

    static member inline formatPath(segment1: string, segment2: int, segment3:string, segment4: string, queryString: (string * string) list) : string =
        Router.PathRouting.encodeParts [ segment1; unbox<string> segment2; segment3; segment4 + Router.encodeQueryString queryString ]

    static member inline formatPath(segment1: string, segment2: int, segment3:string, segment4: string, queryString: (string * int) list) : string =
        Router.PathRouting.encodeParts [ segment1; unbox<string> segment2; segment3; segment4 + Router.encodeQueryStringInts queryString ]

    static member inline formatPath(segment1: string, segment2: int, segment3:int, segment4: string, queryString: (string * string) list) : string =
        Router.PathRouting.encodeParts [ segment1; unbox<string> segment2; unbox<string> segment3; segment4 + Router.encodeQueryString queryString ]

    static member inline formatPath(segment1: string, segment2: int, segment3:int, segment4: string, queryString: (string * int) list) : string =
        Router.PathRouting.encodeParts [ segment1; unbox<string> segment2; unbox<string> segment3; segment4 + Router.encodeQueryStringInts queryString ]

    static member inline formatPath(segment1: string, segment2: int, segment3:int, segment4: string, segment5: string, segment6: string, queryString: (string * string) list) : string =
        Router.PathRouting.encodeParts [ segment1; unbox<string> segment2; unbox<string> segment3; segment4; segment5; segment6 + Router.encodeQueryString queryString ]

    static member inline formatPath(segment1: string, segment2: int, segment3:int, segment4: string, segment5: string, segment6: string, queryString: (string * int) list) : string =
        Router.PathRouting.encodeParts [ segment1; unbox<string> segment2; unbox<string> segment3; segment4; segment5; segment6 + Router.encodeQueryStringInts queryString ]

    static member inline formatPath(segment1: string, segment2: int, segment3:int, segment4: int, segment5: string, queryString: (string * string) list) : string =
        Router.PathRouting.encodeParts [ segment1; unbox<string> segment2; unbox<string> segment3; unbox<string> segment4; segment5 + Router.encodeQueryString queryString ]

    static member inline formatPath(segment1: string, segment2: int, segment3:int, segment4: int, segment5: string, queryString: (string * int) list) : string =
        Router.PathRouting.encodeParts [ segment1; unbox<string> segment2; unbox<string> segment3; unbox<string> segment4; segment5 + Router.encodeQueryStringInts queryString ]

    static member inline formatPath(segment1: string, segment2: int, segment3:string, segment4: int, segment5: string, queryString: (string * string) list) : string =
        Router.PathRouting.encodeParts [ segment1; unbox<string> segment2; segment3; unbox<string> segment4; segment5 + Router.encodeQueryString queryString ]

    static member inline formatPath(segment1: string, segment2: int, segment3:string, segment4: int, segment5: string, queryString: (string * int) list) : string =
        Router.PathRouting.encodeParts [ segment1; unbox<string> segment2; segment3; unbox<string> segment4; segment5 + Router.encodeQueryStringInts queryString ]

    static member inline formatPath(segment1: string, segment2: int, segment3:string, segment4: string, segment5, queryString: (string * string) list) : string =
        Router.PathRouting.encodeParts [ segment1; unbox<string> segment2; segment3; segment4; segment5 + Router.encodeQueryString queryString ]

    static member inline formatPath(segment1: string, segment2: int, segment3:string, segment4: string, segment5, queryString: (string * int) list) : string =
        Router.PathRouting.encodeParts [ segment1; unbox<string> segment2; segment3; segment4; segment5 + Router.encodeQueryStringInts queryString ]

    static member inline formatPath(fullPath: string) : string =
        Router.PathRouting.encodeParts [ fullPath ]

    static member inline formatPath(fullPath: string list) : string =
        Router.PathRouting.encodeParts fullPath

    static member inline formatPath(segment: string, value: int) : string =
        Router.PathRouting.encodeParts [ segment; string value ]

    static member inline formatPath(segment1: string, value1: int, value2: int) : string =
        Router.PathRouting.encodeParts [ segment1; string value1; string value2 ]

    static member inline formatPath(segment1: string, segment2: string, value1: int) : string =
        Router.PathRouting.encodeParts [ segment1; segment2; string value1 ]

    static member inline formatPath(segment1: string, value1: int, segment2: string) : string =
        Router.PathRouting.encodeParts [ segment1; string value1; segment2 ]

    static member inline formatPath(segment1: string, value1: int, segment2: string, value2: int) : string =
        Router.PathRouting.encodeParts [ segment1; string value1; segment2; string value2 ]

    static member inline formatPath(segment1: string, value1: int, segment2: string, value2: int, segment3: string) : string =
        Router.PathRouting.encodeParts [ segment1; string value1; segment2; string value2; segment3 ]

    static member inline formatPath(segment1: string, value1: int, segment2: string, value2: int, segment3: string, segment4: string) : string =
        Router.PathRouting.encodeParts [ segment1; string value1; segment2; string value2; segment3; segment4 ]

    static member inline formatPath(segment1: string, value1: int, segment2: string, value2: int, segment3: string, value3: int) : string =
        Router.PathRouting.encodeParts [ segment1; string value1; segment2; string value2; segment3; string value3 ]

    static member inline formatPath(segment1: string, value1: int, value2: int, value3: int) : string =
        Router.PathRouting.encodeParts [ segment1; string value1; string value2; string value3 ]

    static member inline formatPath(segment1: string, value1: int, value2: int, segment2: string) : string =
        Router.PathRouting.encodeParts [ segment1; string value1; string value2; segment2 ]

[<Erase>]
type Cmd =
    static member inline navigateBack() : Cmd<_> =
        Cmd.ofSub (fun _ -> history.back())
    static member inline navigateBack(n: int) : Cmd<_> =
        Cmd.ofSub (fun _ -> history.go(-n))
    static member inline navigateForward(n: int) : Cmd<_> =
        Cmd.ofSub (fun _ -> history.go(n))
    static member inline navigate([<ParamArray>] xs: string array) : Cmd<_> =
        Cmd.ofSub (fun _ -> Router.navigate(xs))

    static member inline navigate(xs: string list, queryString: (string * string) list) : Cmd<_> =
        Cmd.ofSub (fun _ -> Router.navigate(xs, queryString))

    static member inline navigate(segment: string, queryString: (string * string) list) : Cmd<'Msg> =
        Cmd.ofSub (fun _ -> Router.navigate(segment, queryString))

    static member inline navigate(segment: string, queryString: (string * string) list, mode: HistoryMode) : Cmd<'Msg> =
        Cmd.ofSub (fun _ -> Router.navigate(segment, queryString, mode))

    static member inline navigate(segment: string, queryString: (string * int) list) : Cmd<'Msg> =
        Cmd.ofSub (fun _ -> Router.navigate(segment, queryString))

    static member inline navigate(segment: string, queryString: (string * int) list, mode: HistoryMode) : Cmd<'Msg> =
        Cmd.ofSub (fun _ -> Router.navigate(segment, queryString, mode))

    static member inline navigate(segment1: string, segment2: string, queryString: (string * string) list) : Cmd<'Msg> =
        Cmd.ofSub (fun _ -> Router.navigate(segment1, segment2 , queryString))

    static member inline navigate(segment1: string, segment2: string, queryString: (string * string) list, mode: HistoryMode) : Cmd<'Msg> =
        Cmd.ofSub (fun _ -> Router.navigate(segment1, segment2 , queryString, mode))

    static member inline navigate(segment1: string, segment2: string, queryString: (string * int) list) : Cmd<'Msg> =
        Cmd.ofSub (fun _ -> Router.navigate(segment1, segment2 , queryString))

    static member inline navigate(segment1: string, segment2: string, queryString: (string * int) list, mode: HistoryMode) : Cmd<'Msg> =
        Cmd.ofSub (fun _ -> Router.navigate(segment1, segment2 , queryString, mode))

    static member inline navigate(segment1: string, segment2: string, segment3:int, queryString: (string * string) list) : Cmd<'Msg> =
        Cmd.ofSub (fun _ -> Router.navigate(segment1, segment2, unbox<string> segment3 , queryString))

    static member inline navigate(segment1: string, segment2: string, segment3:int, queryString: (string * string) list, mode: HistoryMode) : Cmd<'Msg> =
        Cmd.ofSub (fun _ -> Router.navigate(segment1, segment2,string  segment3 , queryString, mode))

    static member inline navigate(segment1: string, segment2: string, segment3:int, queryString: (string * int) list) : Cmd<'Msg> =
        Cmd.ofSub (fun _ -> Router.navigate(segment1, segment2, unbox<string> segment3 , queryString))

    static member inline navigate(segment1: string, segment2: string, segment3:int, queryString: (string * int) list, mode: HistoryMode) : Cmd<'Msg> =
        Cmd.ofSub (fun _ -> Router.navigate(segment1, segment2, unbox<string> segment3 , queryString, mode))

    static member inline navigate(segment1: string, segment2: string, segment3:string, queryString: (string * string) list) : Cmd<'Msg> =
        Cmd.ofSub (fun _ -> Router.navigate(segment1, segment2, segment3 , queryString))

    static member inline navigate(segment1: string, segment2: string, segment3:string, queryString: (string * string) list, mode: HistoryMode) : Cmd<'Msg> =
        Cmd.ofSub (fun _ -> Router.navigate(segment1, segment2, segment3 , queryString, mode))

    static member inline navigate(segment1: string, segment2: string, segment3:string, queryString: (string * int) list) : Cmd<'Msg> =
        Cmd.ofSub (fun _ -> Router.navigate(segment1, segment2, segment3 , queryString))

    static member inline navigate(segment1: string, segment2: string, segment3:string, queryString: (string * int) list, mode: HistoryMode) : Cmd<'Msg> =
        Cmd.ofSub (fun _ -> Router.navigate(segment1, segment2, segment3 , queryString, mode))

    static member inline navigate(segment1: string, segment2: int, segment3:string, queryString: (string * string) list) : Cmd<'Msg> =
        Cmd.ofSub (fun _ -> Router.navigate(segment1, unbox<string> segment2, segment3 , queryString))

    static member inline navigate(segment1: string, segment2: int, segment3:string, queryString: (string * string) list, mode: HistoryMode) : Cmd<'Msg> =
        Cmd.ofSub (fun _ -> Router.navigate(segment1, unbox<string> segment2, segment3 , queryString, mode))

    static member inline navigate(segment1: string, segment2: int, segment3:string, queryString: (string * int) list) : Cmd<'Msg> =
        Cmd.ofSub (fun _ -> Router.navigate(segment1, unbox<string> segment2, segment3 , queryString))

    static member inline navigate(segment1: string, segment2: int, segment3:string, queryString: (string * int) list, mode: HistoryMode) : Cmd<'Msg> =
        Cmd.ofSub (fun _ -> Router.navigate(segment1, unbox<string> segment2, segment3 , queryString, mode))

    static member inline navigate(segment1: string, segment2: string, segment3:string, segment4: string, queryString: (string * string) list) : Cmd<'Msg> =
        Cmd.ofSub (fun _ -> Router.navigate(segment1, segment2, segment3, segment4 , queryString))

    static member inline navigate(segment1: string, segment2: string, segment3:string, segment4: string, queryString: (string * string) list, mode: HistoryMode) : Cmd<'Msg> =
        Cmd.ofSub (fun _ -> Router.navigate(segment1, segment2, segment3, segment4 , queryString, mode))

    static member inline navigate(segment1: string, segment2: string, segment3:string, segment4: string, queryString: (string * int) list) : Cmd<'Msg> =
        Cmd.ofSub (fun _ -> Router.navigate(segment1, segment2, segment3, segment4 , queryString))

    static member inline navigate(segment1: string, segment2: string, segment3:string, segment4: string, queryString: (string * int) list, mode: HistoryMode) : Cmd<'Msg> =
        Cmd.ofSub (fun _ -> Router.navigate(segment1, segment2, segment3, segment4 , queryString, mode))

    static member inline navigate(segment1: string, segment2: string, segment3:string, segment4: string, segment5, queryString: (string * string) list) : Cmd<'Msg> =
        Cmd.ofSub (fun _ -> Router.navigate(segment1, segment2, segment3, segment4, segment5 , queryString))

    static member inline navigate(segment1: string, segment2: string, segment3:string, segment4: string, segment5, queryString: (string * string) list, mode: HistoryMode) : Cmd<'Msg> =
        Cmd.ofSub (fun _ -> Router.navigate(segment1, segment2, segment3, segment4, segment5 , queryString, mode))

    static member inline navigate(segment1: string, segment2: string, segment3:string, segment4: string, segment5, queryString: (string * int) list) : Cmd<'Msg> =
        Cmd.ofSub (fun _ -> Router.navigate(segment1, segment2, segment3, segment4, segment5 , queryString))

    static member inline navigate(segment1: string, segment2: string, segment3:string, segment4: string, segment5, queryString: (string * int) list, mode: HistoryMode) : Cmd<'Msg> =
        Cmd.ofSub (fun _ -> Router.navigate(segment1, segment2, segment3, segment4, segment5 , queryString, mode))

    static member inline navigate(segment1: string, segment2: int, segment3:string, segment4: string, queryString: (string * string) list) : Cmd<'Msg> =
        Cmd.ofSub (fun _ -> Router.navigate(segment1, segment2, segment3, segment4 , queryString))

    static member inline navigate(segment1: string, segment2: int, segment3:string, segment4: string, queryString: (string * string) list, mode: HistoryMode) : Cmd<'Msg> =
        Cmd.ofSub (fun _ -> Router.navigate(segment1, segment2, segment3, segment4 , queryString, mode))

    static member inline navigate(segment1: string, segment2: int, segment3:string, segment4: string, queryString: (string * int) list) : Cmd<'Msg> =
        Cmd.ofSub (fun _ -> Router.navigate(segment1, segment2, segment3, segment4 , queryString))

    static member inline navigate(segment1: string, segment2: int, segment3:string, segment4: string, queryString: (string * int) list, mode: HistoryMode) : Cmd<'Msg> =
        Cmd.ofSub (fun _ -> Router.navigate(segment1, segment2, segment3, segment4 , queryString, mode))

    static member inline navigate(segment1: string, segment2: int, segment3:int, segment4: string, queryString: (string * string) list) : Cmd<'Msg> =
        Cmd.ofSub (fun _ -> Router.navigate(segment1, segment2, segment3, segment4 , queryString))

    static member inline navigate(segment1: string, segment2: int, segment3:int, segment4: string, queryString: (string * string) list, mode: HistoryMode) : Cmd<'Msg> =
        Cmd.ofSub (fun _ -> Router.navigate(segment1, segment2, segment3, segment4 , queryString, mode))

    static member inline navigate(segment1: string, segment2: int, segment3:int, segment4: string, queryString: (string * int) list) : Cmd<'Msg> =
        Cmd.ofSub (fun _ -> Router.navigate(segment1, segment2, segment3, segment4 , queryString))

    static member inline navigate(segment1: string, segment2: int, segment3:int, segment4: string, queryString: (string * int) list, mode: HistoryMode) : Cmd<'Msg> =
        Cmd.ofSub (fun _ -> Router.navigate(segment1, segment2, segment3, segment4 , queryString, mode))

    static member inline navigate(segment1: string, segment2: int, segment3:int, segment4: string, segment5: string, segment6: string, queryString: (string * string) list) : Cmd<'Msg> =
        Cmd.ofSub (fun _ -> Router.navigate(segment1, segment2, segment3, segment4, segment5, segment6, queryString))

    static member inline navigate(segment1: string, segment2: int, segment3:int, segment4: string, segment5: string, segment6: string, queryString: (string * string) list, mode: HistoryMode) : Cmd<'Msg> =
        Cmd.ofSub (fun _ -> Router.navigate(segment1, segment2, segment3, segment4, segment5, segment6, queryString, mode))

    static member inline navigate(segment1: string, segment2: int, segment3:int, segment4: string, segment5: string, segment6: string, queryString: (string * int) list) : Cmd<'Msg> =
        Cmd.ofSub (fun _ -> Router.navigate(segment1, segment2, segment3, segment4, segment5, segment6, queryString))

    static member inline navigate(segment1: string, segment2: int, segment3:int, segment4: string, segment5: string, segment6: string, queryString: (string * int) list, mode: HistoryMode) : Cmd<'Msg> =
        Cmd.ofSub (fun _ -> Router.navigate(segment1, segment2, segment3, segment4, segment5, segment6, queryString, mode))

    static member inline navigate(segment1: string, segment2: int, segment3:int, segment4: int, segment5: string, queryString: (string * string) list) : Cmd<'Msg> =
        Cmd.ofSub (fun _ -> Router.navigate(segment1, segment2, segment3, segment4, segment5 , queryString))

    static member inline navigate(segment1: string, segment2: int, segment3:int, segment4: int, segment5: string, queryString: (string * string) list, mode: HistoryMode) : Cmd<'Msg> =
        Cmd.ofSub (fun _ -> Router.navigate(segment1, segment2, segment3, segment4, segment5 , queryString, mode))

    static member inline navigate(segment1: string, segment2: int, segment3:int, segment4: int, segment5: string, queryString: (string * int) list) : Cmd<'Msg> =
        Cmd.ofSub (fun _ -> Router.navigate(segment1, segment2, segment3, segment4, segment5 , queryString))

    static member inline navigate(segment1: string, segment2: int, segment3:int, segment4: int, segment5: string, queryString: (string * int) list, mode: HistoryMode) : Cmd<'Msg> =
        Cmd.ofSub (fun _ -> Router.navigate(segment1, segment2, segment3, segment4, segment5 , queryString, mode))

    static member inline navigate(segment1: string, segment2: int, segment3:string, segment4: int, segment5: string, queryString: (string * string) list) : Cmd<'Msg> =
        Cmd.ofSub (fun _ -> Router.navigate(segment1, segment2, segment3, segment4, segment5 , queryString))

    static member inline navigate(segment1: string, segment2: int, segment3:string, segment4: int, segment5: string, queryString: (string * string) list, mode: HistoryMode) : Cmd<'Msg> =
        Cmd.ofSub (fun _ -> Router.navigate(segment1, segment2, segment3, segment4, segment5 , queryString, mode))

    static member inline navigate(segment1: string, segment2: int, segment3:string, segment4: int, segment5: string, queryString: (string * int) list) : Cmd<'Msg> =
        Cmd.ofSub (fun _ -> Router.navigate(segment1, segment2, segment3, segment4, segment5 , queryString))

    static member inline navigate(segment1: string, segment2: int, segment3:string, segment4: int, segment5: string, queryString: (string * int) list, mode: HistoryMode) : Cmd<'Msg> =
        Cmd.ofSub (fun _ -> Router.navigate(segment1, segment2, segment3, segment4, segment5 , queryString, mode))

    static member inline navigate(segment1: string, segment2: int, segment3:string, segment4: string, segment5, queryString: (string * string) list) : Cmd<'Msg> =
        Cmd.ofSub (fun _ -> Router.navigate(segment1, segment2, segment3, segment4, segment5 , queryString))

    static member inline navigate(segment1: string, segment2: int, segment3:string, segment4: string, segment5, queryString: (string * string) list, mode: HistoryMode) : Cmd<'Msg> =
        Cmd.ofSub (fun _ -> Router.navigate(segment1, segment2, segment3, segment4, segment5 , queryString, mode))

    static member inline navigate(segment1: string, segment2: int, segment3:string, segment4: string, segment5, queryString: (string * int) list) : Cmd<'Msg> =
        Cmd.ofSub (fun _ -> Router.navigate(segment1, segment2, segment3, segment4, segment5 , queryString))

    static member inline navigate(segment1: string, segment2: int, segment3:string, segment4: string, segment5, queryString: (string * int) list, mode: HistoryMode) : Cmd<'Msg> =
        Cmd.ofSub (fun _ -> Router.navigate(segment1, segment2, segment3, segment4, segment5 , queryString, mode))

    static member inline navigate(fullPath: string) : Cmd<'Msg> =
        Cmd.ofSub (fun _ -> Router.navigate(fullPath))

    static member inline navigate(fullPath: string, mode: HistoryMode) : Cmd<'Msg> =
        Cmd.ofSub (fun _ -> Router.navigate(fullPath, mode))

    static member inline navigate(segment: string, value: int) : Cmd<'Msg> =
        Cmd.ofSub (fun _ -> Router.navigate(segment, value))

    static member inline navigate(segment: string, value: int, mode: HistoryMode) : Cmd<'Msg> =
        Cmd.ofSub (fun _ -> Router.navigate(segment, value, mode))

    static member inline navigate(segment1: string, value1: int, value2: int) : Cmd<'Msg> =
        Cmd.ofSub (fun _ -> Router.navigate(segment1, value1, value2))

    static member inline navigate(segment1: string, value1: int, value2: int, mode: HistoryMode) : Cmd<'Msg> =
        Cmd.ofSub (fun _ -> Router.navigate(segment1, value1, value2, mode))

    static member inline navigate(segment1: string, segment2: string, value1: int) : Cmd<'Msg> =
        Cmd.ofSub (fun _ -> Router.navigate(segment1, segment2, value1))

    static member inline navigate(segment1: string, segment2: string, value1: int, mode: HistoryMode) : Cmd<'Msg> =
        Cmd.ofSub (fun _ -> Router.navigate(segment1, segment2, value1, mode))

    static member inline navigate(segment1: string, value1: int, segment2: string) : Cmd<'Msg> =
        Cmd.ofSub (fun _ -> Router.navigate(segment1, value1, segment2))

    static member inline navigate(segment1: string, value1: int, segment2: string, mode: HistoryMode) : Cmd<'Msg> =
        Cmd.ofSub (fun _ -> Router.navigate(segment1, value1, segment2, mode))

    static member inline navigate(segment1: string, value1: int, segment2: string, value2: int) : Cmd<'Msg> =
        Cmd.ofSub (fun _ -> Router.navigate(segment1, value1, segment2, value2))

    static member inline navigate(segment1: string, value1: int, segment2: string, value2: int, mode: HistoryMode) : Cmd<'Msg> =
        Cmd.ofSub (fun _ -> Router.navigate(segment1, value1, segment2, value2, mode))

    static member inline navigate(segment1: string, value1: int, segment2: string, value2: int, segment3: string) : Cmd<'Msg> =
        Cmd.ofSub (fun _ -> Router.navigate(segment1, value1, segment2, value2, segment3))

    static member inline navigate(segment1: string, value1: int, segment2: string, value2: int, segment3: string, mode: HistoryMode) : Cmd<'Msg> =
        Cmd.ofSub (fun _ -> Router.navigate(segment1, value1, segment2, value2, segment3, mode))

    static member inline navigate(segment1: string, value1: int, segment2: string, value2: int, segment3: string, segment4: string) : Cmd<'Msg> =
        Cmd.ofSub (fun _ -> Router.navigate(segment1, value1, segment2, value2, segment3, segment4))

    static member inline navigate(segment1: string, value1: int, segment2: string, value2: int, segment3: string, segment4: string, mode: HistoryMode) : Cmd<'Msg> =
        Cmd.ofSub (fun _ -> Router.navigate(segment1, value1, segment2, value2, segment3, segment4, mode))

    static member inline navigate(segment1: string, value1: int, segment2: string, value2: int, segment3: string, value3: int) : Cmd<'Msg> =
        Cmd.ofSub (fun _ -> Router.navigate(segment1, value1, segment2, value2, segment3, value3))

    static member inline navigate(segment1: string, value1: int, segment2: string, value2: int, segment3: string, value3: int, mode: HistoryMode) : Cmd<'Msg> =
        Cmd.ofSub (fun _ -> Router.navigate(segment1, value1, segment2, value2, segment3, value3, mode))

    static member inline navigate(segment1: string, value1: int, value2: int, value3: int) : Cmd<'Msg> =
        Cmd.ofSub (fun _ -> Router.navigate(segment1, value1, value2, value3))

    static member inline navigate(segment1: string, value1: int, value2: int, value3: int, mode: HistoryMode) : Cmd<'Msg> =
        Cmd.ofSub (fun _ -> Router.navigate(segment1, value1, value2, value3, mode))

    static member inline navigate(segment1: string, value1: int, value2: int, segment2: string) : Cmd<'Msg> =
        Cmd.ofSub (fun _ -> Router.navigate(segment1, value1, value2, segment2))

    static member inline navigate(segment1: string, value1: int, value2: int, segment2: string, mode: HistoryMode) : Cmd<'Msg> =
        Cmd.ofSub (fun _ -> Router.navigate(segment1, value1, value2, segment2, mode))

module Route =
    let (|Int|_|) (input: string) =
        match Int32.TryParse input with
        | true, value -> Some value
        | _ -> None

    let (|Int64|_|) (input: string) =
        match Int64.TryParse input with
        | true, value -> Some value
        | _ -> None

    let (|Guid|_|) (input: string) =
        match Guid.TryParse input with
        | true, value -> Some value
        | _ -> None

    let (|Number|_|) (input: string) =
        match Double.TryParse input with
        | true, value -> Some value
        | _ -> None

    let (|Decimal|_|) (input: string) =
        match Decimal.TryParse input with
        | true, value -> Some value
        | _ -> None

    let (|Bool|_|) (input: string) =
        match input.ToLower() with
        | "1"|"true"  -> Some true
        | "0"|"false" -> Some false
        | "" -> Some true
        | _ -> None

    /// Used to parse the query string parameter of the route.
    ///
    /// For example to match against
    ///
    /// `/users?id={value}`
    ///
    /// You can pattern match:
    ///
    /// `[ "users"; Route.Query [ "id", value ] ] -> value`
    ///
    /// When `{value}` is an integer then you can pattern match:
    ///
    /// `[ "users"; Route.Query [ "id", Route.Int userId ] ] -> userId`
    let (|Query|_|) (input: string) =
        try
            let urlParams = Router.createUrlSearchParams input
            Some [ for entry in urlParams.entries() -> entry.[0], entry.[1] ]
        with
        | _ -> None