module App

open Elmish
open Elmish.React
open Feliz
open Extensions
open Thoth.Json
open Fable.SimpleHttp

open System

let rnd = Random()

let init() =
    { CurrentStories = Stories.New
      StoryItems = HasNotStartedYet
      TotalStoryItems = 0, []
      LoadedStoryItems = 0
      LoadingStoryItems = 0 }, Cmd.ofMsg (LoadStoryItems Started)

let storiesEndpoint stories =
  let fromBaseUrl = sprintf "https://hacker-news.firebaseio.com/v0/%sstories.json"
  match stories with
  | Stories.Best -> fromBaseUrl "best"
  | Stories.Top -> fromBaseUrl "top"
  | Stories.New -> fromBaseUrl "new"
  | Stories.Job -> fromBaseUrl "job"

let loadStoryItem (itemId: int) = async {
  let endpoint = sprintf "https://hacker-news.firebaseio.com/v0/item/%d.json" itemId
  let! (status, responseText) = Http.get endpoint
  do! Async.Sleep (rnd.Next 2000)
  
  match status with
  | HttpOk -> 
    match Decode.fromString itemDecoder responseText with
    | Ok storyItem -> return LoadedStoryItem (itemId, Ok storyItem)
    | Error error -> return LoadedStoryItem (itemId, Error error)
  | HttpError -> 
    return LoadedStoryItem (itemId, Error ("Error occured while loading " + string itemId))
}

let loadStoryIds stories = async {
  let endpoint = storiesEndpoint stories
  let! (status, responseText) = Http.get endpoint
  match status with
  | HttpOk -> 
    let storyIds = Decode.fromString (Decode.list Decode.int) responseText
    match storyIds with
    | Ok storyIds ->   
      return LoadStoryItems (Finished (Ok storyIds))

    | Error error ->
      return LoadStoryItems (Finished (Error error))
  
  | HttpError -> 
    return LoadStoryItems (Finished (Error responseText))
}

let update (msg: Msg) (state: State) =
  match msg with
  | ChangeStories stories -> 
    let nextState = 
      { state with 
          StoryItems = InProgress; CurrentStories = stories
          TotalStoryItems = 0, []
          LoadedStoryItems = 0
          LoadingStoryItems = 0 }

    let nextCmd = Cmd.fromAsync (loadStoryIds stories)
    nextState, nextCmd

  | LoadStoryItems Started ->
      let nextState = { state with StoryItems = InProgress; }
      let nextCmd = Cmd.fromAsync (loadStoryIds state.CurrentStories)
      nextState, nextCmd

  | LoadStoryItems (Finished (Ok storyIds)) ->
    let first10StoryIds = storyIds |> List.truncate 10
    let first10StoriesMap = Map.ofList [ for id in first10StoryIds -> id, Deferred.InProgress ]
    let nextState = 
      { state with 
          StoryItems = Resolved (Ok first10StoriesMap)
          TotalStoryItems = storyIds |> List.length, storyIds
          LoadedStoryItems = 10
          LoadingStoryItems = 10 }
     
    let nextCmd = Cmd.batch [ for id in first10StoryIds -> Cmd.fromAsync (loadStoryItem id) ] 
    nextState, nextCmd
  
  | LoadStoryItems (Finished (Error error)) ->
    let nextState = { state with StoryItems = Resolved (Error error) }
    nextState, Cmd.none

  | LoadedStoryItem (itemId, result) ->
    let loadingStoryItems = state.LoadingStoryItems
    match result with
    | Ok item ->
      match state.StoryItems with
      | Resolved (Ok storiesMap) ->
        let modifiedStoriesMap =
          storiesMap
          |> Map.remove itemId
          |> Map.add itemId (Resolved (Ok item))

        let nextState = 
          { state with 
              StoryItems = Resolved (Ok modifiedStoriesMap)
              LoadingStoryItems = loadingStoryItems - 1 }

        nextState, Cmd.none
    
      | _ ->
        state, Cmd.none
    | Error err -> 
      match state.StoryItems with 
      | Resolved (Ok storiesMap) ->
        let modifiedStoriesMap =
          storiesMap
          |> Map.remove itemId
          |> Map.add itemId (Resolved (Error err))
        
        let nextState = 
          { state with 
              StoryItems = Resolved (Ok modifiedStoriesMap) 
              LoadingStoryItems = loadingStoryItems - 1 }

        nextState, Cmd.none
      
      | _ ->
        state, Cmd.none
  
  | LoadMoreStoryItems ->
    let currentStories = 
      match state.StoryItems with
      | Resolved (Ok storiesMap) -> storiesMap
      | _ -> Map.empty

    let storyItemIds = state.TotalStoryItems |> snd
    let loadedStoryItems = state.LoadedStoryItems
    let nextStoryIds =
      storyItemIds
      |> List.skip loadedStoryItems
      |> List.truncate 10
    let nextStoriesMap = Map.ofList [ for id in nextStoryIds -> id, Deferred.InProgress ]
    let allStoriesMap = Map (Seq.concat [ Map.toSeq nextStoriesMap; Map.toSeq currentStories ])
    let nextState = 
      { state with 
          StoryItems = Resolved (Ok allStoriesMap)
          LoadedStoryItems = loadedStoryItems + 10
          LoadingStoryItems = 10  }
    
    let nextCmd = Cmd.batch [ for id in nextStoryIds -> Cmd.fromAsync (loadStoryItem id) ]
    nextState, nextCmd

let storyCatgories =  [
  Stories.New
  Stories.Top
  Stories.Best
  Stories.Job
]

let storiesName stories =
  match stories with
  | Stories.New -> "New"
  | Stories.Top -> "Top"
  | Stories.Best -> "Best"
  | Stories.Job -> "Job"

let renderTabs selectedStories dispatch =
  let switchStories stories =
    if selectedStories <> stories
    then dispatch (ChangeStories stories)

  Html.div [
    prop.className [ Bulma.Tabs; Bulma.IsToggle; Bulma.IsFullwidth ]
    prop.children [
      Html.ul [
        for stories in storyCatgories ->
        Html.li [
          prop.classes [ if selectedStories = stories then Bulma.IsActive ]
          prop.onClick (fun _ -> switchStories stories)
          prop.children [
            Html.a [ Html.span (storiesName stories) ]
          ]
        ]
      ]
    ]
  ]

let renderError (errorMsg: string) = 
  Html.h1 [
    prop.style [ style.color.red ]
    prop.text errorMsg
  ]

open Fable.DateFunctions

let printTimeAgo item =
  let now = DateTime.Now
  let date = DateTime.getFromUnixSeconds item.time
  let ago = now.FormatDistance date.DateTime
  ago |> string |> sprintf "%s ago"

let renderItemContent (item: HackernewsItem) = 
  match item.url with
  | Some url ->
    Html.div [
      prop.className [ Bulma.Columns; Bulma.IsMobile; Bulma.IsVcentered ]
      prop.children [
        Html.div [
          prop.className [ Bulma.Column; Bulma.IsNarrow; ]
          prop.children [
            Html.span [
              prop.className Bulma.IconText
              prop.children [
                Html.span [
                  prop.className Bulma.Icon
                  prop.children [
                    Html.i [
                      prop.className [ FA.Fas; FA.Fa2X; FA.FaPoll; ]
                    ]
                  ]
                ]

                Html.span item.score
              ]
            ]
          ]
        ]

        Html.div [
          prop.className [ Bulma.Column; ]
          prop.children [
            Html.a [
              prop.style [ style.textDecoration.underline ]
              prop.target.blank
              prop.href url
              prop.text item.title
            ]
          ]
        ]

        Html.div [
          prop.className [ Bulma.Column; Bulma.IsNarrow ]
          prop.children [
            Html.span (printTimeAgo item)
          ]
        ]
      ]
    ]
  | None ->
    Html.p item.title

let spinner = 
  Html.div [
    prop.style [ style.textAlign.center; style.marginTop 20 ]
    prop.children [
      Html.i [
        prop.className [ FA.Fa; FA.FaCog; FA.FaSpin; FA.Fa2X ]
      ]
    ]
  ]

let renderStoryItem (itemId: int) storyItem =
  let renderItem =
    match storyItem with
    | HasNotStartedYet -> Html.none
    | InProgress -> spinner
    | Resolved (Error error) -> renderError error
    | Resolved (Ok storyItem) -> renderItemContent storyItem
  
  Html.div[
    prop.className Bulma.Box
    prop.key itemId
    prop.style [ style.marginTop 15; style.marginBottom 15 ]
    prop.children [ renderItem ]
  ]

let byTime : (int * DeferredStoryItem) -> option<DateTime> =
  fun (id, x) -> 
    let sortOpt =
      match x with
      | Resolved (Ok item) ->
        Some (DateTime.getFromUnixSeconds item.time).DateTime
      | Resolved (Error err) ->
        None
      | _ -> None
      
    sortOpt
 
let renderStories items dispatch =
  match items with
  | HasNotStartedYet -> Html.none
  | InProgress -> spinner
  | Resolved (Error error) -> renderError error
  | Resolved (Ok items) ->
    items
    |> Map.toList
    |> List.sortByDescending byTime
    |> List.map (fun (id, storyItem) -> renderStoryItem id storyItem)
    |> Html.div

let title =
  Html.h1 [
    prop.className Bulma.Title
    prop.text "Elmish Hackernews"
  ]

let renderButton state dispatch =
  let areStoriesLoading = state.LoadingStoryItems > 0 
  let noMoreStoriesToLoad = (state.TotalStoryItems |> fst) - state.LoadedStoryItems <= 0

  Html.div [
    prop.hidden noMoreStoriesToLoad
    prop.children [
      Html.button [
        prop.disabled areStoriesLoading
        prop.className [ Bulma.Button; Bulma.IsPrimary ]
        prop.text "Load more"
        prop.onClick (fun _ -> dispatch LoadMoreStoryItems)
      ]
    ]
  ]

let render (state: State) (dispatch: Msg -> unit) =
  Html.div [
    prop.style [ style.padding 20 ]
    prop.children [
      title
      renderTabs state.CurrentStories dispatch
      renderStories state.StoryItems dispatch
      renderButton state dispatch
    ]
  ]    

Program.mkProgram init update render
|> Program.withReactSynchronous "elmish-app"
|> Program.run