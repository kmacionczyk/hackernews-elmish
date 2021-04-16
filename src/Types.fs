[<AutoOpen>]
module Types

// General purpose types, generic types

let (|HttpOk|HttpError|) status =
    match status with
    | 200 -> HttpOk
    | _ -> HttpError

type Deferred<'t> =
    | HasNotStartedYet
    | InProgress
    | Resolved of 't

type DeferredResult<'t> = Deferred<Result<'t, string>>

type AsyncOperationStatus<'t> =
    | Started
    | Finished of 't

// Domain types

[<RequireQualifiedAccess>]
type Stories =
    | New
    | Top
    | Best
    | Job

type HackernewsItem =
    { id: int
      title: string
      url: string option
      score: int
      time: int }

type DeferredStoryItem = DeferredResult<HackernewsItem>

// Json parsing
// Decode the single item
open Thoth.Json

let itemDecoder : Decoder<HackernewsItem> =
    Decode.object
        (fun fields ->
            { id = fields.Required.At [ "id" ] Decode.int
              title = fields.Required.At [ "title" ] Decode.string
              url = fields.Optional.At [ "url" ] Decode.string
              score = fields.Required.At [ "score" ] Decode.int
              time = fields.Required.At [ "time" ] Decode.int })

// UI related Types

type State =
    { CurrentStories: Stories
      StoryItems: DeferredResult<Map<int, DeferredStoryItem>>
      TotalStoryItems: int * List<int>
      LoadedStoryItems: int
      LoadingStoryItems: int }

type Msg =
    | ChangeStories of Stories
    | LoadStoryItems of AsyncOperationStatus<Result<int list, string>>
    | LoadedStoryItem of int * Result<HackernewsItem, string>
    | LoadMoreStoryItems
