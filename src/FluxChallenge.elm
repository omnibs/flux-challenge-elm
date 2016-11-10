module FluxChallenge exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Html.App as Html
import Task
import Http
import Json.Decode as Json
import DarkJedi exposing (..)

-- MODEL

type Slot = Empty | Far DarkJedi | Nearby DarkJedi

type alias Model = { jedis : List Slot }

init : (Model, Cmd Msg)
init =
  (Model [Empty, Empty, Empty, Empty, Empty], fetchJedi Apprentice 3616)

-- UPDATE

type MasterApprentice = Master | Apprentice

type Msg
  = Up
  | Down
  | FetchSucceed MasterApprentice DarkJedi
  | FetchFail Http.Error

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Up ->
      case model.jedis of
        (Far apprentice :: _) ->
          pageUp model apprentice
        (Nearby apprentice :: _) ->
          pageUp model apprentice
        _ ->
          (model, Cmd.none)

    Down ->
      case (List.reverse model.jedis) of
        (Far master :: _) ->
          pageDown model master
        (Nearby master :: _) ->
          pageDown model master
        _ ->
          (model, Cmd.none)

    FetchSucceed masterApprentice jedi ->
      let
        jedis = updateList masterApprentice jedi model.jedis
        model = {model | jedis = jedis}
      in
      -- add jedi and make another call if necessary
      (model, fetchEmpty model.jedis masterApprentice jedi)

    FetchFail _ ->
      -- maybe call again?
      (model, Cmd.none)

pageDown : Model -> DarkJedi -> (Model, Cmd Msg)
pageDown model master =
  let
    jedis = (List.drop 2 model.jedis) ++ [Empty, Empty]
    new_model = {model | jedis = jedis}
  in
    case master.apprentice.id of
      Just id ->
        (new_model, fetchJedi Apprentice id)
      _ -> (model, Cmd.none)


pageUp : Model -> DarkJedi -> (Model, Cmd Msg)
pageUp model apprentice =
  let
    jedis = [Empty, Empty] ++ (List.take 3 model.jedis)
    new_model = {model | jedis = jedis}
  in
    case apprentice.master.id of
      Just id ->
        (new_model, fetchJedi Master id)
      _ -> (model, Cmd.none)

fetchEmpty : List Slot -> MasterApprentice -> DarkJedi -> Cmd Msg
fetchEmpty jedis masterApprentice jedi =
  case masterApprentice of
    Master ->
      case jedi.master.id of
        Just id ->
          case jedis of
            (Empty :: _) ->
              fetchJedi masterApprentice id
            _ -> Cmd.none
        _ -> Cmd.none
    Apprentice ->
      case jedi.apprentice.id of
        Just id ->
          case (List.reverse jedis) of
            (Empty :: _) ->
              fetchJedi masterApprentice id
            _ -> Cmd.none
        _ -> Cmd.none

updateList : MasterApprentice -> DarkJedi -> List Slot -> List Slot
updateList masterApprentice =
  case masterApprentice of
    Master ->
      replaceLastEmptyFromTop
    Apprentice ->
      replaceFirstEmptyFromBottom

replaceLastEmptyFromTop : DarkJedi -> List Slot -> List Slot
replaceLastEmptyFromTop s l =
  List.reverse (doReplaceLastEmptyFromTop s [] l)

doReplaceLastEmptyFromTop : DarkJedi -> List Slot -> List Slot -> List Slot
doReplaceLastEmptyFromTop s acc l =
  case l of
    [Empty] ->
      (Far s) :: acc
    (Empty :: Empty :: rest) ->
      let
        acc = (Empty :: acc)
        l = List.drop 1 l
      in
        doReplaceLastEmptyFromTop s acc l
    (Empty :: rest) ->
      let
        acc = (Far s :: acc)
      in
        doReplaceLastEmptyFromTop s acc rest
    (nonempty :: rest) ->
      let
        acc = nonempty :: acc
      in
        doReplaceLastEmptyFromTop s acc rest
    [] -> acc

replaceFirstEmptyFromBottom : DarkJedi -> List Slot -> List Slot
replaceFirstEmptyFromBottom s l =
  let
    l = List.reverse l
  in
    List.reverse (replaceLastEmptyFromTop s l)

fetchJedi : MasterApprentice -> Int -> Cmd Msg
fetchJedi masterApprentice id =
  let
    url = "http://localhost:3000/dark-jedis/" ++ toString(id)
  in
    Task.perform FetchFail (FetchSucceed masterApprentice) (Http.get decodeDarkJedi url)

-- VIEW

view : Model -> Html Msg
view model =
  div [ class "app-container" ]
  [ div [ class "css-root" ]
    [ h1 [ class "css-planet-monitor" ]
      [ text "Obi-Wan currently on Tatooine" ]
    , section [ class "css-scrollable-list" ]
      [ ul [ class "css-slots" ] (buildSlots model)
      , div [ class "css-scroll-buttons" ]
        [ button [ class "css-button-up", onClick Up ]
          []
        , button [ class "css-button-down", onClick Down ]
          []
        ]
      ]
    ]
  ]

buildSlots : Model -> List (Html Msg)
buildSlots model =
  List.map buildSlot model.jedis

buildSlot : Slot -> Html Msg
buildSlot slot =
  case slot of
    Empty ->
      li [ class "css-slot" ]
        [ h3 []
          [ text "" ]
        , h6 []
          [ text "" ]
        ]
    Far jedi ->
      li [ class "css-slot" ]
        [ h3 []
          [ text jedi.name ]
        , h6 []
          [ text ("Homeworld: " ++ jedi.homeworld.name) ]
        ]
    Nearby jedi ->
      li [ class "css-slot", style [("color", "red")] ]
        [ h3 []
          [ text jedi.name ]
        , h6 []
          [ text ("Homeworld: " ++ jedi.homeworld.name) ]
        ]

main =
  Html.program
    { init = init
    , update = update
    , view = view
    , subscriptions = \_ -> Sub.none
    }
