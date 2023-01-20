module Projet exposing (..)
import Browser
import Html exposing (Html, text, pre)
import Http
import Random



-- MAIN


main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }



-- MODEL


type Model
  = Failure
  | Loading
  | Success String



init : () -> (Model, Cmd Msg)
init _ =
  ( Loading
  , randomWord(Http.get
      { url = "http://localhost:8000/src/mots.txt"
      , expect = Http.expectString GotText
      })
  )


randomWord: Cmd Msg -> String
randomWord =
  Random.generate NewFace (Random.int 1 6)
  



-- UPDATE


type Msg
  = GotText (Result Http.Error String)
  |Roll
  |NewFace int


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotText result ->
      case result of
        Ok fullText ->
          (Success fullText, Cmd.none)

        Err _ ->
          (Failure, Cmd.none)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- VIEW


view : Model -> Html Msg
view model =
  case model of
    Failure ->
      text "I was unable to load your book."

    Loading ->
      text "Loading..."

    Success fullText ->
      pre [] [ text fullText ]