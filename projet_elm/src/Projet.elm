module Projet exposing (..)
import Browser
import Http exposing(..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode exposing (Decoder, map4, field, int, string)
import Random
import List exposing (map5)




-- MAIN


main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
  { text : String
  , definition : String
  , inputUser : String
  , tempInput : String
  , reponse : String
  , essai : Int
  , style : String 
  }


init : Model
init =
  Model Http.get{ url = "http://localhost:8000/mots.txt", expect = Http.expectString GotText} "la définition" " " "votre réponse ici" "manger" 0 "white"


getDef : String -> String
getDef mot =
  Http.get
    { url = "https://api.dictionaryapi.dev/api/v2/entries/en/"++mot
    , expect = Http.expectJson GotDef defDecoder
    }


defDecoder : Decoder def
defDecoder =
  map5 Quote
    (field "word" string)
    (field "phonetic" string)
    (field "phonetics" phoneticsDecoder)
    (field "origin" string)
    (field "mearing" mearingDecoders)

phoneticsDecoder : Decoder phonetics
phoneticsDecoder =
  map3
-- UPDATE


type Msg
  = Definition String
  | InputUser String
  | GotText
  | GotDef
  | Changer
  | Test

rool : int -> Random.Generator Int
rool edge =
  Random.int 0 edge

motRandom : model -> String
motRandom model =
  head(drop ((rool (model.text).length)-1) (model.text).words)


update : Msg -> Model -> Model
update msg model =
  case msg of
    Definition definition ->
      { model | definition = definition }
    InputUser temp ->
      { model | tempInput = temp }

    Changer ->
      {model | definition = "la bite a dudule"
      , reponse = motRandom model
      , essai = 0
      , style = "white"
      , tempInput = "votre réponse ici"}

    Test ->
      {model | inputUser = model.tempInput
      , style = 
      if model.inputUser == model.reponse then
        "green"
      else
        "red"
      ,essai = 
      if model.inputUser /= model.reponse then
        model.essai +1
      else
        model.essai
      }


-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ viewText  model.definition 
    , viewInput "text"  model.tempInput InputUser
    , viewText "nombre d'essais: "
    , viewText (String.fromInt(model.essai))
    , viewBouton "Tester la réponse" Test
    , viewValidation model
    , viewBouton "changer de defintion" Changer
    , viewText (model.inputUser)
    , viewText(model.tempInput)
    ]


viewInput : String -> String -> (String -> msg) -> Html msg
viewInput t v toMsg =
  input [ type_ t, value v, onInput toMsg ] []

viewText : String ->  Html msg
viewText t =
  text t

viewBouton : String -> (msg) -> Html msg
viewBouton t toMsg =
  button [ onClick toMsg ] [ text t ]

viewValidation : Model -> Html msg
viewValidation model =
  if model.reponse == model.inputUser then
    div [ style "color" model.style ] [ text "BONNE REPONSE" ]
  else
    div [ style "color" model.style ] [ text "MAUVAISE REPONSE" ]