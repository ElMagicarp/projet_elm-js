module Projet exposing (..)
import Browser
import Http exposing(..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode exposing (Decoder, map5, map4, field, int, string)
import Random
import List exposing (..)
import String exposing (words)





-- MAIN


main =
  Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }



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


init : ()->(Model, Cmd Msg)
init _ =
   (Model "" "la définition" " " "votre réponse ici" "manger" 0 "white"
   , Http.get{ url = "http://localhost:8000/mots.txt", expect = Http.expectString GotText}
   )

{-
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
-}

-- UPDATE


type Msg
  = Definition String
  | InputUser String
  | GotText (Result Http.Error String)
  | GotDef
  | Changer
  | Test
  | Random Int

rool : Model -> Cmd Msg
rool model =
  Random.generate Random (Random.int 0 (length(words model.text)))

motRandom : Int -> Model -> String
motRandom index model =
 let mot = head (drop index (words model.text)) in case mot of
  Just str -> str
  Nothing -> "ERROR"

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Definition definition ->
      ({ model | definition = definition },Cmd.none)
      
    InputUser temp ->
      ({ model | tempInput = temp },Cmd.none)

    Changer ->
      ({model | definition = " "
      , reponse = "bonjour"
      , essai = 0
      , style = "white"
      , tempInput = "votre réponse ici"}, rool model)
    
    GotText result ->
      case result of
        Ok fullText ->
          ({model | definition = ""
          , text = fullText
          , reponse = "bonjour"
          , essai = 0
          , style = "white"
          , tempInput = "votre réponse ici"}, Cmd.none)
        Err _->
          (model,Cmd.none)

    Random result ->
      ({model | reponse = (motRandom result model) }, Cmd.none)

    Test ->
      ({model | inputUser = model.tempInput
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
      }, Cmd.none)

    GotDef -> 
      (model,Cmd.none)


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

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none