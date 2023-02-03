module Projet exposing (..)
import Browser
import Http exposing(..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode exposing (Decoder,map, map2, map4,map5, field, int, string)
import Random
import List exposing (..)
import String exposing (words)
import Json.Decode exposing (..)
import Platform.Cmd exposing (none)
import Browser.Dom exposing (getViewportOf)

-- MAIN

main =
  Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }


-- MODEL

type  Model 
  = Loading
  | Success Data

type alias Data 
  = { text : String
    , definition : Def
    , inputUser : String
    , tempInput : String
    , reponse : String
    , essai : Int
    , state : String
    , resultat : String
    , color : String
    }
  
--INIT HTTP

init : ()->(Model, Cmd Msg)
init _ =
   (Loading
   , Http.get{ url = "http://localhost:8000/mots.txt", expect = Http.expectString GotText}
  )

getDef : String -> Cmd Msg
getDef mot =
  Http.get
    { url = "https://api.dictionaryapi.dev/api/v2/entries/en/"++mot
    , expect = Http.expectJson GotDef defDecoder
    }

--JSON
{-}
type alias Def = 
  List Meanings

type alias DefMea = 
  {definitions:String}

type alias Meanings = 
  {partOfSpeech : String 
  , definitions : DefMea}


defMeaDecoder : Decoder DefMea
defMeaDecoder =
  Json.Decode.map DefMea
    (field "definitions" string)


meaningDecoder : Decoder Meanings
meaningDecoder =
  Json.Decode.map2 Meanings
    (field "partOfSpeech" string)
    (field "definitions" defMeaDecoder)


defDecoder =
  at ["meanings"] (Json.Decode.list meaningDecoder)
-}

type alias Def = 
    { meanings : List Meanings 
    }

type alias Meanings =
    { partOfSpeech : String
    , definitions : List Definitions
    }

type alias Definitions =
    { definition : String
    , example : String
    , synonyms : List String
    , antonyms : List String
    }

listMeaningsDecoder : Json.Decode.Decoder (List Meanings)
listMeaningsDecoder =
    Json.Decode.field "meanings" (Json.Decode.list meaningsDecoder)

meaningsDecoder : Json.Decode.Decoder Meanings
meaningsDecoder =
    Json.Decode.map2 Meanings
        (Json.Decode.field "partOfSpeech" Json.Decode.string)
        (Json.Decode.field "definitions" (Json.Decode.list definitionsDecoder))

definitionsDecoder : Json.Decode.Decoder Definitions
definitionsDecoder =
    Json.Decode.map4 Definitions
        (Json.Decode.field "definition" Json.Decode.string)
        (Json.Decode.field "example" Json.Decode.string)
        (Json.Decode.field "synonyms" (Json.Decode.list Json.Decode.string))
        (Json.Decode.field "antonyms" (Json.Decode.list Json.Decode.string))

listDefDecoder : Json.Decode.Decoder (List Def)
listDefDecoder =
    Json.Decode.list defDecoder

defDecoder : Json.Decode.Decoder Def
defDecoder =
    Json.Decode.map Def
        listMeaningsDecoder


{-
defDecoder =
  at ["meanings"] (Json.Decode.list meaningDecoder)

meaningDecoder =
  Json.Decode.map Meanings
    (field "partOfSpeech" string)
-}
-- UPDATE


type Msg
  = InputUser String
  | GotText (Result Http.Error String)
  | GotDef (Result Http.Error Def)
  | Changer
  | Test
  | Random Int

rool : Model -> Cmd Msg
rool model =
  case model of
    Loading ->
      rool model
    Success data ->
      Random.generate Random (Random.int 0 (length(words data.text)))

motRandom : Int -> Model -> String
motRandom index model =
  case model of
      Loading ->
        " "
      Success data ->
        let mot = head (drop index (words data.text)) in case mot of
          Just str -> str
          Nothing -> "ERROR"


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case model of
    Loading ->
      case msg of 
      GotText result ->
        case result of
          Ok fullText ->
             update Changer (Success{ text = fullText
                                    , definition = {meanings = []}
                                    , inputUser =""
                                    , tempInput =""
                                    , reponse =""
                                    , essai =0
                                    , state ="GotText"
                                    , resultat =""
                                    , color =""})
          Err _->
            (model,Cmd.none)
      _ -> (model,Cmd.none)

    Success data ->  
      case msg of 
        InputUser temp ->
          (Success{ data | inputUser = temp, state = "input" },Cmd.none)

        Changer ->
          (Success{data | essai = 0
          , resultat = " "
          , inputUser = ""
          , state = "changer"
          }, rool model)

        Random result ->
          (Success{data | reponse = (motRandom result model)
            }, getDef (motRandom result model))

        Test ->
          (Success{data | resultat = 
          if data.reponse == data.inputUser then
            "Bonne Réponse"  
          else
            "Mauvaise Réponse"
          ,color = 
          if data.reponse == data.inputUser then
            "green"  
          else
            "red"
          ,essai = 
          if data.inputUser /= data.reponse then
            data.essai +1
          else
            data.essai
          , state = 
          if data.reponse == data.inputUser then
            "testTrue"
          else
            "testFalse"}, Cmd.none)

        GotDef result -> 
          case result of
              Ok def ->
                (Success{data | definition = def}, Cmd.none)
              Err _ ->
                (model, Cmd.none)
        
        GotText result ->
          case result of
            Ok fullText ->
              (Success{ data | text = fullText}, rool model)
            Err _->
              (model,Cmd.none)
          


-- VIEW

view : Model -> Html Msg
view model =
  div[]
   [ h1[] [text "Trouve le mot !!"]
   , viewPage model
   ]

viewPage : Model -> Html Msg
viewPage model =
  case model of
    Loading -> div[][viewText ("Loading..... ") "h1"]
    Success data ->
      div[]
        [ viewEssai model
        , br[][]
        , viewDef data.definition 
        , div[][viewText  "Tapez votre réponse ci-dessous" "h2"]
        , if data.state == "testTrue" then 
            div[][viewInput "text"  data.inputUser InputUser]
          else
            div[][viewInput "text"  data.inputUser InputUser ,viewBouton "Tester la réponse" Test]
        , br[][]
        , div[][viewValidation model, viewBouton "changer de defintion" Changer]
        ]

viewEssai : Model -> Html Msg
viewEssai model =
  case model of
    Loading -> div[][]
    Success data ->
      div[]
      [ viewText ("nombre d'essais: "++String.fromInt(data.essai)) ""
      ]

viewList : List (Html Msg) -> Html Msg
viewList list =
  ul[] list

viewDef : Def -> Html Msg
viewDef def  =
  viewList(List.map viewMeanings def.meanings)
    

viewMeanings : Meanings -> Html Msg
viewMeanings  meanings =
  --if meanings.partOfSpeech /= "" then
      div[][
        ul[][
          li[][ text "meaning", br[][] ,
            ul[][
              li[][ viewText meanings.partOfSpeech "h4", br[][] ,
                ol[][
                  li[][
                      viewText "data" ""
                    , br[][] 
                  ]
                ]
              ]
            ]
          ]
        ]
      ]
  {-else
    div[][
        ul[][
          li[][ text "meaning", br[][] ,
            ul[][
              li[][ text "ERROR JSON.DECODE RETURN NOTHING", br[][] ,
                ol[][
                  li[][
                      viewText ("Not found") ""
                  ]
                ]
              ]
            ]
          ]
        ]
      ]
  -}

viewInput : String -> String -> (String -> msg) -> Html msg
viewInput t v toMsg =
  input [ type_ t, Html.Attributes.value v, onInput toMsg ] []

viewText : String -> String ->  Html msg
viewText t style =
  case style of 
    "h1"->
      h1[] [text t]
    "h2"->
      h2[] [text t]
    "h3"->
      h3[] [text t]
    "h4"->
      h4[] [text t]
    "h5"->
      h5[] [text t]
    "h6"->
      h6[] [text t]
    _ -> 
      text t
   

viewBouton : String -> (msg) -> Html msg
viewBouton t toMsg =
  button [ onClick toMsg ] [ text t ]

viewValidation : Model -> Html msg
viewValidation model =
  case model of
    Loading -> div[][]
    Success data ->
      div [ style "color" data.color] [ text data.resultat ]


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none