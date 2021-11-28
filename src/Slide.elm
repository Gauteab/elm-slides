module Slide exposing (..)

import Array exposing (Array)
import Browser
import Browser.Events
import Element exposing (..)
import Html exposing (Html)
import Json.Decode as Decode
import Keyboard.Event exposing (KeyboardEvent, decodeKeyboardEvent)
import List.Zipper as Zipper exposing (Zipper)
import Markdown


type alias Model =
    { index : Int }


type Action
    = NextSlide
    | PreviousSlide
    | NoAction


type alias Presentation =
    Platform.Program () Model Msg


code : List (Attribute msg) -> String -> String -> Element msg
code attr language content =
    md attr ("```" ++ language ++ content ++ "```")


bullets_ : String -> List (Attribute msg) -> List String -> Element msg
bullets_ point attributes =
    column attributes << List.map (\s -> text <| point ++ s)


bullets : String -> List (Attribute msg) -> List (Element msg) -> Element msg
bullets point attributes =
    column attributes << List.map (\e -> row [] [ text point, e ])


md : List (Attribute msg) -> String -> Element msg
md attributes =
    el attributes << Element.html << Markdown.toHtml []


mapIndex f model =
    { model | index = f model.index }



-- UPDATE


type Msg
    = NoOp
    | KeyboardEvent KeyboardEvent


actionFromMsg : Msg -> Action
actionFromMsg msg =
    case msg of
        NoOp ->
            NoAction

        KeyboardEvent { key } ->
            case key of
                Just "ArrowRight" ->
                    NextSlide

                Just "ArrowLeft" ->
                    PreviousSlide

                _ ->
                    NoAction


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( case actionFromMsg msg of
        NoAction ->
            model

        NextSlide ->
            mapIndex ((+) 1) model

        PreviousSlide ->
            mapIndex ((-) 1) model
    , Cmd.none
    )


init () =
    ( Model 0, Cmd.none )


view : Array (Element msg) -> List (Attribute msg) -> Model -> Html msg
view slides attr model =
    Element.layout [] <|
        el attr <|
            Maybe.withDefault none (Array.get model.index slides)


presentation attr slides =
    Browser.element
        { init = init
        , update = update
        , view = view (Array.fromList slides) attr
        , subscriptions =
            \_ ->
                Browser.Events.onKeyDown <| Decode.map KeyboardEvent decodeKeyboardEvent
        }
