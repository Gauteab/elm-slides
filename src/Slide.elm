port module Slide exposing (..)

import Browser
import Browser.Events
import Element exposing (..)
import Html exposing (Html)
import Json.Decode as Decode
import Json.Encode as Encode
import Keyboard.Event exposing (KeyboardEvent, decodeKeyboardEvent)
import List.Zipper as Zipper exposing (Zipper)
import Markdown


port setStorage : Encode.Value -> Cmd msg


type Action
    = NextSlide
    | PreviousSlide
    | NoAction


type alias Presentation =
    Platform.Program () Model Msg


type alias SlideShow a =
    Zipper (Slide a)


type alias Slide a =
    Element a


createSlideShow : List (Slide msg) -> SlideShow msg
createSlideShow =
    Zipper.fromList >> Maybe.withDefault (Zipper.singleton (text "There are no slides in this presentation :("))


previousSlide s =
    Zipper.previous s |> Maybe.withDefault s


nextSlide s =
    Zipper.next s |> Maybe.withDefault s


code attr language content =
    md attr ("```" ++ language ++ content ++ "```")


bullets_ point attributes =
    column attributes << List.map (\s -> text <| point ++ s)


bullets point attributes =
    column attributes << List.map (\e -> row [] [ text point, e ])


md attributes =
    el attributes << Element.html << Markdown.toHtml []


type alias Model =
    { slides : SlideShow Msg }


mapSlides f model =
    { model | slides = f model.slides }



-- UPDATE


type Msg
    = NoOp
    | KeyboardEvent KeyboardEvent


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
            mapSlides nextSlide model

        PreviousSlide ->
            mapSlides previousSlide model
    , Cmd.none
    )


init slides () =
    ( Model <| createSlideShow slides, Cmd.none )


view attr model =
    Element.layout [] <|
        el attr <|
            Zipper.current model.slides


presentation attr slides =
    Browser.element
        { init = init slides
        , update = update
        , view = view attr
        , subscriptions =
            \_ ->
                Browser.Events.onKeyDown <| Decode.map KeyboardEvent decodeKeyboardEvent
        }
