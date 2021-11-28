module Slide exposing (..)

import Browser
import Browser.Events
import Element exposing (..)
import Html exposing (Html)
import Json.Decode as Decode
import Keyboard.Event exposing (KeyboardEvent, decodeKeyboardEvent)
import List.Zipper as Zipper exposing (Zipper)
import Markdown


type alias Model =
    { slides : SlideShow Msg }


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


previousSlide : Zipper a -> Zipper a
previousSlide s =
    Zipper.previous s |> Maybe.withDefault s


nextSlide : Zipper a -> Zipper a
nextSlide s =
    Zipper.next s |> Maybe.withDefault s


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


mapSlides : (SlideShow Msg -> SlideShow Msg) -> Model -> Model
mapSlides f model =
    { model | slides = f model.slides }



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
            mapSlides nextSlide model

        PreviousSlide ->
            mapSlides previousSlide model
    , Cmd.none
    )


init : List (Slide Msg) -> () -> ( Model, Cmd msg )
init slides () =
    ( Model <| createSlideShow slides, Cmd.none )


view : List (Attribute msg) -> { a | slides : Zipper (Element msg) } -> Html msg
view attr model =
    Element.layout [] <|
        el attr <|
            Zipper.current model.slides


presentation : List (Attribute Msg) -> List (Slide Msg) -> Program () Model Msg
presentation attr slides =
    Browser.element
        { init = init slides
        , update = update
        , view = view attr
        , subscriptions =
            \_ ->
                Browser.Events.onKeyDown <| Decode.map KeyboardEvent decodeKeyboardEvent
        }
