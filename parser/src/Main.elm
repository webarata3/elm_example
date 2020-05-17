module Main exposing (Model, Msg(..), Route(..), init, main, routeParser, subscriptions, update, view, viewLink)

import Browser
import Browser.Navigation as Nav
import Html exposing (Html, a, li, text, ul)
import Html.Attributes exposing (href)
import Url
import Url.Parser exposing ((</>), (<?>), Parser, int, map, oneOf, parse, s)
import Url.Parser.Query as Query



-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



-- MODEL


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , route : Route
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( Model key url TopPage, Cmd.none )



-- URL PARSER


type Route
    = TopPage
    | PokemonPage Int (Maybe Int)


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ Url.Parser.map TopPage Url.Parser.top
        , Url.Parser.map PokemonPage (Url.Parser.s "pokemon" </> int <?> Query.int "generation")
        ]


urlToRoute : Url.Url -> Maybe Route
urlToRoute url =
    Url.Parser.parse routeParser url



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            case urlToRoute url of
                Just a ->
                    ( { model | url = url, route = a }
                    , Cmd.none
                    )

                Nothing ->
                    ( { model | url = url }
                    , Cmd.none
                    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    case model.route of
        TopPage ->
            { title = "parserのテスト"
            , body =
                let
                    contentPath =
                        "/pokemon/"
                in
                [ ul []
                    [ viewLink <| contentPath ++ "1?generation=1"
                    , viewLink <| contentPath ++ "1?generation=2"
                    , viewLink <| contentPath ++ "2?generation=1"
                    , viewLink <| contentPath ++ "3?generation=1"
                    ]
                ]
            }

        PokemonPage no maybeGeneration ->
            { title = "ポケモン図鑑 No. " ++ String.fromInt no
            , body =
                [ text <| String.fromInt no ++ " " ++ (String.fromInt <| Maybe.withDefault 1 maybeGeneration) ]
            }


viewLink : String -> Html msg
viewLink path =
    li [] [ a [ href path ] [ text path ] ]
