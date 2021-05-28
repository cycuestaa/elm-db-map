port module Main exposing (Model, Msg(..), init, main, toJs, update, view)

---for json.decode: exposing (Decoder, Value)

import Browser
import Browser.Events
import Html exposing (..)
import Html.Attributes as Attribute exposing (..)
import Html.Events as Events exposing (on, onClick)
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Encode as Encode



---import Table -- requires elm install ericgj/elm-csv-decode
--- how to switch current verison
-- ---------------------------
-- PORTS
-- ---------------------------


port toJs : String -> Cmd msg


port firebaseWrite : String -> Cmd msg


port firebaseRead : (String -> msg) -> Sub msg



-- ---------------------------
-- MAIN
-- ---------------------------


main : Program String Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- ---------------------------
-- MODEL
-- ---------------------------


type alias Model =
    { title : String
    , gkey : String
    , center : LatLng
    , count : Maybe Int
    }


blankModel : String -> Model
blankModel gkey =
    { title = "Blank map"
    , gkey = gkey
    , center = LatLng 39.4 -0.41
    , count = Nothing
    }


init : String -> ( Model, Cmd Msg )
init gkey =
    ( blankModel gkey, Cmd.none )


type alias LatLng =
    { lat : Float, lng : Float }



-- ---------------------------
-- Subscriptions
-- ---------------------------


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onClick (Decode.succeed Click)
        , firebaseRead ReceiveValue
        ]



-- ---------------------------
-- UPDATE
-- ---------------------------


type Msg
    = OnDragEnd MapBounds
    | ClearAll
    | Click
    | ReceiveValue String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnDragEnd detail ->
            let
                _ =
                    Debug.log "detail" <| Debug.toString detail
            in
            ( model, Cmd.none )

        ClearAll ->
            ( model, Cmd.none )

        Click ->
            case model.count of
                Just n ->
                    ( model, firebaseWrite (String.fromInt (n + 1)) )

                Nothing ->
                    ( model
                    , Cmd.none
                    )

        ReceiveValue value ->
            case String.toInt value of
                Just n ->
                    ( { model | count = Just n }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )


type alias MapBounds =
    { north : Float
    , east : Float
    , south : Float
    , west : Float
    }


decodeMapBounds : Decoder MapBounds
decodeMapBounds =
    Decode.map4 MapBounds
        (Decode.field "north" Decode.float)
        (Decode.field "east" Decode.float)
        (Decode.field "south" Decode.float)
        (Decode.field "west" Decode.float)



-- ---------------------------
-- VIEW
-- ---------------------------


type alias Document msg =
    { title : String
    , body : List (Html msg)
    }



--view : Model -> List (Html Msg)
--view model =
--  [ gmap model
--  ]
------- elm install ericgj/elm-csv-decode
------- how to show a table


type alias Pantry =
    { id : String
    , name : String
    , address : String
    , center : LatLng
    }


type alias Shelter =
    { id : String
    , name : String
    , address : String
    , center : LatLng
    }


type alias Buildings =
    { id : String
    , name : String
    , address : String
    , center : LatLng
    }


type Size
    = Single
    | Double
    | Triple


sizeToString : Size -> String
sizeToString size =
    case size of
        Single ->
            "single"

        Double ->
            "double"

        Triple ->
            "triple"


type Color
    = Yellow
    | Gray
    | White


colorToString : Color -> String
colorToString color =
    case color of
        Yellow ->
            "bg-yellow"

        Gray ->
            "bg-gray"

        White ->
            "bg-white"


inputBox : Html Msg -> Html Msg
inputBox x =
    div
        [ class "input-box"
        ]
        [ x
        ]


emptyBox : Html Msg
emptyBox =
    div
        [ class "input-box"
        ]
        [ span [ class "empty" ] [ text "empty" ]
        ]


stackBox : Float -> Html Msg
stackBox x =
    div
        [ class "input-box"
        ]
        [ text <| String.fromFloat x
        ]


cell : Html.Attribute Msg -> Size -> Color -> String -> Html Msg
cell attr size color content =
    button
        [ class <|
            String.join " " <|
                [ "cell", sizeToString size, colorToString color ]
        , attr
        ]
        [ text content ]


view : Model -> Document Msg
view model =
    { title = "Community-Care"
    , body =
        [ div []
            [ h1 [ class "h1" ] [ text "Community Care Resources" ] ]
        , div []
            [ p [ class "p" ] [ text "Welcome! We have aggregated food and shelter resources around Chicago." ] ]
        , div []
            [ button
                [ type_ "Button"
                ]
                [ text "Food Pantries!" ]
            , button
                [ type_ "Button"
                ]
                [ text "Shelters" ]
            , button
                [ type_ "Button"
                ]
                [ text "Vacant/Abandoned Buildings" ]
            , div
                [ class "calculator" ]
                (List.repeat (3 - List.length [ 2 ]) emptyBox
                    --model.stack
                    ++ List.map stackBox (List.reverse <| List.take 3 [ 3 ])
                    --model.stack
                    ++ [ inputBox <|
                            case model.count of
                                Just n ->
                                    Html.text <|
                                        "The worldwide count is "
                                            ++ String.fromInt n
                                            ++ "."

                                Nothing ->
                                    Html.text "Loading worldwide count..."
                       , section
                       ]
                )
            , h2 [] [ gmap model ]
            ]
        , div []
            [ p [ class "p" ] [ text "by ccuestaa" ] ]
        ]
    }



----- MAKE INTO A TABLE


section : Html Msg
section =
    div [ class "section" ]
        [ cell (onClick ClearAll) Single Gray "←"
        ]



----


gmap : Model -> Html Msg
gmap model =
    Html.node "google-map"
        [ Attribute.attribute "api-key" model.gkey
        , Attribute.attribute "latitude" <| String.fromFloat model.center.lat
        , Attribute.attribute "longitude" <| String.fromFloat model.center.lng
        , Attribute.attribute "drag-events" "true"
        , Events.on "google-map-dragend" (Decode.at [ "detail", "bounds" ] decodeMapBounds |> Decode.map OnDragEnd)
        ]
        [ gmarker model.center ]


gmarker : LatLng -> Html msg
gmarker { lat, lng } =
    Html.node "google-map-marker"
        [ Attribute.attribute "latitude" <| String.fromFloat lat
        , Attribute.attribute "longitude" <| String.fromFloat lng
        ]
        []
