module Main exposing (..)

import Browser
import Html as H
import Html.Attributes as A
import Html.Events as E
import Maybe.Extra as M
import Random as R



-- MODEL


type alias Model =
    { xs : List Float
    , ys : List Float
    , xInput : String
    , yInput : String
    , errors : List String
    }



-- INIT


init : ( Model, Cmd Message )
init =
    ( Model
        []
        []
        "2.31, 1.56, 5.48, 0.03, 6.13, 2.36, 2.14, 2.01, 2.11, 4.57"
        "4.58, 4.27, 5.61, 1.49, 6.07, 4.75, 4.30, 4.37, 4.43, 4.87"
        []
    , Cmd.none
    )



-- MESSAGE


type Message
    = None
    | UpdateXs String
    | UpdateYs String
    | ParseXs
    | ParseYs
    | ParseData



-- UPDATE
-- TODO: We want to:
-- Gather data
-- plot it as scatterplot
-- calculate first component
-- replot with first component (perhaps on second chart?)
-- Allow user to tinker with weights to see how to they change chart.


update : Message -> Model -> ( Model, Cmd Message )
update message model =
    case message of
        UpdateXs s ->
            { model | xInput = s }
                |> noCmd

        ParseXs ->
            let
                maybeXs =
                    parseFloats model.xInput
            in
            case maybeXs of
                Nothing ->
                    { model | errors = "xS must be comma seperated values" :: model.errors }
                        |> noCmd

                Just newXs ->
                    { model | xs = newXs }
                        |> noCmd

        UpdateYs s ->
            { model | yInput = s }
                |> noCmd

        ParseYs ->
            let
                maybeYs =
                    parseFloats model.yInput
            in
            case maybeYs of
                Nothing ->
                    { model | errors = "ys must be comma seperated values" :: model.errors }
                        |> noCmd

                Just newYs ->
                    { model | ys = newYs }
                        |> noCmd

        ParseData ->
            update ParseXs model
                |> Tuple.first
                |> update ParseYs
                |> Tuple.first
                |> noCmd

        None ->
            noCmd model


parseFloats : String -> Maybe (List Float)
parseFloats s =
    String.split "," s
        |> List.map (String.toFloat << String.trim)
        |> Debug.log "ys"
        |> M.combine



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Message
subscriptions model =
    Sub.none



-- MAIN


main : Program (Maybe {}) Model Message
main =
    Browser.element
        { init = always init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- VIEW


view : Model -> H.Html Message
view model =
    -- The inline style is being used for example purposes in order to keep this example simple and
    -- avoid loading additional resources. Use a proper stylesheet when building your own app.
    H.div []
        [ H.div [ A.class "row" ]
            [ H.div [ A.class "col" ]
                [ H.textarea
                    [ A.placeholder "xs"
                    , E.onInput UpdateXs
                    , A.class "form-control"
                    ]
                    []
                ]
            ]
        , H.div [ A.class "row" ]
            [ H.div [ A.class "col" ]
                [ H.textarea
                    [ A.placeholder "ys"
                    , E.onInput UpdateYs
                    , A.class "form-control"
                    ]
                    []
                ]
            ]
        , H.div [ A.class "row" ]
            [ H.div [ A.class "col" ]
                [ H.button
                    [ A.class "btn btn-primary"
                    , E.onClick ParseData
                    ]
                    [ H.text "Chart"
                    ]
                ]
            ]
        ]


noCmd : Model -> ( Model, Cmd Message )
noCmd m =
    ( m, Cmd.none )
