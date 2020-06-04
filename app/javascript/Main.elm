module Main exposing (..)

import Axis
import Browser
import Html as H
import Html.Attributes as A
import Html.Events as E
import Maybe.Extra as M
import Random as R
import Scale exposing (ContinuousScale)
import Svg exposing (Svg)
import Svg.Attributes as SvgA
import TypedSvg.Attributes exposing (transform)
import TypedSvg.Types exposing (Transform(..))



-- MODEL


type alias Model =
    { xs : List Float
    , ys : List Float
    , xInput : String
    , yInput : String
    , errors : List String
    }



-- INIT


init : ( Model, Cmd Msg )
init =
    ( Model
        [ 2.31, 1.56, 5.48, 0.03, 6.13, 2.36, 2.14, 2.01, 2.11, 4.57 ]
        [ 4.58, 4.27, 5.61, 1.49, 6.07, 4.75, 4.3, 4.37, 4.43, 4.87 ]
        "2.31, 1.56, 5.48, 0.03, 6.13, 2.36, 2.14, 2.01, 2.11, 4.57"
        "4.58, 4.27, 5.61, 1.49, 6.07, 4.75, 4.30, 4.37, 4.43, 4.87"
        []
    , Cmd.none
    )



-- MESSAGE


type Msg
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


update : Msg -> Model -> ( Model, Cmd Msg )
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


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- MAIN


main : Program (Maybe {}) Model Msg
main =
    Browser.element
        { init = always init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- VIEW


view : Model -> H.Html Msg
view model =
    H.div []
        [ H.div [ A.class "row" ]
            [ H.div [ A.class "col" ]
                [ H.textarea
                    [ A.placeholder "xs"
                    , A.value model.xInput
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
                    , A.value model.yInput
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
        , H.div [ A.class "row" ]
            [ H.div [ A.class "col" ]
                [ chart model.xs model.ys ]
            , H.div [ A.class "col" ]
                [ chart model.xs model.ys ]
            , H.div [ A.class "col" ]
                [ H.div [ A.class "row" ]
                    [ H.div [ A.class "col" ]
                        [ H.text "up" ]
                    , H.div [ A.class "row" ]
                        [ H.div [ A.class "col" ]
                            [ H.text "down" ]
                        ]
                    ]
                ]
            ]
        ]


chartWidth =
    xAxLength + 50


chartHeight =
    yAxLength + 50


chart : List Float -> List Float -> Svg Msg
chart xs ys =
    Svg.svg
        [ SvgA.width (String.fromInt chartWidth)
        , SvgA.height (String.fromInt chartHeight)
        , SvgA.viewBox ("0 0 " ++ String.fromFloat chartWidth ++ " " ++ String.fromFloat chartHeight)
        ]
        (drawChart xs ys)


drawChart : List Float -> List Float -> List (Svg Msg)
drawChart xs ys =
    let
        min =
            Maybe.withDefault 1 << List.minimum

        max =
            Maybe.withDefault 1 << List.maximum

        ySc =
            yScale ( max ys, min ys )

        xSc =
            xScale ( min xs, max xs )

        ys_ =
            List.map (Scale.convert ySc) ys
                |> List.map String.fromFloat

        xs_ =
            List.map (Scale.convert xSc) xs
                |> List.map String.fromFloat

        xys =
            List.map2 Tuple.pair xs_ ys_

        drawPoint ( x, y ) =
            Svg.circle
                [ SvgA.cx x
                , SvgA.cy y
                , SvgA.r "5"
                , transform [ Translate 20 0 ]
                ]
                []
    in
    Svg.g [ transform [ Translate 20 0 ] ] [ Axis.left [ Axis.tickCount 10 ] ySc ]
        :: Svg.g [ transform [ Translate 20 500 ] ] [ Axis.bottom [ Axis.tickCount 10 ] xSc ]
        :: List.map drawPoint xys


yAxLength =
    500


yScale : ( Float, Float ) -> ContinuousScale Float
yScale =
    makeScale yAxLength


xAxLength =
    500


xScale : ( Float, Float ) -> ContinuousScale Float
xScale =
    makeScale xAxLength


makeScale : Float -> ( Float, Float ) -> ContinuousScale Float
makeScale length domain =
    Scale.linear ( 0, length ) domain
        |> Scale.nice 1


noCmd : Model -> ( Model, Cmd Msg )
noCmd m =
    ( m, Cmd.none )
