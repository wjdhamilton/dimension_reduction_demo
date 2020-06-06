module Main exposing (..)

import Axis
import Browser
import Html as H
import Html.Attributes as A
import Html.Events as E
import Maybe.Extra as M
import Random as R
import Scale exposing (ContinuousScale)
import Statistics as Stats
import Svg exposing (Svg)
import Svg.Attributes as SvgA
import TypedSvg.Attributes exposing (transform)
import TypedSvg.Types exposing (Transform(..))



-- MODEL


type alias Model =
    { xs : List Float
    , ys : List Float
    , d1x : List Float
    , d1y : List Float
    , xInput : String
    , yInput : String
    , errors : List String
    , d1Alpha1 : Float
    , d1Alpha2 : Float
    }



-- INIT


init : ( Model, Cmd Msg )
init =
    ( Model
        [ 2.31, 1.56, 5.48, 0.03, 6.13, 2.36, 2.14, 2.01, 2.11, 4.57 ]
        [ 4.58, 4.27, 5.61, 1.49, 6.07, 4.75, 4.3, 4.37, 4.43, 4.87 ]
        [ 2.31, 1.56, 5.48, 0.03, 6.13, 2.36, 2.14, 2.01, 2.11, 4.57 ]
        [ 4.58, 4.27, 5.61, 1.49, 6.07, 4.75, 4.3, 4.37, 4.43, 4.87 ]
        "2.31, 1.56, 5.48, 0.03, 6.13, 2.36, 2.14, 2.01, 2.11, 4.57"
        "4.58, 4.27, 5.61, 1.49, 6.07, 4.75, 4.30, 4.37, 4.43, 4.87"
        []
        1
        1
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
    | UpdateD1Alpha1 String
    | UpdateD1Alpha2 String



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

        UpdateD1Alpha1 v ->
            let
                new =
                    Maybe.withDefault 1.0 (String.toFloat v)

                transformed =
                    List.map (\x -> x * new) model.xs
            in
            { model
                | d1Alpha1 = new
                , d1x = transformed
            }
                |> noCmd

        UpdateD1Alpha2 v ->
            let
                new =
                    Maybe.withDefault 1.0 (String.toFloat v)

                transformed =
                    List.map (\y -> y * new) model.ys
            in
            { model
                | d1Alpha2 = new
                , d1y = transformed
            }
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
                [ chart model ]
            , H.div [ A.class "col" ]
                [ H.div [ A.class "row" ]
                    [ H.div [ A.class "col" ]
                        [ H.div [ A.class "row" ]
                            [ H.div [ A.class "col" ]
                                [ coefficientInput model.d1Alpha1 UpdateD1Alpha1 ]
                            ]
                        , H.div [ A.class "row" ]
                            [ H.div [ A.class "col" ]
                                [ coefficientInput model.d1Alpha2 UpdateD1Alpha2 ]
                            ]
                        ]
                    ]
                ]
            ]
        ]


coefficientInput : Float -> (String -> Msg) -> H.Html Msg
coefficientInput v f =
    H.input
        [ A.type_ "number"

        -- , A.value (String.fromFloat v)
        , E.onInput f
        ]
        []


chartWidth =
    xAxLength + 50


chartHeight =
    yAxLength + 50


chart : Model -> Svg Msg
chart model =
    Svg.svg
        [ SvgA.width (String.fromInt chartWidth)
        , SvgA.height (String.fromInt chartHeight)
        , SvgA.viewBox ("0 0 " ++ String.fromFloat chartWidth ++ " " ++ String.fromFloat chartHeight)
        ]
        (drawChart model)


drawChart : Model -> List (Svg Msg)
drawChart { xs, ys, d1x, d1y } =
    let
        forSvg scale vals =
            List.map (Scale.convert scale) vals
                |> List.map String.fromFloat

        min =
            Maybe.withDefault 1 << List.minimum

        max =
            Maybe.withDefault 1 << List.maximum

        ySc =
            let
                combo =
                    ys ++ d1y
            in
            yScale ( max combo, min combo )

        xSc =
            let
                combo =
                    xs ++ d1x
            in
            xScale ( min combo, max combo )

        ys_ =
            forSvg ySc ys

        xs_ =
            forSvg xSc xs

        d1x_ =
            forSvg xSc d1x

        d1y_ =
            forSvg ySc d1y

        xys =
            List.map2 Tuple.pair xs_ ys_

        d1xys =
            List.map2 Tuple.pair d1x_ d1y_

        drawPoint c ( x, y ) =
            Svg.circle
                [ SvgA.cx x
                , SvgA.cy y
                , SvgA.r "5"
                , SvgA.fill c
                , SvgA.stroke c
                , SvgA.fillOpacity "0.8"
                , transform [ Translate 20 0 ]
                ]
                []
    in
    Svg.g [ transform [ Translate 20 0 ] ] [ Axis.left [ Axis.tickCount 10 ] ySc ]
        :: Svg.g [ transform [ Translate 20 500 ] ] [ Axis.bottom [ Axis.tickCount 10 ] xSc ]
        :: List.map (drawPoint "black") xys
        ++ List.map (drawPoint "red") d1xys


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
