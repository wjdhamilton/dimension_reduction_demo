module Main exposing (..)

import Axis
import Browser
import Data
import Html as H
import Html.Attributes as A
import Html.Events as E
import Maybe.Extra as M
import Path
import Random as R
import Round
import Scale exposing (ContinuousScale)
import Shape
import Statistics as Stats
import Svg exposing (Svg)
import Svg.Attributes as SvgA
import TypedSvg.Attributes exposing (fill, transform)
import TypedSvg.Types exposing (Paint(..), Transform(..))



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
    , variance : Maybe Float
    }



-- INIT


init : ( Model, Cmd Msg )
init =
    Model
        []
        []
        []
        []
        ""
        ""
        []
        1
        1
        Nothing
        |> update (LoadDataSet Data.InterestRates)



-- MESSAGE


type Msg
    = None
    | UpdateXs String
    | UpdateYs String
    | ParseData
    | UpdateD1Alpha1 String
    | UpdateD1Alpha2 String
    | LoadDataSet Data.DataName


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        UpdateXs s ->
            { model | xInput = s }
                |> update ParseData

        UpdateYs s ->
            { model | yInput = s }
                |> update ParseData

        ParseData ->
            let
                result =
                    Result.map2 (\xs ys -> { model | xs = xs, ys = ys })
                        (parseFloats model.xInput "xs must be comma separated values")
                        (parseFloats model.yInput "ys must be comma separated values")
            in
            case result of
                Ok m ->
                    noCmd m

                Err e ->
                    { model | errors = [ e ] }
                        |> noCmd

        UpdateD1Alpha1 v ->
            let
                d1a1 =
                    Maybe.withDefault 1.0 (String.toFloat v)

                d1a2 =
                    findLoading d1a1

                transformedX =
                    List.map (\x -> x * d1a1) model.xs

                transformedY =
                    List.map (\x -> x * d1a2) model.ys
            in
            { model
                | d1Alpha1 = d1a1
                , d1Alpha2 = d1a2
                , d1x = transformedX
                , d1y = transformedY
            }
                |> noCmd

        UpdateD1Alpha2 v ->
            let
                d1a2 =
                    Maybe.withDefault 1.0 (String.toFloat v)

                d1a1 =
                    findLoading d1a2

                transformedX =
                    List.map (\x -> x * d1a1) model.xs

                transformedY =
                    List.map (\x -> x * d1a2) model.ys
            in
            { model
                | d1Alpha1 = d1a1
                , d1Alpha2 = d1a2
                , d1x = transformedX
                , d1y = transformedY
            }
                |> noCmd

        LoadDataSet n ->
            let
                dataSet =
                    Data.findDataSet n
            in
            { model
                | xs = dataSet.x1
                , ys = dataSet.x2
                , d1x = List.map (\x -> x * dataSet.alpha1) dataSet.x1
                , d1y = List.map (\x -> x * dataSet.alpha2) dataSet.x2
                , d1Alpha1 = dataSet.alpha1
                , d1Alpha2 = dataSet.alpha2
            }
                |> noCmd

        None ->
            noCmd model


findLoading : Float -> Float
findLoading w =
    Basics.sqrt (1 - w ^ 2)


parseFloats : String -> String -> Result String (List Float)
parseFloats s e =
    case
        String.split "," s
            |> List.map (String.toFloat << String.trim)
            |> M.combine
    of
        Nothing ->
            Err e

        Just r ->
            Ok r



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
                [ H.label [] [ H.text "X1" ]
                , H.textarea
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
                [ H.label [] [ H.text "X2" ]
                , H.textarea
                    [ A.placeholder "ys"
                    , A.value model.yInput
                    , E.onInput UpdateYs
                    , A.class "form-control"
                    ]
                    []
                ]
            ]
        , H.hr [] []
        , H.div [ A.class "row" ]
            [ H.div [ A.class "col" ]
                [ chart model ]
            , H.div [ A.class "col" ]
                [ H.div [ A.class "row" ]
                    [ H.div [ A.class "col" ]
                        [ H.div [ A.class "row" ]
                            [ H.div [ A.class "col" ]
                                [ H.label [] [ H.text "alpha 1" ]
                                , coefficientInput model.d1Alpha1 UpdateD1Alpha1
                                ]
                            ]
                        , H.div [ A.class "row" ]
                            [ H.div [ A.class "col" ]
                                [ H.label [] [ H.text "alpha 2" ]
                                , coefficientInput model.d1Alpha2 UpdateD1Alpha2
                                ]
                            ]
                        , displayVariance "X1 Variance" model.xs
                        , displayVariance "X2 Variance" model.ys
                        , displayVariance "D1 Variance" (calcPCA model)
                        ]
                    ]
                ]
            ]
        ]


coefficientInput : Float -> (String -> Msg) -> H.Html Msg
coefficientInput v f =
    H.input
        [ A.type_ "number"
        , A.class "form-control"
        , A.step "0.01"
        , E.onInput f
        , A.value (String.fromFloat v)
        ]
        []


displayVariance : String -> List Float -> H.Html Msg
displayVariance title value =
    H.div [ A.class "row" ]
        [ H.div [ A.class "col" ]
            [ H.label [] [ H.text title ]
            , H.p []
                [ H.text
                    (Maybe.withDefault ""
                        (variance value
                            |> Maybe.andThen (Just << Round.round 3)
                        )
                    )
                ]
            ]
        ]


chartPadding =
    50


chartWidth =
    xAxLength + chartPadding


chartHeight =
    yAxLength + chartPadding


chart : Model -> Svg Msg
chart model =
    Svg.svg
        [ SvgA.width (String.fromInt chartWidth)
        , SvgA.height (String.fromInt chartHeight)
        , SvgA.viewBox ("0 0 " ++ String.fromFloat chartWidth ++ " " ++ String.fromFloat chartHeight)
        ]
        (drawChart model)


drawChart : Model -> List (Svg Msg)
drawChart { xs, ys, d1x, d1y, d1Alpha1, d1Alpha2 } =
    let
        forSvg scale vals =
            List.map (Scale.convert scale) vals
                |> List.map String.fromFloat

        min =
            Maybe.withDefault 1 << List.minimum

        max =
            Maybe.withDefault 1 << List.maximum

        allYs =
            ys

        allXs =
            xs

        ySc =
            yScale ( max allYs, min allYs )

        xSc =
            xScale ( min allXs, max allXs )

        d_fit =
            Stats.range (min <| allYs ++ allXs) (max <| allYs ++ allXs) 1
                |> List.map
                    (\x ->
                        Just
                            ( Scale.convert xSc (d1Alpha1 * x)
                            , Scale.convert ySc (d1Alpha2 * x)
                            )
                    )
                |> Debug.log "d_fit"

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

        dataFit =
            leastSquares xSc ySc xs ys "black"

        d1Fit =
            leastSquares xSc ySc d1x d1y "red"

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

        yAxis =
            Svg.g [ transform [ Translate 20 10 ] ] [ Axis.left [ Axis.tickCount 10 ] ySc ]

        xAxis =
            Svg.g [ transform [ Translate 20 510 ] ] [ Axis.bottom [ Axis.tickCount 10 ] xSc ]
    in
    xAxis
        :: yAxis
        :: dataFit
        :: d1Fit
        :: List.map (drawPoint "black") xys
        ++ List.map (drawPoint "red") d1xys


leastSquares : ContinuousScale Float -> ContinuousScale Float -> List Float -> List Float -> String -> Svg Msg
leastSquares xSc ySc xs ys colour =
    let
        ( xMin, xMax ) =
            Scale.range xSc
    in
    leastSquaresLine xs ys xMin xMax
        |> List.map2 Tuple.pair (Scale.ticks xSc 10)
        |> List.map (\( x, y ) -> Just ( Scale.convert xSc x, Scale.convert ySc y ))
        |> Shape.line Shape.linearCurve
        |> (\path ->
                Path.element path
                    [ SvgA.stroke colour
                    , fill PaintNone
                    , SvgA.strokeWidth "2"
                    , transform [ Translate 20 0 ]
                    ]
           )


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


calcPCA : Model -> List Float
calcPCA model =
    let
        x1 =
            model.xs

        x2 =
            model.ys

        x1x2 =
            List.map2 Tuple.pair x1 x2

        pca =
            List.map (\( a, b ) -> a * model.d1Alpha1 + b * model.d1Alpha2) x1x2
    in
    pca


variance : List Float -> Maybe Float
variance fs =
    Stats.variance fs


leastSquaresLine : List Float -> List Float -> Float -> Float -> List Float
leastSquaresLine xs ys min max =
    let
        beta =
            sXY xs ys / sXX xs

        overX =
            mean xs

        overY =
            mean ys

        r =
            Stats.range min max 1
    in
    List.map (\x -> overY + (beta * (x - overX))) r


sXX : List Float -> Float
sXX xs =
    let
        meanX =
            mean xs
    in
    List.sum << List.map (\x -> (x - meanX) ^ 2) <| xs


sXY : List Float -> List Float -> Float
sXY xs ys =
    let
        meanX =
            mean xs

        meanY =
            mean ys

        xys =
            List.map2 Tuple.pair xs ys
    in
    List.sum << List.map (\( x, y ) -> (x - meanX) * (y - meanY)) <| xys


mean : List Float -> Float
mean ns =
    List.sum ns / (toFloat <| List.length ns)
