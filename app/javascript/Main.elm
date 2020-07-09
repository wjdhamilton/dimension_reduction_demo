module Main exposing (..)

import Axis
import Browser exposing (..)
import Data
import Html as H
import Html.Attributes as A
import Html.Events as E
import Maybe.Extra as M
import Path
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
    , d2x : List Float
    , d2y : List Float
    , xInput : String
    , yInput : String
    , a11Input : String
    , a12Input : String
    , a21Input : String
    , a22Input : String
    , errors : List String
    , d1Alpha1 : Float
    , d1Alpha2 : Float
    , d2Alpha1 : Float
    , d2Alpha2 : Float
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
        []
        []
        ""
        ""
        ""
        ""
        ""
        ""
        []
        1
        1
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
    | UpdateD2Alpha1 String
    | UpdateD2Alpha2 String
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
                ( d1a1, transformedX ) =
                    changeWeight v model.xs
            in
            { model
                | d1Alpha1 = d1a1
                , d1x = transformedX
                , a11Input = v
            }
                |> noCmd

        UpdateD1Alpha2 v ->
            let
                ( d1a2, transformedY ) =
                    changeWeight v model.ys
            in
            { model
                | d1Alpha2 = d1a2
                , d1y = transformedY
            }
                |> noCmd

        UpdateD2Alpha1 v ->
            let
                ( d2a1, transformedX ) =
                    changeWeight v model.xs
            in
            { model
                | d2Alpha1 = d2a1
                , d2x = transformedX
            }
                |> noCmd

        UpdateD2Alpha2 v ->
            let
                ( d2a2, transformedY ) =
                    changeWeight v model.ys
            in
            { model
                | d2Alpha2 = d2a2
                , d2y = transformedY
            }
                |> noCmd

        LoadDataSet n ->
            let
                dataSet =
                    Data.findDataSet n

                listToCsv ns =
                    List.map String.fromFloat ns
                        |> List.intersperse ","
                        |> List.foldl (++) ""
            in
            { model
                | xs = dataSet.x1
                , ys = dataSet.x2
                , xInput = listToCsv dataSet.x1
                , yInput = listToCsv dataSet.x2
                , d1x = List.map (\x -> x * dataSet.alpha11) dataSet.x1
                , d1y = List.map (\x -> x * dataSet.alpha12) dataSet.x2
                , d2x = List.map (\x -> x * dataSet.alpha21) dataSet.x1
                , d2y = List.map (\x -> x * dataSet.alpha22) dataSet.x2
                , d1Alpha1 = dataSet.alpha11
                , d1Alpha2 = dataSet.alpha12
                , d2Alpha1 = dataSet.alpha21
                , d2Alpha2 = dataSet.alpha22
            }
                |> noCmd

        None ->
            noCmd model


changeWeight : String -> List Float -> ( Float, List Float )
changeWeight v xs =
    let
        alpha =
            Maybe.withDefault 1.0 (String.toFloat v)

        transformed =
            List.map (\x -> x * alpha) xs
    in
    ( alpha, transformed )


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
subscriptions _ =
    Sub.none



-- MAIN


main : Program (Maybe {}) Model Msg
main =
    Browser.document
        { init = always init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- VIEW


view : Model -> Document Msg
view model =
    { title = "Dimension Reduction Demo"
    , body =
        H.div []
            [ H.div [ A.class "row" ]
                [ H.div [ A.class "col" ]
                    [ H.h1 [] [ H.text "Principle Component Analysis" ] ]
                ]
            , H.div [ A.class "row" ]
                [ H.div [ A.class "col" ]
                    [ H.h2 [] [ H.text "X1" ]
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
                    [ H.h2 [] [ H.text "X2" ]
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
                                    [ H.label [] [ H.text "\\(\\alpha_{11}\\)" ]
                                    , coefficientInput model.a11Input UpdateD1Alpha1
                                    ]
                                ]
                            , H.div [ A.class "row" ]
                                [ H.div [ A.class "col" ]
                                    [ H.label [] [ H.text "\\(\\alpha_{12}\\)" ]
                                    , coefficientInput model.a12Input UpdateD1Alpha2
                                    ]
                                ]
                            , H.div [ A.class "row" ]
                                [ H.div [ A.class "col" ]
                                    [ H.label [] [ H.text "\\(\\alpha_{22}\\)" ]
                                    , coefficientInput model.a21Input UpdateD2Alpha1
                                    ]
                                ]
                            , H.div [ A.class "row" ]
                                [ H.div [ A.class "col" ]
                                    [ H.label [] [ H.text "\\(\\alpha_{22}\\)" ]
                                    , coefficientInput model.a22Input UpdateD2Alpha2
                                    ]
                                ]
                            , let
                                d1 =
                                    List.map2 (+) model.d1x model.d1y

                                d2 =
                                    List.map2 (+) model.d2x model.d2y
                              in
                              H.div [ A.class "row mt-5" ]
                                [ H.div [ A.class "col border border-primary" ]
                                    [ displayVariance "X1 Variance" model.xs
                                    , displayVariance "X2 Variance" model.ys
                                    , displayVariance "D1 Variance" (List.map2 (+) model.d1x model.d1y)
                                    , displayVariance "D2 Variance" (List.map2 (+) model.d2x model.d2y)
                                    , H.div [ A.class "row" ]
                                        [ H.div [ A.class "col" ]
                                            [ H.label [] [ H.text "Corr(D1,D2)" ]
                                            , H.p []
                                                [ H.text (Round.round 3 (correlation d1 d2))
                                                ]
                                            ]
                                        ]
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
            :: []
    }


coefficientInput : String -> (String -> Msg) -> H.Html Msg
coefficientInput v f =
    H.input
        [ A.type_ "number"
        , A.class "form-control"
        , A.step "0.01"
        , E.onInput f
        , A.value v
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
        , SvgA.viewBox ("-30 -10 " ++ String.fromFloat chartWidth ++ " " ++ String.fromFloat chartHeight)
        ]
        (drawChart model)


drawChart : Model -> List (Svg Msg)
drawChart { xs, ys, d1x, d1y, d2x, d2y } =
    let
        forSvg scale vals =
            List.map (Scale.convert scale) vals
                |> List.map String.fromFloat

        min =
            Maybe.withDefault 1 << List.minimum

        max =
            Maybe.withDefault 1 << List.maximum

        allYs =
            ys ++ d1y ++ d2y

        allXs =
            xs ++ d1x ++ d2x

        ySc =
            yScale ( max allYs, min allYs )

        xSc =
            xScale ( min allXs, max allXs )

        ys_ =
            forSvg ySc ys

        xs_ =
            forSvg xSc xs

        d1x_ =
            forSvg xSc d1x

        d1y_ =
            forSvg ySc d1y

        d2x_ =
            forSvg xSc d2x

        d2y_ =
            forSvg ySc d2y

        xys =
            List.map2 Tuple.pair xs_ ys_

        d1xys =
            List.map2 Tuple.pair d1x_ d1y_

        d2xys =
            List.map2 Tuple.pair d2x_ d2y_

        dataFit =
            leastSquares (min allXs) (max allXs) xSc ySc xs ys "black"

        d1Fit =
            leastSquares (min allXs) (max allXs) xSc ySc d1x d1y "red"

        d2Fit =
            leastSquares (min allXs) (max allXs) xSc ySc d2x d2y "darkorange"

        drawPoint c ( x, y ) =
            Svg.circle
                [ SvgA.cx x
                , SvgA.cy y
                , SvgA.r "5"
                , SvgA.fill c
                , SvgA.stroke c
                , SvgA.fillOpacity "0.8"
                ]
                []

        yAxis =
            Svg.g [ transform [ Translate (Scale.convert xSc 0) 0 ] ] [ Axis.left [ Axis.tickCount 10 ] ySc ]

        xAxis =
            Svg.g [ transform [ Translate 0 (Scale.convert ySc 0) ] ] [ Axis.bottom [ Axis.tickCount 10 ] xSc ]
    in
    xAxis
        :: yAxis
        :: dataFit
        :: d1Fit
        :: d2Fit
        :: List.map (drawPoint "black") xys
        ++ List.map (drawPoint "red") d1xys
        ++ List.map (drawPoint "darkorange") d2xys


leastSquares : Float -> Float -> ContinuousScale Float -> ContinuousScale Float -> List Float -> List Float -> String -> Svg Msg
leastSquares xMin xMax xSc ySc xs ys colour =
    leastSquaresLine xs ys xMin xMax
        |> List.map (\( x, y ) -> Just ( Scale.convert xSc x, Scale.convert ySc y ))
        |> Shape.line Shape.linearCurve
        |> (\path ->
                Path.element path
                    [ SvgA.stroke colour
                    , fill PaintNone
                    , SvgA.strokeWidth "2"
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


{-| TODO Start variable (0 below) is the x coordinate for where the axis will be drawn.
Can eliminate many transform commands below by setting this here (currently 20)
-}
makeScale : Float -> ( Float, Float ) -> ContinuousScale Float
makeScale length domain =
    Scale.linear ( 0, length ) domain
        |> Scale.nice 10


noCmd : Model -> ( Model, Cmd Msg )
noCmd m =
    ( m, Cmd.none )


variance : List Float -> Maybe Float
variance =
    Stats.variance


leastSquaresLine : List Float -> List Float -> Float -> Float -> List ( Float, Float )
leastSquaresLine xs ys min max =
    let
        overX =
            mean xs

        overY =
            mean ys

        beta =
            sXY xs ys
                / sXX xs

        alpha =
            overY
                - beta
                * overX

        range =
            Stats.range min max 1
    in
    List.map (\x -> ( x, alpha + (beta * x) )) range


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
    List.sum ns / toFloat (List.length ns)


correlation : List Float -> List Float -> Float
correlation xs ys =
    let
        xMean =
            mean xs

        yMean =
            mean ys
    in
    List.map2 Tuple.pair xs ys
        |> List.foldr (\( x, y ) n -> n + (x - xMean) * (y - yMean)) 0
        |> (\x -> x / toFloat (List.length xs - 1))
        |> (\x -> x / (std xs * std ys))


std : List Float -> Float
std =
    Maybe.withDefault 0 << Stats.deviation
