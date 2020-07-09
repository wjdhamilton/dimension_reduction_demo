module Data exposing (DataName(..), DataSet, findDataSet)

import Dict


type alias DataSet =
    { x1 : List Float
    , x2 : List Float
    , alpha11 : Float
    , alpha12 : Float
    , alpha21 : Float
    , alpha22 : Float
    , name : String
    }


dataSetDict =
    Dict.singleton (toString InterestRates) interestRates
        |> Dict.insert (toString LandCover) landCover


findDataSet : DataName -> DataSet
findDataSet n =
    Maybe.withDefault interestRates <| Dict.get (toString n) dataSetDict


type DataName
    = InterestRates
    | LandCover
    | HousePriceAndIncome


toString : DataName -> String
toString s =
    case s of
        InterestRates ->
            "InterestRates"

        LandCover ->
            "LandCover"

        HousePriceAndIncome ->
            "HousePriceAndIncome"


interestRates : DataSet
interestRates =
    DataSet
        [ 2.31, 1.56, 5.48, 0.03, 6.13, 2.36, 2.14, 2.01, 2.11, 4.57 ]
        [ 4.58, 4.27, 5.61, 1.49, 6.07, 4.75, 4.3, 4.37, 4.43, 4.87 ]
        0.5083
        -0.8612
        -0.8612
        -0.5083
        "Short and Long Term Interest Rates"


landCover : DataSet
landCover =
    DataSet
        [ 16.6, 15.9, 27.6, 48.8, 29.2, 57.1, 41.8, 23.0 ]
        [ 47.9, 45.2, 38.8, 25.5, 37.5, 21.1, 27.5, 43.2 ]
        -0.5468
        -0.8373
        -0.8373
        0.5468
        "Percentages of land of two types in eight English regions"


housePriceAndIncome : DataSet
housePriceAndIncome =
    DataSet
        [ 221.5, 152.6, 124.5, 125.2, 94.4, 88.7, 62.1, 69.4, 67.0 ]
        [ 46.3, 38.5, 29.6, 33.8, 31.9, 29.4, 27.4, 28.6, 27.8 ]
        0.993
        0.115
        1
        1
        "House price and household income in nine English regions"
