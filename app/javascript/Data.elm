module Data exposing (DataName(..), DataSet, findDataSet)

import Dict


type alias DataSet =
    { x1 : List Float
    , x2 : List Float
    , alpha1 : Float
    , alpha2 : Float
    , name : String
    }


dataSetDict =
    Dict.singleton (toString InterestRates) interestRates


findDataSet : DataName -> DataSet
findDataSet n =
    Maybe.withDefault interestRates <| Dict.get (toString n) dataSetDict


type DataName
    = InterestRates


toString : DataName -> String
toString s =
    case s of
        InterestRates ->
            "InterestRates"


interestRates : DataSet
interestRates =
    DataSet
        [ 2.31, 1.56, 5.48, 0.03, 6.13, 2.36, 2.14, 2.01, 2.11, 4.57 ]
        [ 4.58, 4.27, 5.61, 1.49, 6.07, 4.75, 4.3, 4.37, 4.43, 4.87 ]
        0.861
        0.508
        "Short and Long Term Interest Rates"
