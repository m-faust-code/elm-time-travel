module TimeTravel exposing (addTimeTravel)

import Playground exposing (..)
import Set

addTimeTravel rawGame = 
    {initialState = initialStateWithTimeTravel rawGame
    , updateState = updateWithTimeTravel rawGame
    , view = viewWithTimeTravel rawGame
    }

initialStateWithTimeTravel rawGame =
    rawGame.initialState

viewWithTimeTravel rawGame computer model =
    rawGame.view computer model

updateWithTimeTravel rawGame computer model =
    rawGame.updateState computer model