module Game (init, update, view) where

import Effects exposing (Effects)
import Html exposing (Html)
import Graphics.Element exposing (..)


type alias Position =
  { x : Float
  , y : Float
  }


type alias Player =
  { pos : Position
  }


type alias Model =
  { player : Player
  }


type Action
  = NoOp


init : (Model, Effects Action)
init =
  let
    model =
      { player = initialPlayer
      }
  in
    (model, Effects.none)


initialPlayer =
  { pos = Position 0 0
  }

view : Signal.Address Action -> Model -> Html
view address model =
  Html.fromElement (show "hoge")


update : Action -> Model -> (Model, Effects Action)
update act model =
  (model, Effects.none)
