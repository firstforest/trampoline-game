module Game (init, update, view, input) where

import Effects exposing (Effects)
import Html exposing (Html)
import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Color
import Time

import Debug


width = 300


height = 480


type alias Position =
  { x : Float
  , y : Float
  }


type alias Player =
  { pos : Position
  , dy : Float
  }


type alias Model =
  { player : Player
  , center : Position
  }


type Action
  = NoOp


input =
  Signal.sampleOn (Time.fps 60) (Signal.constant NoOp)


init : (Model, Effects Action)
init =
  let
    model =
      { player = initialPlayer
      , center = Position 0 (height / 4)
      }
  in
    (model, Effects.none)


initialPlayer =
  { pos = Position 0 0
  , dy = 8
  }


playerForm : Position -> Player -> Form
playerForm center { pos } =
  circle 5
  |> filled Color.red
  |> move (pos.x - center.x, pos.y - center.y)


background center =
  let
    c =
      floor center.y % height
        |> toFloat

    halfW =
      width / 2
  in
    group
    [ rect width height
        |> filled Color.lightBlue
    , segment (-halfW, -(c - (height/2))) (halfW, -(c - (height/2)))
        |> traced defaultLine
    ]


view : Signal.Address Action -> Model -> Html
view address model =
  let
    elem =
      collage width height
        [ background model.center
        , playerForm model.center model.player
        ]
  in
    Html.fromElement elem


update : Action -> Model -> (Model, Effects Action)
update act model =
  let
    player' =
      updatePlayer model.player

    center' =
      Position player'.pos.x (player'.pos.y + (height / 4))

    model' =
      { model | player <- player'
      , center <- center'
      }
  in
    (Debug.log "model" model', Effects.none)


updatePlayer ({ pos } as player) =
  let
    pos' =
      { pos | y <- pos.y + player.dy
      }
  in
    { player | pos <- pos'
    }
