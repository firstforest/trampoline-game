module Game (init, update, view, input) where

import Effects exposing (Effects)
import Html exposing (Html)
import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Color
import Time
import Keyboard
import Text

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
  | Jump


input =
  Signal.merge
    (Signal.map isJump Keyboard.space)
    (Signal.sampleOn (Time.fps 60) (Signal.constant NoOp))


isJump b =
  if b then
    Jump
  else
    NoOp


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
  , dy = 0
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


positionElem { pos } =
  pos.y
  |> floor
  |> toString
  |> Text.fromString
  |> rightAligned
  |> Graphics.Element.width width


trampolineForm center player =
  let
    base =
      -center.y

    py =
      min base (player.pos.y - center.y)
  in
    path [ (-100, base)
         , (0, py)
         , (100, base)
         ]
    |> traced defaultLine


view : Signal.Address Action -> Model -> Html
view address model =
  let
    elem =
      collage width height
        [ background model.center
        , trampolineForm model.center model.player
        , playerForm model.center model.player
        ]
  in
    Html.fromElement
    <|  layers
          [ elem
          , positionElem model.player
          ]



update : Action -> Model -> (Model, Effects Action)
update act model =
  updateWithAction act model
  |> succeedGame


succeedGame model =
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


updateWithAction act ({player} as model) =
  case act of
    Jump ->
      let
        dy' =
          if -50 < player.pos.y && player.pos.y < 0 && player.dy <= 0  then
            10 + -player.dy
          else
            player.dy

        player' =
          { player |
            dy <- dy'
          }
      in
        { model | player <- player'
        }

    _ ->
      model

updatePlayer ({ pos } as player) =
  let
    pos' =
      { pos | y <- pos.y + player.dy
      }

    dy' =
      if pos.y < -50 && player.dy < 0 then
        abs player.dy * 0.7
      else
        player.dy - 0.1
  in
    { player | pos <- pos'
    , dy <- dy'
    }
