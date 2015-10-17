import StartApp
import Game


init =
  Game.init


view address model =
  Game.view address model


update act model =
  Game.update act model


app =
  StartApp.start
    { init = init
    , view = view
    , update = update
    , inputs = [] }


main =
  app.html
