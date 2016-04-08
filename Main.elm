import StartApp
import Effects exposing (Never)
import Task
import UserCard exposing (init, update, view)

app =
  StartApp.start
    { init = init "daveashworth"
    , update = update
    , view = view
    , inputs = []
    }


main =
  app.html


port tasks : Signal (Task.Task Never ())
port tasks =
  app.tasks
