module UserCard(init, update, view) where

import Effects exposing (Effects, Never)
import Html exposing (Html, a, button, div, h2, h3, hr, i, img, input, label, li, section, span, text, ul, Attribute)
import Html.Attributes exposing (style, src, href, class, id, target, type', placeholder)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Json
import Task


-- MODEL

type alias Model =
  { username : String
  , userDetails : UserDetails
  }

type alias UserDetails =
  { name : String
  , avatarUrl : String
  , githubUrl : String
  , followers : Int
  , following : Int
  , publicRepos : Int
  , location : String
  }

init : String -> (Model, Effects Action)
init username =
  ( Model username (UserDetails "waiting..." "assets/waiting.gif" "" 0 0 0 "I'm Homeless")
  , getGithubUser username
  )


-- UPDATE

type Action
  = NoOp
  | NewUser (Maybe UserDetails)
  | ViewProfile
  | ViewNotes
  | ViewRepos

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    NoOp ->
      (model, Effects.none)

    NewUser maybeDetails ->
      case maybeDetails of
        Nothing ->
          (Model model.username model.userDetails, Effects.none)

        Just details ->
          (Model model.username details, Effects.none)

    ViewProfile ->
      (model, Effects.none)

    ViewNotes ->
      (model, Effects.none)

    ViewRepos ->
      (model, Effects.none)


-- VIEW

{--
<div className="search-box">
   <label><input type="search" ref="username" placeholder="Type Username + Enter"/></label>
</div>
--}

(=>) = (,)

view : Signal.Address Action -> Model -> Html
view action model =
  div []
    [ section [ id "card" ]
        [ div [ class "search-box" ]
            [ label []
                [ input [ type' "search", placeholder "Type a Github Username + Enter" ] [] ]
            ]
        , section [ class "github-profile" ]
            [ div [ class "github-profile-info" ]
                [ a [ href "#" ]
                    [ img [ src model.userDetails.avatarUrl ] [] ]
                , h2 []
                    [ a [ href model.userDetails.githubUrl, target "_blank" ] [ text model.userDetails.name ] ]
                , h3 [] [ text model.userDetails.location ]
                ]
            , div [ style [ "height" => "5px", "background" => "#fff" ] ] [ text " " ]
            , div [ class "github-profile-state" ]
                [ ul []
                    [ li [] [ a [ href "#" ] [ i [] [ text (toString model.userDetails.followers) ], span [] [ text "Followers" ] ] ]
                    , li [] [ a [ href "#" ] [ i [] [ text (toString model.userDetails.publicRepos) ], span [] [ text "Repositories" ] ] ]
                    , li [] [ a [ href "#" ] [ i [] [ text (toString model.userDetails.following) ], span [] [ text "Following" ] ] ]
                    ]
                ]
            ]
        ]
    ]


getGithubUser : String -> Effects Action
getGithubUser username =
  Http.get decodeData (githubUserUrl username)
    |> Task.toMaybe
    |> Task.map NewUser
    |> Effects.task


githubUserUrl : String -> String
githubUserUrl username =
  Http.url ("https://api.github.com/users/" ++ username) []


-- takes a Json.Decoder that decodes the json string into UserDetails...
-- followers, following, location, public_repos
decodeData : Json.Decoder UserDetails
decodeData =
  Json.object7 UserDetails
    (Json.at ["name"] Json.string)
    (Json.at ["avatar_url"] Json.string)
    (Json.at ["html_url"] Json.string)
    (Json.at ["followers"] Json.int)
    (Json.at ["following"] Json.int)
    (Json.at ["public_repos"] Json.int)
    (Json.at ["location"] (Json.oneOf [Json.string, Json.null "I'm Homeless"]))
