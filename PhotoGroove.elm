module PhotoGroove exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (..)
import Array exposing (Array)
import Random
import Http


urlPrefix : String
urlPrefix =
    "http://elm-in-action.com/"


viewThumbnail : Maybe String -> Photo -> Html Msg
viewThumbnail selectedUrl thumbnail =
    img
        [ src (urlPrefix ++ thumbnail.url)
        , classList [ ( "selected", selectedUrl == Just thumbnail.url ) ]
        , onClick (SelectByUrl thumbnail.url)
        ]
        []


viewSizeChooser : ThumbnailSize -> ThumbnailSize -> Html Msg
viewSizeChooser selectedSize size =
    label []
        [ input
            [ type_ "radio"
            , name "size"
            , onClick (SetSize size)
            , checked (size == selectedSize)
            ]
            []
        , text (sizeToString size)
        ]


viewLargeImage : Maybe String -> Html Msg
viewLargeImage maybeUrl =
    case maybeUrl of
        Nothing ->
            text ""

        Just url ->
            img [ class "large", src (urlPrefix ++ "large/" ++ url) ] []


view : Model -> Html Msg
view model =
    div [ class "content" ]
        [ h1 [] [ text "Photo Groove" ]
        , button [ onClick SupriseMe ] [ text "Suprise Me!" ]
        , h3 [] [ text "Thumbnail Size:" ]
        , div [ id "choose-size" ] (List.map (viewSizeChooser model.choosenSize) [ Small, Medium, Large ])
        , div [ id "thumbnails", class (sizeToClass model.choosenSize) ] (List.map (viewThumbnail model.selectedUrl) model.photos)
        , viewLargeImage model.selectedUrl
        ]


type ThumbnailSize
    = Small
    | Medium
    | Large


sizeToString : ThumbnailSize -> String
sizeToString size =
    case size of
        Small ->
            "small"

        Medium ->
            "medium"

        Large ->
            "large"


sizeToClass : ThumbnailSize -> String
sizeToClass size =
    case size of
        Small ->
            "small"

        Medium ->
            "medium"

        Large ->
            "large"


type alias Photo =
    { url : String }


type alias Model =
    { photos : List Photo
    , selectedUrl : Maybe String
    , loadingError : Maybe String
    , choosenSize : ThumbnailSize
    }


initialModel : Model
initialModel =
    { photos = []
    , selectedUrl = Nothing
    , loadingError = Nothing
    , choosenSize = Medium
    }


initialCmd : Cmd Msg
initialCmd =
    "http://elm-in-action.com/photos/list"
        |> Http.getString
        |> Http.send LoadPhotos


type Msg
    = SelectByUrl String
    | SelectByIndex Int
    | SupriseMe
    | SetSize ThumbnailSize
    | LoadPhotos (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectByUrl url ->
            ( { model | selectedUrl = Just url }, Cmd.none )

        SelectByIndex index ->
            let
                newSelectedUrl : Maybe String
                newSelectedUrl =
                    model.photos
                        |> Array.fromList
                        |> Array.get index
                        |> Maybe.map .url
            in
                ( { model | selectedUrl = newSelectedUrl }, Cmd.none )

        SupriseMe ->
            let
                randomPhotoPicker : Random.Generator Int
                randomPhotoPicker =
                    Random.int 0 (List.length model.photos - 1)
            in
                ( model, Random.generate SelectByIndex randomPhotoPicker )

        SetSize size ->
            ( { model | choosenSize = size }, Cmd.none )

        LoadPhotos (Ok responseStr) ->
            let
                urls : List String
                urls =
                    String.split "," responseStr

                photos : List Photo
                photos =
                    List.map Photo urls
            in
                ( { model
                    | photos = photos
                    , selectedUrl = List.head urls
                  }
                , Cmd.none
                )

        LoadPhotos (Err _) ->
            ( model, Cmd.none )


main : Program Never Model Msg
main =
    Html.program
        { init = ( initialModel, initialCmd )
        , view = view
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }
