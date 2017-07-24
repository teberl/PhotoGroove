module PhotoGroove exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (..)
import Array exposing (Array)
import Random


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


viewLarge : Maybe String -> Html Msg
viewLarge maybeUrl =
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
        , viewLarge model.selectedUrl
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
    , choosenSize = Small
    }


type Msg
    = SelectByUrl String
    | SelectByIndex Int
    | SupriseMe
    | SetSize ThumbnailSize


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


main : Program Never Model Msg
main =
    Html.program
        { init = ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = (\model -> Sub.none)
        }
