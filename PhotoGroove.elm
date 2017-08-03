module PhotoGroove exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes as Attr exposing (id, class, classList, src, name, type_, title, checked, alt)
import Array exposing (Array)
import Random
import Http
import Json.Decode exposing (string, int, list, Decoder)
import Json.Decode.Pipeline exposing (decode, required, optional)


urlPrefix : String
urlPrefix =
    "http://elm-in-action.com/"


generateThumbnailTitleText : Photo -> String
generateThumbnailTitleText thumbnail =
    thumbnail.title ++ " [" ++ toString thumbnail.size ++ " KB]"


viewThumbnail : Maybe String -> Photo -> Html Msg
viewThumbnail selectedUrl thumbnail =
    img
        [ src (urlPrefix ++ thumbnail.url)
        , title (generateThumbnailTitleText thumbnail)
        , alt (generateThumbnailTitleText thumbnail)
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


paperSlider : List (Attribute msg) -> List (Html msg) -> Html msg
paperSlider =
    node "paper-slider"


viewFilter : String -> Int -> Html Msg
viewFilter name magnitude =
    div [ class "filter-slider" ]
        [ label [] [ text name ]
        , paperSlider [ Attr.max "11" ] []
        , label [] [ text (toString magnitude) ]
        ]


viewOrError : Model -> Html Msg
viewOrError model =
    case model.loadingError of
        Nothing ->
            view model

        Just errorMessage ->
            div [ class "error-message" ]
                [ h1 [] [ text "Photo Groove" ]
                , p [] [ text errorMessage ]
                ]


view : Model -> Html Msg
view model =
    div [ class "content" ]
        [ h1 [] [ text "Photo Groove" ]
        , button [ onClick SupriseMe ] [ text "Suprise Me!" ]
        , div [ class "filters" ]
            [ viewFilter "Hue" 0
            , viewFilter "Ripple" 0
            , viewFilter "Noise" 0
            ]
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
    { url : String
    , size : Int
    , title : String
    }


photoDecoder : Decoder Photo
photoDecoder =
    decode Photo
        |> required "url" string
        |> required "size" int
        |> optional "title" string "(untitled)"


type alias Model =
    { photos : List Photo
    , hue : Int
    , ripple : Int
    , noise : Int
    , selectedUrl : Maybe String
    , loadingError : Maybe String
    , choosenSize : ThumbnailSize
    }


type Msg
    = SelectByUrl String
    | SetHue Int
    | SetRipple Int
    | SetNoise Int
    | SelectByIndex Int
    | SupriseMe
    | SetSize ThumbnailSize
    | LoadPhotos (Result Http.Error (List Photo))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectByUrl url ->
            ( { model | selectedUrl = Just url }, Cmd.none )

        SetHue hue ->
            ( { model | hue = hue }, Cmd.none )

        SetRipple ripple ->
            ( { model | ripple = ripple }, Cmd.none )

        SetNoise noise ->
            ( { model | noise = noise }, Cmd.none )

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

        LoadPhotos (Ok photos) ->
            ( { model
                | photos = photos
                , selectedUrl = Maybe.map .url (List.head photos)
              }
            , Cmd.none
            )

        LoadPhotos (Err _) ->
            ( { model
                | loadingError = Just "Error! (Have you tried to turn it off and on again ?!)"
              }
            , Cmd.none
            )


initialModel : Model
initialModel =
    { photos = []
    , hue = 0
    , ripple = 0
    , noise = 0
    , selectedUrl = Nothing
    , loadingError = Nothing
    , choosenSize = Medium
    }


initialCmd : Cmd Msg
initialCmd =
    list photoDecoder
        |> Http.get "http://elm-in-action.com/photos/list.json"
        |> Http.send LoadPhotos


main : Program Never Model Msg
main =
    Html.program
        { init = ( initialModel, initialCmd )
        , view = viewOrError
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }
