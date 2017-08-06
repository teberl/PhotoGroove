port module PhotoGroove exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick, on)
import Html.Attributes as Attr exposing (id, class, classList, src, name, type_, title, checked, alt)
import Array exposing (Array)
import Random
import Http
import Json.Decode exposing (string, int, list, Decoder, at)
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
            canvas [ id "main-canvas", class "large" ] []


paperSlider : List (Attribute msg) -> List (Html msg) -> Html msg
paperSlider =
    node "paper-slider"


onImmediateValueChange : (Int -> msg) -> Attribute msg
onImmediateValueChange toMsg =
    -- let
    --     targetImmediateValue : Decoder Int
    --     targetImmediateValue =
    --         at [ "target", "immediateValue" ] int
    --     msgDecoder : Decoder msg
    --     msgDecoder =
    --         Json.Decode.map toMsg targetImmediateValue
    -- in
    --     on "immediate-value-change" msgDecoder
    at [ "target", "immediateValue" ] int
        |> Json.Decode.map toMsg
        |> on "immediate-value-changed"


viewFilter : String -> (Int -> Msg) -> Int -> Html Msg
viewFilter name toMsg magnitude =
    div [ class "filter-slider" ]
        [ label [] [ text name ]
        , paperSlider [ Attr.max "11", onImmediateValueChange toMsg ] []
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
        , div [ class "status" ] [ text model.status ]
        , div [ class "filters" ]
            [ viewFilter "Hue" SetHue model.hue
            , viewFilter "Ripple" SetRipple model.ripple
            , viewFilter "Noise" SetNoise model.noise
            ]
        , h3 [] [ text "Thumbnail Size:" ]
        , div [ id "choose-size" ]
            (List.map (viewSizeChooser model.choosenSize) [ Small, Medium, Large ])
        , div
            [ id "thumbnails"
            , class (sizeToClass model.choosenSize)
            ]
            (List.map (viewThumbnail model.selectedUrl) model.photos)
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


port setFilters : FilterOptions -> Cmd msg


port statusChanges : (String -> msg) -> Sub msg


type alias FilterOptions =
    { url : String
    , filters : List { name : String, amount : Float }
    }


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
    , status : String
    , hue : Int
    , ripple : Int
    , noise : Int
    , selectedUrl : Maybe String
    , loadingError : Maybe String
    , choosenSize : ThumbnailSize
    }


type Msg
    = SelectByUrl String
    | SetStatus String
    | SetHue Int
    | SetRipple Int
    | SetNoise Int
    | SelectByIndex Int
    | SupriseMe
    | SetSize ThumbnailSize
    | LoadPhotos (Result Http.Error (List Photo))


applyFilters : Model -> ( Model, Cmd Msg )
applyFilters model =
    case model.selectedUrl of
        Just selectedUrl ->
            let
                filters =
                    [ { name = "Hue", amount = toFloat model.hue / 11 }
                    , { name = "Ripple", amount = toFloat model.ripple / 11 }
                    , { name = "Noise", amount = toFloat model.noise / 11 }
                    ]

                url =
                    urlPrefix ++ "large/" ++ selectedUrl
            in
                ( model, setFilters { url = url, filters = filters } )

        Nothing ->
            ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectByUrl url ->
            applyFilters { model | selectedUrl = Just url }

        SetStatus status ->
            ( { model | status = status }, Cmd.none )

        SetHue hue ->
            applyFilters { model | hue = hue }

        SetRipple ripple ->
            applyFilters { model | ripple = ripple }

        SetNoise noise ->
            applyFilters { model | noise = noise }

        SelectByIndex index ->
            let
                newSelectedUrl : Maybe String
                newSelectedUrl =
                    model.photos
                        |> Array.fromList
                        |> Array.get index
                        |> Maybe.map .url
            in
                applyFilters { model | selectedUrl = newSelectedUrl }

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
            applyFilters
                { model | photos = photos, selectedUrl = Maybe.map .url (List.head photos) }

        LoadPhotos (Err _) ->
            ( { model
                | loadingError = Just "Error! (Have you tried to turn it off and on again ?!)"
              }
            , Cmd.none
            )


initialModel : Model
initialModel =
    { photos = []
    , status = ""
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


init : Float -> ( Model, Cmd Msg )
init flags =
    let
        status =
            "Initializing Pasta v" ++ toString flags
    in
        ( { initialModel | status = status }, initialCmd )


main : Program Float Model Msg
main =
    Html.programWithFlags
        { init = init
        , view = viewOrError
        , update = update
        , subscriptions = \_ -> statusChanges SetStatus
        }
