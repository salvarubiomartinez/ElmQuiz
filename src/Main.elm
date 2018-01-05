module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode
import Json.Encode as Encode


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }



-- MODEL


type alias Model =
    { searchTerm : String
    , selectedCity : CityLocation
    , cities : List String
    , error : Maybe String
    }


type alias CityLocation =
    { geobyteslatitude : String
    , geobyteslongitude : String
    }


type alias JsonResponse =
    List String



-- INIT


init : ( Model, Cmd Msg )
init =
    ( Model "" (CityLocation "0.0" "0.0") [] Maybe.Nothing, Cmd.none )


urlSearchCities : String
urlSearchCities =
    "https://salvadorrubiomartinez.nowapps.org/AutoCompleteCity.php?q="


urlCityDetails : String
urlCityDetails =
    "https://salvadorrubiomartinez.nowapps.org/GetCityDetails.php?q="



-- UPDATE


type Msg
    = SearchCity String
    | GetCities (Result Http.Error JsonResponse)
    | SelectCity String
    | GetCoordenates (Result Http.Error CityLocation)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SearchCity searchTerm ->
            if (String.length searchTerm) >= 3 then
                ( { model | searchTerm = searchTerm }, searchCity searchTerm )
            else
                ( { model | searchTerm = searchTerm, cities = [] }, Cmd.none )

        GetCities (Ok cities) ->
            ( { model | cities = cities, error = Maybe.Nothing }, Cmd.none )

        GetCities (Err error) ->
            ( { model | cities = [], error = Just "could not get the cities", selectedCity = CityLocation "0.0" "0.0" }, Cmd.none )

        SelectCity city ->
            ( { model | searchTerm = city, cities = [] }, getCoordenates city )

        GetCoordenates (Ok city) ->
            ( { model | selectedCity = city, error = Maybe.Nothing }, Cmd.none )

        GetCoordenates (Err error) ->
            ( { model | error = Just "Error retriving coordenates" }, Cmd.none )


searchCity : String -> Cmd Msg
searchCity searchTerm =
    Http.send GetCities <|
        Http.get (urlSearchCities ++ searchTerm) <|
            decodeResponse


getCoordenates : String -> Cmd Msg
getCoordenates city =
    Http.send GetCoordenates <|
        Http.get (urlCityDetails ++ city) <|
            decodeCityLocation


decodeCityLocation : Decode.Decoder CityLocation
decodeCityLocation =
    Decode.map2 CityLocation
        (Decode.field "geobyteslatitude" Decode.string)
        (Decode.field "geobyteslongitude" Decode.string)


decodeResponse : Decode.Decoder JsonResponse
decodeResponse =
    Decode.list Decode.string


type alias Coordenates =
    { latitude : Float
    , longitude : Float
    }



-- VIEW


view : Model -> Html Msg
view model =
    let
        location =
            let
                latitude =
                    case (String.toFloat model.selectedCity.geobyteslatitude) of
                        Ok lat ->
                            lat

                        Err error ->
                            0.0

                longitude =
                    case (String.toFloat model.selectedCity.geobyteslongitude) of
                        Ok lat ->
                            lat

                        Err error ->
                            0.0
            in
                Coordenates latitude longitude

        showError =
            case model.error of
                Nothing ->
                    False

                Just err ->
                    True

        error =
            case model.error of
                Nothing ->
                    ""

                Just err ->
                    err
    in
        div []
            [ if showError then
                div [ class "alert alert-danger" ] [ text error ]
              else
                div [] []
            , div []
                [ input
                    [ class "form-control"
                    , type_ "text"
                    , placeholder "Name"
                    , onInput SearchCity
                    , value model.searchTerm
                    ]
                    []
                ]
            , div [ class "list-group" ] (List.map (\city -> button [ type_ "button", class "list-group-item", onClick (SelectCity city) ] [ Html.text city ]) model.cities)
            , googleMap
                [ style
                    [ ( "height", "600px" )
                    , ( "width", "100%" )
                    ]
                , attribute "api-key" "AIzaSyDGP0nmkgVQ9x8f5YxHHG2ssmPPumtl6H4"
                , property "latitude" (Encode.float location.latitude)
                , property "longitude" (Encode.float location.longitude)
                ]
                (if location.latitude /= 0.0 && location.longitude /= 0.0 then
                    [ googleMapMarker
                        [ property "latitude" (Encode.float location.latitude)
                        , property "longitude" (Encode.float location.longitude)
                        ]
                        []
                    ]
                 else
                    []
                )
            ]


googleMap : List (Attribute a) -> List (Html a) -> Html a
googleMap =
    Html.node "google-map"


googleMapMarker : List (Attribute a) -> List (Html a) -> Html a
googleMapMarker =
    Html.node "google-map-marker"
