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
    , cities : List Prediction
    , error : Maybe String
    , coordenates : Maybe Location
    }


type alias CityDetailsResponse =
    { result : CityDetailsResult
    }


type alias CityDetailsResult =
    { geometry : Geometry
    }


type alias Geometry =
    { location : Location
    }


type alias Location =
    { lat : Float
    , lng : Float
    }


type alias PredictionResponse =
    { predictions : List Prediction
    }


type alias Prediction =
    { description : String
    , place_id : String
    }



-- INIT


init : ( Model, Cmd Msg )
init =
    ( Model "" [] Maybe.Nothing Maybe.Nothing, Cmd.none )


urlSearchCities : String
urlSearchCities =
    "https://maps.googleapis.com/maps/api/place/autocomplete/json?key=" ++ googleApiKey ++ "&input="


urlCityDetails : String
urlCityDetails =
    "https://maps.googleapis.com/maps/api/place/details/json?key=" ++ googleApiKey ++ "&place_id="


googleApiKey : String
googleApiKey =
    "AIzaSyDGP0nmkgVQ9x8f5YxHHG2ssmPPumtl6H4"



-- UPDATE


type Msg
    = SearchCity String
    | GetCities (Result Http.Error PredictionResponse)
    | SelectCity Prediction
    | GetCoordenates (Result Http.Error CityDetailsResponse)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SearchCity searchTerm ->
            if (String.length searchTerm) >= 2 then
                ( { model
                    | searchTerm = searchTerm
                  }
                , searchCity searchTerm
                )
            else
                ( { model
                    | searchTerm = searchTerm
                    , cities = []
                  }
                , Cmd.none
                )

        GetCities (Ok cities) ->
            ( { model
                | cities = cities.predictions
                , error = Maybe.Nothing
              }
            , Cmd.none
            )

        GetCities (Err error) ->
            ( { model
                | cities = []
                , error = Just "could not get the cities"
                , coordenates = Maybe.Nothing
              }
            , Cmd.none
            )

        SelectCity city ->
            ( { model
                | searchTerm = city.description
                , cities = []
              }
            , getCoordenates city.place_id
            )

        GetCoordenates (Ok response) ->
            ( { model
                | coordenates = Just response.result.geometry.location
                , error = Maybe.Nothing
              }
            , Cmd.none
            )

        GetCoordenates (Err error) ->
            ( { model
                | error = Just "Error retrieving coordenates"
              }
            , Cmd.none
            )


searchCity : String -> Cmd Msg
searchCity searchTerm =
    Http.send GetCities <|
        Http.get (urlSearchCities ++ searchTerm) <|
            decodeResponse


decodeResponse : Decode.Decoder PredictionResponse
decodeResponse =
    Decode.map PredictionResponse
        (Decode.field "predictions" (Decode.list decodePrediction))


decodePrediction : Decode.Decoder Prediction
decodePrediction =
    Decode.map2 Prediction
        (Decode.field "description" Decode.string)
        (Decode.field "place_id" Decode.string)


getCoordenates : String -> Cmd Msg
getCoordenates cityId =
    Http.send GetCoordenates <|
        Http.get (urlCityDetails ++ cityId) <|
            decodeCityLocation


decodeCityLocation : Decode.Decoder CityDetailsResponse
decodeCityLocation =
    Decode.map CityDetailsResponse
        (Decode.field "result" decodeCityDetailsResult)


decodeCityDetailsResult : Decode.Decoder CityDetailsResult
decodeCityDetailsResult =
    Decode.map CityDetailsResult
        (Decode.field "geometry" decodeGeometry)


decodeGeometry : Decode.Decoder Geometry
decodeGeometry =
    Decode.map Geometry
        (Decode.field "location" decodeLocation)


decodeLocation : Decode.Decoder Location
decodeLocation =
    Decode.map2 Location
        (Decode.field "lat" Decode.float)
        (Decode.field "lng" Decode.float)



-- VIEW


view : Model -> Html Msg
view model =
    let
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
            , div [ class "list-group" ]
                (List.map
                    (\city ->
                        button
                            [ type_ "button"
                            , class "list-group-item"
                            , onClick (SelectCity city)
                            ]
                            [ Html.text city.description ]
                    )
                    model.cities
                )
            , (case model.coordenates of
                Nothing ->
                    div [] []

                Just coordenates ->
                    googleMap
                        [ style
                            [ ( "height", "600px" )
                            , ( "width", "100%" )
                            ]
                        , attribute "api-key" "AIzaSyDGP0nmkgVQ9x8f5YxHHG2ssmPPumtl6H4"
                        , property "latitude" (Encode.float coordenates.lat)
                        , property "longitude" (Encode.float coordenates.lng)
                        ]
                        [ googleMapMarker
                            [ property "latitude" (Encode.float coordenates.lat)
                            , property "longitude" (Encode.float coordenates.lng)
                            ]
                            []
                        ]
              )
            ]


googleMap : List (Attribute a) -> List (Html a) -> Html a
googleMap =
    Html.node "google-map"


googleMapMarker : List (Attribute a) -> List (Html a) -> Html a
googleMapMarker =
    Html.node "google-map-marker"
