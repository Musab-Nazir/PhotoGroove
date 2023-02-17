module Main exposing (main)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Url exposing (Url)
import Html exposing (Html, a, footer, h1, li, nav, text, ul)
import Html.Attributes exposing (classList, href)
import Html.Lazy exposing (lazy)
import Url.Parser as Parser

import PhotoGroove as Gallery

type alias Model = 
  { page: Page
  , key: Nav.Key
  }

type Page
  = HomePage
  | GalleryPage Gallery.Model
  | NotFound

type Route
  = Home
  | Gallery

parser : Parser.Parser (Route -> a) a
parser =
  Parser.oneOf
    [ Parser.map Home Parser.top
    , Parser.map Gallery (Parser.s "gallery")
    ]

view : Model -> Document Msg
view model =
  let
    content =
      case model.page of
        HomePage ->
          text "This is the home page"
        GalleryPage gallery ->
            Gallery.view gallery
                |> Html.map GotGalleryMsg
        NotFound ->
          text "Not Found"
  in
  { title = "Photo Groove, SPA Style"
  , body =
      [ lazy viewHeader model.page
      , content
      , viewFooter
      ]
  }

viewFooter : Html msg
viewFooter =
  footer [] [ text "One is never alone with a rubber duck. -Douglas Adams" ]

viewHeader : Page -> Html Msg
viewHeader page =
  let
    logo =
      h1 [] [ text "Photo Groove" ]
    links =
      ul []
        [ navLink Home { url = "/", caption = "home" }
        , navLink Gallery { url = "/gallery", caption = "Gallery" }
        ]

    navLink : Route -> { url : String, caption : String } -> Html msg
    navLink route { url, caption } =
      li [ classList [ ( "active", isActive {link = route, page=page}) ] ]
      [ a [ href url ] [ text caption ] ]
  in
    nav [] [ logo, links ]

isActive : { link : Route, page : Page } -> Bool
isActive { link, page } =
  case ( link, page ) of
    ( Gallery, GalleryPage _) -> True
    ( Gallery, _ ) -> False
    ( Home, HomePage ) -> True
    ( Home, GalleryPage _ ) -> False
    ( _, NotFound ) -> False

type Msg
  = ClickedLink Browser.UrlRequest
  | ChangedUrl Url
  | GotGalleryMsg Gallery.Msg

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
      ClickedLink urlRequest ->
          case urlRequest of
              Browser.External href ->
                ( model, Nav.load href )
              Browser.Internal url ->
                ( model, Nav.pushUrl model.key (Url.toString url) )
      ChangedUrl url ->
        updateUrl url model
      GotGalleryMsg galleryMsg ->
        case model.page of 
          GalleryPage gallery ->
            toGallery model (Gallery.update galleryMsg gallery)
          _ -> (model, Cmd.none)

toGallery : Model -> ( Gallery.Model, Cmd Gallery.Msg ) -> ( Model, Cmd Msg ) 
toGallery model ( gallery, cmd ) =
  ( { model | page = GalleryPage gallery }
  , Cmd.map GotGalleryMsg cmd
  )

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init version url key =
    updateUrl url { page = NotFound, key = key }

updateUrl : Url -> Model -> ( Model, Cmd Msg )
updateUrl url model =
  case Parser.parse parser url of
  Just Gallery ->
      toGallery model (Gallery.init ())
  Just Home ->
      ( { model | page = HomePage }, Cmd.none )
  Nothing ->
      ( { model | page = NotFound }, Cmd.none )

main : Program () Model Msg
main =
  Browser.application
    { init = init
    , onUrlRequest = ClickedLink
    , onUrlChange = ChangedUrl
    , subscriptions = \_ -> Sub.none
    , update = update
    , view = view
    }
