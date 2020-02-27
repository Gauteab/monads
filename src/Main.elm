module Main exposing (..)

import Browser
import Html exposing (Html, div, h1, img, text)
import Html.Attributes exposing (src)



---- MODEL ----


type alias Model =
    {}


init : ( Model, Cmd Msg )
init =
    ( {}, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ img [ src "/logo.svg" ] []
        , h1 [] [ text "Your Elm App is working!" ]
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }



--type alias State =
--    Int
--
--
--type ST a
--    = S (State -> ( a, State ))
--
--
--app : ST a -> State -> ( a, State )
--app (S st) x =
--    st x
--
--
--fmap : (a -> b) -> ST a -> ST b
--fmap g st =
--    S
--        (\s ->
--            let
--                ( x, s_ ) =
--                    app st s
--            in
--            ( g x, s_ )
--        )
--
--
--pure : a -> ST a
--pure x =
--    S (\s -> ( x, s ))
--
--
--apply : ST (a -> b) -> ST a -> ST b
--apply stf stx =
--    S
--        (\s ->
--            let
--                ( f, s_ ) =
--                    app stf s
--
--                ( x, s__ ) =
--                    app stx s_
--            in
--            ( f x, s__ )
--        )
--
--
--type Tree a
--    = Leaf a
--    | Node (Tree a) (Tree a)
--
--
--fresh : ST Int
--fresh =
--    S (\n -> ( n, n + 1 ))
--
--
--alabel : Tree a -> ST (Tree Int)
--alabel tree =
--    case tree of
--        Leaf a ->
--            fmap Leaf fresh
--
--        Node l r ->
--            apply (fmap Node (alabel l)) (alabel r)
--
--
--tr =
--    Node (Node (Leaf 0) (Leaf 1)) (Leaf 2)
--
--
--t =
--    Tuple.first (app (alabel tr) 0)
--


type alias Functor a b fa fb =
    { map : (a -> b) -> fa -> fb }


listf : Functor a b (List a) (List b)
listf =
    { map = List.map }


maybef : Functor a b (Maybe a) (Maybe b)
maybef =
    { map = Maybe.map }



--fmap : Functor a b fa fb -> (a -> b) -> fa -> fb


fmap instance f fa =
    instance.map f fa


mapIf : Functor a a fa fa -> (a -> Bool) -> (a -> a) -> fa -> fa
mapIf functor p f =
    functor.map
        (\x ->
            if p x then
                f x

            else
                x
        )



--- Applicative


type alias Applicative a b fa fb fab =
    { functor : Functor a b fa fb
    , lift : a -> fa
    , apply : fab -> fa -> fb
    }


maybea : Applicative a b (Maybe a) (Maybe b) (Maybe (a -> b))
maybea =
    Applicative maybef
        Just
        (\x y ->
            case ( x, y ) of
                ( Just f, Just a ) ->
                    Just (f a)

                _ ->
                    Nothing
        )


apply : Applicative a b fa fb fab -> fab -> fa -> fb
apply =
    .apply


lift : Applicative a b fa fb fab -> a -> fa
lift =
    .lift



--map2 : Applicative a b fa fb fab -> (a -> b -> c) -> fa -> fb -> fc
--map2 app f x y =
--    app.functor.map f x |> andMap app y
----map2 (+) (Just 1) (Just 2) --> Just 3


andMap : Applicative a b fa fb fab -> fa -> fab -> fb
andMap app a b =
    apply app b a


output =
    apply maybea (Just ((+) 1)) (Just 2)



--mapIf listf ((==) 0 << modBy 2) ((+) 1) [ 1, 2, 3, 4 ]
