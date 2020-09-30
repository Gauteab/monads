module Main exposing (..)

--- Functors


type Functor a b fa fb
    = Functor ((a -> b) -> fa -> fb)


type alias Functor_ a fa =
    Functor a a fa fa


listF : Functor a b (List a) (List b)
listF =
    Functor List.map


maybeF : Functor a b (Maybe a) (Maybe b)
maybeF =
    Functor Maybe.map


map : Functor a b fa fb -> (a -> b) -> fa -> fb
map (Functor f) =
    f


mapIf : Functor a a fa fa -> (a -> Bool) -> (a -> a) -> fa -> fa
mapIf functor p f =
    map functor <|
        \x ->
            if p x then
                f x

            else
                x



--- Applicatives


type alias Applicative a fa fbc fb fc =
    { pure : a -> fa
    , andMap : fb -> fbc -> fc
    }


type alias Applicative_ fa fab fb fc =
    Applicative fa fab fab fb fc


maybeA : Applicative a (Maybe a) (Maybe (b -> c)) (Maybe b) (Maybe c)
maybeA =
    let
        pure : a -> Maybe a
        pure =
            Just

        andMap : Maybe b -> Maybe (b -> c) -> Maybe c
        andMap x y =
            case ( x, y ) of
                ( Just a, Just f ) ->
                    Just (f a)

                _ ->
                    Nothing
    in
    Applicative pure andMap


listA : Applicative a (List a) (List (b -> c)) (List b) (List c)
listA =
    let
        pure : a -> List a
        pure =
            List.singleton

        andMap : List b -> List (b -> c) -> List c
        andMap xs fs =
            List.concatMap (\f -> List.map (\x -> f x) xs) fs
    in
    Applicative pure andMap


maybeA2 =
    Applicative2 maybeA.pure (map2 maybeA maybeA)


listA2 =
    Applicative2 listA.pure (map2 listA listA)


map2 a1 a2 f x y =
    a1.pure f
        |> a1.andMap x
        |> a2.andMap y


map3 a1 a2 a3 f x y z =
    a1.pure f
        |> a1.andMap x
        |> a2.andMap y
        |> a3.andMap z


type alias Pure a fa =
    a -> fa


type alias Map2 a b c fa fb fc =
    (a -> b -> c) -> fa -> fb -> fc


{-| Instance supporting pure and map2
-}
type alias Applicative2 a fa b c d fb fc fd =
    { pure : Pure a fa
    , map2 : Map2 b c d fb fc fd
    }


{-| Simple Instance supporting pure and map2
-}
type alias Applicative2_ a fa ta fta =
    Applicative2 ta fta a ta ta fa fta fta



--- Traversable
--type alias Traversable a fb ta ftb =
--    (a -> fb) -> ta -> ftb


type alias Traverse a b fb ta tb ftb =
    Applicative2_ b fb tb ftb -> (a -> fb) -> ta -> ftb



-- which of these signatures is best?
--
-- traverseList :
--     Applicative2_ b fb (List b) fListB
--     -> (a -> fb)
--     -> List a
--     -> fListB
--
-- traverseList : Traverse a b fb (List a) (List b) fListB


listTraverse :
    Applicative2_ b fb (List b) fListB
    -> (a -> fb)
    -> List a
    -> fListB
listTraverse app f list =
    case list of
        [] ->
            app.pure []

        x :: xs ->
            app.map2 (::) (f x) (listTraverse app f xs)


main_ =
    [ print "Traverse:"
    , print <| listTraverse listA2 (\x -> [ x, -x ]) [ 1, 2, 3 ]
    , print <| listTraverse listA2 identity [ [ 1, 2 ], [ 3, 4 ] ]
    , print <| listTraverse maybeA2 String.toInt [ "1", "2" ]
    , print <| listTraverse maybeA2 String.toInt [ "a", "2" ]
    ]


print : a -> ()
print a =
    let
        _ =
            Debug.log "" a
    in
    ()


test3 =
    maybeA.pure ((+) 1) |> maybeA.andMap (Just 1)


test =
    maybeA.pure (+)
        |> maybeA.andMap (Just 1)
        |> maybeA.andMap (Just 2)


test2 =
    maybeA.pure (::)
        |> maybeA.andMap (Just 1)
        |> maybeA.andMap (Just [ 2 ])
