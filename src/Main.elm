module Main exposing (..)


type alias Functor a b fa fb =
    { map : (a -> b) -> fa -> fb }


listF : Functor a b (List a) (List b)
listF =
    { map = List.map }


maybeF : Functor a b (Maybe a) (Maybe b)
maybeF =
    { map = Maybe.map }


map : Functor a b fa fb -> (a -> b) -> fa -> fb
map =
    .map


mapIf : Functor a a fa fa -> (a -> Bool) -> (a -> a) -> fa -> fa
mapIf functor p f =
    functor.map
        (\x ->
            if p x then
                f x

            else
                x
        )


type alias A2 a1 fa1 fbc1 fb1 fc1 a2 fa2 fbc2 fb2 fc2 =
    ( Applicative a1 fa1 fbc1 fb1 fc1, Applicative a2 fa2 fbc2 fb2 fc2 )


type alias Applicative a fa fbc fb fc =
    { pure : a -> fa
    , apply : fbc -> fb -> fc
    }


pure : Applicative a fa fbc fb fc -> (a -> fa)
pure =
    .pure


apply : Applicative a fa fbc fb fc -> (fbc -> fb -> fc)
apply =
    .apply


andMap : Applicative a fa fbc fb fc -> (fb -> fbc -> fc)
andMap app a b =
    apply app b a


maybeA : Applicative a (Maybe a) (Maybe (b -> c)) (Maybe b) (Maybe c)
maybeA =
    let
        pure_ : a -> Maybe a
        pure_ =
            Just

        apply_ : Maybe (b -> c) -> Maybe b -> Maybe c
        apply_ x y =
            case ( x, y ) of
                ( Just f, Just a ) ->
                    Just (f a)

                _ ->
                    Nothing
    in
    Applicative pure_ apply_


listA : Applicative a (List a) (List (b -> c)) (List b) (List c)
listA =
    let
        pure_ : a -> List a
        pure_ =
            List.singleton

        apply_ : List (b -> c) -> List b -> List c
        apply_ gs xs =
            List.concatMap (\g -> List.map (\x -> g x) xs) gs
    in
    Applicative pure_ apply_


maybeA2 : A2 a (Maybe a) (Maybe (b -> c)) (Maybe b) (Maybe c) d (Maybe d) (Maybe (e -> f)) (Maybe e) (Maybe f)
maybeA2 =
    ( maybeA, maybeA )


maybeA3 =
    ( maybeA, maybeA, maybeA )


map2 : ( Applicative b fbc fbc c a, Applicative d fa a e fc ) -> b -> c -> e -> fc
map2 ( a1, a2 ) f x y =
    pure a1 f
        |> andMap a1 x
        |> andMap a2 y


map3 : ( Applicative b fbc fbc c a, Applicative d fa a e f, Applicative g h f i fc ) -> b -> c -> e -> i -> fc
map3 ( a1, a2, a3 ) f x y z =
    pure a1 f
        |> andMap a1 x
        |> andMap a2 y
        |> andMap a3 z


print : a -> ()
print a =
    let
        x =
            Debug.log "" a
    in
    ()



--traverseListMaybe : (List b -> Maybe (List b)) -> (a -> Maybe b) -> List a -> Maybe (List b)
--traverseListMaybe :
--    Applicative_ (a -> List a -> List a) c c b d
--    -> Applicative_ (List e) f d unknown f
--    -> (g -> b)
--    -> List g
--    -> f
--type alias Traversable a b c d e = {traverse:Applicative a b c d e  }
--traverseList : A2 a b c d e listB maybeListB h i j -> (a -> maybeA) -> listA -> maybeListB
--traverseList : Applicative -> Applicative (List b) maybeListB  ->(a->fa)->(List a)->maybeListB


traverseList :
    --Applicative (a -> List a -> List a) fbc fbc b c
    ---> Applicative (List d) fc c fc fc
    A2 (a -> List a -> List a) fbc fbc b c (List d) fc c fc fc
    -> (e -> b)
    -> List e
    -> fc
traverseList ( a1, a2 ) f list =
    let
        go =
            traverseList ( a1, a2 ) f
    in
    case list of
        [] ->
            pure a2 []

        x :: xs ->
            map2 ( a1, a2 ) (::) (f x) (go xs)


main_ =
    [ print ""
    , print <| traverseList ( listA, listA ) (\x -> [ x, -x ]) [ 1, 2, 3 ]
    , print <| traverseList ( listA, listA ) identity [ [ 1, 2 ], [ 3, 4 ] ]
    , print <| traverseList ( maybeA, maybeA ) String.toInt [ "1" ]
    ]


test3 =
    maybeA.pure ((+) 1) |> andMap maybeA (Just 1)


test =
    Just (+)
        |> andMap maybeA (Just 1)
        |> andMap maybeA (Just 2)


test2 =
    Just (::)
        |> andMap maybeA (Just 1)
        |> andMap maybeA (Just [ 2 ])
