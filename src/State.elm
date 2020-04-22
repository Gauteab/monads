module State exposing (..)


type alias State =
    Int


type ST a
    = S (State -> ( a, State ))


app : ST a -> State -> ( a, State )
app (S st) x =
    st x


fmap : (a -> b) -> ST a -> ST b
fmap g st =
    S
        (\s ->
            let
                ( x, s_ ) =
                    app st s
            in
            ( g x, s_ )
        )


pure : a -> ST a
pure x =
    S (\s -> ( x, s ))


apply : ST (a -> b) -> ST a -> ST b
apply stf stx =
    S
        (\s ->
            let
                ( f, s_ ) =
                    app stf s

                ( x, s__ ) =
                    app stx s_
            in
            ( f x, s__ )
        )


type Tree a
    = Leaf a
    | Node (Tree a) (Tree a)


fresh : ST Int
fresh =
    S (\n -> ( n, n + 1 ))


alabel : Tree a -> ST (Tree Int)
alabel tree =
    case tree of
        Leaf a ->
            fmap Leaf fresh

        Node l r ->
            apply (fmap Node (alabel l)) (alabel r)


tr =
    Node (Node (Leaf 0) (Leaf 1)) (Leaf 2)


t =
    Tuple.first (app (alabel tr) 0)
