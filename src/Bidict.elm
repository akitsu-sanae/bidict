module Bidict exposing
    ( Bidict
    , empty
    , fromList
    , getByKey
    , getByValue
    , insert
    , isEmpty
    , keys
    , member
    , memberKey
    , memberValue
    , removeByKey
    , singleton
    , size
    , values
    )

import Dict exposing (Dict)


type alias Bidict comparable1 comparable2 =
    { key2Value : Dict comparable1 comparable2
    , value2Key : Dict comparable2 comparable1
    }



-- Build


empty : Bidict comparable1 comparable2
empty =
    Bidict Dict.empty Dict.empty


singleton : comparable1 -> comparable2 -> Bidict comparable1 comparable2
singleton k v =
    Bidict (Dict.singleton k v) (Dict.singleton v k)


insert : comparable1 -> comparable2 -> Bidict comparable1 comparable2 -> Bidict comparable1 comparable2
insert k v dict =
    Bidict (Dict.insert k v dict.key2Value) (Dict.insert v k dict.value2Key)


removeByKey : comparable1 -> Bidict comparable1 comparable2 -> Bidict comparable1 comparable2
removeByKey k dict =
    case Dict.get k dict.key2Value of
        Nothing ->
            dict

        Just v ->
            Bidict (Dict.remove k dict.key2Value) (Dict.remove v dict.value2Key)



-- Query


isEmpty : Bidict comparable1 comparable2 -> Bool
isEmpty dict =
    Dict.isEmpty dict.key2Value


memberKey : comparable1 -> Bidict comparable1 comparable2 -> Bool
memberKey k dict =
    Dict.member k dict.key2Value


memberValue : comparable2 -> Bidict comparable1 comparable2 -> Bool
memberValue v dict =
    Dict.member v dict.value2Key


member : comparable1 -> comparable2 -> Bidict comparable1 comparable2 -> Bool
member k v dict =
    case Dict.get k dict.key2Value of
        Nothing ->
            False

        Just value ->
            v == value


getByKey : comparable1 -> Bidict comparable1 comparable2 -> Maybe comparable2
getByKey k dict =
    Dict.get k dict.key2Value


getByValue : comparable2 -> Bidict comparable1 comparable2 -> Maybe comparable1
getByValue v dict =
    Dict.get v dict.value2Key


size : Bidict comparable1 comparable2 -> Int
size dict =
    Dict.size dict.key2Value



-- Lists


keys : Bidict comparable1 comparable2 -> List comparable1
keys dict =
    Dict.keys dict.key2Value


values : Bidict comparable1 comparable2 -> List comparable2
values dict =
    Dict.keys dict.value2Key


fromList : List ( comparable1, comparable2 ) -> Bidict comparable1 comparable2
fromList list =
    let
        rev =
            List.map (\( k, v ) -> ( v, k )) list
    in
    Bidict (Dict.fromList list) (Dict.fromList rev)



-- Transform
-- Combine
