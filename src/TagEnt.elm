module TagEnt exposing (..)

import Dict exposing (Dict)
import Set exposing (Set)


type alias Entity =
    String


type alias Tag =
    String


type alias TagEnt =
    { tags_ : Dict Entity (Set Tag)
    , ents_ : Dict Tag (Set Entity)
    }


tags : TagEnt -> List Tag
tags { ents_ } =
    Dict.toList ents_ |> List.map Tuple.first


entities : TagEnt -> List Entity
entities { tags_ } =
    Dict.toList tags_ |> List.map Tuple.first


asTree : TagEnt -> List ( Entity, List Tag )
asTree { tags_ } =
    Dict.toList tags_ |> List.map (Tuple.mapSecond Set.toList)


tagEntities : Tag -> TagEnt -> List Entity
tagEntities tag { ents_ } =
    Dict.get tag ents_ |> Maybe.map Set.toList |> Maybe.withDefault []


entityTags : Entity -> TagEnt -> List Tag
entityTags ent { tags_ } =
    Dict.get ent tags_ |> Maybe.map Set.toList |> Maybe.withDefault []


appendTo : ( comparable, comparable2 ) -> Dict comparable (Set comparable2) -> Dict comparable (Set comparable2)
appendTo ( key, val ) =
    Dict.update key (Maybe.withDefault Set.empty >> Set.insert val >> Just)


{-| automatically creates Entity and Tag, if they don't exist
-}
addEdge : ( Entity, Tag ) -> TagEnt -> TagEnt
addEdge ( ent, tag ) rel =
    { rel | ents_ = appendTo ( tag, ent ) rel.ents_, tags_ = appendTo ( ent, tag ) rel.tags_ }


addEntity : Entity -> TagEnt -> TagEnt
addEntity ent rel =
    { rel | tags_ = Dict.insert ent Set.empty rel.tags_ }


addTag : Tag -> TagEnt -> TagEnt
addTag tag rel =
    { rel | ents_ = Dict.insert tag Set.empty rel.ents_ }


removeTag : Tag -> TagEnt -> TagEnt
removeTag tag rel =
    { rel
        | tags_ = Dict.map (\_ set -> Set.remove tag set) rel.tags_
        , ents_ = Dict.update tag (\_ -> Nothing) rel.ents_
    }


removeEntity : Entity -> TagEnt -> TagEnt
removeEntity ent rel =
    { rel
        | ents_ = Dict.map (\_ set -> Set.remove ent set) rel.ents_
        , tags_ = Dict.update ent (\_ -> Nothing) rel.tags_
    }


example : TagEnt
example =
    { tags_ =
        Dict.fromList
            [ ( "ent1", Set.fromList [ "tag1", "tag2", "tag3" ] )
            , ( "ent2", Set.fromList [ "tag1", "tag4", "tag2" ] )
            , ( "ent3", Set.fromList [] )
            ]
    , ents_ =
        Dict.fromList
            [ ( "tag1", Set.fromList [ "ent1", "ent2" ] )
            , ( "tag2", Set.fromList [ "ent1", "ent2" ] )
            , ( "tag3", Set.fromList [ "ent1" ] )
            , ( "tag4", Set.fromList [ "ent2" ] )
            ]
    }
