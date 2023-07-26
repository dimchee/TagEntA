module TagEnt exposing (..)

import Dict exposing (Dict)
import Set exposing (Set)


type alias Entity =
    String


type alias Tag =
    String


type alias TagEnt =
    { tags : Dict Entity (Set Tag)
    , ents : Dict Tag (Set Entity)
    }


asTree : TagEnt -> List ( Entity, List Tag )
asTree { tags } =
    Dict.toList tags |> List.map (Tuple.mapSecond Set.toList)


tagEntities : Tag -> TagEnt -> List Entity
tagEntities tag { ents } =
    Dict.get tag ents |> Maybe.map Set.toList |> Maybe.withDefault []


entityTags : Entity -> TagEnt -> List Tag
entityTags ent { tags } =
    Dict.get ent tags |> Maybe.map Set.toList |> Maybe.withDefault []


appendTo : ( comparable, comparable2 ) -> Dict comparable (Set comparable2) -> Dict comparable (Set comparable2)
appendTo ( key, val ) =
    Dict.update key (Maybe.withDefault Set.empty >> Set.insert val >> Just)


{-| automatically creates Entity and Tag, if they don't exist
-}
addEdge : ( Entity, Tag ) -> TagEnt -> TagEnt
addEdge ( ent, tag ) rel =
    { rel | ents = appendTo ( tag, ent ) rel.ents, tags = appendTo ( ent, tag ) rel.tags }


addEntity : Entity -> TagEnt -> TagEnt
addEntity ent rel =
    { rel | tags = Dict.insert ent Set.empty rel.tags }


addTag : Tag -> TagEnt -> TagEnt
addTag tag rel =
    { rel | ents = Dict.insert tag Set.empty rel.ents }


removeTag : Tag -> TagEnt -> TagEnt
removeTag tag rel =
    { rel
        | tags = Dict.map (\_ set -> Set.remove tag set) rel.tags
        , ents = Dict.update tag (\_ -> Nothing) rel.ents
    }


removeEntity : Entity -> TagEnt -> TagEnt
removeEntity ent rel =
    { rel
        | ents = Dict.map (\_ set -> Set.remove ent set) rel.ents
        , tags = Dict.update ent (\_ -> Nothing) rel.tags
    }


example : TagEnt
example =
    { tags =
        Dict.fromList
            [ ( "ent1", Set.fromList [ "tag1", "tag2", "tag3" ] )
            , ( "ent2", Set.fromList [ "tag1", "tag4", "tag2" ] )
            , ( "ent3", Set.fromList [] )
            ]
    , ents =
        Dict.fromList
            [ ( "tag1", Set.fromList [ "ent1", "ent2" ] )
            , ( "tag2", Set.fromList [ "ent1", "ent2" ] )
            , ( "tag3", Set.fromList [ "ent1" ] )
            , ( "tag4", Set.fromList [ "ent2" ] )
            ]
    }
