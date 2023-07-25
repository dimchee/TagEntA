module Main exposing (..)

import Browser
import View


main : Program () () ()
main =
    Browser.sandbox
        { init = ()
        , view =
            \_ ->
                View.body
                    [ View.container (View.entity "entity1") [ View.tag "tag1", View.new ]
                    , View.search "search..."
                    , View.add
                    ]
        , update = \_ _ -> ()
        }
