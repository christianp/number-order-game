port module NumberOrderGame exposing (..)

import Browser
import Html as H exposing (Html, div)
import Html.Attributes as HA
import Html.Events as HE
import Random
import Random.List exposing (choices)
import Tuple exposing (first)

port setCSSVariable : (String, String) -> Cmd msg

type alias Numbers = List Int

type alias Game =
    { numbers: List Int
    , used_numbers : List Int
    , choices : List (Maybe Int)
    , turn: Int
    , bg_hue : Float
    }

type alias Stats =
    { played : Int
    , won : Int
    , correctly_placed : Int
    , incorrectly_placed : Int
    }

type alias Model = 
    { game : Game
    , stats : Stats
    , n : Int
    }

type Msg
    = PlaceNumber Int
    | SetGame Game
    | Restart

fi = String.fromInt
ff = String.fromFloat

listGet : Int -> List a -> Maybe a
listGet i = List.drop i >> List.head

listSet : Int -> a -> List a -> List a
listSet i x l = List.concat [ List.take i l,  x::(List.drop (i+1) l) ]

main = Browser.document
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

pick_bg_hue : Random.Generator Float
pick_bg_hue = Random.float 0 360

random_game : Int -> Random.Generator Game
random_game n = Random.List.choices n (List.range 100 999) |> Random.map first |> Random.map2 new_game pick_bg_hue

new_game : Float -> Numbers -> Game
new_game bg_hue numbers = 
    { numbers = numbers
    , used_numbers = []
    , choices = List.map (\_ -> Nothing) numbers
    , turn = 0
    , bg_hue = bg_hue
    }

generate_game : Model -> Cmd Msg
generate_game model = Random.generate SetGame (random_game model.n)

init : () -> (Model, Cmd Msg)
init _ = (init_model, generate_game init_model)

init_game = new_game 0 []

init_stats =
    { played = 0
    , won = 0
    , correctly_placed = 0
    , incorrectly_placed = 0
    }

init_model = 
    { game = init_game
    , stats = init_stats
    , n = 5
    }

nocmd model = (model, Cmd.none)

update msg model = case (Debug.log "msg" msg) of
    SetGame game -> ({ model | game = game }, setCSSVariable ("bg-hue", ff game.bg_hue))

    PlaceNumber i -> case (List.head model.game.numbers, listGet i model.game.choices) of
        (Just n, Just Nothing) -> 
            let
                game = model.game
                ngame = { game | numbers = List.drop 1 game.numbers, used_numbers = List.concat [game.used_numbers, List.take 1 game.numbers], choices = listSet i (Just n) game.choices, turn = game.turn + 1 }
            in
                { model | game = ngame } |> nocmd

        _ -> nocmd model

    Restart -> 
        let
            game = model.game
            stats = model.stats
            failed = not (correct_order game)
            correctly_placed = List.map2 (==) (List.map Just (List.sort game.used_numbers)) game.choices |> List.filter identity |> List.length
            nstats : Stats
            nstats = 
                { stats 
                | played = stats.played + 1
                , won = stats.won + (if failed then 0 else 1)
                , correctly_placed = stats.correctly_placed + correctly_placed
                , incorrectly_placed = stats.incorrectly_placed + model.n - correctly_placed
                }
            nmodel = { model | stats = nstats }
        in
            (nmodel, generate_game nmodel)

subscriptions model = Sub.none

correct_order : Game -> Bool
correct_order game = List.filterMap identity game.choices |> (\l -> l == (List.sort l))

view model = 
    let
        game = model.game
        stats = model.stats

        failed = not (correct_order game)

        complete = game.numbers == []

        view_choices = 
            let
                view_choice i c = case c of
                    Just n -> H.text <| fi n

                    Nothing -> 
                        if failed then
                            H.text "-"
                        else
                            H.button
                                [ HE.onClick (PlaceNumber i) 
                                , HA.class "place-number"
                                ]
                                [ H.text "-" ]
            in
                H.ol
                    [ HA.id "numbers" ]
                    (List.indexedMap
                        (\i -> \c -> 
                            H.li 
                                []
                                [view_choice i c]
                        ) 
                        game.choices
                    )

        view_status = 
                H.div
                    [ HA.id "status"
                    , HA.classList
                        [ ("complete", complete)
                        , ("failed", failed)
                        , ("restart", complete || failed)
                        ]
                    ]
                    <|

                    if failed || complete then
                            [ H.p [] [ H.text <| if correct_order game then "You did it!" else "Nope, not this time." ]
                            , H.button
                                [ HA.type_ "button"
                                , HE.onClick Restart
                                , HA.id "restart-button"
                                ]
                                [ H.text "Start a new game" ]
                            ]
                    else
                        case game.numbers of
                            x::_ -> 
                                    [ H.p [ HA.id "current-number" ] [ H.text <| fi x ] ]

                            [] -> []

        stat label value =
            H.div
                [ HA.class "stat"]
                [ H.span [HA.class "label"] [H.text <| label ++ ": "]
                , H.span [HA.class "number"] [H.text value]
                ]

        view_stats =
            H.div
                [ HA.id "stats" ]

                [ stat "Games won" ((fi stats.won) ++ " / " ++ (fi stats.played))
                , stat "Correctly placed" ((fi stats.correctly_placed) ++ " / " ++ (fi <| stats.correctly_placed + stats.incorrectly_placed))
                ]
    in
        { title = "Order those numbers!"
        , body =
            [ H.header
                []
                [ H.h1 [] [ H.text "Order those numbers!" ] ]
            , H.main_
                []
                [ view_choices
                , view_status
                , view_stats
                ]
            ]
        }

