module Main where

import Control.Monad.State

import Pokemon
import Battle
import Parse


-- ELIMINAR AL QUITAR TRAINERS
import Data.Array


main = do
    pokeMap <- parsePokemon
    moveMap <- parseMoves
    itemMap <- parseItems
    print pokeMap
    print moveMap
    print itemMap
    (winner, st) <- runStateT battle (BattleState leftTR rightTR)
    print winner


leftTR = Trainer
    { active    = 0
    , pokeballs = listArray (0,1) $
        [ Monster
            { species = Species 3 "venasaur" [Bug] (Stats 10 10 10 10 10 10) Nothing []
            , nickname = "ONE"
            , lvl = 80
            , hpLeft = 0
            , moves = listArray (0,0) [MonsterMove (Move "tackle" Normal True 15 20) 10]
            , stats = Stats 10 10 10 10 10 10
            , indV = Stats 10 10 10 10 10 10
            , effV = Stats 10 10 10 10 10 10
            , condition = NONE
            }
        , Monster
            { species = Species 3 "venasaur" [Bug] (Stats 10 10 10 10 10 10) Nothing []
            , nickname = "TWO"
            , lvl = 80
            , hpLeft = 100
            , moves = listArray (0,15) [MonsterMove (Move "tackle" Normal True 15 20) 10, MonsterMove (Move "tackle" Normal True 15 20) 10, MonsterMove (Move "tackle" Normal True 15 20) 10, MonsterMove (Move "tackle" Normal True 15 20) 10, MonsterMove (Move "tackle" Normal True 15 20) 10, MonsterMove (Move "tackle" Normal True 15 20) 10, MonsterMove (Move "tackle" Normal True 15 20) 10, MonsterMove (Move "tackle" Normal True 15 20) 10, MonsterMove (Move "tackle" Normal True 15 20) 10, MonsterMove (Move "tackle" Normal True 15 20) 10, MonsterMove (Move "tackle" Normal True 15 20) 10, MonsterMove (Move "tackle" Normal True 15 20) 10, MonsterMove (Move "tackle" Normal True 15 20) 10, MonsterMove (Move "tackle" Normal True 15 20) 10, MonsterMove (Move "tackle" Normal True 15 20) 10, MonsterMove (Move "tackle" Normal True 15 20) 10, MonsterMove (Move "tackle" Normal True 15 20) 10, MonsterMove (Move "tackle" Normal True 15 20) 10, MonsterMove (Move "tackle" Normal True 15 20) 10, MonsterMove (Move "tackle" Normal True 15 20) 10, MonsterMove (Move "tackle" Normal True 15 20) 10, MonsterMove (Move "tackle" Normal True 15 20) 10, MonsterMove (Move "tackle" Normal True 15 20) 10, MonsterMove (Move "tackle" Normal True 15 20) 10, MonsterMove (Move "tackle" Normal True 15 20) 10, MonsterMove (Move "tackle" Normal True 15 20) 10, MonsterMove (Move "tackle" Normal True 15 20) 10, MonsterMove (Move "tackle" Normal True 15 20) 10, MonsterMove (Move "tackle" Normal True 15 20) 10, MonsterMove (Move "tackle" Normal True 15 20) 10, MonsterMove (Move "tackle" Normal True 15 20) 10, MonsterMove (Move "tackle" Normal True 15 20) 10, MonsterMove (Move "tackle" Normal True 15 20) 10, MonsterMove (Move "tackle" Normal True 15 20) 10]
            , stats = Stats 10 10 10 10 10 10
            , indV = Stats 10 10 10 10 10 10
            , effV = Stats 10 10 10 10 10 10
            , condition = NONE
            }
        ]
    , items = []
    }

rightTR = Trainer
    { active    = 1
    , pokeballs = listArray (0,2) $
        [ Monster
            { species = Species 3 "venasaur" [Bug] (Stats 10 10 10 10 10 10) Nothing []
            , nickname = "UNO"
            , lvl = 80
            , hpLeft = 100
            , moves = listArray (0,0) [MonsterMove (Move "tackle" Normal True 15 20) 10]
            , stats = Stats 10 10 10 10 10 10
            , indV = Stats 10 10 10 10 10 10
            , effV = Stats 10 10 10 10 10 10
            , condition = NONE
            }
        , Monster
            { species = Species 3 "venasaur" [Bug] (Stats 10 10 10 10 10 10) Nothing []
            , nickname = "DOS"
            , lvl = 80
            , hpLeft = 0
            , moves = listArray (0,0) [MonsterMove (Move "tackle" Normal True 15 20) 10]
            , stats = Stats 10 10 10 10 10 10
            , indV = Stats 10 10 10 10 10 10
            , effV = Stats 10 10 10 10 10 10
            , condition = NONE
            }
        , Monster
            { species = Species 3 "venasaur" [Bug] (Stats 10 10 10 10 10 10) Nothing []
            , nickname = "TRES"
            , lvl = 80
            , hpLeft = 0
            , moves = listArray (0,0) [MonsterMove (Move "tackle" Normal True 15 20) 10]
            , stats = Stats 10 10 10 10 10 10
            , indV = Stats 10 10 10 10 10 10
            , effV = Stats 10 10 10 10 10 10
            , condition = NONE
            }
        ]
    , items = []
    }
