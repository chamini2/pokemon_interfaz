module Main where

import Control.Monad.State
import Control.Concurrent
import Control.Concurrent.MVar

import Pokemon
import Battle
import Parse
import GUI


-- ELIMINAR AL QUITAR TRAINERS
import Data.Array


main = do
    pokeMap <- parsePokemon
    moveMap <- parseMoves
    itemMap <- parseItems

    teams <- newEmptyMVar

    {-print pokeMap-}
    {-print moveMap-}
    {-print itemMap-}

    forkIO (selection pokeMap teams "Left")
    leftTeamS <- takeMVar teams

    let leftTeam = map (\s -> Monster
                            { species   = s
                            , nickname  = ""
                            , lvl       = 1
                            , hpLeft    = 100
                            , moves     = listArray (0,0) [MonsterMove (Move "tackle" Normal True 15 20) 10]
                            , stats     = Stats 10 10 10 10 10 10
                            , indV      = Stats 10 10 10 10 10 10
                            , effV      = Stats 10 10 10 10 10 10
                            , condition = NONE
                                } ) leftTeamS

    forkIO (selection pokeMap teams "Right")
    rightTeamS <- takeMVar teams

    let rightTeam = map (\s -> Monster
                            { species   = s
                            , nickname  = ""
                            , lvl       = 1
                            , hpLeft    = 100
                            , moves     = listArray (0,0) [MonsterMove (Move "tackle" Normal True 15 20) 10]
                            , stats     = Stats 10 10 10 10 10 10
                            , indV      = Stats 10 10 10 10 10 10
                            , effV      = Stats 10 10 10 10 10 10
                            , condition = NONE
                                } ) rightTeamS


    {-(winner, st) <- runStateT battle (BattleState (leftTR leftTeam) (rightTR rightTeam))-}
    {-print winner-}


leftTR team = Trainer
    { active    = 0
    , pokeballs = listArray (0, ((length team) - 1)) team
    , items = []
    }

rightTR team = Trainer
    { active    = 0
    , pokeballs = listArray (0, ((length team) - 1)) team
    , items = []
    }
