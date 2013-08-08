module Main where

import Data.Array
import Control.Monad.State as ST
import Control.Monad.Reader

import Graphics.UI.Gtk hiding (get)
import Graphics.UI.Gtk.Builder hiding (get)

import Pokemon
import Battle
import Parse
import GUI


main :: IO ()
main = do
    pokeMap <- parsePokemon
    moveMap <- parseMoves
    itemMap <- parseItems

    teams <- newEmptyMVar

    forkIO (selection pokeMap teams "Left")
    leftTeam <- takeMVar teams

    forkIO (selection pokeMap teams "Right")
    rightTeam <- takeMVar teams

    print pokeMap
    print moveMap
    print itemMap

    builder    <- builderNew
    builderAddFromFile builder "new.glade"

    flip execStateT (BattleState leftTR rightTR) (runReaderT interface builder)

interface :: ReaderT Builder (StateT BattleState IO) ()
interface = do
    liftIO $ initGUI
    builder <- ask
    mainWindow <- liftIO $ builderGetObject builder castToWindow "main"
    liftIO $ onDestroy mainWindow mainQuit
    liftIO $ widgetShow mainWindow

    selectWindow <- liftIO $ builderGetObject builder castToVBox "select"
    battleWindow <- liftIO $ builderGetObject builder castToVBox "battle"
    itemWindow   <- liftIO $ builderGetObject builder castToVBox "item"
    pkmnWindow   <- liftIO $ builderGetObject builder castToVBox "pkmn"

    selectBehavior
    menuBehavior
    --itemBehavior
    --pkmnBehavior
    battleBehavior

    liftIO $ mapM widgetHideAll [itemWindow, pkmnWindow, battleWindow]
    liftIO $ widgetShowAll selectWindow

    liftIO $ mainGUI

hideAndShow builder castTo hideStr showStr = do
    hideWidget <- builderGetObject builder castTo hideStr
    showWidget <- builderGetObject builder castTo showStr
    widgetHideAll hideWidget
    widgetShowAll showWidget


selectBehavior :: ReaderT Builder (StateT BattleState IO) (ConnectId Button)
selectBehavior = do
    builder    <- ask
    doneButton <- liftIO $ builderGetObject builder castToButton "select-done"
    liftIO $ onClicked doneButton $ do
        hideAndShow builder castToVBox  "select" "battle"
        hideAndShow builder castToTable "fight"  "option"

menuBehavior :: ReaderT Builder (StateT BattleState IO) (ConnectId Button)
menuBehavior = do
    builder <- ask

    fightButton <- liftIO $ builderGetObject builder castToButton "fight-button"
    liftIO $ onClicked fightButton $
        hideAndShow builder castToTable "option" "fight"

    itemButton  <- liftIO $ builderGetObject builder castToButton "item-button"
    liftIO $ onClicked itemButton $
        hideAndShow builder castToVBox "battle" "item"

    pkmnButton  <- liftIO $ builderGetObject builder castToButton "pkmn-button"
    liftIO $ onClicked pkmnButton $
        hideAndShow builder castToVBox "battle" "pkmn"

    runButton   <- liftIO $ builderGetObject builder castToButton "run-button"
    liftIO $ onClicked runButton $ do
        mainWindow <- builderGetObject builder castToWindow "main"
        widgetDestroy mainWindow

--itemBehavior :: IO ()
itemBehavior = undefined

--pkmnBehavior :: IO ()
pkmnBehavior = undefined

--battleBehavior :: IO ()
battleBehavior = do
    builder <- ask
    st      <- get
    liftIO $ mapM (attackButton st builder leftTR) [1..4]
    --moveButton <- builderGetObject builder castToButton "move1"
    --onClicked moveButton $ do
    --    (ok, newSt) <- runStateT (runReaderT (tryAttack 1) builder) st
    --    if ok
    --        then hideAndShow builder castToTable "fight" "option"
    --        else return ()
    where
        attackButton st builder leftTR i = do
            moveButton <- builderGetObject builder castToButton ("move" ++ show i)
            onClicked moveButton $ do
                (ok, newSt) <- runStateT (runReaderT (tryAttack i) builder) st
                if ok
                    then hideAndShow builder castToTable "fight" "option"
                    else return ()



leftTR = Trainer
    { active    = 0
    , pokeballs = listArray (0,0) $
        [ Monster
            { species = Species 3 "venasaur" [Bug] (Stats 10 10 10 10 10 10) Nothing []
            , nickname = "VENASAUR"
            , lvl = 80
            , hpLeft = 100
            , moves = listArray (0,0) [MonsterMove (Move "tackle" Normal True 15 20) 10]
            , stats = Stats 10 10 10 10 10 10
            , indV = Stats 10 10 10 10 10 10
            , effV = Stats 10 10 10 10 10 10
            , condition = NONE
            }
        ]
    }

rightTR = Trainer
    { active    = 0
    , pokeballs = listArray (0,0) $
        [ Monster
            { species = Species 3 "venasaur" [Bug] (Stats 10 10 10 10 10 10) Nothing []
            , nickname = "BULBASAUR"
            , lvl = 80
            , hpLeft = 100
            , moves = listArray (0,0) [MonsterMove (Move "tackle" Normal True 15 20) 10]
            , stats = Stats 10 10 10 10 10 10
            , indV = Stats 10 10 10 10 10 10
            , effV = Stats 10 10 10 10 10 10
            , condition = NONE
            }
        ]
    }
