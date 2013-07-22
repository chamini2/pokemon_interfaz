module Battle
    ( Command(..)
    , Winner(..)
    , BattleState(..)
    , tryAttack
    ) where

import Control.Monad.State
import Control.Monad.Reader

import Graphics.UI.Gtk.Builder

import Pokemon

data BattleState = BattleState
    { left  :: Trainer
    , right :: Trainer
    }

data Winner = LeftTrainer
            | RightTrainer
            deriving (Eq, Show)

data Command = FIGHT MonsterMove
             | PKMN Monster
             | RUN
             | ITEM Item
             deriving (Eq)

tryAttack :: Int -> ReaderT Builder (StateT BattleState IO) Bool
tryAttack i = do
    s <- get
    liftIO $ putStrLn $ show (left s)
    liftIO $ putStrLn $ show (right s)
    let newSt = s { left = (right s) }
    put newSt
    return True
