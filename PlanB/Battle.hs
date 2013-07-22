module Battle
    ( Command(..)
    , Winner(..)
    , BattleState(..)
    , battle
    ) where

import Data.Char (isDigit, toLower)
import Data.Array
import Control.Monad.State

import Pokemon

data BattleState = BattleState
    { left  :: Trainer
    , right :: Trainer
    } deriving (Show)

data Winner = LeftTrainer
            | RightTrainer
            | NoTrainer
            deriving (Eq, Show)

data Command = FIGHT (Int, MonsterMove)
             | PKMN (Int, Monster)
             | RUN
             | ITEM Item
             deriving (Eq)

-------------------------------------------------------------------------------

battle :: StateT BattleState IO Winner
battle = do
    printBattle
    left  <- gets left
    right <- gets right
    case (defeated left, defeated right) of
        (True, True) -> return NoTrainer
        (_,    True) -> return LeftTrainer
        (True,    _) -> return RightTrainer
        otherwise    -> do
            chooseActiveLeft
            chooseActiveRight
            commandL <- turnLeft
            commandR <- turnRight

            return NoTrainer

fainted :: Monster -> Bool
fainted = (== 0) . hpLeft

defeated :: Trainer -> Bool
defeated = all fainted . elems . pokeballs

monsterOut :: Trainer -> Monster
monsterOut (Trainer act pokes _) = pokes ! act

-------------------------------------------------------------------------------

chooseActiveLeft = do
    left <- gets left
    let mons = monsterOut left
    if fainted mons
        then do
            liftIO $ putStrLn $ nickname mons ++ " can't battle anymore!"
            printPokeballs
            input <- liftIO $ getLine
            if all isDigit input && (not . null) input
                then do
                    let index = read input :: Int
                    if any (index==) $ indices $ pokeballs left
                        then do
                            let newLeft = left {active = index}
                            modify (\s -> s {left = newLeft})
                            chooseActiveLeft
                        else chooseActiveLeft
                else chooseActiveLeft
        else return ()

turnLeft = do
    left <- gets left
    liftIO $ putStrLn $ "What will " ++ (nickname . monsterOut) left ++ " do?!"
    liftIO $ putStrLn "___________________________"
    liftIO $ putStrLn "| FIGHT     -       PKMN  |"
    liftIO $ putStrLn "| ITEM      - PRINT - RUN |"
    liftIO $ putStrLn "‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾"
    input <- liftIO getLine
    liftIO $ putStrLn "\n"
    case map toLower $ filter (/=' ') input of
        "fight"   -> fightLeft
        "pkmn"    -> pkmnLeft
        "item"    -> itemLeft
        "print"   -> printBattle >> turnLeft
        "run"     -> return RUN
        otherwise -> turnLeft

fightLeft = do
    left <- gets left
    let mons = monsterOut left
    printMoves
    input <- liftIO getLine
    if all isDigit input && (not . null) input
        then do
            let index = read input :: Int
            if any (index==) $ indices $ moves mons
                then do
                    let move = (moves mons) ! index
                    if monPP move > 0
                        then return $ FIGHT (index, move)
                        else fightLeft
                else fightLeft
        else if map toLower input == "cancel"
            then turnLeft
            else fightLeft

pkmnLeft = do
    left <- gets left
    printPokeballs
    return . PKMN $ ((active left), monsterOut left)

itemLeft = do
    undefined

-------------------------------------------------------------------------------

chooseActiveRight = do
    right <- gets right
    let mons  = monsterOut right
        pokes = length . indices $ pokeballs right
    if fainted mons
        then do
            let index    = ((active right) + 1) `mod` pokes
                newRight = right {active = index}
            modify (\s -> s {right = newRight})
            chooseActiveRight
        else return ()


turnRight = do
    right <- gets right
    let notFaintedMons =
            filter (\(_, mon) -> (not $ fainted mon) && mon /= monsterOut right)
             . assocs $ pokeballs right
        usableMoves    =
            filter ((0>) . monPP . snd) . assocs . moves $ monsterOut right
    if not $ null usableMoves
        then return . FIGHT $ head usableMoves
        else return . PKMN  $ head notFaintedMons

-------------------------------------------------------------------------------

printBattle = do
    leftMon  <- gets $ monsterOut . left
    rightMon <- gets $ monsterOut . right
    liftIO $ putStrLn "\n"
    liftIO $ printMonster rightMon
    liftIO $ putStrLn "\n GARY\n------\r\t\t\t\t-----\n\t\t\t\t ASH\n"
    liftIO $ printMonster leftMon
    liftIO $ putStrLn "\n"
    where
        printMonster (Monster mSpec mNick mLvl mHp _ mStats _ _ mCond) = do
            putStrLn $ mNick ++ "\r\t\t\t\tLVL " ++ show mLvl
            putStr $ showCond ++ "\r\t\t\t\t"
            putStrLn $ show mHp ++ "/" ++ (show . hp) mStats ++ " HP"
            where
                showCond = if mCond == NONE then "" else show mCond

printPokeballs = do
    left  <- gets left
    liftIO $ putStrLn $ "Choose a new Pokémon to battle!"
    liftIO $ mapM_ printPokeball $ (assocs . pokeballs) left
    where
        printPokeball (i, m) = putStrLn $ show i ++ " -> "++ nickname m

printMoves = do
    moves <- gets $ assocs . moves . monsterOut . left
    liftIO $ putStrLn "MOVES | CANCEL"
    liftIO $ mapM_ printMove moves
    where
        printMove (i,(MonsterMove (Move mName mType mPhy mPp mPwr) leftPp)) = do
            putStr   $ show i ++ " -> "
            putStr   $ mName ++ " [" ++ show mType ++ "] "
            putStrLn $ "(" ++ show leftPp ++ "/" ++ show mPp ++ ") PP"
