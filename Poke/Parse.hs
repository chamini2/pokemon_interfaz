module Parse
    ( parsePokemon
    , parseMoves
    , parseItems
    ) where

import Data.Char (toUpper)
import Data.Maybe (isJust, fromJust)
import qualified Data.Map.Strict as DM (Map, fromList, toList, empty, map, (!))
import qualified Data.IntMap.Strict as IM (IntMap, fromList, toList,
                                           empty, map, (!))

import Text.ParserCombinators.Parsec

import Pokemon

cell = many (noneOf ",\n\r")

pokeFile = endBy speciesLine newline
speciesLine = sepBy cell (char ',') >>= return . parseSpecies

parsePokemon :: IO (IM.IntMap Species)
parsePokemon = do
    input <- readFile "load/pokemon.csv"
    case (parse pokeFile) "pokemon" input of
        Left error -> putStrLn ("Error: " ++ show error) >> return IM.empty
        Right pokes -> return . parseEvolutions $ IM.fromList pokes

parseSpecies :: [String] -> (Int, ((String, String), Species))
parseSpecies [sNo,sName,sTp1,sTp2,sHp,sAtk,sDef,sSAtk,sSDef,sSpd,sPEv,sHow] =
    (read sNo, ((sPEv, sHow), Species
        { no           = read sNo
        , name         = sName
        , pokeType     = parseTypes sTp1 sTp2
        , base         = parseStats sHp sAtk sDef sSAtk sSDef sSpd
        , preEvolution = Nothing    -- can't be done now
        , evolutions   = []         -- can't be done now
        }
    ) )
    where
        parseTypes tp1 tp2 | null tp2  = [read tp1]
                           | otherwise = [read tp1, read tp2]
        parseStats hp atk def sAtk sDef spd =
            Stats (read hp)   (read atk)  (read def)
                  (read sAtk) (read sDef) (read spd)

parseEvolutions :: IM.IntMap ((String, String), Species) -> IM.IntMap Species
parseEvolutions intMap = IM.map parseEvolutions intMapPE
    where
        intMapPE = IM.map parsePreEvs intMap
        parsePreEvs ((sPEv, sHow), species)
            | not $ null sPEv = species
                { preEvolution =
                    Just $ Evolution (snd $ intMap IM.! (read sPEv))
                                     (parseHow sHow)
                }
            | otherwise = species
        parseEvolutions species@(Species sNo _ _ _ _ _) = species
            { evolutions = let pre = fromJust . preEvolution
                in map (\(_, e) -> Evolution {evoSp = e, how  = how . pre $ e})
                    $ filter (\(_, x) -> (==) sNo $ no . evoSp $ pre x)
                        $ filter (\(_, x) -> isJust $ preEvolution x)
                            (IM.toList intMapPE)
            }

        parseHow ('L':cs) = LVL $ read cs
        parseHow "trade"  = Trade
        parseHow (c:cs)   = Stone . read $ toUpper c : cs ++ "Stone"

-------------------------------------------------------------------------------

moveFile = endBy moveLine newline
moveLine = sepBy cell (char ',') >>= return . parseMove

parseMoves :: IO (DM.Map String Move)
parseMoves = do
    input <- readFile "load/moves.csv"
    case (parse moveFile) "moves" input of
        Left error -> putStrLn ("Error: " ++ show error) >> return DM.empty
        Right moves -> return $ DM.fromList moves

parseMove :: [String] -> (String, Move)
parseMove [mName, mType, mPhysical, mPp, mPower] =
    (mName, Move
        { moveName = mName
        , moveType = read mType
        , physical = read mPhysical
        , pp       = read mPp
        , power    = read mPower
        }
    )

-------------------------------------------------------------------------------

itemFile = endBy itemLine newline
itemLine = sepBy cell (char ',') >>= return . parseItem

parseItems :: IO (DM.Map String Item)
parseItems = do
    input <- readFile "load/items.csv"
    case (parse itemFile) "items" input of
        Left error -> putStrLn ("Error: " ++ show error) >> return DM.empty
        Right items -> return $ DM.fromList items

parseItem :: [String] -> (String, Item)
parseItem [iType, iName, iEffect] = case iType of
    "PokeBall" -> (iName, PokeBall iName (read iEffect))
    "Potion"   -> (iName, Potion   iName (read iEffect))
    "Restore"  -> (iName, Restore  iName (read iEffect))
    "Revive"   -> (iName, Revive   iName (read iEffect))
    "Ether"    -> (iName, Ether    iName (read iEffect))
    "Elixir"   -> (iName, Elixir   iName (read iEffect))
    "Ailment"  -> (iName, Ailment  iName (read iEffect))
