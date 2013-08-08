module GUI
    (selection,
    battleGUI) where
     

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import Control.Monad
import qualified Data.IntMap.Strict as IM (IntMap, fromList, toList,
                                           empty, map, (!))
import Data.Char (isDigit)
import Control.Concurrent
import Control.Concurrent.MVar


import Parse
import Pokemon

-------------------------------------------------------------------------------

-- Pokemon selection Menu
selection pokeMap team trainer = do
    initGUI
    Just xml <- xmlNew "selection.glade"

    window <-xmlGetWidget xml castToWindow "main"
    doneButton <- xmlGetWidget xml castToButton "doneSelecting"
    mainLabel <- xmlGetWidget xml castToLabel "label1"

    set mainLabel [ labelText := trainer ++ "Trainter.\nSelect up to six Pokémons."]

    -- LLena la informacion para la seleccion de los pokemons
    forM_ [1..151] $ \i -> do
        image <- xmlGetWidget xml castToImage ("image"++show(i))
        set image $ [ imageFile := ( "../sprites/Red&Blue/fronttest/rbspr" ++ show(i) ++ ".png" ) ]

    -- Lista con todos los checkbox
    let checkStrings = map ("checkbutton" ++) $ map show [1..151]

    checkButtons <- mapM (\s -> do
                            b <- xmlGetWidget xml castToCheckButton s
                            buttonSetLabel b (getNo s)
                            return b) checkStrings
    -- Selecting pokemons 
    onClicked doneButton $ do
        pressedButtons <- filterM (\b -> toggleButtonGetActive (b :: CheckButton)) checkButtons
        let pkmnL = length pressedButtons

        if pkmnL <= 6 && pkmnL > 0 
            then do
                labels <- mapM (buttonGetLabel) pressedButtons
                let pokemons = map (\k -> pokeMap IM.! (read k :: Int)) labels
                putMVar team pokemons 
                widgetDestroy window
            else
                putStrLn "No pueden ser mas de 6"
           

    onDestroy window mainQuit
    widgetShowAll window
    mainGUI


getNo s@(a:as) = if isDigit a then s else getNo as

-------------------------------------------------------------------------------

-- Battle GUI
battleGUI = do
    initGUI
    Just xmlB   <- xmlNew "battle.glade"
    Just xmlP   <- xmlNew "pkmn.glade"
    Just xmlS   <- xmlNew "skills.glade"
    Just xmlI   <- xmlNew "items.glade"
    Just xmlR   <- xmlNew "run.glade"

    -- Ventanas para manejar la aplicacion
    window      <- xmlGetWidget xmlB castToWindow "main"
    pkmnSelect  <- xmlGetWidget xmlP castToWindow "main"
    skills      <- xmlGetWidget xmlS castToWindow "main"
    items       <- xmlGetWidget xmlI castToWindow "main"
    run         <- xmlGetWidget xmlR castToWindow "main"

    -- Botones para esperar callbacks
    -- Ventana principal
    fightButton <- xmlGetWidget xmlB castToButton "fight"
    pkmnButton  <- xmlGetWidget xmlB castToButton "pkmn"
    itemButton  <- xmlGetWidget xmlB castToButton "item"
    runButton   <- xmlGetWidget xmlB castToButton "run"

    -- Pokemons images
    imageleft  <- xmlGetWidget xmlB castToImage "imageleft"
    imageRight <- xmlGetWidget xmlB castToImage "imageright"

    -- callbacks
    pkmnGUI xmlP pkmnSelect 
    skillsGUI xmlS skills
    itemGUI xmlI items
    runGUI xmlR run window
 
    onClicked fightButton $ do 
        widgetShowAll skills

    onClicked runButton $ do
        widgetShowAll run

    onClicked itemButton $ do
        widgetShowAll items

    onClicked pkmnButton $ do
        widgetShowAll pkmnSelect


    onExpose window $ \e -> do 
        putStrLn "Refrescando"
        return True

          
    onDestroy window mainQuit
    widgetShowAll window
    mainGUI

----------------------------------------------------------------------------------

-- Skill selection
skillsGUI xml window = do
    cancelFight <- xmlGetWidget xml castToButton "cancelFight"
    mv1 <- xmlGetWidget xml castToButton "move1"
    mv2 <- xmlGetWidget xml castToButton "move2"
    mv3 <- xmlGetWidget xml castToButton "move3"
    mv4 <- xmlGetWidget xml castToButton "move4"

    onClicked cancelFight $ do
        widgetHideAll window
    onClicked mv1 $ do
        skill <- buttonGetLabel mv1
        putStrLn skill
    onClicked mv2 $ do
        skill <- buttonGetLabel mv2
        putStrLn skill
    onClicked mv3 $ do
        skill <- buttonGetLabel mv3
        putStrLn skill
    onClicked mv4 $ do
        skill <- buttonGetLabel mv4
        putStrLn skill
    


-- Running
runGUI xml window mainWindow = do
    doRun       <- xmlGetWidget xml castToButton "run"
    notRun      <- xmlGetWidget xml castToButton "cancelRun"
    onClicked doRun $ do
        widgetDestroy mainWindow
    onClicked notRun $ do
        widgetHideAll window

-- Item selection
itemGUI xml window = do
    cancelItem  <- xmlGetWidget xml castToButton "cancelItem"
    onClicked cancelItem $ do
        widgetHideAll window

-- Pokemon selection
pkmnGUI xml window = do
    cancelPkmn  <- xmlGetWidget xml castToButton "cancelSelection"
    onClicked cancelPkmn $ do
        widgetHideAll window


