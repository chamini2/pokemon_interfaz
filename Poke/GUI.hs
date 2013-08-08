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



main = do
    pokeMap <- parsePokemon
    teams <- newEmptyMVar

    forkIO (selection pokeMap teams "Left")

    leftTeam <- takeMVar teams

    forkIO (selection pokeMap teams "Right")

    rightTeam <- takeMVar teams

    putStrLn "Left Team"
    mapM_ print leftTeam

    putStrLn "Right Team"
    mapM_ print rightTeam
    

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
    {-checkButtons <- mapM (xmlGetWidget xml castToCheckButton) checkStrings-}

    checkButtons <- mapM (\s -> do
                            b <- xmlGetWidget xml castToCheckButton s
                            buttonSetLabel b (getNo s)
                            return b) checkStrings
    {-let handlers = map (\b -> onToggled (b :: CheckButton) $ do putStrLn "Boton Presionado") checkButtons-}
    {-sequence_ handlers-}

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

    imageleft <- xmlGetWidget xmlB castToImage "imageleft"
    set imageleft $ [ imageFile := "./1.png" ]


    -- Pokemon selection
    cancelPkmn  <- xmlGetWidget xmlP castToButton "cancelSelection"

    -- Skill selection
    cancelFight <- xmlGetWidget xmlS castToButton "cancelFight"

    -- Item selection
    cancelItem  <- xmlGetWidget xmlI castToButton "cancelItem"

    -- Running
    doRun       <- xmlGetWidget xmlR castToButton "run"
    notRun      <- xmlGetWidget xmlR castToButton "cancelRun"


    -- Callbacks

    -- Pokemon selection
    onClicked pkmnButton $ do
        widgetShowAll pkmnSelect

    onClicked cancelPkmn $ do
        widgetHideAll pkmnSelect

    -- Skills selection
    onClicked fightButton $ do 
        widgetShowAll skills

    onClicked cancelFight $ do
        widgetHideAll skills

    -- Item selection
    onClicked itemButton $ do
        widgetShowAll items

    onClicked cancelItem $ do
        widgetHideAll items

    -- Running
    onClicked doRun $ do
        widgetDestroy window

    onClicked notRun $ do
        widgetHideAll run

    onClicked runButton $ do
        widgetShowAll run


    onDestroy window mainQuit
    widgetShowAll window
    mainGUI
