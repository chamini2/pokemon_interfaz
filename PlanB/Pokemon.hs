{- |
Module      :  <File name or $Header$ to be replaced automatically>
Description :  Pokemon definition module
Copyright   :  Matteo Ferrando 2013
License     :  ?

Maintainer  :  matteo.ferrando2@gmail.com
Stability   :  experimental
Portability :  portable

In this module we define how is defined every aspect of the Pokemon world.
-}

module Pokemon
    ( Type(..)
    , Stats(..)
    , Species(..)
    , Evolution(..)
    , EvolutionMethod(..)
    , Monster(..)
    , Move(..)
    , MonsterMove(..)
    , Trainer(..)
    , Item(..)
    , Condition(..)
    ) where

import Data.Array (Array)

data Type = Bug
          | Dark
          | Dragon
          | Electric
          | Fighting
          | Fire
          | Flying
          | Ghost
          | Grass
          | Ground
          | Ice
          | Normal
          | Poison
          | Psychic
          | Rock
          | Steel
          | Water
          deriving (Bounded, Eq, Ord, Enum, Show, Read)

data Stats = Stats
    { hp        :: !Int
    , attack    :: !Int
    , defense   :: !Int
    , spAttack  :: !Int
    , spDefense :: !Int
    , speed     :: !Int
    } deriving (Eq, Show, Read)

data Species = Species
    { no           :: !Int
    , name         :: !String
    , pokeType     :: [Type]
    , base         :: !Stats
    , preEvolution :: Maybe Evolution
    , evolutions   :: [Evolution]
    } deriving (Eq, Show, Read)

data Evolution = Evolution
    { evoSp  :: Species
    , how    :: EvolutionMethod
    } deriving (Eq, Read)

-- to avoid infinite reursion printing
instance Show Evolution where
    show (Evolution sp how) = show (no sp, name sp, how)

data EvolutionMethod = Stone Stone
                     | LVL Int
                     | Trade
                     deriving (Eq, Show, Read)

data Stone = DawnStone
           | DuskStone
           | FireStone
           | LeafStone
           | MoonStone
           | ShinyStone
           | SunStone
           | ThunderStone
           | WaterStone
           deriving (Eq, Show, Read)

data Monster = Monster
    { species   :: !Species
    , nickname  :: !String
    , lvl       :: !Int
    , hpLeft    :: !Int
    , moves     :: Array Int MonsterMove
    , stats     :: !Stats
    , indV      :: !Stats
    , effV      :: !Stats
    , condition :: !Condition
    } deriving (Eq, Show, Read)

data Move = Move
    { moveName :: !String
    , moveType :: !Type
    , physical :: !Bool
    , pp       :: !Int
    , power    :: !Int
    } deriving (Eq, Show, Read)

data MonsterMove = MonsterMove
    { monMove :: Move
    , monPP   :: Int
    } deriving (Eq, Show, Read)

data Trainer = Trainer
    { active    :: Int
    , pokeballs :: Array Int Monster
    , items     :: [Item]
    } deriving (Eq, Show, Read)

data Item = PokeBall String Float       -- Catch rate
          | Potion   String Int         -- HP retored
          | Restore  String Bool        -- All HP, Bool if normalizes condition
          | Revive   String Int         -- Percentage of live when revived
          | Ether    String Bool        -- Bool if restores all PP
          | Elixir   String Bool        -- Bool if restores all PP
          | Ailment  String Condition   -- Cures a Condition
          deriving (Eq, Show, Read)

data Condition = NONE -- no special condition
               | BRN  -- burn
               | FRZ  -- freeze
               | PAR  -- paralysis
               | PSN  -- poison
               | SLP  -- sleep
               deriving (Eq, Show, Read)
               ---- the rest is unnecesary
               ---- volatile status
               -- | Confusion             -- confusion
               -- | Curse                 -- curse
               -- | Embargo               -- embargo
               -- | Encore                -- encore
               -- | Flinch                -- flinch
               -- | HealBlock             -- heal block
               -- | Identification        -- identification
               -- | Infatuation           -- infatuation
               -- | Nightmare             -- nightmare
               -- | PartiallyTrapped      -- partially trapped
               -- | PerishSong            -- perish song
               -- | Seeding               -- seeding
               -- | Taunt                 -- taunt
               -- | TelekineticLevitation -- telekinetic levitation
               -- | Torment               -- torment
               -- | Trapped               -- trapped
               ---- volatile battle status
               -- | AquaRing             -- aqua ring
               -- | Bracing              -- bracing
               -- | CenterOfAttention    -- center of attention
               -- | DefenseCurl          -- defense curl
               -- | FocusEnergy          -- focus energy
               -- | Glowing              -- glowing
               -- | Rooting              -- rooting
               -- | MagicCoat            -- magic coat
               -- | MagneticLevitation   -- magnetic levitation
               -- | Minimize             -- minimize
               -- | Protection           -- protection
               -- | Recharging           -- recharging
               -- | SemiInvulnerable     -- semi invulnerable
               -- | Substitute           -- substitute
               -- | TakingAim            -- taking aim
               -- | TakingInSunlight     -- taking in sunlight
               -- | Withdrawing          -- withdrawing
               -- | WhippingUpAWhirlwind -- whipping up a whirlwind

typeEffect :: Type      -- Type to determine the effect.
           -> ( [Type]  -- (2x dmg)   Type is super effective to [Type]
              , [Type]  -- (0.5x dmg) Type not very effective to [Type]
              , [Type]  -- (0x dmg)   Type has no effect to [Type]
              )
typeEffect typ = case typ of
    Bug      -> ([Grass, Psychic, Dark], [Fighting, Flying, Poison, Ghost, Steel, Fire], [])
    Dark     -> ([Ghost, Psychic], [Fighting, Steel, Dark], [])
    Dragon   -> ([Dragon], [Steel], [])
    Electric -> ([Flying, Water], [Grass, Electric, Dragon], [Ground])
    Fighting -> ([Normal, Rock, Steel, Ice, Dark], [Flying, Poison, Bug, Psychic], [Ghost])
    Fire     -> ([Bug, Steel, Grass, Ice], [Rock, Fire, Water, Dragon], [])
    Flying   -> ([Fighting, Bug, Grass], [Rock, Steel, Electric], [])
    Ghost    -> ([Ghost, Psychic], [Steel, Dark], [Normal])
    Grass    -> ([Ground, Rock, Water], [Flying, Poison, Bug, Steel, Fire, Grass, Dragon], [])
    Ground   -> ([Poison, Rock, Steel, Fire, Electric], [Bug, Grass], [Flying])
    Ice      -> ([Flying, Ground, Grass, Dragon], [Steel, Fire, Water], [])
    Normal   -> ([], [Rock, Steel], [Ghost])
    Poison   -> ([Grass], [Poison, Ground, Rock, Ghost], [Steel])
    Psychic  -> ([Fighting, Poison], [Steel, Psychic], [Dark])
    Rock     -> ([Flying, Bug, Fire, Ice], [Fighting, Ground, Steel], [])
    Steel    -> ([Rock, Ice], [Steel, Fire, Water, Electric], [])
    Water    -> ([Ground, Rock, Fire], [Water, Grass, Dragon], [])
