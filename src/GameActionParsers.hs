module GameActionParsers where

import Text.Parsec
import Data.Char

import GameDefinitions

data GameAction = LookAroundAction | LookAtSelfAction | LookAtAction Name |
  LookNAction | LookEAction | LookSAction | LookWAction |
  GoNAction | GoEAction | GoSAction | GoWAction |
  UseOnAction Name Name | PickUpAction Name |
  InteractWithAction Name | EquipAction Name |
  AttackAction Name | QuestlogListAction | QuestlogReadAction Name |
  SaveAction String | LoadAction String | ExitAction
  deriving (Show)

lookAroundActionParser :: Parsec String () GameAction
lookAroundActionParser = do
  spaces
  string "look"
  spaces
  string "around"
  spaces
  return (LookAroundAction)

lookAtSelfActionParser :: Parsec String () GameAction
lookAtSelfActionParser = do
  spaces
  string "look"
  spaces
  string "at"
  spaces
  string "self"
  spaces
  return (LookAtSelfAction)

trimNameEnd :: String -> String
trimNameEnd name = reverse $ (dropWhile isSpace) $ reverse name

lookAtActionParser :: Parsec String () GameAction
lookAtActionParser = do
  spaces
  string "look"
  spaces
  string "at"
  spaces
  nameVal <- many1 (noneOf "\t\n")
  spaces
  return (LookAtAction (trimNameEnd nameVal))

lookDirActionParser :: String -> Parsec String () GameAction
lookDirActionParser dir = do
  spaces
  string "look"
  spaces
  string dir
  spaces
  case dir of
    "north" -> return LookNAction
    "east" -> return LookEAction
    "south" -> return LookSAction
    "west" -> return LookWAction

goDirActionParser :: String -> Parsec String () GameAction
goDirActionParser dir = do
  spaces
  string "go"
  spaces
  string dir
  spaces
  case dir of
    "north" -> return GoNAction
    "east" -> return GoEAction
    "south" -> return GoSAction
    "west" -> return GoWAction

useOnActionParser :: Parsec String () GameAction
useOnActionParser = do
  spaces
  string "use"
  spaces
  itemVal <- manyTill (noneOf "\t\n") (try (string " on "))
  spaces 
  targetVal <- many1 (noneOf "\t\n")
  spaces
  return (UseOnAction (trimNameEnd itemVal) (trimNameEnd targetVal))

pickUpActionParser :: Parsec String () GameAction
pickUpActionParser = do
  spaces
  string "pick"
  spaces
  string "up"
  spaces
  nameVal <- many1 (noneOf "\t\n")
  spaces
  return (PickUpAction (trimNameEnd nameVal))

interactWithActionParser :: Parsec String () GameAction
interactWithActionParser = do
  spaces
  string "interact"
  spaces
  string "with"
  spaces
  nameVal <- many1 (noneOf "\t\n")
  spaces
  return (InteractWithAction (trimNameEnd nameVal))

equipActionParser :: Parsec String () GameAction
equipActionParser = do
  spaces
  string "equip"
  spaces
  nameVal <- many1 (noneOf "\t\n")
  spaces
  return (EquipAction (trimNameEnd nameVal))

attackActionParser :: Parsec String () GameAction
attackActionParser = do
  spaces
  string "attack"
  spaces
  nameVal <- many1 (noneOf "\t\n")
  spaces
  return (AttackAction (trimNameEnd nameVal))

questlogListActionParser :: Parsec String () GameAction
questlogListActionParser = do
  spaces
  string "questlog"
  spaces
  string "list"
  spaces
  return (QuestlogListAction)

questlogReadActionParser :: Parsec String () GameAction
questlogReadActionParser = do
  spaces
  string "questlog"
  spaces
  nameVal <- many1 (noneOf "\t\n")
  spaces
  return (QuestlogReadAction (trimNameEnd nameVal))

saveActionParser :: Parsec String () GameAction
saveActionParser = do
  spaces
  string "save"
  spaces
  value <- many1 (noneOf " \t\n")
  spaces
  return (SaveAction value)

loadActionParser :: Parsec String () GameAction
loadActionParser = do
  spaces
  string "load"
  spaces
  value <- many1 (noneOf " \t\n")
  spaces
  return (LoadAction value)

exitActionParser :: Parsec String () GameAction
exitActionParser = do
  spaces
  string "exit"
  spaces
  return (ExitAction)

gameActionParser :: Parsec String () GameAction
gameActionParser = 
  try lookAroundActionParser <|>
  try lookAtSelfActionParser <|>
  try lookAtActionParser <|>
  try (lookDirActionParser "north") <|>
  try (lookDirActionParser "east") <|>
  try (lookDirActionParser "south") <|>
  try (lookDirActionParser "west") <|>
  try (goDirActionParser "north") <|>
  try (goDirActionParser "east") <|>
  try (goDirActionParser "south") <|>
  try (goDirActionParser "west") <|>
  try useOnActionParser <|>
  try pickUpActionParser <|>
  try interactWithActionParser <|>
  try equipActionParser <|>
  try attackActionParser <|>
  try questlogListActionParser <|>
  try questlogReadActionParser <|>
  try saveActionParser <|>
  try loadActionParser <|>
  exitActionParser
