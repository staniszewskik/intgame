module CommonParsers where

import Text.Parsec

import GameDefinitions

integerParser :: Parsec String () Integer
integerParser = read <$> many1 digit

itemInnerParser :: Parsec String () ItemRef
itemInnerParser = do
  refPrefix <- string "item_"
  refValue <- many1 (noneOf " <")
  return $ refPrefix ++ refValue

itemRefParser :: Parsec String () ItemRef
itemRefParser = do
  string "<itemRef>"
  spaces
  refInner <- itemInnerParser
  spaces
  string "</itemRef>"
  return refInner

itemIdParser :: Parsec String () ItemRef
itemIdParser = do
  string "<id>"
  spaces
  refInner <- itemInnerParser
  spaces
  string "</id>"
  return refInner

objectInnerParser :: Parsec String () ObjectRef
objectInnerParser = do
  refPrefix <- string "object_"
  refValue <- many1 (noneOf " <")
  return $ refPrefix ++ refValue

objectRefParser :: Parsec String () ObjectRef
objectRefParser = do
  string "<objectRef>"
  spaces
  refInner <- objectInnerParser
  spaces
  string "</objectRef>"
  return refInner

objectIdParser :: Parsec String () ObjectRef
objectIdParser = do
  string "<id>"
  spaces
  refInner <- objectInnerParser
  spaces
  string "</id>"
  return refInner

characterInnerParser :: Parsec String () CharacterRef
characterInnerParser = do
  refPrefix <- string "character_"
  refValue <- many1 (noneOf " <")
  return $ refPrefix ++ refValue

characterRefParser :: Parsec String () CharacterRef
characterRefParser = do
  string "<characterRef>"
  spaces
  refInner <- characterInnerParser
  spaces
  string "</characterRef>"
  return refInner

characterIdParser :: Parsec String () CharacterRef
characterIdParser = do
  string "<id>"
  spaces
  refInner <- characterInnerParser
  spaces
  string "</id>"
  return refInner

monsterInnerParser :: Parsec String () MonsterRef
monsterInnerParser = do
  refPrefix <- string "monster_"
  refValue <- many1 (noneOf " <")
  return $ refPrefix ++ refValue

monsterRefParser :: Parsec String () MonsterRef
monsterRefParser = do
  string "<monsterRef>"
  spaces
  refInner <- monsterInnerParser
  spaces
  string "</monsterRef>"
  return refInner

monsterIdParser :: Parsec String () MonsterRef
monsterIdParser = do
  string "<id>"
  spaces
  refInner <- monsterInnerParser
  spaces
  string "</id>"
  return refInner

questInnerParser :: Parsec String () QuestRef
questInnerParser = do
  refPrefix <- string "quest_"
  refValue <- many1 (noneOf " <")
  return $ refPrefix ++ refValue

questRefParser :: Parsec String () QuestRef
questRefParser = do
  string "<questRef>"
  spaces
  refInner <- questInnerParser
  spaces
  string "</questRef>"
  return refInner

questIdParser :: Parsec String () QuestRef
questIdParser = do
  string "<id>"
  spaces
  refInner <- questInnerParser
  spaces
  string "</id>"
  return refInner

positionParser :: Bool -> Parsec String () Position
positionParser start = do
  tagName <- if start then return "startPos" else return "position"
  string $ "<" ++ tagName ++ ">"
  spaces
  xValue <- integerParser
  spaces
  char ','
  spaces
  yValue <- integerParser
  spaces
  string $ "</" ++ tagName ++ ">"
  return (Position xValue yValue)

flagNameParser :: Parsec String () FlagName
flagNameParser = do
  spaces
  value <- many1 (noneOf " ,<")
  spaces
  return value

flagParser :: Parsec String () Flag
flagParser = do
  value <- try (spaces *> string "not" <* spaces) <|> flagNameParser
  if value == "not"
    then do
      value' <- flagNameParser
      return (Flag True value')
    else return (Flag False value)

requiresParser :: Parsec String () Requires
requiresParser = do
  string "<requires>"
  spaces
  values <- sepBy1 flagParser (spaces *> string "," <* spaces)
  string "</requires>"
  return (Requires values)

nameParser :: Parsec String () Name
nameParser = do
  string "<name>"
  value <- many1 (noneOf "<")
  string "</name>"
  return value

textParser :: Parsec String () DescText
textParser = do
  string "<text>"
  value <- many (noneOf "<")
  string "</text>"
  return value

descriptionParser :: Parsec String () Description
descriptionParser = do
  string "<description>"
  spaces
  reqValue <- many (try requiresParser)
  spaces
  textValue <- textParser
  spaces
  string "</description>"
  return (Description reqValue textValue)

healthParser :: Parsec String () Health
healthParser = do
  string "<health>"
  spaces
  healthVal <- integerParser
  spaces
  string "</health>"
  return healthVal

damageParser :: Parsec String () Damage
damageParser = do
  string "<damage>"
  spaces
  damageVal <- integerParser
  spaces
  string "</damage>"
  return damageVal

defenceParser :: Parsec String () Defence
defenceParser = do
  string "<defence>"
  spaces
  defenceVal <- integerParser
  spaces
  string "</defence>"
  return defenceVal
