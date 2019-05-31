module GameDataParsers where

import Text.Parsec

import GameDefinitions
import CommonParsers
import RoomParsers
import ResultParsers

winConditionParser :: Parsec String () WinCondition
winConditionParser = do
  string "<winCondition>"
  spaces
  reqValue <- requiresParser
  spaces
  descValue <- descriptionParser
  spaces
  string "</winCondition>"
  return (WinCondition reqValue descValue)

whatParser :: Parsec String () What
whatParser = do
  string "<what>"
  spaces
  whatInner <- itemInnerParser
  spaces
  string "</what>"
  return whatInner

useParser :: Parsec String () Use
useParser = do
  string "<use>"
  spaces
  reqValue <- many (try requiresParser)
  spaces
  whatVal <- whatParser
  spaces
  resultVal <- resultParser
  spaces
  string "</use>"
  return (Use reqValue whatVal resultVal)

pickUpParser :: Parsec String () PickUp
pickUpParser = do
  string "<pickUp>"
  spaces
  setFlagValues <- many (try (setFlagParser <* spaces))
  spaces
  descValue <- descriptionParser
  spaces
  string "</pickUp>"
  return (PickUp setFlagValues descValue)

interactParser :: Parsec String () Interact
interactParser = do
  string "<interact>"
  spaces
  reqValue <- many (try requiresParser)
  spaces
  resultVal <- resultParser
  spaces
  string "</interact>"
  return (Interact reqValue resultVal)

itemParser :: Parsec String () Item
itemParser = do
  string "<item>"
  spaces
  idVal <- itemIdParser
  spaces
  nameVal <- nameParser
  spaces
  descValues <- many1 (try (descriptionParser <* spaces))
  spaces
  useValues <- many (try (useParser <* spaces))
  spaces
  pickUpVal <- many (try pickUpParser)
  spaces
  interactValues <- many (try (interactParser <* spaces))
  spaces
  damageVal <- many (try damageParser)
  spaces
  defenceVal <- many (try defenceParser)
  spaces
  string "</item>"
  return (Item idVal nameVal descValues
                useValues pickUpVal interactValues
                damageVal defenceVal)

objectParser :: Parsec String () Object
objectParser = do
  string "<object>"
  spaces
  idVal <- objectIdParser
  spaces
  nameVal <- nameParser
  spaces
  descValues <- many1 (try (descriptionParser <* spaces))
  spaces
  useValues <- many (try (useParser <* spaces))
  spaces
  interactValues <- many (try (interactParser <* spaces))
  spaces
  string "</object>"
  return (Object idVal nameVal descValues
                  useValues interactValues)

characterParser :: Parsec String () Character
characterParser = do
  string "<character>"
  spaces
  idVal <- characterIdParser
  spaces
  nameVal <- nameParser
  spaces
  descValues <- many1 (try (descriptionParser <* spaces))
  spaces
  interactValues <- many (try (interactParser <* spaces))
  spaces
  string "</character>"
  return (Character idVal nameVal descValues
                    interactValues)

completionParser :: Parsec String () Completion
completionParser = do
  string "<completion>"
  spaces
  reqValue <- requiresParser
  spaces
  resultVal <- resultParser
  spaces
  string "</completion>"
  return (Completion reqValue resultVal)

questParser :: Parsec String () Quest
questParser = do
  string "<quest>"
  spaces
  idVal <- questIdParser
  spaces
  nameVal <- nameParser
  spaces
  descValues <- many1 (try (descriptionParser <* spaces))
  spaces
  completionVal <- completionParser
  spaces
  string "</quest>"
  return (Quest idVal nameVal descValues completionVal)
  
killParser :: Parsec String () Kill
killParser = do
  string "<kill>"
  spaces
  resultVal <- resultParser
  spaces
  string "</kill>"
  return (Kill resultVal)

monsterParser :: Parsec String () Monster
monsterParser = do
  string "<monster>"
  spaces
  idVal <- monsterIdParser
  spaces
  nameVal <- nameParser
  spaces
  descValues <- many1 (try (descriptionParser <* spaces))
  spaces
  healthVal <- healthParser
  spaces
  damageVal <- damageParser
  spaces
  defenceVal <- defenceParser
  spaces
  killVal <- killParser
  spaces
  string "</monster>"
  return (Monster idVal nameVal descValues
                  healthVal damageVal defenceVal
                  killVal)

gameDataParser :: Parsec String () GameData
gameDataParser = do
  string "<game>"
  spaces
  posValue <- positionParser True
  spaces
  descValue <- descriptionParser
  spaces
  winCondValue <- winConditionParser
  spaces
  roomValues <- many1 (try (roomParser <* spaces))
  spaces
  itemValues <- many (try (itemParser <* spaces))
  spaces
  objectValues <- many (try (objectParser <* spaces))
  spaces
  characterValues <- many (try (characterParser <* spaces))
  spaces
  questValues <- many (try (questParser <* spaces))
  spaces
  monsterValues <- many (try (monsterParser <* spaces))
  spaces
  string "</game>"
  return (GameData posValue descValue winCondValue
                   roomValues itemValues objectValues
                   characterValues questValues monsterValues)
