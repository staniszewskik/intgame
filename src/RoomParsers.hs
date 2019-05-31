module RoomParsers where

import Text.Parsec

import GameDefinitions
import CommonParsers

goNorthParser :: Parsec String () GoNorth
goNorthParser = do
  string "<goNorth>"
  spaces
  reqValue <- many (try requiresParser)
  spaces
  descValue <- descriptionParser
  spaces
  string "</goNorth>"
  return (GoNorth reqValue descValue)

goEastParser :: Parsec String () GoEast
goEastParser = do
  string "<goEast>"
  spaces
  reqValue <- many (try requiresParser)
  spaces
  descValue <- descriptionParser
  spaces
  string "</goEast>"
  return (GoEast reqValue descValue)

goSouthParser :: Parsec String () GoSouth
goSouthParser = do
  string "<goSouth>"
  spaces
  reqValue <- many (try requiresParser)
  spaces
  descValue <- descriptionParser
  spaces
  string "</goSouth>"
  return (GoSouth reqValue descValue)

goWestParser :: Parsec String () GoWest
goWestParser = do
  string "<goWest>"
  spaces
  reqValue <- many (try requiresParser)
  spaces
  descValue <- descriptionParser
  spaces
  string "</goWest>"
  return (GoWest reqValue descValue)

northParser :: Parsec String () North
northParser = do
  string "<north>"
  spaces
  descValues <- many1 (try (descriptionParser <* spaces))
  spaces
  string "</north>"
  return (North descValues)

eastParser :: Parsec String () East
eastParser = do
  string "<east>"
  spaces
  descValues <- many1 (try (descriptionParser <* spaces))
  spaces
  string "</east>"
  return (East descValues)

southParser :: Parsec String () South
southParser = do
  string "<south>"
  spaces
  descValues <- many1 (try (descriptionParser <* spaces))
  spaces
  string "</south>"
  return (South descValues)

westParser :: Parsec String () West
westParser = do
  string "<west>"
  spaces
  descValues <- many1 (try (descriptionParser <* spaces))
  spaces
  string "</west>"
  return (West descValues)

roomParser :: Parsec String () Room
roomParser = do
  string "<room>"
  spaces
  posValue <- positionParser False
  spaces
  descValues <- many1 (try (descriptionParser <* spaces))
  spaces
  itemRefs <- many (try (itemRefParser <* spaces))
  spaces
  objectRefs <- many (try (objectRefParser <* spaces))
  spaces
  characterRefs <- many (try (characterRefParser <* spaces))
  spaces
  monsterRefs <- many (try (monsterRefParser <* spaces))
  spaces
  goNorthVal <- many (try (goNorthParser <* spaces))
  spaces
  goEastVal <- many (try (goEastParser <* spaces))
  spaces
  goSouthVal <- many (try (goSouthParser <* spaces))
  spaces
  goWestVal <- many (try (goWestParser <* spaces))
  spaces
  northValues <- many (try (northParser <* spaces))
  spaces
  eastValues <- many (try (eastParser <* spaces))
  spaces
  southValues <- many (try (southParser <* spaces))
  spaces
  westValues <- many (try (westParser <* spaces))
  spaces
  string "</room>"
  return (Room posValue descValues
               itemRefs objectRefs characterRefs monsterRefs
               goNorthVal goEastVal goSouthVal goWestVal
               northValues eastValues southValues westValues)