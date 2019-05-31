module PlayerDataParsers where

import Text.Parsec

import GameDefinitions
import CommonParsers

inventoryParser :: Parsec String () [ItemRef]
inventoryParser = do
  string "<inventory>"
  spaces
  itemIds <- many (try (itemIdParser <* spaces))
  spaces
  string "</inventory>"
  return itemIds

weaponParser :: Parsec String () WeaponRef
weaponParser = do
  string "<weapon>"
  spaces
  refInner <- itemInnerParser
  spaces
  string "</weapon>"
  return refInner

armorParser :: Parsec String () ArmorRef
armorParser = do
  string "<armor>"
  spaces
  refInner <- itemInnerParser
  spaces
  string "</armor>"
  return refInner

maxHPParser :: Parsec String () MaxHP
maxHPParser = do
  string "<maxHP>"
  spaces
  maxHPVal <- integerParser
  spaces
  string "</maxHP>"
  return maxHPVal

baseDamageParser :: Parsec String () BaseDamage
baseDamageParser = do
  string "<baseDamage>"
  spaces
  baseDamageVal <- integerParser
  spaces
  string "</baseDamage>"
  return baseDamageVal

baseDefenceParser :: Parsec String () BaseDefence
baseDefenceParser = do
  string "<baseDefence>"
  spaces
  baseDefenceVal <- integerParser
  spaces
  string "</baseDefence>"
  return baseDefenceVal

pickedUpParser :: Parsec String () [PickedUpRef]
pickedUpParser = do
  string "<pickedUp>"
  spaces
  pickedUpIds <- many (try (itemIdParser <* spaces))
  spaces
  string "</pickedUp>"
  return pickedUpIds

questlogParser :: Parsec String () [QuestRef]
questlogParser = do
  string "<questlog>"
  spaces
  questIds <- many (try (questIdParser <* spaces))
  spaces
  string "</questlog>"
  return questIds

flagIdParser :: Parsec String () FlagName
flagIdParser = do
  string "<id>"
  spaces
  refInner <- many1 (noneOf " <")
  spaces
  string "</id>"
  return refInner

setFlagsParser :: Parsec String () [FlagName]
setFlagsParser = do
  string "<setFlags>"
  spaces
  flagIds <- many (try (flagIdParser <* spaces))
  spaces
  string "</setFlags>"
  return flagIds

playerDataParser :: Parsec String () PlayerData
playerDataParser = do
  string "<player>"
  spaces
  posValue <- positionParser False
  spaces
  inventoryValues <- inventoryParser
  spaces
  weaponVal <- many (try weaponParser)
  spaces
  armorVal <- many (try armorParser)
  spaces
  healthVal <- healthParser
  spaces
  maxHPVal <- maxHPParser
  spaces
  baseDamageVal <- baseDamageParser
  spaces
  baseDefenceVal <- baseDefenceParser
  spaces
  pickedUpValues <- pickedUpParser
  spaces
  questlogValues <- questlogParser
  spaces
  setFlagsValues <- setFlagsParser
  spaces
  string "</player>"
  return (PlayerData posValue inventoryValues weaponVal armorVal
                     healthVal maxHPVal
                     baseDamageVal baseDefenceVal
                     pickedUpValues questlogValues setFlagsValues)
