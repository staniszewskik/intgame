module ResultParsers where

import Text.Parsec

import GameDefinitions
import CommonParsers

setFlagParser :: Parsec String () SetFlag
setFlagParser = do
  string "<set>"
  spaces
  value <- many1 (noneOf " <")
  spaces
  string "</set>"
  return value

giveParser :: Parsec String () Give
giveParser = do
  string "<give>"
  spaces
  giveInner <- many1 (noneOf " <")
  spaces
  string "</give>"
  return giveInner

removeParser :: Parsec String () Remove
removeParser = do
  string "<remove>"
  spaces
  removeInner <- itemInnerParser
  spaces
  string "</remove>"
  return removeInner

increaseHealthParser :: Parsec String () IncreaseHealth
increaseHealthParser = do
  string "<increaseHealth>"
  spaces
  increaseVal <- integerParser
  spaces
  string "</increaseHealth>"
  return increaseVal

increaseMaxHPParser :: Parsec String () IncreaseMaxHP
increaseMaxHPParser = do
  string "<increaseMaxHP>"
  spaces
  increaseVal <- integerParser
  spaces
  string "</increaseMaxHP>"
  return increaseVal

increaseBaseDamageParser :: Parsec String () IncreaseBaseDamage
increaseBaseDamageParser = do
  string "<increaseBaseDamage>"
  spaces
  increaseVal <- integerParser
  spaces
  string "</increaseBaseDamage>"
  return increaseVal

increaseBaseDefenceParser :: Parsec String () IncreaseBaseDefence
increaseBaseDefenceParser = do
  string "<increaseBaseDefence>"
  spaces
  increaseVal <- integerParser
  spaces
  string "</increaseBaseDefence>"
  return increaseVal

resultParser :: Parsec String () Result
resultParser = do
  string "<result>"
  spaces
  setFlagValues <- many (try (setFlagParser <* spaces))
  spaces
  giveValues <- many (try (giveParser <* spaces))
  spaces
  removeValues <- many (try (removeParser <* spaces))
  spaces
  increaseHealthVal <- many (try increaseHealthParser)
  spaces
  increaseMaxHPVal <- many (try increaseMaxHPParser)
  spaces
  increaseBaseDamageVal <- many (try increaseBaseDamageParser)
  spaces
  increaseBaseDefenceVal <- many (try increaseBaseDefenceParser)
  spaces
  descValue <- descriptionParser
  spaces
  string "</result>"
  return (Result setFlagValues giveValues removeValues
                 increaseHealthVal increaseMaxHPVal
                 increaseBaseDamageVal increaseBaseDefenceVal
                 descValue)