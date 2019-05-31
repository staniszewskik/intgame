module HandleSaveLoad where

import Text.Parsec
import System.IO
import System.Posix.Files

import GameDefinitions
import PlayerDataParsers
import MiscStrings

positionToString :: Position -> String
positionToString (Position xpos ypos) =
  "    <position>" ++ (show xpos) ++ "," ++ (show ypos) ++ "</position>\n"

inventoryToString :: [ItemRef] -> String
inventoryToString itms =
  "    <inventory>\n" ++
  (foldl (\invString invItem -> invString ++ "        <id>" ++ invItem ++ "</id>\n" ) "" itms) ++
  "    </inventory>\n"

weaponToString :: [WeaponRef] -> String
weaponToString [] = ""
weaponToString (w:[]) =
  "    <weapon>" ++ w ++ "</weapon>\n"

armorToString :: [ArmorRef] -> String
armorToString [] = ""
armorToString (a:[]) =
  "    <armor>" ++ a ++ "</armor>\n"

statsToString :: Health -> MaxHP -> BaseDamage -> BaseDefence -> String
statsToString hlth mhth bdmg bdef =
  "    <health>" ++ (show hlth) ++ "</health>\n" ++
  "    <maxHP>" ++ (show mhth) ++ "</maxHP>\n" ++
  "    <baseDamage>" ++ (show bdmg) ++ "</baseDamage>\n" ++
  "    <baseDefence>" ++ (show bdef) ++ "</baseDefence>\n"

pickedUpToString :: [ItemRef] -> String
pickedUpToString pups =
  "    <pickedUp>\n" ++
  (foldl (\pickString pickItem -> pickString ++ "        <id>" ++ pickItem ++ "</id>\n" ) "" pups) ++
  "    </pickedUp>\n"

questlogToString :: [QuestRef] -> String
questlogToString qsts =
  "    <questlog>\n" ++
  (foldl (\questString questItem -> questString ++ "        <id>" ++ questItem ++ "</id>\n" ) "" qsts) ++
  "    </questlog>\n"

setFlagsToString :: [FlagName] -> String
setFlagsToString flgs =
  "    <setFlags>\n" ++
  (foldl (\flagString flagItem -> flagString ++ "        <id>" ++ flagItem ++ "</id>\n" ) "" flgs) ++
  "    </setFlags>\n"

playerToString :: PlayerData -> String
playerToString playerData@(PlayerData posn itms wepn armr
                                      hlth mhth bdmg bdef
                                      pups qsts flgs) =
  "<player>\n" ++
  positionToString posn ++
  inventoryToString itms ++
  weaponToString wepn ++
  armorToString armr ++
  statsToString hlth mhth bdmg bdef ++
  pickedUpToString pups ++
  questlogToString qsts ++
  setFlagsToString flgs ++
  "</player>\n"

handleSave :: Game -> String -> IO ()
handleSave game@(Game gameData@(GameData _ _ wincdn
                                         grooms gitems gobjcs
                                         gchrcs gqests gmstrs)
                      playerData@(PlayerData posn itms wepn armr
                                             hlth mhth bdmg bdef
                                             pups qsts flgs)
                      inCombat combatHealth combatRef)
           saveName = do
  if inCombat
    then putStrLn combatActionFailMsg
    else do
      putStrLn $ "You save your progress to file: " ++ saveName
      writeFile saveName (playerToString playerData)

handleLoad :: Game -> String -> IO Game
handleLoad game@(Game gameData@(GameData _ _ wincdn
                                         grooms gitems gobjcs
                                         gchrcs gqests gmstrs)
                      playerData@(PlayerData posn itms wepn armr
                                             hlth mhth bdmg bdef
                                             pups qsts flgs)
                      inCombat combatHealth combatRef)
           loadName = do
  loadDataExists <- fileExist loadName
  case (inCombat, loadDataExists) of
    (True, _) -> (putStrLn combatActionFailMsg) >> return game
    (_, False) -> (putStrLn "Can't find save with given name") >> return game
    (_, _) -> do
      loadDataString <- readFile loadName
      parsedLoadData <- return $ parse playerDataParser "" loadDataString
      case parsedLoadData of
        Left err -> do
          putStrLn $ "Failed to load progress from file: " ++ loadName
          return game
        Right loadData -> do
          putStrLn $ "You load your progress from file: " ++ loadName
          return (Game gameData loadData False 0 "monster_none")