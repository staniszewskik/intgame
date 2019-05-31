module Main where

import Text.Parsec
import System.IO
import System.Environment
import System.Posix.Files

import GameDefinitions
import GameDataParsers
import PlayerDataParsers
import GameActionParsers
import MiscStrings
import PrintDescribeThings
import CheckContainThings
import FilterFindThings
import ExecuteResult
import Helpers
import HandleSaveLoad
import HandleLooks
import HandleMoves
import HandleUseInteract
import HandlePickUpEquip
import HandleAttack

handleDeath :: Game -> IO ()
handleDeath postCombatGame@(Game _
                                 (PlayerData _ _ _ _
                                             postCombatHlth _ _ _
                                             _ _ _)
                                 _ _ _) = do
  if postCombatHlth > 0
    then checkQuests postCombatGame
    else do
      putStrLn combatDeathMsg
      return ()

checkQuests :: Game -> IO ()
checkQuests game@(Game gameData@(GameData _ _ wincdn
                                          grooms gitems gobjcs
                                          gchrcs gqests gmstrs)
                       playerData@(PlayerData posn itms wepn armr
                                              hlth mhth bdmg bdef
                                              pups qsts flgs)
                       inCombat combatHealth combatRef) = do
  foundQuest <- return $ findApplicableQuest gqests qsts flgs
  case foundQuest of
    [] -> do
      (WinCondition req descValue) <- return wincdn
      if checkRequires req flgs
        then do
          printDescription descValue
          return ()
        else gameLoop game
    ((Quest ref _ _ (Completion _ res@(Result _ _ _ _ _ _ _ descValue))):[]) -> do
      printDescription descValue
      postQuestGame <- return (Game gameData
                                    (PlayerData posn itms wepn armr
                                                hlth mhth bdmg bdef
                                                pups (removeQuest qsts ref) flgs)
                                    inCombat combatHealth combatRef)
      postResultGame <- return $ executeResult postQuestGame res
      checkQuests postResultGame

gameLoop :: Game -> IO ()
gameLoop game@(Game gameData@(GameData _ _ wincdn
                                       grooms gitems gobjcs
                                       gchrcs gqests gmstrs)
                    playerData@(PlayerData posn itms wepn armr
                                           hlth mhth bdmg bdef
                                           pups qsts flgs)
                    inCombat combatHealth combatRef) = do
  putStr "> "
  hFlush stdout
  command <- getLine
  parsedGameAction <- return $ parse gameActionParser "" command
  case parsedGameAction of
    Left _ -> (putStrLn "Come again?") >> (gameLoop game)
    Right gameAction -> case gameAction of
      (LookAroundAction) -> (handleLookAround game) >> (gameLoop game)
      (LookAtSelfAction) -> (handleLookAtSelf game) >> (gameLoop game)
      (LookAtAction name) -> (handleLookAt game name) >> (gameLoop game)
      (LookNAction) -> (handleLookN game) >> (gameLoop game)
      (LookEAction) -> (handleLookE game) >> (gameLoop game)
      (LookSAction) -> (handleLookS game) >> (gameLoop game)
      (LookWAction) -> (handleLookW game) >> (gameLoop game)
      (GoNAction) -> (handleGoN game) >>= gameLoop
      (GoEAction) -> (handleGoE game) >>= gameLoop
      (GoSAction) -> (handleGoS game) >>= gameLoop
      (GoWAction) -> (handleGoW game) >>= gameLoop
      (UseOnAction item trgt) -> (handleUseOn game item trgt) >>= handleDeath
      (PickUpAction name) -> (handlePickUp game name) >>= handleDeath
      (InteractWithAction name) -> (handleInteractWith game name) >>= handleDeath
      (EquipAction name) -> (handleEquip game name) >>= handleDeath
      (AttackAction name) -> (handleAttack game name) >>= handleDeath
      (QuestlogListAction) -> (describeQuestlog gqests qsts) >> (gameLoop game)
      (QuestlogReadAction name) -> (handleQuestlogRead game name) >> (gameLoop game)
      (SaveAction saveName) -> (handleSave game saveName) >> (gameLoop game)
      (LoadAction loadName) -> (handleLoad game loadName) >>= gameLoop
      (ExitAction) -> (putStrLn "Bye!") >> (return ())

initialiseGame :: GameData -> PlayerData -> IO ()
initialiseGame gameData@(GameData startPos startDesc _ _ _ _ _ _ _)
               playerData@(PlayerData posn itms wepn armr hlth mhth bdmg bdef pups qsts flgs) = do
  printDescription startDesc
  newPlayerData <- return (PlayerData startPos itms wepn armr hlth mhth bdmg bdef pups qsts flgs)
  gameLoop (Game gameData newPlayerData False 0 "monster_none")

parseDataStrings :: String -> String -> Bool -> IO ()
parseDataStrings gameDataString playerDataString cleanStart = do
  parsedGameData <- return $ parse gameDataParser "" gameDataString
  case parsedGameData of
        Left err -> do
          putStrLn "Failed to parse game data:"
          putStrLn $ show err
        Right gameData -> do
          parsedPlayerData <- return $ parse playerDataParser "" playerDataString
          case parsedPlayerData of
              Left err -> do
                putStrLn "Failed to parse save data:"
                putStrLn $ show err
              Right playerData -> do
                putStrLn "Successfully parsed all data"
                putStrLn "Starting game now"
                if cleanStart
                  then initialiseGame gameData playerData
                  else gameLoop (Game gameData playerData False 0 "monster_none")

main :: IO ()
main = do
  gameArgs <- getArgs
  argCount <- return $ length gameArgs
  if argCount == 1 || argCount == 2
    then do
      gameDataPath <- return $ head gameArgs
      gameDataExists <- fileExist gameDataPath
      if gameDataExists
        then do
          gameDataString <- readFile gameDataPath
          if argCount == 1
            then do
              playerDataString <- return cleanSave
              parseDataStrings gameDataString playerDataString True
            else do
              playerDataPath <- return $ head $ tail gameArgs
              playerDataExists <- fileExist playerDataPath
              if playerDataExists
                then do
                  playerDataString <- readFile playerDataPath
                  parseDataStrings gameDataString playerDataString False
                else putStrLn "Can't find save with given name"
        else putStrLn "Can't find game with given name"
    else putStrLn "Wrong number of arguments"
