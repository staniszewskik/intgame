module HandleUseInteract where

import GameDefinitions
import MiscStrings
import PrintDescribeThings
import CheckContainThings
import FilterFindThings
import ExecuteResult
import Helpers

handleUseOn :: Game -> Name -> Name -> IO Game
handleUseOn game@(Game gameData@(GameData _ _ wincdn
                                          grooms gitems gobjcs
                                          gchrcs gqests gmstrs)
                       playerData@(PlayerData posn itms wepn armr
                                              hlth mhth bdmg bdef
                                              pups qsts flgs)
                       inCombat combatHealth combatRef)
            item
            trgt = do
  room@(Room _ _
             _ objectRefs _ _
             _ _ _ _
             _ _ _ _) <- return $ findCurrentRoom grooms posn
  foundItem <- return $ findItemByName gitems item
  postActionGame <- if checkRefsContainOptItem foundItem itms
    then do
      ((Item whatRef _ _ _ _ _ _ _):[]) <- return $ foundItem
      foundTrgtItem <- return $ filterOptItem itms $ findItemByName gitems trgt
      foundTrgtObject <- return $ filterOptObject objectRefs $ findObjectByName gobjcs trgt
      possibleUses <- return $ getUses foundTrgtItem foundTrgtObject
      case possibleUses of
        [] -> do
          putStrLn $ "I don't see any \"" ++ trgt ++
                     "\" around here to use \"" ++ item ++ "\" on"
          return game
        _ -> do
          correctWhatUses <- return $ filterByWhat possibleUses whatRef
          case correctWhatUses of
            [] -> do
              putStrLn $ "You can't use \"" ++ item ++
                         "\" on \"" ++ trgt
              return game
            _ -> do
              foundUse <- return $ findApplicableUse correctWhatUses flgs
              case foundUse of
                [] -> do
                  putStrLn "You can't do that"
                  return game
                ((Use _ _ res@(Result _ _ _ _ _ _ _ descValue)):[]) -> do
                  printDescription descValue
                  return $ executeResult game res
    else do
      putStrLn $ "I don't see any \"" ++ item ++ "\" in your inventory"
      return game
  handleRetaliation postActionGame

handleInteractWith :: Game -> Name -> IO Game
handleInteractWith game@(Game gameData@(GameData _ _ wincdn
                                                 grooms gitems gobjcs
                                                 gchrcs gqests gmstrs)
                              playerData@(PlayerData posn itms wepn armr
                                                     hlth mhth bdmg bdef
                                                     pups qsts flgs)
                              inCombat combatHealth combatRef)
                   name = do
  room@(Room _ _
             _ objectRefs characterRefs _
             _ _ _ _
             _ _ _ _) <- return $ findCurrentRoom grooms posn
  foundItem <- return $ filterOptItem itms $ findItemByName gitems name
  foundObject <- return $ filterOptObject objectRefs $ findObjectByName gobjcs name
  foundCharacter <- return $ filterOptCharacter characterRefs $ findCharacterByName gchrcs name
  possibleInteracts <- return $ getInteracts foundItem foundObject foundCharacter
  postActionGame <- case possibleInteracts of
    [] -> do
      putStrLn $ "I don't see any \"" ++ name ++
                 "\" around here to interact with"
      return game
    _ -> do
      foundInteract <- return $ findApplicableInteract possibleInteracts flgs
      case foundInteract of
        [] -> do
          putStrLn "You can't do that"
          return game
        ((Interact _ res@(Result _ _ _ _ _ _ _ descValue)):[]) -> do
          printDescription descValue
          return $ executeResult game res
  handleRetaliation postActionGame
