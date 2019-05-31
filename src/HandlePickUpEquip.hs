module HandlePickUpEquip where

import GameDefinitions
import MiscStrings
import PrintDescribeThings
import CheckContainThings
import FilterFindThings
import ExecuteResult
import Helpers

handlePickUp :: Game -> Name -> IO Game
handlePickUp game@(Game gameData@(GameData _ _ wincdn
                                           grooms gitems gobjcs
                                           gchrcs gqests gmstrs)
                        playerData@(PlayerData posn itms wepn armr
                                               hlth mhth bdmg bdef
                                               pups qsts flgs)
                        inCombat combatHealth combatRef)
             name = do
  room@(Room _ _
             itemRefs _ _ _
             _ _ _ _
             _ _ _ _) <- return $ findCurrentRoom grooms posn
  filteredRefs <- return $ filterPickedUpItems itemRefs pups
  foundPickUpItem <- return $ filterOptItem filteredRefs $ findItemByName gitems name
  postActionGame <- case foundPickUpItem of
    [] -> do
      putStrLn $ "I don't see any \"" ++ name ++ "\" around here to pick up"
      return game
    ((Item pickUpRef _ _ _ ((PickUp sets descValue):[]) _ _ _):[]) -> do
      printDescription descValue
      return $ executeResult (addPickUpInGame game pickUpRef)
                             (Result sets (pickUpRef:[]) [] [] [] [] [] descValue)
  handleRetaliation postActionGame

handleEquip :: Game -> Name -> IO Game
handleEquip game@(Game gameData@(GameData _ _ wincdn
                                          grooms gitems gobjcs
                                          gchrcs gqests gmstrs)
                       playerData@(PlayerData posn itms wepn armr
                                              hlth mhth bdmg bdef
                                              pups qsts flgs)
                       inCombat combatHealth combatRef)
            name = do
  foundItem <- return $ findItemByName gitems name
  postActionGame <- if checkRefsContainOptItem foundItem itms
    then do
      (Item itemRef _ _ _ _ _ itemDmg itemDef) <- return $ head foundItem
      case (itemDmg, itemDef) of
        ([], []) -> do
          putStrLn "This item can't be used as a weapon or armor"
          return game
        ((itemDmgVal:[]), _) -> do
          putStrLn $ "You equip \"" ++ name ++ "\" as a weapon"
          return (Game gameData
                       (PlayerData posn itms (itemRef:[]) armr
                                   hlth mhth bdmg bdef
                                   pups qsts flgs)
                       inCombat combatHealth combatRef)
        (_, (itemDefVal:[])) -> do
          putStrLn $ "You equip \"" ++ name ++ "\" as armor"
          return (Game gameData
                       (PlayerData posn itms wepn (itemRef:[])
                                   hlth mhth bdmg bdef
                                   pups qsts flgs)
                       inCombat combatHealth combatRef)
    else do
      putStrLn $ "I don't see any \"" ++ name ++ "\" in your inventory"
      return game
  handleRetaliation postActionGame
