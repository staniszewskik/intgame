module Helpers where

import GameDefinitions
import MiscStrings
import PrintDescribeThings
import FilterFindThings
import ExecuteResult

changePos :: Position -> XPos -> YPos -> Position
changePos (Position x y) moveX moveY = (Position (x + moveX) (y + moveY))

getDefValue :: Item -> Defence
getDefValue (Item _ _ _ _ _ _ _ (def:[])) = def

getArmorDef :: [Item] -> [ArmorRef] -> Defence
getArmorDef _ [] = 0
getArmorDef gitems (a:[]) = getDefValue $ findItem gitems a

getDmgValue :: Item -> Damage
getDmgValue (Item _ _ _ _ _ _ (dmg:[]) _) = dmg

getWeaponDmg :: [Item] -> [WeaponRef] -> Damage
getWeaponDmg _ [] = 0
getWeaponDmg gitems (w:[]) = getDmgValue $ findItem gitems w

getUses :: [Item] -> [Object] -> [Use]
getUses [] [] = []
getUses ((Item _ _ _ uses _ _ _ _):[]) _ = uses
getUses _ ((Object _ _ _ uses _):[]) = uses

getInteracts :: [Item] -> [Object] -> [Character] -> [Interact]
getInteracts [] [] [] = []
getInteracts ((Item _ _ _ _ _ interacts _ _):[]) _ _ = interacts
getInteracts _ ((Object _ _ _ _ interacts):[]) _ = interacts
getInteracts _ _ ((Character _ _ _ interacts):[]) = interacts

removeQuest :: [QuestRef] -> QuestRef -> [QuestRef]
removeQuest [] _ = []
removeQuest (q:qs) ref =
  if q == ref
    then removeQuest qs ref
    else (q:(removeQuest qs ref))

reduceHealthInGame :: Game -> Damage -> Game
reduceHealthInGame (Game gameData
                         (PlayerData posn itms wepn armr
                                     hlth mhth bdmg bdef
                                     pups qsts flgs)
                         inCombat combatHealth combatRef)
                   recDamage =
  (Game gameData
        (PlayerData posn itms wepn armr
                    (hlth - recDamage) mhth bdmg bdef
                    pups qsts flgs)
        inCombat combatHealth combatRef)

addPickUpInGame :: Game -> PickedUpRef -> Game
addPickUpInGame (Game gameData
                      (PlayerData posn itms wepn armr
                                  hlth mhth bdmg bdef
                                  pups qsts flgs)
                      inCombat combatHealth combatRef)
                pickUpRef =
  (Game gameData
        (PlayerData posn itms wepn armr
                    hlth mhth bdmg bdef
                    (pickUpRef:pups) qsts flgs)
        inCombat combatHealth combatRef)

handleRetaliation :: Game -> IO Game
handleRetaliation game@(Game gameData@(GameData _ _ wincdn
                                                grooms gitems gobjcs
                                                gchrcs gqests gmstrs)
                             playerData@(PlayerData posn itms wepn armr
                                                    hlth mhth bdmg bdef
                                                    pups qsts flgs)
                             inCombat combatHealth combatRef) = do
  if inCombat
    then do
      curMonster@(Monster _ combatName _
                          _ combatDmg _ _) <- return $ findMonster gmstrs combatRef
      fullDef <- return $ bdef + (getArmorDef gitems armr)
      recDamage <- if combatDmg - fullDef < 0
        then return 0
        else return $ combatDmg - fullDef
      putStrLn $ attackedMessage combatName recDamage
      return $ reduceHealthInGame game recDamage
    else return game

handlePlayerAttack :: Game -> Monster -> CombatHealth -> Damage -> IO Game
handlePlayerAttack game@(Game gameData@(GameData _ _ wincdn
                                                 grooms gitems gobjcs
                                                 gchrcs gqests gmstrs)
                              playerData@(PlayerData posn itms wepn armr
                                                     hlth mhth bdmg bdef
                                                     pups qsts flgs)
                              inCombat combatHealth combatRef)
                   monsterVal
                   monsterHealth
                   dealtDamage = do
  (Monster monsterRef name _ _ _ _
           (Kill killRes@(Result _ _ _ _ _ _ _ killDesc))) <- return monsterVal
  putStrLn $ attackMessage name dealtDamage
  if monsterHealth - dealtDamage <= 0
    then do
      printDescription killDesc
      return $ executeResult (Game gameData
                                   playerData
                                   False 0 "monster_none")
                             killRes
    else return (Game gameData
                      playerData
                      True
                      (monsterHealth - dealtDamage)
                      monsterRef)
