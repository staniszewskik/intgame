module ExecuteResult where

import GameDefinitions
import CheckContainThings
import FilterFindThings

findResItms :: [ItemRef] -> [Give] -> [Remove] -> [ItemRef]
findResItms itms givs rems = filterRemovedItems (itms ++ (filterGives givs)) rems

findResWepn :: [WeaponRef] -> [Remove] -> [WeaponRef]
findResWepn [] _ = []
findResWepn (w:[]) rems = if remsContainInvItem w rems then [] else (w:[])

findResArmr :: [ArmorRef] -> [Remove] -> [ArmorRef]
findResArmr [] _ = []
findResArmr (a:[]) rems = if remsContainInvItem a rems then [] else (a:[])

findResHlth :: Health -> MaxHP -> [IncreaseHealth] -> Health
findResHlth hlth _ [] = hlth
findResHlth hlth mhth (incVal:[]) = if hlth + incVal > mhth then mhth else hlth + incVal

findResMhth :: MaxHP -> [IncreaseMaxHP] -> MaxHP
findResMhth mhth [] = mhth
findResMhth mhth (incVal:[]) = mhth + incVal

findResBdmg :: BaseDamage -> [IncreaseBaseDamage] -> BaseDamage
findResBdmg bdmg [] = bdmg
findResBdmg bdmg (incVal:[]) = bdmg + incVal

findResBdef :: BaseDefence -> [IncreaseBaseDefence] -> BaseDefence
findResBdef bdef [] = bdef
findResBdef bdef (incVal:[]) = bdef + incVal

findResQsts :: [QuestRef] -> [Give] -> [QuestRef]
findResQsts qsts givs = qsts ++ (filterQuestGives givs)

findResFlgs :: [FlagName] -> [SetFlag] -> [FlagName]
findResFlgs flgs [] = flgs
findResFlgs flgs (f:fs) =
  if setFlagsContainFlag f flgs
    then findResFlgs flgs fs
    else findResFlgs (f:flgs) fs

executeResult :: Game -> Result -> Game
executeResult game@(Game gameData@(GameData _ _ wincdn
                                            grooms gitems gobjcs
                                            gchrcs gqests gmstrs)
                         playerData@(PlayerData posn itms wepn armr
                                                hlth mhth bdmg bdef
                                                pups qsts flgs)
                         inCombat combatHealth combatRef)
              result@(Result sets givs rems
                             incHlth incMhth incBdmg incBdef _) =
  (Game gameData
        (PlayerData posn (findResItms itms givs rems) (findResWepn wepn rems) (findResArmr armr rems)
                    (findResHlth hlth (findResMhth mhth incMhth) incHlth) (findResMhth mhth incMhth)
                    (findResBdmg bdmg incBdmg) (findResBdef bdef incBdef)
                    pups (findResQsts qsts givs) (findResFlgs flgs sets))
        inCombat combatHealth combatRef)
