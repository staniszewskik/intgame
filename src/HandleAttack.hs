module HandleAttack where

import GameDefinitions
import MiscStrings
import PrintDescribeThings
import CheckContainThings
import FilterFindThings
import ExecuteResult
import Helpers

handleAttack :: Game -> Name -> IO Game
handleAttack game@(Game gameData@(GameData _ _ wincdn
                                           grooms gitems gobjcs
                                           gchrcs gqests gmstrs)
                        playerData@(PlayerData posn itms wepn armr
                                               hlth mhth bdmg bdef
                                               pups qsts flgs)
                        inCombat combatHealth combatRef)
             name = do
  room@(Room _ _
             _ _ _ monsterRefs
             _ _ _ _
             _ _ _ _) <- return $ findCurrentRoom grooms posn
  foundMonster <- return $ findMonsterByName gmstrs name
  postActionGame <- if canAttackFoundMonster foundMonster monsterRefs
    then do
      monsterVal@(Monster monsterRef _ _ monsterHealth _ combatDef
                          _) <- return $ head foundMonster
      fullDmg <- return $ bdmg + (getWeaponDmg gitems wepn)
      dealtDamage <- if fullDmg - combatDef < 0
        then return 0
        else return $ fullDmg - combatDef
      if inCombat
        then if monsterRef == combatRef
          then handlePlayerAttack game monsterVal combatHealth dealtDamage
          else (putStrLn "You can't change targets during combat") >> return game
        else handlePlayerAttack game monsterVal monsterHealth dealtDamage
    else do
      putStrLn $ "I don't see any \"" ++ name ++ "\" to attack around here"
      return game
  handleRetaliation postActionGame
