module HandleMoves where

import GameDefinitions
import MiscStrings
import PrintDescribeThings
import CheckContainThings
import FilterFindThings
import Helpers

handleGoN :: Game -> IO Game
handleGoN game@(Game gameData@(GameData _ _ wincdn
                                        grooms gitems gobjcs
                                        gchrcs gqests gmstrs)
                     playerData@(PlayerData posn itms wepn armr
                                            hlth mhth bdmg bdef
                                            pups qsts flgs)
                     inCombat combatHealth combatRef) = do
  room@(Room _ _
             _ _ _ _
             gn ge gs gw
             _ _ _ _) <- return $ findCurrentRoom grooms posn
  case (inCombat, gn) of
    (True, _) -> (putStrLn combatActionFailMsg) >> return game
    (_, []) -> (putStrLn goDirFailMsg) >> return game
    (_, (GoNorth req descValue):[]) -> if not $ checkOptRequires req flgs
      then (putStrLn goDirFailMsg) >> return game
      else do
        printDescription descValue
        return (Game gameData
                     (PlayerData (changePos posn 0 1) itms wepn armr
                                 hlth mhth bdmg bdef
                                 pups qsts flgs)
                     inCombat combatHealth combatRef)

handleGoE :: Game -> IO Game
handleGoE game@(Game gameData@(GameData _ _ wincdn
                                        grooms gitems gobjcs
                                        gchrcs gqests gmstrs)
                     playerData@(PlayerData posn itms wepn armr
                                            hlth mhth bdmg bdef
                                            pups qsts flgs)
                     inCombat combatHealth combatRef) = do
  room@(Room _ _
             _ _ _ _
             gn ge gs gw
             _ _ _ _) <- return $ findCurrentRoom grooms posn
  case (inCombat, ge) of
    (True, _) -> (putStrLn combatActionFailMsg) >> return game
    (_, []) -> (putStrLn goDirFailMsg) >> return game
    (_, (GoEast req descValue):[]) -> if not $ checkOptRequires req flgs
      then (putStrLn goDirFailMsg) >> return game
      else do
        printDescription descValue
        return (Game gameData
                     (PlayerData (changePos posn 1 0) itms wepn armr
                                 hlth mhth bdmg bdef
                                 pups qsts flgs)
                     inCombat combatHealth combatRef)

handleGoS :: Game -> IO Game
handleGoS game@(Game gameData@(GameData _ _ wincdn
                                        grooms gitems gobjcs
                                        gchrcs gqests gmstrs)
                     playerData@(PlayerData posn itms wepn armr
                                            hlth mhth bdmg bdef
                                            pups qsts flgs)
                     inCombat combatHealth combatRef) = do
  room@(Room _ _
             _ _ _ _
             gn ge gs gw
             _ _ _ _) <- return $ findCurrentRoom grooms posn
  case (inCombat, gs) of
    (True, _) -> (putStrLn combatActionFailMsg) >> return game
    (_, []) -> (putStrLn goDirFailMsg) >> return game
    (_, (GoSouth req descValue):[]) -> if not $ checkOptRequires req flgs
      then (putStrLn goDirFailMsg) >> return game
      else do
        printDescription descValue
        return (Game gameData
                     (PlayerData (changePos posn 0 (-1)) itms wepn armr
                                 hlth mhth bdmg bdef
                                 pups qsts flgs)
                     inCombat combatHealth combatRef)

handleGoW :: Game -> IO Game
handleGoW game@(Game gameData@(GameData _ _ wincdn
                                        grooms gitems gobjcs
                                        gchrcs gqests gmstrs)
                     playerData@(PlayerData posn itms wepn armr
                                            hlth mhth bdmg bdef
                                            pups qsts flgs)
                     inCombat combatHealth combatRef) = do
  room@(Room _ _
             _ _ _ _
             gn ge gs gw
             _ _ _ _) <- return $ findCurrentRoom grooms posn
  case (inCombat, gw) of
    (True, _) -> (putStrLn combatActionFailMsg) >> return game
    (_, []) -> (putStrLn goDirFailMsg) >> return game
    (_, (GoWest req descValue):[]) -> if not $ checkOptRequires req flgs
      then (putStrLn goDirFailMsg) >> return game
      else do
        printDescription descValue
        return (Game gameData
                     (PlayerData (changePos posn (-1) 0) itms wepn armr
                                 hlth mhth bdmg bdef
                                 pups qsts flgs)
                     inCombat combatHealth combatRef)
