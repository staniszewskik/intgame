module HandleLooks where

import GameDefinitions
import MiscStrings
import PrintDescribeThings
import FilterFindThings

handleLookAround :: Game -> IO ()
handleLookAround game@(Game gameData@(GameData _ _ wincdn
                                               grooms gitems gobjcs
                                               gchrcs gqests gmstrs)
                            playerData@(PlayerData posn itms wepn armr
                                                   hlth mhth bdmg bdef
                                                   pups qsts flgs)
                            inCombat combatHealth combatRef) = do
  room@(Room _ descValues
             itemRefs objectRefs characterRefs monsterRefs
             _ _ _ _
             _ _ _ _) <- return $ findCurrentRoom grooms posn
  printOptDesc $ findApplicableDesc descValues flgs
  filteredRefs <- return $ filterPickedUpItems itemRefs pups
  describeItems gitems filteredRefs flgs
  describeObjects gobjcs objectRefs flgs
  describeCharacters gchrcs characterRefs flgs
  describeMonsters gmstrs monsterRefs flgs

handleLookAtSelf :: Game -> IO ()
handleLookAtSelf game@(Game gameData@(GameData _ _ wincdn
                                               grooms gitems gobjcs
                                               gchrcs gqests gmstrs)
                            playerData@(PlayerData posn itms wepn armr
                                                   hlth mhth bdmg bdef
                                                   pups qsts flgs)
                            inCombat combatHealth combatRef) = do
  describeHealth hlth mhth
  describeDamage gitems wepn bdmg
  describeDefence gitems armr bdef
  describeInventory gitems itms

handleQuestlogRead :: Game -> Name -> IO ()
handleQuestlogRead game@(Game gameData@(GameData _ _ wincdn
                                                 grooms gitems gobjcs
                                                 gchrcs gqests gmstrs)
                              playerData@(PlayerData posn itms wepn armr
                                                     hlth mhth bdmg bdef
                                                     pups qsts flgs)
                              inCombat combatHealth combatRef)
                   name = do
  foundQuest <- return $ findQuestByName gqests name
  questDescribed <- tryDescribingFoundQuest foundQuest qsts flgs
  if not questDescribed
    then putStrLn $ "There's no entry for \"" ++ name ++ "\" in your questlog"
    else return ()

handleLookAt :: Game -> Name -> IO ()
handleLookAt game@(Game gameData@(GameData _ _ wincdn
                                           grooms gitems gobjcs
                                           gchrcs gqests gmstrs)
                        playerData@(PlayerData posn itms wepn armr
                                               hlth mhth bdmg bdef
                                               pups qsts flgs)
                        inCombat combatHealth combatRef)
             name = do
  room@(Room _ descValues
             itemRefs objectRefs characterRefs monsterRefs
             _ _ _ _
             _ _ _ _) <- return $ findCurrentRoom grooms posn
  filteredRefs <- return $ filterPickedUpItems itemRefs pups
  allItemRefs <- return $ filteredRefs ++ itms
  foundItem <- return $ findItemByName gitems name
  itemDescribed <- tryDescribingFoundItem foundItem allItemRefs flgs
  foundObject <- return $ findObjectByName gobjcs name
  objectDescribed <- tryDescribingFoundObject foundObject objectRefs
                                              flgs itemDescribed
  foundCharacter <- return $ findCharacterByName gchrcs name
  characterDescribed <- tryDescribingFoundCharacter foundCharacter characterRefs
                                                    flgs objectDescribed
  foundMonster <- return $ findMonsterByName gmstrs name
  monsterDescribed <- tryDescribingFoundMonster foundMonster monsterRefs
                                                flgs characterDescribed
  if not monsterDescribed
    then putStrLn $ "I don't see any \"" ++ name ++ "\" around here"
    else return ()

handleLookN :: Game -> IO ()
handleLookN game@(Game gameData@(GameData _ _ wincdn
                                          grooms gitems gobjcs
                                          gchrcs gqests gmstrs)
                       playerData@(PlayerData posn itms wepn armr
                                              hlth mhth bdmg bdef
                                              pups qsts flgs)
                       inCombat combatHealth combatRef) = do
  room@(Room _ _
             _ _ _ _
             _ _ _ _
             ln le ls lw) <- return $ findCurrentRoom grooms posn
  case ln of
    ((North descValues):[]) ->
      printOptDesc $ findApplicableDesc descValues flgs
    [] -> putStrLn lookDirFailMsg

handleLookE :: Game -> IO ()
handleLookE game@(Game gameData@(GameData _ _ wincdn
                                          grooms gitems gobjcs
                                          gchrcs gqests gmstrs)
                       playerData@(PlayerData posn itms wepn armr
                                              hlth mhth bdmg bdef
                                              pups qsts flgs)
                       inCombat combatHealth combatRef) = do
  room@(Room _ _
             _ _ _ _
             _ _ _ _
             ln le ls lw) <- return $ findCurrentRoom grooms posn
  case le of
    ((East descValues):[]) ->
      printOptDesc $ findApplicableDesc descValues flgs
    [] -> putStrLn lookDirFailMsg

handleLookS :: Game -> IO ()
handleLookS game@(Game gameData@(GameData _ _ wincdn
                                          grooms gitems gobjcs
                                          gchrcs gqests gmstrs)
                       playerData@(PlayerData posn itms wepn armr
                                              hlth mhth bdmg bdef
                                              pups qsts flgs)
                       inCombat combatHealth combatRef) = do
  room@(Room _ _
             _ _ _ _
             _ _ _ _
             ln le ls lw) <- return $ findCurrentRoom grooms posn
  case ls of
    ((South descValues):[]) ->
      printOptDesc $ findApplicableDesc descValues flgs
    [] -> putStrLn lookDirFailMsg

handleLookW :: Game -> IO ()
handleLookW game@(Game gameData@(GameData _ _ wincdn
                                          grooms gitems gobjcs
                                          gchrcs gqests gmstrs)
                       playerData@(PlayerData posn itms wepn armr
                                              hlth mhth bdmg bdef
                                              pups qsts flgs)
                       inCombat combatHealth combatRef) = do
  room@(Room _ _
             _ _ _ _
             _ _ _ _
             ln le ls lw) <- return $ findCurrentRoom grooms posn
  case lw of
    ((West descValues):[]) ->
      printOptDesc $ findApplicableDesc descValues flgs
    [] -> putStrLn lookDirFailMsg
