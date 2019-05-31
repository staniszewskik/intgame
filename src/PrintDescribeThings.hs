module PrintDescribeThings where

import GameDefinitions
import CheckContainThings
import FilterFindThings

printDescription :: Description -> IO ()
printDescription (Description _ descText) = putStrLn descText

printOptDesc :: [Description] -> IO ()
printOptDesc [] = return ()
printOptDesc (d:[]) = printDescription d

printItemName :: [Item] -> ItemRef -> IO ()
printItemName gitems r = do
  (Item _ name _ _ _ _ _ _) <- return $ findItem gitems r
  putStrLn name

printQuestName :: [Quest] -> QuestRef -> IO ()
printQuestName gqests r = do
  (Quest _ name _ _) <- return $ findQuest gqests r
  putStrLn name

describeItem :: Item -> [FlagName] -> IO ()
describeItem (Item _ _ descValues _ _ _ _ _) fs =
  printOptDesc $ findApplicableDesc descValues fs

describeItems :: [Item] -> [ItemRef] -> [FlagName] -> IO ()
describeItems _ [] _ = return ()
describeItems is (r:rs) fs = do
  describeItem (findItem is r) fs
  describeItems is rs fs

describeObject :: Object -> [FlagName] -> IO ()
describeObject (Object _ _ descValues _ _) fs =
  printOptDesc $ findApplicableDesc descValues fs

describeObjects :: [Object] -> [ObjectRef] -> [FlagName] -> IO ()
describeObjects _ [] _ = return ()
describeObjects os (r:rs) fs = do
  describeObject (findObject os r) fs
  describeObjects os rs fs

describeCharacter :: Character -> [FlagName] -> IO ()
describeCharacter (Character _ _ descValues _) fs =
  printOptDesc $ findApplicableDesc descValues fs

describeCharacters :: [Character] -> [CharacterRef] -> [FlagName] -> IO ()
describeCharacters _ [] _ = return ()
describeCharacters cs (r:rs) fs = do
  describeCharacter (findCharacter cs r) fs
  describeCharacters cs rs fs

describeMonster :: Monster -> [FlagName] -> IO ()
describeMonster (Monster _ _ descValues _ _ _ _) fs =
  printOptDesc $ findApplicableDesc descValues fs

describeMonsters :: [Monster] -> [MonsterRef] -> [FlagName] -> IO ()
describeMonsters _ [] _ = return ()
describeMonsters ms (r:rs) fs = do
  describeMonster (findMonster ms r) fs
  describeMonsters ms rs fs

describeQuest :: Quest -> [FlagName] -> IO ()
describeQuest (Quest _ _ descValues _) fs =
  printOptDesc $ findApplicableDesc descValues fs

describeHealth :: Health -> MaxHP -> IO ()
describeHealth hlth mhth =
  putStrLn $ "You have " ++ (show hlth) ++
              " points of health out of " ++ (show mhth)

describeDamage :: [Item] -> [WeaponRef] -> BaseDamage -> IO ()
describeDamage _ [] bdmg =
  putStrLn $ "You have no weapon equipped and deal " ++
              (show bdmg) ++ " points of damage with each attack"
describeDamage gitems (w:[]) bdmg = do
  (Item _ name _ _ _ _ (dmg:[]) _) <- return $ findItem gitems w
  putStrLn $ "You have \"" ++ name ++ "\" equipped as a weapon and deal " ++
              (show $ bdmg + dmg) ++ " points of damage with each attack"

describeDefence :: [Item] -> [ArmorRef] -> BaseDefence -> IO ()
describeDefence _ [] bdef =
  putStrLn $ "You have no armor equipped and absorb " ++
              (show bdef) ++ " points of damage from each attack"
describeDefence gitems (a:[]) bdef = do
  (Item _ name _ _ _ _ _ (def:[])) <- return $ findItem gitems a
  putStrLn $ "You have \"" ++ name ++ "\" equipped as armor and absorb " ++
              (show $ bdef + def) ++ " points of damage from each attack"

describeInventory' :: [Item] -> [ItemRef] -> IO ()
describeInventory' gitems (r:[]) = printItemName gitems r
describeInventory' gitems (r:rs) = do
  printItemName gitems r
  describeInventory' gitems rs

describeInventory :: [Item] -> [ItemRef] -> IO ()
describeInventory _ [] = putStrLn "Your inventory is empty"
describeInventory gitems itms = do
  putStrLn "You have the following items in your inventory:"
  describeInventory' gitems itms

describeQuestlog' :: [Quest] -> [QuestRef] -> IO ()
describeQuestlog' gqests (r:[]) = printQuestName gqests r
describeQuestlog' gqests (r:rs) = do
  printQuestName gqests r
  describeQuestlog' gqests rs

describeQuestlog :: [Quest] -> [QuestRef] -> IO ()
describeQuestlog _ [] = putStrLn "Your questlog is empty"
describeQuestlog gqests qsts = do
  putStrLn "You have the following entries in your questlog:"
  describeQuestlog' gqests qsts

tryDescribingFoundItem :: [Item] -> [ItemRef] -> [FlagName] -> IO Bool
tryDescribingFoundItem [] _ _ = return False
tryDescribingFoundItem (i:[]) rs flgs =
  if checkRefsContainItem i rs
    then (describeItem i flgs) >> return True else return False

tryDescribingFoundObject :: [Object] -> [ObjectRef] -> [FlagName] -> Bool -> IO Bool
tryDescribingFoundObject _ _ _ True = return True
tryDescribingFoundObject [] _ _ _ = return False
tryDescribingFoundObject (o:[]) rs flgs _ =
  if checkRefsContainObject o rs
    then (describeObject o flgs) >> return True else return False

tryDescribingFoundCharacter :: [Character] -> [CharacterRef] -> [FlagName] -> Bool -> IO Bool
tryDescribingFoundCharacter _ _ _ True = return True
tryDescribingFoundCharacter [] _ _ _ = return False
tryDescribingFoundCharacter (c:[]) rs flgs _ =
  if checkRefsContainCharacter c rs
    then (describeCharacter c flgs) >> return True else return False

tryDescribingFoundMonster :: [Monster] -> [MonsterRef] -> [FlagName] -> Bool -> IO Bool
tryDescribingFoundMonster _ _ _ True = return True
tryDescribingFoundMonster [] _ _ _ = return False
tryDescribingFoundMonster (m:[]) rs flgs _ =
  if checkRefsContainMonster m rs
    then (describeMonster m flgs) >> return True else return False

tryDescribingFoundQuest :: [Quest] -> [QuestRef] -> [FlagName] -> IO Bool
tryDescribingFoundQuest [] _ _ = return False
tryDescribingFoundQuest (q:[]) rs flgs =
  if checkRefsContainQuest q rs
    then (describeQuest q flgs) >> return True else return False
