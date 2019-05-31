module FilterFindThings where

import GameDefinitions
import CheckContainThings

filterPickedUpItems :: [ItemRef] -> [ItemRef] -> [ItemRef]
filterPickedUpItems [] _ = []
filterPickedUpItems (roomRef:roomRefs) pickedRefs =
  if pickedUpsContainRoomRef roomRef pickedRefs
    then filterPickedUpItems roomRefs pickedRefs
    else (roomRef:(filterPickedUpItems roomRefs pickedRefs))

filterGives :: [Give] -> [ItemRef]
filterGives [] = []
filterGives (g:gs) =
  if checkGivesItem g
    then (g:(filterGives gs))
    else filterGives gs

filterRemovedItems :: [ItemRef] -> [ItemRef] -> [ItemRef]
filterRemovedItems [] _ = []
filterRemovedItems (i:is) rems =
  if remsContainInvItem i rems
    then filterRemovedItems is rems
    else (i:(filterRemovedItems is rems))

filterQuestGives :: [Give] -> [QuestRef]
filterQuestGives [] = []
filterQuestGives (g:gs) =
  if not $ checkGivesItem g
    then (g:(filterQuestGives gs))
    else filterQuestGives gs

filterByWhat :: [Use] -> What -> [Use]
filterByWhat [] _ = []
filterByWhat ((u@(Use _ useWhat _)):us) w =
  if useWhat == w
    then (u:(filterByWhat us w))
    else filterByWhat us w

filterOptItem :: [ItemRef] -> [Item] -> [Item]
filterOptItem _ [] = []
filterOptItem rs (i:[]) = if checkRefsContainItem i rs then (i:[]) else []

filterOptObject :: [ObjectRef] -> [Object] -> [Object]
filterOptObject _ [] = []
filterOptObject rs (o:[]) = if checkRefsContainObject o rs then (o:[]) else []

filterOptCharacter :: [CharacterRef] -> [Character] -> [Character]
filterOptCharacter _ [] = []
filterOptCharacter rs (c:[]) = if checkRefsContainCharacter c rs then (c:[]) else []

findCurrentRoom :: [Room] -> Position -> Room
findCurrentRoom (r:[]) _ = r
findCurrentRoom (r:rs) pos =
  if checkPosition r pos
    then r else findCurrentRoom rs pos

findApplicableDesc :: [Description] -> [FlagName] -> [Description]
findApplicableDesc [] _ = []
findApplicableDesc (d:ds) fs =
  if checkDescription d fs
    then (d:[]) else findApplicableDesc ds fs

findApplicableQuest :: [Quest] -> [QuestRef] -> [FlagName] -> [Quest]
findApplicableQuest _ [] _ = []
findApplicableQuest gqests (q:qs) fs =
  if checkQuest (findQuest gqests q) fs
    then ((findQuest gqests q):[]) else findApplicableQuest gqests qs fs

findApplicableUse :: [Use] -> [FlagName] -> [Use]
findApplicableUse [] _ = []
findApplicableUse (u:us) fs = if checkUse u fs
  then (u:[]) else findApplicableUse us fs

findApplicableInteract :: [Interact] -> [FlagName] -> [Interact]
findApplicableInteract [] _ = []
findApplicableInteract (i:is) fs = if checkInteract i fs
  then (i:[]) else findApplicableInteract is fs

findItem :: [Item] -> ItemRef -> Item
findItem (i:[]) _ = i
findItem (i:is) ref =
  if checkItemRef i ref
    then i else findItem is ref

findObject :: [Object] -> ObjectRef -> Object
findObject (o:[]) _ = o
findObject (o:os) ref = if checkObjectRef o ref
                      then o else findObject os ref

findCharacter :: [Character] -> CharacterRef -> Character
findCharacter (c:[]) _ = c
findCharacter (c:cs) ref = if checkCharacterRef c ref
                      then c else findCharacter cs ref

findMonster :: [Monster] -> MonsterRef -> Monster
findMonster (m:[]) _ = m
findMonster (m:ms) ref =
  if checkMonsterRef m ref
    then m else findMonster ms ref

findQuest :: [Quest] -> QuestRef -> Quest
findQuest (q:[]) _ = q
findQuest (q:qs) ref =
  if checkQuestRef q ref
    then q else findQuest qs ref

findItemByName :: [Item] -> Name -> [Item]
findItemByName [] _ = []
findItemByName (i:is) name =
  if checkItemName i name
    then (i:[]) else findItemByName is name

findObjectByName :: [Object] -> Name -> [Object]
findObjectByName [] _ = []
findObjectByName (o:os) name =
  if checkObjectName o name
    then (o:[]) else findObjectByName os name

findCharacterByName :: [Character] -> Name -> [Character]
findCharacterByName [] _ = []
findCharacterByName (c:cs) name =
  if checkCharacterName c name
    then (c:[]) else findCharacterByName cs name

findMonsterByName :: [Monster] -> Name -> [Monster]
findMonsterByName [] _ = []
findMonsterByName (m:ms) name =
  if checkMonsterName m name
    then (m:[]) else findMonsterByName ms name

findQuestByName :: [Quest] -> Name -> [Quest]
findQuestByName [] _ = []
findQuestByName (q:qs) name =
  if checkQuestName q name
    then (q:[]) else findQuestByName qs name
