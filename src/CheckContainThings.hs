module CheckContainThings where

import GameDefinitions

checkPosition :: Room -> Position -> Bool
checkPosition (Room (Position rx ry) _ _ _ _ _ _ _ _ _ _ _ _ _)
              (Position px py) = (rx == px && ry == py)

checkSingleFlag :: Flag -> [FlagName] -> Bool
checkSingleFlag (Flag neg name) fs =
  if setFlagsContainFlag name fs
    then not neg else neg

checkReqFlags :: [Flag] -> [FlagName] -> Bool
checkReqFlags [] _ = True
checkReqFlags (reqFlag:reqFlags) fs =
  if (checkSingleFlag reqFlag fs) == False
    then False else checkReqFlags reqFlags fs

checkRequires :: Requires -> [FlagName] -> Bool
checkRequires (Requires reqFlags) fs = checkReqFlags reqFlags fs

checkOptRequires :: [Requires] -> [FlagName] -> Bool
checkOptRequires [] _ = True
checkOptRequires (r:[]) fs = checkRequires r fs

checkDescription :: Description -> [FlagName] -> Bool
checkDescription (Description req _) fs = checkOptRequires req fs

checkItemRef :: Item -> ItemRef -> Bool
checkItemRef (Item id _ _ _ _ _ _ _) ref = (id == ref)

checkObjectRef :: Object -> ObjectRef -> Bool
checkObjectRef (Object id _ _ _ _) ref = (id == ref)

checkCharacterRef :: Character -> CharacterRef -> Bool
checkCharacterRef (Character id _ _ _) ref = (id == ref)

checkMonsterRef :: Monster -> MonsterRef -> Bool
checkMonsterRef (Monster id _ _ _ _ _ _) ref = (id == ref)

checkItemName :: Item -> Name -> Bool
checkItemName (Item _ n _ _ _ _ _ _) name = (n == name)

checkObjectName :: Object -> Name -> Bool
checkObjectName (Object _ n _ _ _) name = (n == name)

checkCharacterName :: Character -> Name -> Bool
checkCharacterName (Character _ n _ _) name = (n == name)

checkMonsterName :: Monster -> Name -> Bool
checkMonsterName (Monster _ n _ _ _ _ _) name = (n == name)

checkQuestRef :: Quest -> QuestRef -> Bool
checkQuestRef (Quest id _ _ _) ref = (id == ref)

checkQuestName :: Quest -> Name -> Bool
checkQuestName (Quest _ n _ _) name = (n == name)

checkGivesItem :: Give -> Bool
checkGivesItem ('i':'t':'e':'m':'_':gs) = True
checkGivesItem _ = False

checkQuest :: Quest -> [FlagName] -> Bool
checkQuest (Quest _ _ _ (Completion req _)) fs = checkRequires req fs

checkUse :: Use -> [FlagName] -> Bool
checkUse (Use req _ _) fs = checkOptRequires req fs

checkInteract :: Interact -> [FlagName] -> Bool
checkInteract (Interact req _) fs = checkOptRequires req fs

canAttackFoundMonster :: [Monster] -> [MonsterRef] -> Bool
canAttackFoundMonster [] _ = False
canAttackFoundMonster (m:[]) rs = checkRefsContainMonster m rs

setFlagsContainFlag :: FlagName -> [FlagName] -> Bool
setFlagsContainFlag _ [] = False
setFlagsContainFlag name (f:fs) =
  if name == f
    then True else setFlagsContainFlag name fs

pickedUpsContainRoomRef :: ItemRef -> [ItemRef] -> Bool
pickedUpsContainRoomRef _ [] = False
pickedUpsContainRoomRef roomRef (p:ps) =
  if roomRef == p
    then True else pickedUpsContainRoomRef roomRef ps

checkRefsContainItem :: Item -> [ItemRef] -> Bool
checkRefsContainItem _ [] = False
checkRefsContainItem item@(Item id _ _ _ _ _ _ _) (r:rs) =
  if id == r then True else checkRefsContainItem item rs

checkRefsContainOptItem :: [Item] -> [ItemRef] -> Bool
checkRefsContainOptItem [] _ = False
checkRefsContainOptItem (i:[]) rs = checkRefsContainItem i rs

checkRefsContainObject :: Object -> [ObjectRef] -> Bool
checkRefsContainObject _ [] = False
checkRefsContainObject object@(Object id _ _ _ _) (r:rs) =
  if id == r then True else checkRefsContainObject object rs

checkRefsContainCharacter :: Character -> [CharacterRef] -> Bool
checkRefsContainCharacter _ [] = False
checkRefsContainCharacter character@(Character id _ _ _) (r:rs) =
  if id == r then True else checkRefsContainCharacter character rs

checkRefsContainMonster :: Monster -> [MonsterRef] -> Bool
checkRefsContainMonster _ [] = False
checkRefsContainMonster monster@(Monster id _ _ _ _ _ _) (r:rs) =
  if id == r then True else checkRefsContainMonster monster rs

checkRefsContainQuest :: Quest -> [QuestRef] -> Bool
checkRefsContainQuest _ [] = False
checkRefsContainQuest quest@(Quest id _ _ _) (r:rs) =
  if id == r then True else checkRefsContainQuest quest rs

remsContainInvItem :: ItemRef -> [ItemRef] -> Bool
remsContainInvItem _ [] = False
remsContainInvItem invItem (r:rs) =
  if invItem == r
    then True else remsContainInvItem invItem rs
