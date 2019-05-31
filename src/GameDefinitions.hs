module GameDefinitions where

type XPos = Integer
type YPos = Integer

data Position = Position XPos YPos deriving (Show)

type Negated = Bool
type FlagName = String

data Flag = Flag Negated FlagName deriving (Show)

data Requires = Requires [Flag] deriving (Show)

type DescText = String

-- a description can have no requirement (usually the last, default one)
data Description = Description [Requires] DescText deriving (Show)

-- a win condition must have a requirement or it would trigger on starting the game
data WinCondition = WinCondition Requires Description deriving (Show)

type ItemRef = String
type ObjectRef = String
type CharacterRef = String
type MonsterRef = String

-- again, doesn't have to have a requirement
data GoNorth = GoNorth [Requires] Description deriving (Show)
data GoEast = GoEast [Requires] Description deriving (Show)
data GoSouth = GoSouth [Requires] Description deriving (Show)
data GoWest = GoWest [Requires] Description deriving (Show)

-- just descriptions for looking in a direction
data North = North [Description] deriving (Show)
data East = East [Description] deriving (Show)
data South = South [Description] deriving (Show)
data West = West [Description] deriving (Show)

-- going or looking in a direction is optional
data Room = Room Position [Description]
  [ItemRef] [ObjectRef] [CharacterRef] [MonsterRef]
  [GoNorth] [GoEast] [GoSouth] [GoWest]
  [North] [East] [South] [West]
  deriving (Show)

type Name = String

-- what item from inventory must be used on this item/object (reference string)
type What = String

-- adds an entry (if it doesn't exist already) to the player's set flag list
type SetFlag = String

-- adds or removes an item reference string to/from the player's inventory
-- (watch out for equipped items!)
-- give can also be used for giving quests
type Give = String
type Remove = String

-- just some stat rewards for completing quests or consuming items
type IncreaseHealth = Integer
type IncreaseMaxHP = Integer
type IncreaseBaseDamage = Integer
type IncreaseBaseDefence = Integer

data Result = Result [SetFlag] [Give] [Remove]
  [IncreaseHealth] [IncreaseMaxHP]
  [IncreaseBaseDamage] [IncreaseBaseDefence]
  Description
  deriving (Show)

-- optional requirements allow multiple use tags (usually one correct one,
-- and a few that just describe failure, give hints, etc.)
data Use = Use [Requires] What Result deriving (Show)

-- adds the item to the player's inventory, also to the list of picked up items
-- (so it can't be picked up again)
data PickUp = PickUp [SetFlag] Description deriving (Show)

-- does things with an item/object on its own (press button, drink potion)
-- also used for talking with characters
data Interact = Interact [Requires] Result deriving (Show)

type Damage = Integer
type Defence = Integer

-- an item doesn't need a pick up tag, it can just be received from a result
-- also, some items may have interact tags (like potions)
-- if an item has a damage or defence tag (never both at the same time),
-- it can be equipped
data Item = Item ItemRef Name [Description]
  [Use] [PickUp] [Interact] [Damage] [Defence]
  deriving (Show)

-- very similar to item, just can't be picked up
-- (which means that it also can't be equipped)
data Object = Object ObjectRef Name [Description]
  [Use] [Interact]
  deriving (Show)

-- pretty much the same thing as an object, just a bit more limited
-- (can't use items on characters) and used for giving quests
data Character = Character CharacterRef Name [Description]
  [Interact]
  deriving (Show)

type QuestRef = String

-- on passing the requirements removes from questlog and executes result
data Completion = Completion Requires Result deriving (Show)

data Quest = Quest QuestRef Name [Description] Completion deriving (Show)

type Health = Integer

-- execute the result when the monster's health falls below 0
-- (and reenable going in a direction, and stop attacking the player)
data Kill = Kill Result deriving (Show)

-- attacking a monster disables going in a direction until it dies
-- and the player is attacked after every taken action
data Monster = Monster MonsterRef Name [Description]
  Health Damage Defence Kill
  deriving (Show)

-- you load this in from a file at launch, but afterwards nothing is changed here
-- all changes happen in the PlayerData object, where they are stored,
-- loaded to and saved from
data GameData = GameData Position Description WinCondition
  [Room] [Item] [Object] [Character] [Quest] [Monster]
  deriving (Show)

type WeaponRef = String
type ArmorRef = String

type MaxHP = Integer
type BaseDamage = Integer
type BaseDefence = Integer

type PickedUpRef = String

data PlayerData = PlayerData Position [ItemRef] [WeaponRef] [ArmorRef]
  Health MaxHP BaseDamage BaseDefence
  [PickedUpRef] [QuestRef] [FlagName]
  deriving (Show)

type InCombat = Bool
type CombatHealth = Integer
type CombatRef = String

data Game = Game GameData PlayerData
  InCombat CombatHealth CombatRef deriving (Show)
