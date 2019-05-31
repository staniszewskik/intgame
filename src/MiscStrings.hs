module MiscStrings where

import GameDefinitions

descriptionTest1 :: String
descriptionTest1 = "<description>\n<text>test desc text!</text>\n</description>"

descriptionTest2 :: String
descriptionTest2 = "<description>\n<requires>questComplete</requires>\n<text>test</text>\n</description>"

winConditionTest :: String
winConditionTest =
  "<winCondition>\n" ++
  "    <requires>questComplete</requires>\n" ++
  "    <description>\n" ++
  "        <text>Good job me!</text>\n" ++
  "    </description>\n" ++
  "</winCondition>"

cleanSave :: String
cleanSave =
  "<player>\n" ++
  "    <position>0,0</position>\n" ++
  "    <inventory></inventory>\n" ++
  "    <health>10</health>\n" ++
  "    <maxHP>10</maxHP>\n" ++
  "    <baseDamage>1</baseDamage>\n" ++
  "    <baseDefence>1</baseDefence>\n" ++
  "    <pickedUp></pickedUp>\n" ++
  "    <questlog></questlog>\n" ++
  "    <setFlags></setFlags>\n" ++
  "</player>"

lookDirFailMsg :: String
lookDirFailMsg = "There's nothing of interest there"
goDirFailMsg :: String
goDirFailMsg = "You can't go there"
combatActionFailMsg :: String
combatActionFailMsg = "You can't do this during combat"
combatDeathMsg :: String
combatDeathMsg = "You fall dead to the ground"

attackMessage :: Name -> Damage -> String
attackMessage name 0 = "You attack \"" ++ name ++
                        "\" but deal no damage"
attackMessage name dealtDamage = "You attack \"" ++ name ++
                                  "\" and deal " ++ (show dealtDamage) ++
                                  " points of damage"

attackedMessage :: Name -> Damage -> String
attackedMessage combatName 0 = "You are attacked by \"" ++ combatName ++
                                "\" but receive no damage"
attackedMessage combatName recDamage = "You are attacked by \"" ++ combatName ++
                                        "\" and receive " ++ (show recDamage) ++
                                        " points of damage"
