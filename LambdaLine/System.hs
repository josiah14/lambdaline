module LambdaLine.System
( batteryPercentage
, batteryTimeLeft
, currentDate
, currentDirectory
, currentHostName
, currentTime
, currentUserName
) where
import LambdaLine.PromptSegment (PromptSegment)
import Control.Monad
import Data.Time.Clock
import Network.BSD
import System.Directory
import System.Posix.User

batteryPercentage :: PromptSegment
batteryPercentage = undefined

batteryTimeLeft :: PromptSegment
batteryTimeLeft = undefined

-- This function indicates whether the battery is full, charging, or
-- discharging
batteryStatus :: PromptSegment
batteryStatus = undefined

currentDirectory :: PromptSegment
currentDirectory = ioToPromptSegment getCurrentDirectory

currentHostName :: PromptSegment
currentHostName = ioToPromptSegment getHostName

-- first input is the format, second is the separator.
--
-- E.g. currentTime "DD-MM-YYYY" "/"
--      => 29/03/2014
--      currentTime "YYYY-MM-DD" "-"
--      => 2014-03-29
currentTime :: String -> String -> PromptSegment
currentTime format separator =
  case format
    of "YYYY-MM-DD" -> undefined
       "DD-MM-YYYY" -> undefined
       "MM-DD-YYYY" -> undefined
       "YYYY-MM-DD" -> undefined
       "DD-MM-YY"   -> undefined
       "MM-DD-YY"   -> undefined
       "YY-MM-DD"   -> undefined
       "YY-DD-MM"   -> undefined


currentDate :: String -> PromptSegment
currentDate format = undefined

currentUserName :: PromptSegment
currentUserName = ioToPromptSegment getEffectiveUserName

ioToPromptSegment :: IO String -> PromptSegment
ioToPromptSegment = liftM Just

