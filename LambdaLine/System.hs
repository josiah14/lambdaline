module LambdaLine.System
( batteryPercentage
, batteryTimeLeft
-- , currentDate
, currentDirectory
, currentHostName
-- , currentTime
, currentUserName
) where
import LambdaLine.PromptSegment
import LambdaLine.Shells.ShellPromptSegment
import Data.Functor
-- import Control.Monad
-- import Data.Time.Clock
-- import Network.BSD
import System.Directory
-- import System.Posix.User

batteryPercentage :: ShellPromptSegment String
batteryPercentage = undefined

batteryTimeLeft :: ShellPromptSegment String
batteryTimeLeft = undefined

-- This function indicates whether the battery is full, charging, or
-- discharging
-- batteryStatus :: ShellPromptSegment String
-- batteryStatus = undefined

currentDirectory :: ShellPromptSegment String
currentDirectory = convertToPromptSegment $ Just <$> getCurrentDirectory

currentHostName :: ShellPromptSegment String
currentHostName = undefined -- getHostName

-- first input is the format, second is the separator.
--
-- E.g. currentTime "DD-MM-YYYY" "/"
--      => 29/03/2014
--      currentTime "YYYY-MM-DD" "-"
--      => 2014-03-29
-- currentTime :: String -> String -> ShellPromptSegment String
-- currentTime format separator =
--   case format
--     of "YYYY-MM-DD" -> undefined
--        "DD-MM-YYYY" -> undefined
--        "MM-DD-YYYY" -> undefined
--        "YYYY-MM-DD" -> undefined
--        "DD-MM-YY"   -> undefined
--        "MM-DD-YY"   -> undefined
--        "YY-MM-DD"   -> undefined
--        "YY-DD-MM"   -> undefined


-- currentDate :: String -> ShellPromptSegment String
-- currentDate format = undefined

currentUserName :: ShellPromptSegment String
currentUserName = undefined -- getEffectiveUserName

-- ioToPromptSegment :: IO String -> ShellPromptSegment String
-- ioToPromptSegment = liftM Just

