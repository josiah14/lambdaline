module LambdaLine.System
( batteryPercentage
, batteryTimeLeft
-- , currentDate
, currentDirectory
, currentHostName
-- , currentTime
, currentUserName
) where
import LambdaLine.Segment
import LambdaLine.Shells.ShellSegment
import Data.Functor
-- import Control.Monad
-- import Data.Time.Clock
-- import Network.BSD
import System.Directory
-- import System.Posix.User

batteryPercentage :: ShellSegment String
batteryPercentage = undefined

batteryTimeLeft :: ShellSegment String
batteryTimeLeft = undefined

-- This function indicates whether the battery is full, charging, or
-- discharging
-- batteryStatus :: ShellSegment String
-- batteryStatus = undefined

currentDirectory :: ShellSegment String
currentDirectory = mkSegment $ Just <$> getCurrentDirectory

currentHostName :: ShellSegment String
currentHostName = undefined -- getHostName

-- first input is the format, second is the separator.
--
-- E.g. currentTime "DD-MM-YYYY" "/"
--      => 29/03/2014
--      currentTime "YYYY-MM-DD" "-"
--      => 2014-03-29
-- currentTime :: String -> String -> ShellSegment String
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


-- currentDate :: String -> ShellSegment String
-- currentDate format = undefined

currentUserName :: ShellSegment String
currentUserName = undefined -- getEffectiveUserName

-- ioToSegment :: IO String -> ShellSegment String
-- ioToSegment = liftM Just

