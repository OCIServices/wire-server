{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Galley.Types.TeamEvents
    ( teamCreate
    , teamUpdate
    , teamDelete
    ) where

import Control.Lens
import Data.ByteString.Conversion hiding (fromList)
import Data.Int
import Data.Id
import Galley.Types.Teams
import Data.Time.Clock (getCurrentTime)
import Data.Time.Clock.POSIX
import Prelude hiding (head, mapM)

import Proto.Galley.Types.TeamEvents

teamCreate :: TeamId -> UserId -> IO TeamEvent
teamCreate tid uid = TeamEvent
    TeamEvent'TEAM_CREATE (toByteString' tid) <$> now <*> return (Just (evData 1 [uid]))

teamUpdate :: TeamId -> [TeamMember] -> IO TeamEvent
teamUpdate tid membs = do
    n <- now
    let bUsers = view userId <$> filter (`hasPermission` SetBilling) membs
    let eData = evData (fromIntegral $ length membs) bUsers
    return $ TeamEvent TeamEvent'TEAM_UPDATE (toByteString' tid) n (Just eData)

teamDelete :: TeamId -> IO TeamEvent
teamDelete tid = TeamEvent
    TeamEvent'TEAM_DELETE (toByteString' tid) <$> now <*> return Nothing

----------------------------------------------------------------------------
-- utils

evData :: Int32 -> [UserId] -> TeamEvent'EventData
evData c uids = TeamEvent'EventData c (toByteString' <$> uids)

now :: IO Int64
now = round . utcTimeToPOSIXSeconds <$> getCurrentTime
