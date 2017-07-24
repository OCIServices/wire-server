{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Galley.Types.TeamEvents
    ( teamCreate
    , teamUpdate
    , teamDelete
    ) where

import Control.Monad.IO.Class
import Data.ByteString.Conversion hiding (fromList)
import Data.Int
import Data.Id
import Data.Time.Clock (getCurrentTime)
import Data.Time.Zones.Internal
import Prelude hiding (head, mapM)

import Proto.Galley.Types.TeamEvents

teamCreate :: TeamId -> UserId -> IO TeamEvent
teamCreate tid uid = do
    now <- utcTimeToInt64 <$> liftIO getCurrentTime
    return $ TeamEvent TeamEvent'TEAM_CREATE (toByteString' tid) now (Just (evData 1 [uid]))

teamUpdate :: TeamId -> Int32 -> [UserId] -> IO TeamEvent
teamUpdate tid count uids = do
    now <- utcTimeToInt64 <$> liftIO getCurrentTime
    return $ TeamEvent TeamEvent'TEAM_UPDATE (toByteString' tid) now (Just (evData count uids))

teamDelete :: TeamId -> IO TeamEvent
teamDelete tid = do
    now <- utcTimeToInt64 <$> liftIO getCurrentTime
    return $ TeamEvent TeamEvent'TEAM_DELETE (toByteString' tid) now Nothing

----------------------------------------------------------------------------
-- utils

evData :: Int32 -> [UserId] -> TeamEvent'EventData
evData c uids = TeamEvent'EventData c (toByteString' <$> uids)
