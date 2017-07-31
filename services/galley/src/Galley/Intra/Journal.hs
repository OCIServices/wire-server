{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Galley.Intra.Journal
    ( journal
    , JournaledEvent (..)
    ) where

import Control.Lens
import Control.Monad.IO.Class
import Data.Int
import Data.Foldable (for_)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import Data.Id
import Galley.Types.Teams
import Data.Time.Clock (getCurrentTime)
import Data.Time.Clock.POSIX
import Galley.App
import Prelude hiding (head, mapM)
import Proto.Galley.Intra.TeamEvents

import qualified Data.UUID as UUID
import qualified Galley.Aws as Aws

-- [Note: journaling]
-- Team journal operations to SQS are a no-op when the service
-- is started without journaling arguments

class ToJournal a where
    toJournal :: Int64 -> a -> TeamEvent

data JournaledEvent = JournaledTeamCreate TeamId UserId
                    | JournaledTeamUpdate TeamId [TeamMember]
                    | JournaledTeamDelete TeamId

----------------------------------------------------------------------------
journal :: JournaledEvent -> Galley ()
journal ev = do
    mEnv <- view aEnv
    for_ mEnv $ \e -> do
        t <- now
        Aws.execute e $ Aws.enqueue $ journalEvent ev t
  where
    journalEvent (JournaledTeamCreate tid uid) tim =
        TeamEvent TeamEvent'TEAM_CREATE (bytes tid) tim (Just (evData 1 [uid]))
    journalEvent (JournaledTeamUpdate tid mems) tim = do
        let bUsers = view userId <$> filter (`hasPermission` SetBilling) mems
        let eData = evData (fromIntegral $ length mems) bUsers
        TeamEvent TeamEvent'TEAM_UPDATE (bytes tid) tim (Just eData)
    journalEvent (JournaledTeamDelete tid) tim =
        TeamEvent TeamEvent'TEAM_DELETE (bytes tid) tim Nothing

-- OR --

instance ToJournal JournaledEvent where
    toJournal tim (JournaledTeamCreate tid uid) =
        TeamEvent TeamEvent'TEAM_CREATE (bytes tid) tim (Just (evData 1 [uid]))
    toJournal tim (JournaledTeamUpdate tid mems) = do
        let bUsers = view userId <$> filter (`hasPermission` SetBilling) mems
        let eData = evData (fromIntegral $ length mems) bUsers
        TeamEvent TeamEvent'TEAM_UPDATE (bytes tid) tim (Just eData)
    toJournal tim (JournaledTeamDelete tid) =
        TeamEvent TeamEvent'TEAM_DELETE (bytes tid) tim Nothing

journal' :: (ToJournal a) => a -> Galley ()
journal' ev = do
    mEnv <- view aEnv
    for_ mEnv $ \e -> do
        t <- now
        Aws.execute e $ Aws.enqueue $ toJournal t ev

bytes :: Id a -> ByteString
bytes = toStrict . UUID.toByteString . toUUID

evData :: Int32 -> [UserId] -> TeamEvent'EventData
evData c uids = TeamEvent'EventData c (bytes <$> uids)

now :: MonadIO m => m Int64
now = liftIO $ round . utcTimeToPOSIXSeconds <$> getCurrentTime
