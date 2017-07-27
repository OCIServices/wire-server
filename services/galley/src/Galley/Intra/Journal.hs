{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Galley.Intra.Journal
    ( teamCreate
    , teamUpdate
    , teamDelete
    , bytes
    , evData
    , now
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

teamCreate :: TeamId -> UserId -> Galley ()
teamCreate t u = journal (t, u) teamCreate'
  where
    teamCreate' :: (TeamId, UserId) -> IO TeamEvent
    teamCreate' (tid, uid) = TeamEvent TeamEvent'TEAM_CREATE (bytes tid) <$> now <*> pure (Just (evData 1 [uid]))

teamUpdate :: TeamId -> [TeamMember] -> Galley ()
teamUpdate t m = journal (t, m) teamUpdate'
  where
    teamUpdate' :: (TeamId, [TeamMember]) -> IO TeamEvent
    teamUpdate' (tid, mems) = do
        let bUsers = view userId <$> filter (`hasPermission` SetBilling) mems
        let eData = evData (fromIntegral $ length mems) bUsers
        TeamEvent TeamEvent'TEAM_UPDATE (bytes tid) <$> now <*> pure (Just eData)

teamDelete :: TeamId -> Galley ()
teamDelete t = journal t teamDelete'
  where
    teamDelete' :: TeamId -> IO TeamEvent
    teamDelete' tid = TeamEvent TeamEvent'TEAM_DELETE (bytes tid) <$> now <*> pure Nothing

----------------------------------------------------------------------------
-- utils

journal :: a -> (a -> IO TeamEvent) -> Galley ()
journal a f = do
    mEnv <- view aEnv
    for_ mEnv $ \e -> do
        event <- liftIO $ f a
        Aws.execute e (Aws.enqueue event)

bytes :: Id a -> ByteString
bytes = toStrict . UUID.toByteString . toUUID

evData :: Int32 -> [UserId] -> TeamEvent'EventData
evData c uids = TeamEvent'EventData c (bytes <$> uids)

now :: MonadIO m => m Int64
now = liftIO $ round . utcTimeToPOSIXSeconds <$> getCurrentTime
