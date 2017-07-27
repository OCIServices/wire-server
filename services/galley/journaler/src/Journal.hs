{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}

module Journal where

import Cassandra as C
import Cassandra.Settings as C
import Control.Concurrent.Async.Lifted.Safe (mapConcurrently)
import Control.Lens
import Control.Monad.Except
import Data.Id
import Options as O
import System.Logger.Class (Logger)
import Proto.Galley.Intra.TeamEvents
import Galley.Types.Teams

import qualified System.Logger          as Log
import qualified Galley.Data            as Data
import qualified Galley.Intra.Journal   as Journal
import qualified Galley.Aws             as Aws


runCommand :: Logger -> O.Settings -> IO ()
runCommand l s = do
    c <- initCas (s^.rCasSettings)

    void $ C.runClient c $ do
        tids <- teamSelect
        forM tids (journal c)

  where
    journal c tid = C.runClient c $ do
        mems <- Data.teamMembers tid
        -- TODO: Aws.Env
        -- TODO: pagination
        -- TODO: actual journaling
        liftIO $ print tid
        --journalTeamCreate env tid mems

    initCas cas
        = C.init l
        . C.setContacts        (cas^.cHosts) []
        . C.setPortNumber      (fromIntegral $ cas^.cPort)
        . C.setKeyspace        (cas^.cKeyspace)
        . C.setProtocolVersion C.V3
        $ C.defSettings

-- Cassandra
teamSelect :: MonadClient m => m [TeamId]
teamSelect = do
    r <- retry x1 (C.query teamSelect' (params One ()))
    return $ fst <$> filter snd r

-- CQL queries
teamSelect' :: PrepQuery R () (TeamId, Bool)
teamSelect' = "SELECT team, binding FROM team limit 100"

-- Journaling
journalTeamCreate :: Aws.Env -> TeamId -> [TeamMember] -> IO ()
journalTeamCreate env tid mems = do
    let bUsers = view userId <$> filter (`hasPermission` SetBilling) mems
    let eData = Journal.evData (fromIntegral $ length mems) bUsers
    event <- TeamEvent TeamEvent'TEAM_CREATE (Journal.bytes tid) <$> Journal.now <*> pure (Just eData)
    Aws.execute env (Aws.enqueue event)


