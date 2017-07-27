{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

module Options
    ( Settings (..)

    , rJournal
    , rCasSettings

    , cHosts
    , cPort
    , cKeyspace

    , settingsParser

    )
where

import qualified Cassandra as C
import           Control.Lens
import           Data.Monoid
import           Data.Text.Strict.Lens
import           Data.Word
import           Options.Applicative

data Settings = Settings
    { _rJournal     :: Bool
    , _rCasSettings :: CassandraSettings
    }

data CassandraSettings = CassandraSettings
    { _cHosts    :: String
    , _cPort     :: Word16
    , _cKeyspace :: C.Keyspace
    } deriving Show

makeLenses ''Settings
makeLenses ''CassandraSettings

settingsParser :: Parser Settings
settingsParser = Settings
    <$> switch
        ( long "journal"
        <> help "Execute journal of teams (By default, it only prints teamIds)")
    <*> cassandraSettingsParser

cassandraSettingsParser :: Parser CassandraSettings
cassandraSettingsParser = CassandraSettings
    <$> strOption
        ( long    "cassandra-host"
       <> metavar "HOST"
       <> help    "Cassandra Host."
       <> value   "localhost"
       <> showDefault
        )

    <*> option auto
        ( long    "cassandra-port"
       <> metavar "PORT"
       <> help    "Cassandra Port."
       <> value   9042
       <> showDefault
        )
    <*> ( C.Keyspace . view packed <$>
          strOption
          ( long    "cassandra-keyspace"
         <> metavar "STRING"
         <> help    "Cassandra Keyspace."
         <> value   "galley_test"
         <> showDefault
          )
        )
