{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Data.Monoid
import           Journal
import           OpenSSL (withOpenSSL)
import           Options
import           Options.Applicative
import qualified System.Logger as Log


main :: IO ()
main = withOpenSSL $ do
    cmd <- execParser (info (helper <*> settingsParser) desc)
    lgr <- initLogger
    runCommand lgr cmd
  where
    desc = header   "team-journaler"
        <> progDesc "Team event journaler"
        <> fullDesc

    initLogger
        = Log.new
        . Log.setOutput Log.StdOut
        . Log.setFormat Nothing
        . Log.setBufSize 0
        $ Log.defSettings
