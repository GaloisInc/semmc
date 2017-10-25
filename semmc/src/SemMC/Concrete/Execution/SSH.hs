-- | An interface on top of the command line @ssh@ binary
--
-- Clients can connect to remote machines and execute commands.  The interface
-- allows clients to interact with the remote process through handles capturing
-- stdin, stdout, and stderr.
module SemMC.Concrete.Execution.SSH (
  defaultSSHConfig,
  SSHConfig(..),
  SSHHandle(..),
  ssh,
  killConnection,
  SSHError(..),
  SSHException(..)
  ) where

import qualified Control.Exception as E
import qualified System.IO.Error as E
import qualified System.Exit as E

import qualified Control.Concurrent.Async as A
import           Control.Monad
import           Data.Maybe ( fromMaybe )
import           Data.Word ( Word16 )
import qualified System.IO as IO
import qualified System.Process as P
import           Text.Printf ( printf )

data SSHConfig =
  SSHConfig { sshLoginName :: Maybe String
            , sshIdentity :: Maybe FilePath
            , sshConfigFile :: Maybe FilePath
            , sshUsername :: Maybe String
            , sshPort :: Maybe Word16
            , sshExecutablePath :: Maybe FilePath
            -- ^ An absolute path to the @ssh@ executable to run; default is to
            -- search PATH.
            , sshQuiet :: Bool
            -- ^ Suppress all SSH output; defaults to True to keep the handles clear
            }
  deriving (Eq, Ord, Show)

defaultSSHConfig :: SSHConfig
defaultSSHConfig =
  SSHConfig { sshLoginName = Nothing
            , sshIdentity = Nothing
            , sshConfigFile = Nothing
            , sshUsername = Nothing
            , sshPort = Nothing
            , sshExecutablePath = Nothing
            , sshQuiet = True
            }

data SSHHandle = SSHHandle { sshStdout :: IO.Handle
                           , sshStderr :: IO.Handle
                           , sshStdin :: IO.Handle
                           , sshProcHandle :: P.ProcessHandle
                           }
instance Show SSHHandle where
  show (SSHHandle {}) = "SSHHandle <handles omitted>"

data SSHError = SSHError E.IOError
  deriving (Eq, Show)

-- | Exception raised when SSH subcommand -- e.g. the @ls@ in @ssh ls@
-- -- fails at runtime. This exception is distinct from 'SSHError'
-- which is returned (not raised) when the SSH connection itself
-- fails. We use this exception because subcommand failure is
-- unpredictable can happen at any time, and we want to fail fast in
-- that case.
data SSHException = SSHException { command :: [String], exitCode :: E.ExitCode }
  deriving (Show)
instance E.Exception SSHException

-- | Run a command over SSH.
--
-- Returns an 'SSHError' if creating the SSH connection itself fails,
-- and raises 'SSHException' later if the command exits non-zero.
ssh :: SSHConfig
    -- ^ SSH configuration options
    -> String
    -- ^ Hostname to SSH to
    -> [String]
    -- ^ A command to run; this can be empty -- good luck
    -> IO (Either SSHError SSHHandle)
ssh cfg host command = do
  errOrHdl <- E.tryIOError $ do
    (Just hin, Just hout, Just herr, ph) <- P.createProcess p'
    return SSHHandle { sshStdout = hout
                     , sshStderr = herr
                     , sshStdin = hin
                     , sshProcHandle = ph
                     }
  case errOrHdl of
    Right hdl -> do
      forkExitFailureWatchDog (sshProcHandle hdl)
      return (Right hdl)
    Left ioerr -> return (Left (SSHError ioerr))
  where
    p = P.proc (fromMaybe "ssh" (sshExecutablePath cfg)) (makeCommandLine cfg host command)
    p' = p { P.std_in = P.CreatePipe
           , P.std_out = P.CreatePipe
           , P.std_err = P.CreatePipe
           , P.close_fds = True
           }
    -- Monitor the SSH process and raise an exception if it exits
    -- non-zero.
    forkExitFailureWatchDog ph = do
      exitFailureWatchDogThread <- A.async $ do
        exitCode <- P.waitForProcess ph
        when (exitCode /= E.ExitSuccess) $ do
          E.throwIO (SSHException command exitCode)
      A.link exitFailureWatchDogThread

makeCommandLine :: SSHConfig -> String -> [String] -> [String]
makeCommandLine cfg host args =
  concat [ params
         , [host']
         , if sshQuiet cfg then ["-q"] else []
         , args
         ]
  where
    host' = printf "%s%s" (maybe "" (++"@") (sshUsername cfg)) host
    params = foldr applyArg [] [ (sshLoginName cfg, "-l")
                               , (sshIdentity cfg, "-i")
                               , (sshConfigFile cfg, "-F")
                               , (fmap show (sshPort cfg), "-p")
                               ]

applyArg :: (Maybe String, String) -> [String] -> [String]
applyArg (ms, p) acc =
  case ms of
    Nothing -> acc
    Just a -> p : a : acc

-- | Terminate the SSH process
--
-- This is not guaranteed to work at all.
killConnection :: SSHHandle -> IO ()
killConnection hdl = P.terminateProcess (sshProcHandle hdl)
