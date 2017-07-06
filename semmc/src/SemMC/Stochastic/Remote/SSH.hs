module SemMC.Stochastic.Remote.SSH (
  defaultSSHConfig,
  SSHConfig(..),
  SSHHandle(..),
  ssh,
  SSHError(..)
  ) where

import Data.Maybe ( fromMaybe )
import Data.Word ( Word16 )
import qualified System.IO as IO
import qualified System.IO.Error as E
import qualified System.Process as P
import Text.Printf ( printf )

data SSHConfig =
  SSHConfig { sshLoginName :: Maybe String
            , sshIdentity :: Maybe FilePath
            , sshConfigFile :: Maybe FilePath
            , sshUsername :: Maybe String
            , sshPort :: Maybe Word16
            , sshExecutablePath :: Maybe FilePath
            -- ^ An absolute path to the @ssh@ executable to run; default is to
            -- search PATH.
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
            }

data SSHHandle = SSHHandle { sshStdout :: IO.Handle
                           , sshStderr :: IO.Handle
                           , sshStdin :: IO.Handle
                           , sshProcHandle :: P.ProcessHandle
                           }

data SSHError = SSHError E.IOError
  deriving (Eq, Show)

ssh :: SSHConfig
    -- ^ SSH configuration options
    -> String
    -- ^ Hostname to SSH to
    -> [String]
    -- ^ A command to run; this can be empty -- good luck
    -> IO (Either SSHError SSHHandle)
ssh cfg host command = do
  errOrHdl <- E.tryIOError $ do
    (Just hout, Just herr, Just hin, ph) <- P.createProcess p'
    return SSHHandle { sshStdout = hout
                     , sshStderr = herr
                     , sshStdin = hin
                     , sshProcHandle = ph
                     }
  case errOrHdl of
    Right hdl -> return (Right hdl)
    Left ioerr -> return (Left (SSHError ioerr))
  where
    p = P.proc (fromMaybe "ssh" (sshExecutablePath cfg)) (makeCommandLine cfg host command)
    p' = p { P.std_in = P.CreatePipe
           , P.std_out = P.CreatePipe
           , P.std_err = P.CreatePipe
           , P.close_fds = True
           }

makeCommandLine :: SSHConfig -> String -> [String] -> [String]
makeCommandLine cfg host args =
  (host' : params) ++ args
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

