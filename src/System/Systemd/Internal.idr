module System.Systemd.Internal

import Control.Monad.Elin
import Data.List
import Data.String
import System
import System.FFI
import System.Posix.Errno
import System.Posix.File
import System.Posix.Socket

--------------------------------------------------------------------------------
--          Raw Primitives
--------------------------------------------------------------------------------

export %foreign "C:sd_notify_with_fd,systemd-idris"
sd_notify_with_fd : Int -> String -> Bits64 -> AnyPtr -> Bits32 -> Int -> PrimIO Int

--------------------------------------------------------------------------------
--          Internal utilities
--------------------------------------------------------------------------------

private
envvariablename : String
envvariablename = "NOTIFY_SOCKET"

||| Unset all enviornment variables related to Systemd.
||| Calls to functions like `System.Systemd.Daemon.notify` and
||| `System.Systemd.Daemon.getActivatedSockets` will return
||| Nothing after that.
private
unsetEnvironment : IO ()
unsetEnvironment = traverse_ unsetEnv
                             [ envvariablename
                             , "LISTEN_PID"
                             , "LISTEN_FDS"
                             , "LISTEN_FDNAMES"
                             ]

private
sendBufWithFdTo : Socket AF_UNIX -> String -> SockaddrUn -> Fd -> IO Int
sendBufWithFdTo socket state socketaddress filedesc =
  primIO $
    sd_notify_with_fd (cast {to=Int} $ fd $ cast {to=Fd} socket)
                      state
                      (cast {to=Bits64} (strLength state))
                      (ptr AF_UNIX socketaddress)
                      (addrSize AF_UNIX)
                      (cast {to=Int} (fd filedesc))

export
notifyWithFd_ : Bool -> String -> Maybe Fd -> IO ()
notifyWithFd_ unset_env state fd = do
  res <- runElinIO $
           notifyImpl state fd
  case res of
    Left _     =>
      pure ()
    Right res' => do
      when unset_env unsetEnvironment
      pure res'
  where
    isValidPath : String -> Bool
    isValidPath path =
      (length path >= 2) &&
      (isPrefixOf "@" path || isPrefixOf "/" path)
    notifyImpl : Has Errno es => String -> Maybe Fd -> Elin World es ()
    notifyImpl state fd = do
      let socketpath  =  case !(liftIO $ getEnv envvariablename) of
                           Nothing         =>
                             ""
                           Just socketpath =>
                             socketpath
      let socketpath' =  case fastUnpack socketpath of
                           Nil       =>
                             ""
                           (x :: xs) =>
                             case x == '@' of
                               True  =>
                                 fastPack $
                                   '\0' :: xs
                               False =>
                                 fastPack $
                                   (x :: xs)
      socketfd        <- socket AF_UNIX
                                SOCK_DGRAM
      srv <- runIO ( sockaddrUn socketpath'
                   )
      case fd of
         Nothing      =>
           ignore $ sendto socketfd
                  state
                  0
                  srv
         Just socket' => 
           liftIO ( sendBufWithFdTo socketfd
                                   state
                                   srv 
                                   socket'
                 )
