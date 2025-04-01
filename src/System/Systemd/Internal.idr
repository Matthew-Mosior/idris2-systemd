module System.Systemd.Internal

import Data.String
import System
import System.FFI
import System.Posix.File
import System.Posix.Socket.Struct
import System.Posix.Socket.Types

--------------------------------------------------------------------------------
--          Raw Primitives
--------------------------------------------------------------------------------

export %foreign "C:sd_notify_with_fd,systemd-idris"
sd_notify_with_fd : Int -> String -> Bits64 -> AnyPtr -> Bits32 -> Int -> PrimIO Int

--------------------------------------------------------------------------------
--          Internal utilities
--------------------------------------------------------------------------------

||| Unset all enviornment variables related to Systemd.
||| Calls to functions like `System.Systemd.Daemon.notify` and
||| `System.Systemd.Daemon.getActivatedSockets` will return
||| Nothing after that.
private
unsetEnvironment : IO ()
unsetEnvironment = traverse_ unsetEnv
                             [ "NOTIFY_SOCKET"
                             , "LISTEN_PID"
                             , "LISTEN_FDS"
                             , "LISTEN_FDNAMES"
                             ]

sendBufWithFdTo : Socket AF_UNIX -> String -> SockaddrUn -> Fd -> IO Int
sendBufWithFdTo socket state socketaddress filedesc =
  primIO $
    sd_notify_with_fd (cast {to=Int} $ fd $ cast {to=Fd} socket)
                      state
                      (cast {to=Bits64} (strLength state))
                      (ptr AF_UNIX socketaddress)
                      (addrSize AF_UNIX)
                      (cast {to=Int} (fd filedesc))
