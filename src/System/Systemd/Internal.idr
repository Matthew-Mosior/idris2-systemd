module System.Systemd.Internal

import System
import System.FFI
import System.Posix.File
import System.Posix.Socket.Struct

--------------------------------------------------------------------------------
--          Raw Primitives
--------------------------------------------------------------------------------

export %foreign "C:sd_notify_with_fd,systemd-idris"
sd_notify_with_fd : Int -> String -> Bits64 -> Ptr SockaddrUn -> Bits32 -> Int -> PrimIO Int

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


