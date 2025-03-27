module System.Systemd.Daemon.Internal

import System.FFI
import System.Posix.File

--------------------------------------------------------------------------------
--          Raw Primitives
--------------------------------------------------------------------------------

export %foreign "C:sd_notify_with_fd,systemd-idris"
sd_notify_with_fd : Bits32

||| Unset all enviornment variables related to Systemd.
||| Calls to functions like `System.Systemd.Daemon.notify` and
||| `System.Systemd.Daemon.getActivatedSockets` will return
||| Nothing after that.
