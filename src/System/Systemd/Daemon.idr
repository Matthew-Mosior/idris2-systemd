module System.Systemd.Daemon

import public System.Systemd.Daemon.Fd
import public System.Systemd.Internal

import Control.Monad.Elin
import Data.List1
import Data.String
import Data.Zippable
import System
import System.Posix.Errno
import System.Posix.File
import System.Posix.Process
import System.Posix.Socket

||| Notify systemd about an event
||| After notifying systemd the Bool parameter specify if the environment
||| shall be unset (Further call to notify will fail).
||| The String is the event to pass.
||| Returns Nothing if the program was not started with systemd
||| or the environment was previously unset.
export
notify : Bool -> String -> IO ()
notify unset_env state =
  notifyWithFd_ unset_env
                state
                Nothing

||| Notify the watchdog that the program is still alive.
notifyWatchdog :: IO ()
notifyWatchdog = notify False "WATCHDOG=1"

||| Notify the systemd that the program is ready.
notifyReady :: IO ()
notifyReady = notify False "READY=1"

||| Notify systemd of the PID of the program (for after a fork).
notifyPID :: CPid -> IO ()
notifyPID pid = notify False $ "MAINPID=" ++ show pid

||| Notify systemd that the service is reloading its configuration.
notifyReloading :: IO ()
notifyReloading = notify False "RELOADING=1"

||| Notify systemd that the service is beginning its shutdown.
notifyStopping :: IO (Maybe())
notifyStopping = notify False "STOPPING=1"

||| Notify systemd of an 'Errno' error.
notifyErrno :: Errno -> IO (Maybe())
notifyErrno (Errno errorNb) = notify False $ "ERRNO=" ++ show errorNb

||| Notify systemd of the status of the program.
||| An arbitrary String can be passed.
notifyStatus :: String -> IO (Maybe())
notifyStatus msg = notify False $ "STATUS=" ++ msg

||| Notify systemd of a DBUS error like.
||| Correct formatting of the String is left to the caller.
notifyBusError :: String -> IO (Maybe())
notifyBusError msg = notify False $ "BUSERROR=" ++ msg
