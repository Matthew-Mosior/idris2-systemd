module System.Systemd.Daemon

import public System.Systemd.Daemon.Fd
import public System.Systemd.Internal

import System
import System.Posix.Errno
import System.Posix.File
import System.Posix.Socket

||| Notify systemd about an event
||| After notifying systemd the Bool parameter specify if the environment
||| shall be unset (Further call to notify will fail).
||| The String is the event to pass.
||| Returns Nothing if the program was not started with systemd
||| or the environment was previously unset.
export
notify : Bool -> String -> IO (Maybe ())
notify unset_env state =
  notifyWithFd_ unset_env
                state
                Nothing

||| Same as `notify` but send along a socket to be stored.
||| It is up to the caller to properly set the message
||| (i.e: do not forget to set FDSTORE=1).
export
notifyWithFD : Bool -> String -> Socket AF_UNIX -> IO (Maybe ())
notifyWithFD unset_env state sock =
  notifyWithFd unset_env
               state
               (cast {to=Fd} sock)

||| Notify the watchdog that the program is still alive.
export
notifyWatchdog : IO (Maybe ())
notifyWatchdog =
  notify False
         "WATCHDOG=1"

||| Notify the systemd that the program is ready.
export
notifyReady : IO (Maybe ())
notifyReady =
  notify False
         "READY=1"

||| Notify systemd of the PID of the program (for after a fork).
export
notifyPID : Int -> IO (Maybe ())
notifyPID pid =
  notify False
         ("MAINPID=" ++ show pid)

||| Notify systemd that the service is reloading its configuration.
export
notifyReloading : IO (Maybe ())
notifyReloading =
  notify False
         "RELOADING=1"

||| Notify systemd that the service is beginning its shutdown.
export
notifyStopping : IO (Maybe())
notifyStopping =
  notify False
         "STOPPING=1"

||| Notify systemd of an 'Errno' error.
export
notifyErrno : Errno -> IO (Maybe())
notifyErrno (EN errorNb) =
  notify False
         ("ERRNO=" ++ show errorNb)

||| Notify systemd of the status of the program.
||| An arbitrary String can be passed.
export
notifyStatus : String -> IO (Maybe())
notifyStatus msg =
  notify False
         ("STATUS=" ++ msg)

||| Notify systemd of a DBUS error like.
||| Correct formatting of the String is left to the caller.
export
notifyBusError : String -> IO (Maybe())
notifyBusError msg =
  notify False
         ("BUSERROR=" ++ msg)

||| Notify systemd to store a socket for us.
||| To be used along `getActivatedSockets` during a restart.
||| Useful for zero downtime restart.
export
storeFd : Socket AF_UNIX -> IO (Maybe ())
storeFd sock =
  storeFd (cast {to=Fd} sock)

||| Notify systemd to store a socket for us and specify a name.
||| To be used along `getActivatedSocketsWithNames` during a restart.
||| Useful for zero downtime restart.
export
storeFdWithName : Socket AF_UNIX -> String -> IO (Maybe ())
storeFdWithName sock name = 
  storeFdWithName (cast {to=Fd} sock)
                  name

||| Return a list of activated sockets, if the program was started with
||| socket activation.
||| The sockets are in the same order as in the associated .socket file.
||| The sockets will have their family, type, and status set appropriately.
||| Returns Nothing in systems without socket activation (or
||| when the program was not socket activated).
export
getActivatedSockets : IO (Maybe (List (Socket AF_UNIX)))
getActivatedSockets =
  case !System.Systemd.Daemon.Fd.getActivatedSockets of
    Nothing  =>
      pure Nothing
    Just fds =>
      let fds' = map (\(MkFd fd) =>
                        cast {to=Socket AF_UNIX} $
                          cast {to=Int32} fd
                     ) fds
        in pure $
             Just fds'

||| Same as `getActivatedSockets` but return also the names associated
||| with those sockets if `storeFdWithName` was used or specified in the .socket file.
||| If `storeFd` was used to transmit the socket to systemd, the name will be a generic one
||| (i.e: usally "stored").
export
getActivatedSocketsWithNames : IO (Maybe (List (Socket AF_UNIX, String)))
getActivatedSocketsWithNames =
  case !System.Systemd.Daemon.Fd.getActivatedSocketsWithNames of
    Nothing          =>
      pure Nothing
    Just fdsandnames =>
      let fdsandnames' =map (\(MkFd fd, name) =>
                               let sock = cast {to=Socket AF_UNIX} $
                                            cast {to=Int32} fd
                                 in (sock, name)
                            ) fdsandnames
        in pure $
             Just fdsandnames'
