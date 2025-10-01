module System.Systemd.Daemon.Fd

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

private
fdstart : Int
fdstart = 3

||| Same as `System.Systemd.Daemon.notify`, but send along an `Fd`.
||| Note that the caller must set the message, i. e. send @FDSTORE=1@
||| to actually store the file descriptor. In most cases it is probably best
||| to use 'storeFd' or the notify-functions from `System.Systemd.Daemon`.
||| Equivalent to standard `System.Systemd.Daemon.notifyWithFD`.
export
notifyWithFd :  Bool
             -> String
             -> Fd
             -> IO (Maybe ())
notifyWithFd unset_env state sock =
  notifyWithFd_ unset_env
                state
                (Just sock)

||| Notify Systemd to store a file descriptor for us. This together
||| with `getActivatedSockets` allows for zero downtime
||| restarts and socket activation.
||| Equivalent to standard `System.Systemd.Daemon.storeFd`.
export
storeFd :  Fd
        -> IO (Maybe ())
storeFd sock =
  notifyWithFd False
               "FDSTORE=1"
               sock

||| Like `storeFd`, but associate the file descriptor with a name.
||| Best used along with `getActivatedSocketsWithNames`.
||| Equivalent to standard `System.Systemd.Daemon.storeFdWithName`.
export
storeFdWithName :  Fd
                -> String
                -> IO (Maybe ())
storeFdWithName fd name =
  notifyWithFd False
               ("FDSTORE=1\nFDNAME=" ++ name)
               fd

||| Return Just a list of file descriptors if the current process
||| has been activated with one or more socket by systemd, Nothing
||| otherwise.
||| The file descriptors are in the same order as the sockets in the
||| associated .socket file.
||| The sockets will have their family, type,
||| and status set according to the .socket file.
||| Equivalent to standard `System.Systemd.Daemon.getActivatedSockets`.
export
getActivatedSockets : IO (Maybe (List Fd))
getActivatedSockets = do
  Right res <- runElinIO getActivatedSockets'
    | Left err => do
        () <- stdoutLn $
          show err
        pure Nothing
  pure res
  where
    getActivatedSockets' : Elin World [Errno] (Maybe (List Fd))
    getActivatedSockets' = do
      Just listenpid <- liftIO $ getEnv "LISTEN_PID"
        | Nothing =>
            pure Nothing
      Just listenfds <- liftIO $ getEnv "LISTEN_FDS"
        | Nothing =>
            pure Nothing
      mypid <- getpid
      True <- pure $ (cast {to=Int32} listenpid) == mypid
        | False =>
            pure Nothing
      let fds  = map (cast {to=Bits32})
                     [fdstart .. (fdstart + ((cast {to=Int} listenfds) - 1))]
          fds' = map MkFd fds
      for_ fds' $ \fd =>
        addFlags fd O_NONBLOCK
      pure $
        Just fds'

||| Like 'getActivatedSockets', but also return the associated names.
||| If a file descriptor has no associated name, it will be a generic
||| one set by systemd.
||| Equivalent to standard `System.Systemd.Daemon.getActivatedSocketsWithNames`.
export
getActivatedSocketsWithNames : IO (Maybe (List (Fd, String)))
getActivatedSocketsWithNames = do
  Right res <- runElinIO getActivatedSocketsWithNames'
    | Left err => do
        () <- stdoutLn $
          show err
        pure Nothing
  pure res
  where
    getActivatedSocketsWithNames' : Elin World [Errno] (Maybe (List (Fd, String)))
    getActivatedSocketsWithNames' = do
      Just listenfdnames <- liftIO $ getEnv "LISTEN_FDNAMES"
        | Nothing =>
            pure Nothing
      let listenfdnames' = forget $
                             split (== ':') listenfdnames
      Just nonblockfds <- liftIO getActivatedSockets
        | Nothing =>
           pure Nothing
      True <- pure $ length nonblockfds == length listenfdnames'
        | False =>
            pure Nothing
      pure $
        Just $
          zip nonblockfds
              listenfdnames'
