module Main

import Control.Monad.Elin
import Data.ByteString
import Data.String
import System
import System.File
import System.Posix.File
import System.Posix.Process
import System.Posix.Socket
import System.Systemd.Daemon.Fd

unixsocketpath : String
unixsocketpath = "/tmp/testd.sock"

sendTestMessage : String -> IO ()
sendTestMessage msg =
  case !(runElinIO $ sendTestMessage' msg) of
    Left err  =>
      stdoutLn $
        show err
    Right res =>
      pure res
  where
    sendTestMessage' : String -> Elin World [Errno] ()
    sendTestMessage' msg = do
      sock <- socket AF_UNIX
                     SOCK_DGRAM
      srv  <- runIO (sockaddrUn unixsocketpath)
      ()   <- connect_ sock
                       srv
      _    <- sendto sock
                     (fastPack $ take 16 $ fastUnpack (msg ++ (fastPack $ replicate 16 ' '))) -- pad or truncate to 16 bytes
                     0
                     srv
      close' (cast {to=Fd} sock)

setupUnixDatagramSocket : IO ()
setupUnixDatagramSocket =
  case !(runElinIO setupUnixDatagramSocket') of
    Left  err => do
      putStrLn $
        show err
      pure () 
    Right res =>
      pure res
  where
    setupUnixDatagramSocket' : Elin World [Errno] ()
    setupUnixDatagramSocket' = do
      sock   <- socket AF_UNIX
                       SOCK_DGRAM
      ()     <- bind sock
                     unixsocketpath
      let fd =  cast {to=Fd} sock
      _      <- dup2 fd (MkFd 3)
      ()     <- close fd
      ()     <- close' (cast {to=Fd} sock)
      True   <- setEnv "LISTEN_PID"
                       (show !getpid)
                       True
        | False =>
            die "Couldn't set environment variable: LISTEN_PID"
      True   <- setEnv "LISTEN_FDS"
                       "1"
                       True
        | False =>
            die "Couldn't set environment variable: LISTEN_FDS"
      True   <- setEnv "LISTEN_FDNAMES"
                       "testdgram"
                       True
        | False =>
            die "Couldn't set environment variable: LISTEN_FDNAMES"
      pure ()

runTestReceiver : IO ()
runTestReceiver = do
  stdoutLn "Waiting for datagram on FD 3 ..."
  case !getActivatedSocketsWithNames of
    Nothing      =>
      die "Couldn't retrieve activated sockets."
    Just sockets =>
      let socket = filter (\(_, currentfdname) =>
                             currentfdname == "testdgram"
                          ) sockets
        in case socket of
             ((fd, _) :: Nil) =>
               case !(runElinIO $ readNMessages fd 5) of
                 Left  err => do
                   putStrLn $
                     show err
                   pure ()
                 Right res =>
                   pure res
             _                =>
               die "No socket named testdgram found."
  where
    readNMessages : Fd -> Nat -> Elin World [Errno] () 
    readNMessages _  0 = stdoutLn "Received all messages, shutting down."
    readNMessages fd n = do
      read fd ByteString 16 >>= \x => 
        stdoutLn $
          "Got: " ++ show x
      readNMessages fd (minus n 1)

cleanupEnv : IO ()
cleanupEnv = do
  True <- unsetEnv "LISTEN_PID"
    | False =>
        die "Couldn't unset environment variable: LISTEN_PID"
  True <- unsetEnv "LISTEN_FDS"
    | False =>
        die "Couldn't unset environment variable: LISTEN_FDS"
  True <- unsetEnv "LISTEN_FDNAMES"
    | False =>
        die "Couldn't unset environment variable: LISTEN_FDNAMES"
  case !(runElinIO cleanupEnv') of
    Left  err =>
      die $
        show err
    Right res =>
      pure res
  where
    cleanupEnv' : Elin World [Errno] ()
    cleanupEnv' =
      remove unixsocketpath

main : IO ()
main = do
  ()   <- setupUnixDatagramSocket
  tid1 <- fork $ do
            usleep 1000000
            for_ [0..4] $ \_ => do
              sendTestMessage "This is a test msg"
              usleep 500000
  () <- threadWait tid1
  () <- runTestReceiver
  () <- cleanupEnv
  stdoutLn "Test passed!"
