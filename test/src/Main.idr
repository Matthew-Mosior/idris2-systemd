module Main

import System
import System.File
import System.Posix.File
import System.Systemd.Daemon.Fd

main : IO ()
main =
  let s = "foo"
    in case !(System.Systemd.Daemon.Fd.storeFdWithName (MkFd 0) s) of
         Nothing =>
           die $ "Couldn't activate named socket: " ++ s
         Just () =>
           case !System.Systemd.Daemon.Fd.getActivatedSocketsWithNames of
             Nothing      =>
               die "No activated sockets."
             Just sockets =>
               for_ sockets $ \(_, currentsocketname) =>
                 putStrLn $ "Activated socket: " ++ currentsocketname
