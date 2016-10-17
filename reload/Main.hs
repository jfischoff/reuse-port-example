{-# LANGUAGE OverloadedStrings #-}
import Network.Wreq
import Control.Lens
import Control.Concurrent
import Data.Function
import System.Process
import System.Process.Internals
import Control.Monad (unless)
import System.Posix.Types
import Data.List
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BSC
import System.Posix.Signals
import Data.Monoid ((<>))
import System.Environment
import Control.Exception

pidToBS :: CPid -> ByteString
pidToBS (CPid pid) = BSC.pack $ show pid

waitForNewServer :: CPid -> IO ()
waitForNewServer pid = fix $ \next -> do
  resp <- view responseBody <$> get "http://localhost:7000/"
  let pidAsBS = pidToBS pid
  unless (resp == pidAsBS) $ do
    BSC.putStrLn $ "looking for new pid " <> pidAsBS
    BSC.putStrLn $ "got " <> resp
    threadDelay 100000
    next

findProcesses :: String -> IO [CPid]
findProcesses name = do
  processes <- readProcess "ps" ["-e"] []
  return $ map (CPid . read . head . words)
         $ filter (name `isInfixOf`)
         $ lines processes

withPid :: ProcessHandle -> (CPid -> IO ()) -> IO ()
withPid han f = withProcessHandle han $ \secretHandle ->
  case secretHandle of
    OpenHandle pid -> f pid
    ClosedHandle _ -> return ()

pauseSYN :: IO a -> IO a
pauseSYN = bracket_ (system $  "sudo nl-qdisc-add --dev=lo --parent=1:4 "
                            ++ "--id=40: --update plug --buffer"
                    )
                    (system $  "sudo nl-qdisc-add --dev=lo --parent=1:4 "
                            ++ "--id=40: --update plug --release-indefinite"
                    )

reload :: String -> IO ()
reload cmdPath = do
  processHandle <- pauseSYN $ runProcess cmdPath [] Nothing Nothing Nothing
                                         Nothing Nothing

  withPid processHandle $ \newPid -> do
    waitForNewServer newPid

    oldPids <-  filter (newPid /=)
            <$> findProcesses "reuse-server"

    pauseSYN $ mapM_ (signalProcess sigTERM) oldPids

main :: IO ()
main = reload . head =<< getArgs
