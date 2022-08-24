{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Class(lift)
import Control.Monad.Trans.State.Strict
import Data.ByteString as B
import Data.ByteString.Lazy as BSL
import Data.Either as E (fromRight)
import qualified Data.List as L
import Data.Text as T
import Data.Text.Encoding.Base64 (decodeBase64)
import Data.Text.IO as TIO
import Data.Yaml as Y
import Data.Char (isSpace)
import Lib1
import Types(Check)
import Network.Wreq hiding (get, put)
import Control.Lens
import System.Console.Repline
  ( CompleterStyle (Word),
    ExitDecision (Exit),
    HaskelineT,
    Options,
    WordCompleter,
    evalRepl,
  )
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (stderr)

import Data.String.Conversions

type Repl a = HaskelineT (StateT (String, Lib1.State) IO) a

-- Evaluation : handle each line user inputs
cmd :: String -> Repl ()
cmd c
  | trim c == "show" = lift get >>= liftIO . Prelude.putStrLn . Lib1.render . snd
  | trim c == "check" = lift get >>= (pure . Lib1.mkCheck . snd) >>= check >>= liftIO . Prelude.putStrLn
cmd c = liftIO $ Prelude.putStrLn $ "Unknown command: " ++ c

trim :: String -> String
trim = f . f
  where f = L.reverse . L.dropWhile isSpace

check :: Check -> Repl String
check c = do
  (url, _) <- lift get
  let opts = defaults & header "Content-type" .~ ["text/x-yaml"]
  let body = Y.encode c
  resp <- liftIO $ postWith opts (url ++ "/check") body
  pure $ cs $ resp ^. responseBody

-- Tab Completion: return a completion for partial words entered
completer :: Monad m => WordCompleter m
completer n = do
  let names = ["show", "hint", "check", "toggle"]
  return $ Prelude.filter (L.isPrefixOf n) names

ini :: Repl ()
ini = do
  (url, s) <- lift get
  r <- liftIO $ post url B.empty
  d <- liftIO $ Y.decodeThrow $ BSL.toStrict $ r ^. responseBody
  lift $ put (url, Lib1.gameStart d s)
  liftIO $ TIO.putStrLn "Welcome to Bimaru. Press [TAB] for available commands list"

final :: Repl ExitDecision
final = do
  liftIO $ TIO.putStrLn "Goodbye!"
  return Exit

main :: IO ()
main = do
  args <- getArgs
  case args of
    [token] -> run $ T.pack token
    _ -> do
      TIO.hPutStrLn stderr "Token not provided: expected at least one command argument"
      exitFailure

run :: T.Text -> IO ()
run token = do
  -- Dear students, it is not against you, it is against silly source code crawlers on the Internet
  let url = E.fromRight (error "Cannot decode url") $ decodeBase64 $ T.drop 6 "f6675cYmltYXJ1LmhvbWVkaXIuZXU="
  let fullUrl = T.unpack (T.concat ["http://", url, "/game/", token])
  evalStateT (evalRepl (const $ pure ">>> ") cmd [] Nothing Nothing (Word completer) ini final) (fullUrl, Lib1.emptyState)
