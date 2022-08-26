{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}

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
import Data.List.Split as S
import Data.Yaml as Y
import Data.Char (isSpace)
import Lib1
import Types(Check, Row(..), Col(..))
import Network.Wreq hiding (get, put)
import qualified Network.Wreq as Wreq

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

commandShow = "show"
commandHint = "hint"
commandCheck = "check"
commandToggle = "toggle"

-- Evaluation : handle each line user inputs
cmd :: String -> Repl ()
cmd c
  | trim c == commandShow = lift get >>= liftIO . Prelude.putStrLn . Lib1.render . snd
  | trim c == commandCheck = lift get >>= check . (Lib1.mkCheck . snd) >>= liftIO . Prelude.putStrLn
  | commandToggle `L.isPrefixOf` trim c = do
    case tokens c of
      [_, colrow] ->
        case parseColRow colrow of
          Nothing -> liftIO $ Prelude.putStrLn $ "Illegal " ++ commandToggle ++ " argument: " ++ colrow
          Just(col, row) -> lift $ modify (\(u, s) -> (u, Lib1.toggle s col row))
      _ -> liftIO $ Prelude.putStrLn $ "Illegal format, \"" ++ commandToggle ++ " $column$row\" expected, e.g \"" ++ commandToggle ++ " A10\"  " ++ c
  | commandHint `L.isPrefixOf` trim c =
    case tokens c of
      [_, count] ->
        case reads count of
          [(n, "")] -> hints n
          _ -> liftIO $ Prelude.putStrLn $ "Illegal " ++ commandHint ++ " argument: " ++ count
      _ -> liftIO $ Prelude.putStrLn $ "Illegal format, \"" ++ commandHint ++ " $number_of_hints\" expected, e.g \"" ++ commandHint ++ " 1\"  " ++ c
cmd c = liftIO $ Prelude.putStrLn $ "Unknown command: " ++ c

tokens :: String -> [String]
tokens s = L.filter (not . Prelude.null) $ S.splitOn " " s

trim :: String -> String
trim = f . f
  where f = L.reverse . L.dropWhile isSpace

parseColRow :: String -> Maybe (Types.Col, Types.Row)
parseColRow [c, '1', '0'] = fmap (\col -> (col, Row10)) (parseCol c)
parseColRow [c, r] = (,) <$> parseCol c <*> parseRow r
parseColRow _ = Nothing

parseCol :: Char -> Maybe Types.Col
parseCol c =
  case c `L.elemIndices` ['A'..'J'] of
    [i] -> Just $ [(minBound :: Types.Col) ..] !! i
    _ -> Nothing

parseRow :: Char -> Maybe Types.Row
parseRow r =
  case r `L.elemIndices` ['1'..'9'] of
    [i] -> Just $ [(minBound :: Types.Row) ..] !! i
    _ -> Nothing

check :: Check -> Repl String
check c = do
  (url, _) <- lift get
  let opts = defaults & header "Content-type" .~ ["text/x-yaml"]
  let body = Y.encode c
  resp <- liftIO $ postWith opts (url ++ "/check") body
  pure $ cs $ resp ^. responseBody

hints :: Int -> Repl ()
hints n = do
  (url, s) <- lift get
  r <- liftIO $ Wreq.get (url ++ "/hint?limit=" ++ show n)
  d <- liftIO $ Y.decodeThrow $ BSL.toStrict $ r ^. responseBody
  lift $ put (url, Lib1.hint s d)

-- Tab Completion: return a completion for partial words entered
completer :: Monad m => WordCompleter m
completer n = do
  let names = [commandShow, commandHint, commandCheck, commandToggle]
  return $ Prelude.filter (L.isPrefixOf n) names

ini :: Repl ()
ini = do
  (url, s) <- lift get
  r <- liftIO $ post url B.empty
  d <- liftIO $ Y.decodeThrow $ BSL.toStrict $ r ^. responseBody
  lift $ put (url, Lib1.gameStart s d)
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
