module Types (
    Document(..)
) where
import Data.Yaml as Y
import Data.HashMap.Strict as HMS
import Data.Text as T
import Data.List as L ((++), map)
import qualified Data.Vector as V
import Data.Scientific as S


-- Document represents a document which is used to
-- communicate with a game server
data Document =
    DMap [(String, Document)]
    | DList [Document]
    | DInteger Int
    | DString String
    | DNull
    deriving (Show, Eq)

instance FromJSON Document where
    parseJSON Y.Null = pure DNull
    parseJSON (Y.String t) = pure $ DString (T.unpack t)
    parseJSON (Y.Array v) = DList <$> traverse parseJSON (V.toList v)
    parseJSON (Y.Object o) = DMap <$> traverse (\(k, v) -> parseJSON v >>= (\mv -> pure (T.unpack k, mv))) (HMS.toList o)
    parseJSON (Y.Number s) =
        case S.toBoundedInteger s of
            Nothing -> error $ show s ++ " not an integer"
            Just i -> pure $ DInteger i
    parseJSON a = error $ show a ++ " not supported"
