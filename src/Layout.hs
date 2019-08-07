module Layout (changeLayout) where

import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

mapping = Map.fromList $ zip "qwertyuiop[]asdfghjkl;'\\zxcvbnm,./QWERTYUIOP{}ASDFGHJKL:\"|ZXCVBNM<>?" "йцукенгшщзхъфывапролджэ\\ячсмитьбю.ЙЦУКЕНГШЩЗХЪФЫВАПРОЛДЖЭ/ЯЧСМИТЬБЮ,"

findChar c = let l = Map.lookup c mapping
             in case l of
                Just a -> a
                Nothing -> c

changeLayout :: String -> String
changeLayout str = map (findChar) str
