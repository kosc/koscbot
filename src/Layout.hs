module Layout (changeLayout) where

import qualified Data.Map as Map

mapping :: Map.Map Char Char
mapping = Map.fromList $ zip "qwertyuiop[]asdfghjkl;'\\zxcvbnm,./QWERTYUIOP{}ASDFGHJKL:\"|ZXCVBNM<>?" "йцукенгшщзхъфывапролджэ\\ячсмитьбю.ЙЦУКЕНГШЩЗХЪФЫВАПРОЛДЖЭ/ЯЧСМИТЬБЮ,"

findChar :: Char -> Char
findChar c = let l = Map.lookup c mapping
             in case l of
                Just a -> a
                Nothing -> c

changeLayout :: String -> String
changeLayout str = map (findChar) str
