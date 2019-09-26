module Layout (changeLayout) where

import qualified Data.Bimap as Bimap

mapping = Bimap.fromList $ zip "qwertyuiop[]asdfghjkl;'\\zxcvbnm,./QWERTYUIOP{}ASDFGHJKL:\"|ZXCVBNM<>?" "йцукенгшщзхъфывапролджэ\\ячсмитьбю.ЙЦУКЕНГШЩЗХЪФЫВАПРОЛДЖЭ/ЯЧСМИТЬБЮ,"

findChar c = if Bimap.member c mapping then
                mapping Bimap.! c
             else if Bimap.memberR c mapping then
                mapping Bimap.!> c
             else c

changeLayout str = map (findChar) str
