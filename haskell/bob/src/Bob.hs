module Bob (responseFor) where


import           Data.Char (isAlpha, isUpper)
import           Data.Text (Text)
import qualified Data.Text as T


responseFor :: Text -> Text
responseFor txt
  | isQuestion t && isAllCaps t = T.pack "Calm down, I know what I'm doing!"
  | isQuestion t                = T.pack "Sure."
  | isAllCaps t                 = T.pack "Whoa, chill out!"
  | isSilence t                 = T.pack "Fine. Be that way!"
  | otherwise                   = T.pack "Whatever."
    where t = T.strip txt
          isQuestion  = T.isSuffixOf (T.singleton '?')
          isSilence   = T.null
          isAllCaps x = let letters = T.filter isAlpha x in
                          (not . isSilence) letters && T.all isUpper letters
