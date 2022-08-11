{-# LANGUAGE OverloadedStrings #-}

module Language.Granule.Syntax.Preprocessor.Ascii
  ( asciiToUnicode
  , unicodeToAscii
  , asciiUnicodeTableMarkdown
  ) where

import Data.Bifunctor (bimap)
import Data.String (fromString)
import Text.Replace (Replace(..), replaceWithList)
import qualified Data.Text.Lazy as TL

-- TODO(ejconlon) Use Text instead of converting back and forth to strings.
asciiToUnicode :: String -> String
asciiToUnicode = TL.unpack . replaceWithList (map (uncurry Replace . bimap fromString fromString) asciiUnicodeTable) . TL.pack

-- TODO(ejconlon) Use Text instead of converting back and forth to strings.
unicodeToAscii :: String -> String
unicodeToAscii = TL.unpack . replaceWithList (map (uncurry (flip Replace) . bimap fromString fromString) asciiUnicodeTable) . TL.pack

-- NOTE: Update the documentation with 'asciiUnicodeTableMarkdown' if you touch this.
asciiUnicodeTable :: [(String, String)]
asciiUnicodeTable =
    [ ("forall" , "∀")
    , ("Inf" , "∞")
    , ("->" , "→")
    , ("=>" , "⇒")
    , ("<-" , "←")
    , ("/\\" , "∧")
    , ("\\/" , "∨")
    , ("<=" , "≤")
    , (">=" , "≥")
    , ("==" , "≡")
    , ("\\" , "λ")
    ]

asciiUnicodeTableMarkdown :: String
asciiUnicodeTableMarkdown
    = unlines
    $ [ ("| ASCII | Unicode |")
      , ("|:---:|:---:|")
      ]
    <> map mkRow asciiUnicodeTable
  where
    mkRow (x,y) = mconcat ["| `", x, "` | `", y, "` |"]
    -- escapeBackslash = \case
    --   [] -> []
    --   '\\' : cs -> '\\' : '\\' : escapeBackslash cs
    --   c : cs -> c : escapeBackslash cs

