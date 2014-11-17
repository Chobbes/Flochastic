{- Copyright (C) 2014 Calvin Beck

   Permission is hereby granted, free of charge, to any person
   obtaining a copy of this software and associated documentation files
   (the "Software"), to deal in the Software without restriction,
   including without limitation the rights to use, copy, modify, merge,
   publish, distribute, sublicense, and/or sell copies of the Software,
   and to permit persons to whom the Software is furnished to do so,
   subject to the following conditions:

   The above copyright notice and this permission notice shall be
   included in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
   NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
   BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
   ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
   CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE.
-}

{-# LANGUAGE OverloadedStrings #-}

module Flochastic.Parser (parseCard) where

import Flochastic.Card

import Control.Applicative
import Data.Attoparsec.Text

-- | Parse a card from the LaTeX file.
parseCard :: Parser Card
parseCard = do manyTill anyChar (try $ string "\\begin{mdef}")
               str <- manyTill anyChar (try $ string "\\end{mdef}")
               return (Card str Nothing)

-- | Parse the string between a proof block.
parseProof :: Parser String
parseProof = do manyTill anyChar (try $ string "\\begin{proof}")
                str <- manyTill anyChar (try $ string "\\end{proof}")
                return str
