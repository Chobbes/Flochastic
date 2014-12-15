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

import Control.Arrow
import Control.Monad
import Data.Attoparsec.Text
import Data.List
import qualified Data.Text.IO as T
import System.Directory
import System.Environment
import System.FilePath
import Text.LaTeX.Base.Parser
import Text.LaTeX.Base.Render
import Text.LaTeX.Base.Syntax



-- | LaTeX flash cards generated from notes!
main :: IO ()
main = do [latexFile, outputDir] <- getArgs
                                    
          latex <- liftM takeRight $ parseLaTeXFile latexFile
          let preamble = getPreamble latex

          renderCards outputDir latexFile $ map (wrapCard preamble *** wrapCard preamble) (getCards latex)
  where takeRight (Right a) = a


-- | Get relevant cards from a block of LaTeX. Returns list of (minimalCard, fullCard) pairs.
getCards :: LaTeX -> [(LaTeX, LaTeX)]
getCards tex = map ((\l -> (removeEnv "proof" l, l)) . toLaTeX) (matchEnv (`elem` environments) tex)
  where environments = ["mdef", "prop", "lemma", "theorem"]
        toLaTeX (env, args, content) = TeXEnv env args content


-- | Remove an environment out of a block of LaTeX.
removeEnv :: String -> LaTeX -> LaTeX
removeEnv env = texmap isEnv (const TeXEmpty)
  where isEnv e = case e of
                    (TeXEnv texEnv _ _) -> texEnv == env
                    _ -> False


-- | Render LaTeX cards.
renderCards :: FilePath -> FilePath -> [(LaTeX, LaTeX)] -> IO ()
renderCards outDir baseFile cards =
  mapM_ (uncurry renderCard) (zip qaFiles cards)
  where qaFiles = zip questionFiles answerFiles
        questionFiles = map (\n -> joinPath [outDir, flip addExtension "tex" $ dropExtension baseFile ++ "-" ++ show n]) [1 .. length cards]
        answerFiles = map (\n -> joinPath [outDir, flip addExtension "tex" $ dropExtension baseFile ++ "-" ++ show n ++ "-solution"]) [1 .. length cards]


-- | Render LaTeX (question, solution) pair.
renderCard :: (FilePath, FilePath) -> (LaTeX, LaTeX) -> IO ()
renderCard (questionFile, answerFile) (question, answer) = 
  do createDirectoryIfMissing True (dropFileName questionFile)
     createDirectoryIfMissing True (dropFileName answerFile)
     renderFile questionFile question
     renderFile answerFile answer


-- | Wrap a card with a preamble.
wrapCard :: LaTeX -> LaTeX -> LaTeX
wrapCard preamble card = newPreamble <> TeXEnv "document" [] card
  where newPreamble = texmap isDocumentClass (const $ TeXComm "documentclass" args) preamble
        isDocumentClass (TeXComm "documentclass" _) = True
        isDocumentClass _ = False
        args = [MOptArg (map TeXRaw ["varwidth=true", "border=10pt", "convert={size=640x}"]), FixArg (TeXRaw "standalone")]
