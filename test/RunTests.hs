{-# LANGUAGE OverloadedStrings #-}

-- Copyright 2012-2017 Richard Cobbe
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--   http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

module Main(main) where

import Control.Monad
import qualified Control.Monad.Trans.Except as CMTE
import System.Exit
import Test.HUnit
import Test.Utils

import Text.Parsec.Char
import Text.Parsec.Error

main :: IO ()
main =
  do c <- runTestTT posTests
     when (errors c /= 0 || failures c /= 0)
       exitFailure
     let numExpectedFailures = length negTests
     c <- runTestTT ("Test.Utils negative tests" ~: negTests)
     when ((errors c + failures c) /= numExpectedFailures)
       exitFailure

posTests =
  "Test.Utils positive tests" ~:
  ["assertParse" ~: assertParse 'x' (char 'x') "x",
   "assertParse with suffix" ~: assertParse 'x' (char 'x') "xyz",

   "parseRest" ~: assertParse ('x', "yz") (parseRest (char 'x')) "xyz",

   "parseContext" ~:
   assertParse ("xyz", 'x', "yz") (parseContext (char 'x')) "xyz",

   "assertParseFail" ~: assertParseFail (char 'x') "y",

   "assertParseError" ~:
   assertParseError (char 'x') "y" (Expect "\"x\""),

   "assertExcept" ~:
   assertExcept "foo!" (throwStrComp "foo!"),

   "assertExcept'" ~:
   assertExcept' (assertEqual "" "foo!") (throwStrComp "foo!"),

   "assertNoExcept" ~:
   assertNoExcept 42 normalReturnComp,

   "assertNoExcept'" ~:
   assertNoExcept' (assertEqual "" 42) normalReturnComp
   ]

-- Put the label on above, so we can count the number of expected failures.
negTests =
  ["assertParse: bad result" ~: assertParse 'y' (char 'x') "x",
   "assertParse: parse failure" ~: assertParse 'x' (char 'x') "y",

   "assertParseFail: parse success" ~: assertParseFail (char 'x') "x",

   "assertParseError: parse success" ~:
   assertParseError (char 'x') "x" (Message "foo"),
   "assertParseError: bad message" ~:
   assertParseError (char 'x') "y" (Message "foo"),

   "assertExcept: normal return" ~:
   assertExcept "foo!" normalReturnComp,

   "assertExcept: exn mismatch" ~:
   assertExcept "foo!" (throwStrComp "bar!"),

   "assertExcept': normal return" ~:
   assertExcept' (assertEqual "" "foo!") normalReturnComp,

   "assertExcept': bad exn" ~:
   assertExcept' (assertEqual "" "foo!") (throwStrComp "bar!"),

   "assertNoExcept: throw" ~:
   assertNoExcept 42 (throwStrComp "foo!"),

   "assertNoExcept: bad result" ~:
   assertNoExcept 43 normalReturnComp,

   "assertNoExcept': throw" ~:
   assertNoExcept' (assertEqual "" 42) (throwStrComp "foo!"),

   "assertNoExcept': bad result" ~:
   assertNoExcept' (assertEqual "" 43) normalReturnComp
   ]

normalReturnComp :: CMTE.Except String Integer
normalReturnComp = return 42

throwStrComp :: String -> CMTE.Except String Integer
throwStrComp = CMTE.throwE
