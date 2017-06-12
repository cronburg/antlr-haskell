module Test.Text.ANTLR.HUnit where
import           Control.DeepSeq
import           Control.Exception as E
import           Control.Monad
import           Data.List
import           Data.Typeable
import           Data.CallStack
import Test.HUnit.Lang hiding (assertEqual, (@?=))

import Text.ANTLR.Pretty
import qualified Data.Text as T

location :: HasCallStack => Maybe SrcLoc
location = case reverse callStack of
  (_, loc) : _ -> Just loc
  [] -> Nothing

-- | Asserts that the specified actual value is equal to the expected value.
-- The output message will contain the prefix, the expected value, and the
-- actual value.
--
-- If the prefix is the empty string (i.e., @\"\"@), then the prefix is omitted
-- and only the expected and actual values are output.
assertEqual :: (HasCallStack, Eq a, Prettify a)
                              => String -- ^ The message prefix
                              -> a      -- ^ The expected value
                              -> a      -- ^ The actual value
                              -> Assertion
assertEqual preface expected actual =
  unless (actual == expected) $ do
    (prefaceMsg `deepseq` expectedMsg `deepseq` actualMsg `deepseq` E.throwIO (HUnitFailure location $ ExpectedButGot prefaceMsg expectedMsg actualMsg))
  where
    prefaceMsg
      | null preface = Nothing
      | otherwise = Just preface
    expectedMsg = '\n' : T.unpack (pshowIndent 4 expected)
    actualMsg   = '\n' : T.unpack (pshowIndent 4 actual)

-- | Asserts that the specified actual value is equal to the expected value
--   (with the actual value on the left-hand side).
(@?=) :: (HasCallStack, Eq a, Prettify a)
                        => a -- ^ The actual value
                        -> a -- ^ The expected value
                        -> Assertion
actual @?= expected = assertEqual "" expected actual

