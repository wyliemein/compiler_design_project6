{-# LANGUAGE RecordWildCards #-}
module Common ( createTestTree
              , parseTestFile
              , Test(..)
              , Program(..)
              , resultToEither
              , getTestResult
              ) where

import Language.Fox.Utils
import Language.Fox.Types  hiding (Result, Program)

import Test.Tasty
import Test.Tasty.HUnit
import Text.Printf
import System.FilePath   ((</>))
import System.Exit
import Data.List         (isInfixOf)

import qualified Text.JSON as J

--------------------------------------------------------------------------------
-- | A test program is either a filename or a text representation of source
--------------------------------------------------------------------------------
data Program = File | Code Text deriving (Show)
type Result  = Either Text Text
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- | Construct a single compiler test case from a `Program`
--------------------------------------------------------------------------------
mkTest :: Heap -> String -> Program -> Result -> IO (String, Bool)
--------------------------------------------------------------------------------
mkTest mHeap name pgm expect = do
  res <- run mHeap name pgm
  return $ check logF res expect
  where
    logF = dirExt "output" name Log

check :: FilePath -> Result -> Result -> (String, Bool)
check _ (Right resV) (Right expectV)  = ( printf "Expecting %s but got %s" (show expectV) (show resV)
                                        , matchSuccess expectV resV
                                        )
check logF (Left resE) (Left expectE) = ( printf "Expecting the error %s but failed for a different reason. See %s for the error message." (show expectE) logF
                                        , matchError expectE resE
                                        )
check _ (Right resV) (Left expectE)   = ( printf "Expecting the error %s but got the value %s" (show expectE) (show resV)
                                        , False
                                        )
check logF (Left _) (Right expectV)   = ( printf "Expecting the value %s but got an error. See %s for the error message." (show expectV) logF
                                        , False
                                        )

matchError :: String -> String -> Bool
matchError expected result = expected `isInfixOf` result

matchSuccess :: String -> String -> Bool
matchSuccess expected result = trim expected == trim result

mkAnfTest :: TestName -> Program -> Result -> IO (String, Bool)
mkAnfTest name pgm expect = do
  res <- runHelper Anf Nothing name pgm
  return $ check logF res expect
  where
    logF = dirExt "output" name Log

--------------------------------------------------------------------------------
run :: Heap -> FilePath -> Program -> IO Result
--------------------------------------------------------------------------------
run = runHelper Res

runHelper :: Ext -> Heap -> FilePath -> Program -> IO Result
runHelper extension mHeap name pgm = do
  _ <- generateSource name pgm                 -- generate source file
  r <- executeShellCommand logF cmd timeLimit  -- compile & run
  readResult resF logF r
  where
    cmd  = case mHeap of
             Nothing -> printf "make %s" resF
             Just h  -> printf "make %s HEAP=%d" resF h
    resF = dirExt "output" name extension
    logF = dirExt "output" name Log


-- | `timeLimit` for each test is 15 seconds
timeLimit :: Int
timeLimit = 15 * microSec
  where microSec = 1000000

--------------------------------------------------------------------------------
generateSource :: FilePath -> Program -> IO ()
--------------------------------------------------------------------------------
generateSource _    File       = return ()
generateSource name (Code pgm) = writeFile srcF pgm
  where
    srcF = dirExt "input"  name Src

--------------------------------------------------------------------------------
readResult :: FilePath -> FilePath -> ExitCode -> IO Result
--------------------------------------------------------------------------------
readResult resF _     ExitSuccess      = Right <$> readFile resF
readResult _    _    (ExitFailure 100) = Left  <$> return "TIMEOUT!"
readResult _    logF (ExitFailure _  ) = Left  <$> readFile logF

dirExt :: FilePath -> FilePath -> Ext -> FilePath
dirExt dir name e = "tests" </> dir </> name `ext` e

--------------------------------------------------------------------------------
-- | Tests
--------------------------------------------------------------------------------

-- { name   : string
-- , code   : "file" | string
-- , result : { value : string } | { failure : string }
-- , anf    : true | false
-- , heap   : int
-- }

data TestResult = Value   String
                | Failure String
                deriving (Show)

resultToEither :: TestResult -> Either String String
resultToEither (Value s)   = Right s
resultToEither (Failure s) = Left s

type Heap = Maybe Int

data Test = Test { testName   :: String
                 , testCode   :: Program
                 , testResult :: TestResult
                 , testAnf    :: Bool
                 , testHeap   :: Heap
                 }
          deriving (Show)

jsonToStr :: J.JSValue -> J.Result String
jsonToStr (J.JSString s) = return $ J.fromJSString s
jsonToStr j = J.Error $ "Expecting a string, got " ++ J.encode j

instance J.JSON Program where
  showJSON File     = J.showJSON "file"
  showJSON (Code c) = J.showJSON c

  readJSON j = toProgram <$> jsonToStr j
    where
      toProgram "file" = File
      toProgram code   = Code code

instance J.JSON TestResult where
  showJSON (Value v) = J.JSObject (J.toJSObject keys)
    where keys = [("value", J.showJSON v)]
  showJSON (Failure e) = J.JSObject (J.toJSObject keys)
    where keys = [("failure", J.showJSON e)]

  readJSON (J.JSObject o) =
    case J.fromJSObject o of
      [("value", j)]   -> Value <$> jsonToStr j
      [("failure", j)] -> Failure <$> jsonToStr j
      _                -> J.Error $ "Malformed Program: " ++ J.encode o
  readJSON j = J.Error $ "Malformed Program: " ++ J.encode j

instance J.JSON Test where
  showJSON (Test{..}) = J.JSObject (J.toJSObject keys)
    where
      keys = [ ("name",   J.showJSON testName)
             , ("code",   J.showJSON testCode)
             , ("result", J.showJSON testResult)
             , ("anf",    J.showJSON testAnf)
             ] ++ hk
      hk = case testHeap of
             Nothing -> []
             Just n  -> [("heap", J.showJSON n)]
      

  readJSON (J.JSObject o) =
    Test
    <$> (get "name" >>= jsonToStr)
    <*> (get "code" >>= J.readJSON)
    <*> (get "result" >>= J.readJSON)
    <*> (getDef (J.JSBool False) "anf" >>= J.readJSON)
    <*> (m "heap")
    where kvs   = J.fromJSObject o
          get k = case lookup k kvs of
                    Just j -> return j
                    Nothing -> J.Error $ printf "Key %s does not exist in %s" k (J.encode o)
          getDef d k =
            case lookup k kvs of
              Just j -> return j
              Nothing -> return d
          m k = case lookup k kvs of
                  Just j  -> Just <$> J.readJSON j
                  Nothing -> return Nothing
  readJSON _ = J.Error "error"

createTestTree :: Test -> TestTree
createTestTree t@Test{..} = testCase testName $ do
  (msg, c) <- getTestResult t
  assertBool msg c

getTestResult :: Test -> IO (String, Bool)
getTestResult Test{..} = mk testName testCode (resultToEither testResult)
  where
    mk = if testAnf then mkAnfTest else mkTest testHeap

parseTestFile  :: FilePath -> IO [Test]
parseTestFile f = do b <- readFile f
                     case J.decode b of
                       J.Error err -> fail $ "!!! ERROR IN TEST FILE" ++ f ++ " : " ++ err
                       J.Ok res    -> return res
