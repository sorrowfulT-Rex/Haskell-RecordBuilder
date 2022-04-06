import           Control.Monad
import           Control.Monad.Trans.State
import           Data
import           Data.Char
import qualified Data.Map as M
import           Parser
import           System.Environment
import qualified Text.Megaparsec as P

buildData :: Data -> IO ()
buildData (Data preLen name cons) = do
  let consCount = length cons
  putStrLn $ "-- Boilerplates for " ++ name ++ ".\n"
  -- Constructor tests
  forM_ conNames $ \n -> do
    let funcName = addPre "is" n
    putStrLn $ funcName ++ " :: " ++ name ++ " -> " ++ "Bool"
    putStrLn $ funcName ++ " " ++ n ++ " {} = True"
    unless (length cons == 1) . putStrLn
      $ funcName ++ " _ " ++ replicate (length n + 2) ' ' ++ "= False"
    putStrLn ""
  -- Getters
  forM_ fieldNames $ \f -> do
    let funcName = addPre "get" f
    let conMeta  = fieldCons M.! f
    putStr $ funcName ++ " :: " ++ name ++ " -> "
    putStrLn $ "Maybe (" ++ fields M.! f ++ ")"
    putStrLn $ funcName ++ " " ++ preName ++ " = case " ++ preName ++ " of"
    forM_ (fst conMeta) $ \c -> do
      putStr $ "  " ++ c ++ replicate (snd conMeta - length c) ' ' ++ " {}"
      putStrLn $ " -> Just $ " ++ f ++ " " ++ preName
    unless (length (fst conMeta) == consCount) . putStrLn
      $ "  _ " ++ replicate (snd conMeta + 2) ' ' ++ "-> Nothing"
    putStrLn ""
  -- Setters
  forM_ fieldNames $ \f -> do
    let funcName = addPre "set" f
    let conMeta  = fieldCons M.! f
    let valName  = replicate preLen '_' ++ f
    putStr $ funcName ++ " :: " ++ name ++ " -> " ++ fields M.! f ++ " -> "
    putStrLn name
    putStr $ funcName ++ " " ++ preName ++ " " ++ valName ++ " = case "
    putStrLn $ preName ++ " of"
    forM_ (fst conMeta) $ \c -> do
      putStr $ "  " ++ c ++ replicate (snd conMeta - length c) ' ' ++ " {}"
      putStrLn $ " -> " ++ preName ++ " { " ++ f ++ " = " ++ valName ++ " }"
    unless (length (fst conMeta) == consCount) . putStrLn
      $ "  _ " ++ replicate (snd conMeta + 2) ' ' ++ "-> " ++ preName
    putStrLn ""
  -- Modifiers
  forM_ fieldNames $ \f -> do
    let funcName = addPre "modify" f
    let conMeta  = fieldCons M.! f
    putStr $ funcName ++ " :: " ++ name ++ " -> (" ++ fields M.! f ++ " -> "
    putStrLn $ fields M.! f ++ ") -> " ++ name
    putStr $ funcName ++ " " ++ preName
    putStrLn $ " " ++ modName ++ " = case " ++ preName ++ " of"
    forM_ (fst conMeta) $ \c -> do
      putStr $ "  " ++ c ++ replicate (snd conMeta - length c) ' ' ++ " {}"
      putStr $ " -> " ++ preName ++ " { " ++ f ++ " = " ++ modName ++ " $ "
      putStrLn $ f ++ " " ++ preName ++ " }"
    unless (length (fst conMeta) == consCount) . putStrLn
      $ "  _ " ++ replicate (snd conMeta + 2) ' ' ++ "-> " ++ preName
    putStrLn ""
  -- Strict modifiers
  forM_ fieldNames $ \f -> do
    let funcName = addPre "modify" f ++ "'"
    let conMeta  = fieldCons M.! f
    putStr $ funcName ++ " :: " ++ name ++ " -> (" ++ fields M.! f ++ " -> "
    putStrLn $ fields M.! f ++ ") -> " ++ name
    putStr $ funcName ++ " " ++ preName ++ " " ++ modName ++ " = case "
    putStrLn $ preName ++ " of"
    forM_ (fst conMeta) $ \c -> do
      putStr $ "  " ++ c ++ replicate (snd conMeta - length c) ' ' ++ " {}"
      putStr $ " -> " ++ preName ++ " { " ++ f ++ " = " ++ f ++ " " ++ preName
      putStrLn $ " `seq` " ++ modName ++ " (" ++ f ++ " " ++ preName ++ ") }"
    unless (length (fst conMeta) == consCount) . putStrLn
      $ "  _ " ++ replicate (snd conMeta + 2) ' ' ++ "-> " ++ preName
    putStrLn ""
  where
    modName        = replicate preLen '_' ++ "modifier"
    preName        = replicate preLen '_'
                  ++ addPre "mmzk" (takeWhile (/= ' ') name)
    conNames       = M.keys cons
    fields         = M.unions $ M.elems cons
    fieldNames     = M.keys fields
    fieldCons      = M.fromList $ zip fieldNames $ zip consWithField longestCons
      where
        consWithField = flip filter conNames . (. (cons M.!)) . M.member
                    <$> fieldNames
        longestCons   = maximum . fmap length <$> consWithField
    addPre pre str = pre ++ toUpper (head str) : tail str

main :: IO ()
main = do
  path <- head <$> getArgs
  str  <- readFile path
  case evalState (P.runParserT parser "Record Builder: " str) (0, 0) of
    Left e  -> putStrLn $ P.errorBundlePretty e
    Right d -> mapM_ buildData d
