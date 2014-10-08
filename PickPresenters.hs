{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}

import           Data.Maybe (catMaybes)
import           Control.Lens
import           Control.Monad
import           Control.Monad.Random
import           Data.Functor
import           Data.List              (intercalate, nub, partition, isInfixOf)
import qualified System.IO.Strict       as Strict

import           System.Console.CmdArgs hiding ((+=), name)
import qualified System.Console.CmdArgs as Cmd

presentersFile :: String
presentersFile = "presenters.dat"

updateIndex :: (a -> a) -> Int -> [a] -> [a]
updateIndex _ _ [] = []
updateIndex f 0 (x:xs) = (f x:xs)
updateIndex f i (x:xs) = x : updateIndex f (i-1) xs

skim :: (Ord b, Num b) => [b] -> [b]
skim xs = map (subtract (minimum xs)) xs

weights :: [Integer] -> [Double]
weights = map ((7^^) . negate)

normalize :: [Double] -> [Double]
normalize xs = map (/sum xs) xs

pick :: MonadRandom m => Int -> [Integer] -> [Int] -> m [Int]
pick n times excl = do
    rs <- getRandomRs (0,1)
    return . take n . filter (`notElem` excl) . nub . map findR $ rs
  where
    probs = (scanl1 (+) . normalize . weights . skim) times
    findR r = length (takeWhile (<r) probs)

iterateM :: Monad m => Int -> (a -> m a) -> a -> m [a]
iterateM 0 _ a = return [a]
iterateM k f a = (a:) `liftM` (f a >>= iterateM (k-1) f)

simulate :: MonadRandom m => Int -> Int -> Int -> m [[Integer]]
simulate steps size nPres = iterateM steps (simStep nPres) (replicate size 0)

simStep :: MonadRandom m => Int -> [Integer] -> m [Integer]
simStep nPres times = do
  presenters <- pick nPres times []
  return $ doAll (map (updateIndex (+1)) presenters) times

doAll :: [b -> b] -> b -> b
doAll = foldr (.) id

printTable :: [[Integer]] -> IO ()
printTable = mapM_ print

------------------------------------------------------------

data Presenter = P { _name :: String, _pCount :: Integer }
  deriving (Show, Read)

makeLenses ''Presenter

------------------------------------------------------------

data Modes = List | Pick { problems :: [String], exclude :: [String], dryRun :: Bool }
  deriving (Show, Data, Typeable)

pickModes :: Modes
pickModes = modes
  [ List &= auto
  , Pick { problems = def &= args
         , exclude  = def &= typ "NAME" &= Cmd.name "not" &= help "Exclude a presenter"
         , dryRun   = def &= Cmd.name "dry-run" &= help "Don't modify the presenters data file"
         }
  ]
  &= summary "Pick presenters for CS 354"
  &= program "PickPresenters"

main :: IO ()
main = do
  m <- cmdArgs pickModes
  case m of
    List             -> listPresenterData
    Pick ps excl dry -> pickPresenters ps excl dry

listPresenterData :: IO ()
listPresenterData = do
  presenters <- readPresenters
  mapM_ formatPresenter presenters

readPresenters :: IO [Presenter]
readPresenters = read <$> Strict.readFile presentersFile

formatPresenter :: Presenter -> IO ()
formatPresenter (P nm cnt) = putStrLn (show cnt ++ "  " ++ nm)

toIndices :: [Bool] -> [Int]
toIndices bs = catMaybes $ zipWith (\b i -> if b then Just i else Nothing) bs [0..]

pickPresenters :: [String] -> [String] -> Bool -> IO ()
pickPresenters ps excl dry = do
  putStrLn ("Assigning problems " ++ intercalate ", " ps ++ "...")
  presenters <- readPresenters
  let isExcluded pres = any (`isInfixOf` (pres ^. name)) excl
      (excludedPresenters, potentialPresenters) = partition (isExcluded) presenters
      excludedIndices = toIndices (map isExcluded presenters)
  when (length excludedPresenters > 0)
    $ putStrLn ("Excluding " ++ intercalate ", " (map (^. name) excludedPresenters) ++ "...")
  picks <- evalRandIO (pick (length ps) (map _pCount presenters) excludedIndices)
  let presenters' = doAll (map (updateIndex (pCount +~ 1)) picks) presenters
  when (not dry) $ writeFile presentersFile (show presenters')
  formatPicks ps picks presenters

formatPicks :: [String] -> [Int] -> [Presenter] -> IO ()
formatPicks problems picks presenters = do
  mapM_ (\(p, i) -> formatPick p (presenters !! i)) (zip problems picks)
  putStrLn "* * * * * * * * * * * * * * * *"
  mapM_ (\p -> putStr "      " >> formatPresenter p) (map snd . filter ((`notElem` picks) . fst) $ presentersI)
  where
    presentersI = zip [0..] presenters
    formatPick prob pres = do
      putStr ("[" ++ pad 2 prob ++ "]  ") >> formatPresenter pres

pad i s
  | length s < i = (replicate (i - length s) ' ') ++ s
  | otherwise    = s

------------------------------------------------------------
------------------------------------------------------------

f14 :: [String]
f14 =
  [ "Ben Eastburn"
  , "Benji Jones"
  , "Benno Stein"
  , "Blake Mackall"
  , "Derrick Bonafilia"
  , "Devin Gardella"
  , "Lisa Liu"
  , "Llewellyn Smith"
  , "Matt McNaughton"
  , "Mike Flynn"
  , "Michael Shaw"
  , "Michael Shelton"
  , "Noah Grumman"
  , "Paul Lindseth"
  , "Ryan Kwon"
  , "Sam Donow"
  , "Sarah Abramson"
  , "Steven Dreyfus"
  ]

generateF14 = do
  let pData = map (\n -> P n 0) f14
  writeFile presentersFile (show pData)
