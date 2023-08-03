import qualified Data.Tuple as Tuple
import qualified Data.List as List
import qualified Data.Maybe as Maybe

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap

-- | unsafely parses / reads an 'Int'
unsafeReadInt :: ByteString -> Int
unsafeReadInt = fst . Maybe.fromJust . B.readInt

-- | main function
main :: IO ()
main = B.interact 
    ( format
    . runner
    . parse
    )

-- | 'parse' parses the given Kattis input s.t. we have
-- > (n, m, list of activities' start and finish time)
parse :: ByteString -> (Int, Int, [(Int,Int)])
parse 
    = go
    . map B.words  -- [ByteString] -> [[ByteString]]
    . B.lines      -- ByteString -> [ByteString]
  where
    go :: [[ByteString]] -> (Int, Int, [(Int,Int)])
    go ([n,m]:activities) = 
        ( unsafeReadInt n
        , unsafeReadInt m
        , map (\[s, f] -> (unsafeReadInt s, unsafeReadInt f)) activities
        )
    go _ = error "Invalid input"

-- | 'format' formats the resulting answer into the desired Kattis output
format :: Int -> ByteString
format = B.pack . show

-- | 'runner' executes the main logic of this algorithm.
--
--      1. Sorts according to the heuristic (earliest finish time first)
--
--      2. Schedules an activity according to the classroom with the latest
--      compatible start time
runner :: (Int, Int, [(Int,Int)]) -> Int
runner (n,m,activities) 
    = fst 
    $ List.foldl' go (0, initClassrooms)
    $ List.sortOn snd activities -- sort according to our heuristic
  where
    -- initially all @m@ classrooms have $$t_c$$ of 0. 
    -- the 'IntMap' maps $$t_c$$ to its multiplicity i.e., the number of
    -- classrooms which have the same $$t_c$$
    -- Note: if we wanted to actually produce a schedule, the type we would
    -- want is @IntMap [Int]@ where @[Int]@ is the list of classrooms.
    initClassrooms :: IntMap Int
    initClassrooms = IntMap.singleton 0 m

    go :: (Int, IntMap Int) -> (Int, Int) -> (Int, IntMap Int)
    go (ans, classrooms) (s,f) = 
        case IntMap.lookupLT s classrooms of
            Just (classroom, _multiplicity) -> 
                -- we schedule the activity
                ( 1 + ans
                , insertClassroom f 
                    (deleteClassroom classroom classrooms)
                )
            Nothing -> 
                -- or we don't schedule it
                (ans, classrooms)

    -- deleting an element in the multiset
    deleteClassroom :: Int -> IntMap Int -> IntMap Int
    deleteClassroom =  IntMap.alter go
      where
        go Nothing = Nothing
        go (Just count) 
            | count == 1 = Nothing
            | otherwise = Just (count - 1)

    -- inserting an element in the multiset
    insertClassroom :: Int -> IntMap Int -> IntMap Int
    insertClassroom = IntMap.alter go
      where
        go Nothing = Just 1
        go (Just count) = Just (count + 1)
