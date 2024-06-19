import Control.Monad.State
import Control.Monad(forM_)
import System.Random.Stateful
import System.Environment
import Data.List(group,sort)

-- funkcja zwracająca stan po zastosowaniu zmiany
apply :: (s->Bool)->(a->s->s)->a->s->s
apply allow change c s = 
  let r = change c s in
  if allow r then r else s

-- funkcja przekształcająca funkcję apply w działanie na monadzie
applySt :: (s->Bool)->(a->s->s)->a->State s a
applySt allow change x =
  state(\s -> (x, apply allow change x s))
 

-- RANDOM WALK
-- przestrzeń stanów: Int x Int

-- funkcja określająca czy dany stan jest dozwolony
rwAllow :: (Int,Int) -> Bool
rwAllow (n,m) = (n>=0) || (n>=(-3)) && (m==0)

-- funkcja zmieniająca stan planszy
rwChange c (k,l) = case c of
  'U' -> (k,l+1)
  'D' -> (k,l-1)
  'R' -> (k+1,l)
  'L' -> (k-1,l)
  _   -> (k,l)

-- funkcja wykonująca krok losowego spaceru
rwEval :: Char -> State (Int,Int) Char  
rwEval = applySt rwAllow rwChange 

-- funkcja wykonująca losowy spacer po planszy
rwExe :: String -> (Int,Int) -> (Int,Int)
rwExe xs start =  snd $(runState (forM_ xs rwEval))start   

-- funkcja zamieniająca liczbę na znak odpowiadający kierunkowi
decode :: Int -> Char
decode 0 = 'U'
decode 1 = 'D'
decode 2 = 'R'
decode 3 = 'L'
decode _ = 'D'

-- funkcja pobierająca n razy m elementów z listy xs
-- i zwracająca listę list o długości n
splitRs :: Int -> Int ->[a] -> [[a]]
splitRs m 0 _ = []
splitRs m n xs = (take m xs):(splitRs m (n-1) (drop m xs))

-- funkcja zamieniająca listę list liczb na listę ciągów znaków odpowiadających kierunkom
transform :: [[Int]]->[String]
transform = (map.map) decode

-- funkcja wykonująca losowy spacer po planszy
-- zaczynając od punktu startowego (-3,0)
randWalk :: [String] -> [(Int,Int)]
randWalk = map (\x -> rwExe x (-3,0))

-- funkcja zliczająca stany końcowe
zlicz :: Ord a => [a] -> [(a,Int)]
zlicz xs = map (\x->(head x,length x)) (group $ sort xs)

-- główna funkcja wykonująca spacer losowy i zliczająca stany końcowe
calcFinalStates dl ile rint = 
  (zlicz . randWalk . transform) (splitRs dl ile rint)


main = do
  [sDl,sIle] <- getArgs
  let [dl,ile] = map read [sDl,sIle] :: [Int]
  gen <- getStdGen
  let randStream = randomRs (0::Int,3::Int) gen
  
  let fs = calcFinalStates dl ile randStream  

  putStrLn $ show fs
  
