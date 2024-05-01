-- Counter structure with:
--  1. function that specifies what kind of elements are being counted 
--  2. string with counter description
--  3. Number of occurences of counted elements
data Counter = C Char String Int

-- Counter is initialized with:
--  1. function that specifies what kind of elements are being counted 
--  2. string with counter description
init_counter :: Char -> String -> Counter
init_counter c s = C c s 0

-- counting function; increases number of occurences if element a satisfies predicate f
modify :: Counter -> Char -> Counter 
modify (C c s x) c1 =
    if c == c1 then (C c s (x+1))
    else (C c s x)

instance Show Counter where
    show (C c s count) = s ++ ": " ++ (show count)

-- testing section
main = do
    -- let's do some counting :)
    -- TODO - map init_counter (just functions and descriptions in array?) DONE. Maybe aphabet list + mor mapings? :D
    let countersDescriptions = [('a', "a counter"),
                                ('m', "m counter")]
    let countersSet = map (\(x,y) -> init_counter x y) countersDescriptions
    mapM (\x -> putStrLn(show x)) countersSet
    let results = foldl (\x y -> map (\z -> modify z y) x) countersSet "ala ma kota"
    mapM (\x -> putStrLn(show x)) results
