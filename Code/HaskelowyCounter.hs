-- Counter structure with:
--  1. function that specifies what kind of elements are being counted 
--  2. string with counter description
--  3. Number of occurences of counted elements
data Counter a = C (a->Bool) String Int

-- Counter is initialized with:
--  1. function that specifies what kind of elements are being counted 
--  2. string with counter description
init_counter :: (a->Bool) -> String -> Counter a
init_counter f s = C f s 0

-- counting function; increases number of occurences if element a satisfies predicate f
modify :: Counter a -> a -> Counter a
modify (C f s x) a=
    if f a then (C f s (x+1))
    else (C f s x)

instance Show (Counter a) where
    show (C f s c) = s ++ ": " ++ (show c)

-- testing section
main = do
    -- let's do some counting :)
    -- TODO - map init_counter (just functions and descriptions in array?) DONE. Maybe aphabet list + mor mapings? :D
    let countersDescriptions = [((\x -> x=='a'), "a counter"), 
                                ((\x -> x=='b'), "b counter"), 
                                ((\x -> x=='c'), "c counter"), 
                                ((\x -> x=='d'), "d counter"), 
                                ((\x -> x=='e'), "e counter"), 
                                ((\x -> x=='f'), "f counter"), 
                                ((\x -> x=='g'), "g counter"), 
                                ((\x -> x=='h'), "h counter"), 
                                ((\x -> x=='j'), "i counter"), 
                                ((\x -> x=='j'), "j counter"), 
                                ((\x -> x=='k'), "k counter"), 
                                ((\x -> x=='l'), "l counter"), 
                                ((\x -> x=='m'), "m counter"), 
                                ((\x -> x=='n'), "n counter"), 
                                ((\x -> x=='o'), "o counter"), 
                                ((\x -> x=='p'), "p counter"), 
                                ((\x -> x=='r'), "r counter"), 
                                ((\x -> x=='s'), "s counter"), 
                                ((\x -> x=='t'), "t counter"), 
                                ((\x -> x=='u'), "u counter"), 
                                ((\x -> x=='v'), "v counter"), 
                                ((\x -> x=='w'), "w counter"), 
                                ((\x -> x=='x'), "x counter"), 
                                ((\x -> x=='y'), "y counter"), 
                                ((\x -> x=='z'), "z counter")]
    let countersSet = map (\(x,y) -> init_counter x y) countersDescriptions
    mapM (\x -> putStrLn(show x)) countersSet
    let results = foldl (\x y -> map (\z -> modify z y) x) countersSet "ala ma kota"
    mapM (\x -> putStrLn(show x)) results
