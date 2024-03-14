{-# LANGUAGE TemplateHaskell #-}
import Control.Lens
import Control.Lens.TH

data Person = Person { _name :: String, _age :: Int }
$(makeLenses ''Person)

main :: IO ()
main = do
    let p = Person "John" 30         
    putStrLn $ "Name: " ++ p^.name ++ ", age: " ++ show (p^.age)
    let p2 = p & name .~ "Mike"
    putStrLn $ "Name: " ++ p2^.name ++ ", age: " ++ show (p2^.age)
        