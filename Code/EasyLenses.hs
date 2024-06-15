-- data structures of Person with three levels of nesting
data Address = Address { _street :: String, _city :: String, _zip :: String } deriving Show
data Person = Person { _name :: String, _age :: Int, _address :: Address } deriving Show
data Student = Student { _person :: Person, _school :: String } deriving Show


data Lens s a = Lens (s -> a) (a -> s -> s)

compose :: Lens a b -> Lens b c -> Lens a c
compose (Lens get1 set1) (Lens get2 set2) = Lens (get2 . get1) (\c a -> set1 (set2 c (get1 a)) a)

view :: Lens s a -> s -> a
view (Lens get _) = get

set :: Lens s a -> s -> a -> s
set (Lens _ put) s a = put a s

-- lenses

personLens :: Lens Student Person
personLens = Lens _person (\p s -> s { _person = p })

--nameLens :: Lens Person String
--nameLens = Lens _name (\n p -> p { _name = n })

ageLens :: Lens Person Int
ageLens = Lens _age (\a p -> p { _age = a })

addressLens :: Lens Person Address
addressLens = Lens _address (\a p -> p { _address = a })

streetLens :: Lens Address String
streetLens = Lens _street (\s a -> a { _street = s })

cityLens :: Lens Address String
cityLens = Lens _city (\c a -> a { _city = c })

zipLens :: Lens Address String
zipLens = Lens _zip (\z a -> a { _zip = z })

zipPersonLens :: Lens Person String
zipPersonLens = addressLens `compose` zipLens

cityPersonLens :: Lens Person String
cityPersonLens = Lens (_city . _address) (\c p -> p { _address = ((_address p) { _city = c }) })

streetStudentLens :: Lens Student String
streetStudentLens = personLens `compose` addressLens `compose` streetLens


get :: Person -> String
get p = _name p
put :: String -> Person -> Person
put newname p =  p { _name = newname }
nameLens = Lens get put
-- PUTGET:
-- get ( put newname ( Person name age addr ) ) =
-- get ( Person newname age addr ) = newname
-- GETPUT:
-- put ( get ( Person name age addr ) ) ( Person name age addr ) =
-- put name ( Person name age addr ) = Person name age addr
-- PUTPUT:
-- put name1 ( put name2 ( Person n age addr ) ) =
-- put name1 ( Person name2 age addr ) = ( Person name2 age addr ) =
-- put name1 ( Person n age addr )



main :: IO ()
main = do
    let a = Address "123 Elm St" "Springfield" "12345"
    let p = Person "Homer" 40 a
    print $ view zipPersonLens p
    print $ set zipPersonLens p "54321"
    print $ view zipPersonLens p
    print $ set (addressLens `compose` cityLens) p "Worcester"
    print $ view cityPersonLens p
    print $ view cityPersonLens $ set cityPersonLens p "Worcester"

    -- Address updatetextt without lenses
    let a2 = a { _street = "456 Maple St" }

    let s = Student p "MIT"
    -- with lenses
    print $ view streetStudentLens s
    print $ set streetStudentLens s "456 Maple St"
    -- without lenses
    print $ _street (_address (_person s))
    -- set street without lenses
    print $ s {_person = (_person s) {_address = ((_address . _person) s) {_street = "456 Maple"}}}

    print $ view nameLens p
    print $ set nameLens p "Bart"