{-- COM2108 Cracking The Code Assignment Solution
 -- Alicja Szwalek ece20as November/December 2022
--}

module Enigma where
  import Data.Char  -- to use functions on characters
  import Data.Maybe -- breakEnigma uses Maybe type
  import Data.List

  {- Part 1: Simulation of the Enigma -}

  type Rotor = (String, Int)
  type Reflector = [(Char, Char)]
  type Offsets = (Int, Int, Int)
  type Stecker = [(Char, Char)]
  type Cipher = String
  alphabet = ['A'..'Z']
   
  data Enigma = SimpleEnigma Rotor Rotor Rotor Reflector Offsets
                | SteckeredEnigma Rotor Rotor Rotor Reflector Offsets Stecker

  encodeMessage :: String -> Enigma -> String
  encodeMessage "" enigma = "" 
  encodeMessage (x:xs) (SimpleEnigma lr mr rr reflector offsets)
                | xs == "" = [encodeLetter (toUpper x) (SimpleEnigma lr mr rr reflector offsets)]
                | otherwise = encodeLetter (toUpper x) (SimpleEnigma lr mr rr reflector offsets):
                  encodeMessage (cleanInput xs) (SimpleEnigma lr mr rr reflector (advanceRotors offsets (mr, rr)))
  
  encodeMessage (x:xs) (SteckeredEnigma lr mr rr reflector offsets plugboard)
                | xs == "" = [encodeLetter (toUpper x) (SteckeredEnigma lr mr rr reflector offsets plugboard)]
                | otherwise = encodeLetter (toUpper x) (SteckeredEnigma lr mr rr reflector offsets plugboard):
                  encodeMessage (cleanInput xs)(SteckeredEnigma lr mr rr reflector (advanceRotors offsets (mr, rr)) plugboard)
  
  cleanInput :: String -> String
  cleanInput "" = ""
  cleanInput xs = filter (\x -> x `elem` ['A'..'Z']) (map toUpper xs)

  -- encode a letter with simple enigma
  encodeLetter :: Char -> Enigma -> Char
  encodeLetter x (SimpleEnigma lr mr rr reflector offsets) = 
                 decode rr ro (decode mr mo (decode lr lo 
                 (letterSwap reflector (encode lr lo (encode mr mo (encode rr ro x))))))
                    where (lo ,mo, ro) = (advanceRotors offsets (mr,rr))

  -- encode a letter with steckered enigma
  encodeLetter x (SteckeredEnigma lr mr rr reflector offsets plugboard) = 
                 letterSwap plugboard (decode rr ro (decode mr mo (decode lr lo 
                 (letterSwap reflector (encode lr lo (encode mr mo (encode rr ro (letterSwap plugboard x))))))))
                    where (lo ,mo, ro) = (advanceRotors offsets (mr,rr))

  -- letter-swap represents the work of a fixed reflector, it swaps the letters in each tuple
  letterSwap :: [(Char,Char)] -> Char -> Char
  letterSwap [] x = x
  letterSwap ((a, b) : xs) x  
            | x == a = b
            | x == b = a
            | otherwise = letterSwap xs x  

  -- calculates offsets for the three rotors
  advanceRotors :: Offsets -> (Rotor, Rotor) -> (Int, Int, Int)
  advanceRotors (lo, mo, ro) ((_, knock_on_MR),(_, knock_on_RR))
            | mo == knock_on_MR - 1 && ro == knock_on_RR - 1 = 
              ((lo + 1) `mod` 26 , (mo + 1) `mod` 26, (ro + 1) `mod` 26)                 
            | ro == knock_on_RR - 1 = (lo, (mo + 1) `mod` 26, (ro + 1) `mod` 26)
            | mo == knock_on_MR = ((lo + 1) `mod` 26, mo, (ro + 1) `mod` 26)
            | otherwise = (lo, mo, (ro + 1) `mod` 26)
  
  decode :: Rotor -> Int -> Char -> Char
  decode (rotor, _) offset x 
            = reverseEncode rotor (offset) (charPosition((alphaPos(x) + offset) `mod` 26))

  encode :: Rotor -> Int -> Char -> Char
  encode (rotor, _) offset x 
            = charPosition ((alphaPos (rotor !! ((alphaPos x + offset) `mod` 26)) - offset) `mod` 26)  

  charPosition :: Int -> Char
  charPosition n = chr (ord 'A' + n) 

  reverseEncode :: Cipher -> Int -> Char -> Char
  reverseEncode cipher offset x = alphabet !! (((itemIndex x cipher) - offset) `mod` 26)

  -- finds index of an item in a list
  itemIndex :: Eq a => a -> [a] -> Int
  itemIndex item [x] = 0
  itemIndex item (x:xs)
            | item == x = 0
            | otherwise = 1 + itemIndex item xs 
  

  {- Part 2: Finding the Longest Menu -}

  type Menu = [Integer] 
  type Crib = [(Char, Char)] 
  type CribTuple = (Char, Char, Integer)

  longestMenu :: Crib -> Menu
  longestMenu crib = maxLength [forwardChain (a, b, c) [(x, y, z) 
                  | ((x, y), z) <- zip crib [0..]]
                  | ((a, b), c) <- zip crib [0..]]

  {-- forwardChain finds the chain of connected tuples. If there is no plain text letter that equals
  -- the cipher text letter at index c, the menu is complete and it returns it
  --}

  forwardChain :: CribTuple -> [CribTuple] -> Menu
  forwardChain (a, b, c) crib
                  | findSameLetter (a, b, c) crib == [] = [c] 
                  | otherwise = maxLength [c:forwardChain (x, y, z) (replaceLetter c crib)
                  | (x, y, z) <- (findSameLetter (a, b, c) crib)]
  
  -- Given the list of all the menus ,returns the longest one
  maxLength :: [Menu] -> Menu
  maxLength [] = []
  maxLength (x:[]) = x
  maxLength (x:(a:b)) 
                  | b == [] && length x < length a = a
                  | b == [] && length x >= length a = x
                  | length x < length a = maxLength (a:b)
                  | otherwise = maxLength (x:b)    

  {-- Given tuple (a,b,c) and the lists of tuples, 
  -- it returns the list of linked tuples (x,y,z) where b = x  
  --}                                                                    
  findSameLetter :: CribTuple -> [CribTuple] ->[CribTuple]
  findSameLetter (a, b, c) [] = []
  findSameLetter (a, b, c) ((x, y, z):xs) 
                  | b == x = (x, y, z) : findSameLetter (a, b, c) xs
                  | otherwise = findSameLetter (a, b, c) xs

-- nth character replaced by '0'.
  replaceLetter :: Integer -> [CribTuple] -> [CribTuple]
  replaceLetter n ((a, b, c):xs) 
                  | (a,b,c):xs == [] = []
                  | xs == [] && c /= n = [(a,b,c)]
                  | xs == [] && c == n = [('0','0',c)]
                  | c == n = ('0', '0', c) : replaceLetter n xs
                  | otherwise = (a,b,c) : replaceLetter n xs


  {- Part 3: Simulating the Bombe -}

  type SteckerPair = (Char, Char)
  
  breakEnigma :: Crib -> Maybe (Offsets, Stecker)
  breakEnigma crib = convertSteckerList [((lo, mo, ro), possibleStecker) 
                  | lo <- [0..25], mo <- [0..25], ro <- [0..25], guess <- ['A'..'Z'],  
                  let possibleStecker = (findStecker (toTuples crib) menu [(plain, guess)] (lo, mo, ro)), 
                  not(nothingBool possibleStecker)] 
                  where 
                    menu = longestMenu crib
                    plain = fst (crib !! (fromIntegral (menu !! 0)))

 {-- recurses over the menu and updates the stecker at each step, if the stecker is invalid
  -- (Nothing) then return Nothing otherwise return the stecker
 --} 
  findStecker :: [CribTuple] -> Menu -> Stecker -> Offsets -> Maybe Stecker
  findStecker _ [] stecker _ = Just stecker 
  findStecker crib (m:ms) ((p, x):xs) offsets
                  | nothingBool newStecker = Nothing 
                  | otherwise = findStecker crib ms (unMaybe newStecker) offsets
                  where 
                    newStecker = uniqueSteckerPair (p', x') ((p, x):xs)    
                    x' = getCipherFromTuple crib m
                    p' = encodeLetter x (SimpleEnigma rotor1 rotor2 rotor3 reflectorB 
                      (advanceRotors' (rotor2, rotor3) offsets (fromIntegral m))) 
  
  nothingBool :: Maybe a -> Bool 
  nothingBool (Just _) = False
  nothingBool Nothing = True

  toTuples :: Crib -> [CribTuple]
  toTuples crib = [(a, b, c) | ((a, b), c) <- zip crib [0..]]

  convertSteckerList :: [(Offsets, Maybe Stecker)] -> Maybe (Offsets, Stecker)
  convertSteckerList [] = Nothing
  convertSteckerList ((offsets, stecker):xs) = Just (offsets, unMaybe stecker)                 

  -- advanceRotors recursively
  advanceRotors' :: (Rotor, Rotor) -> Offsets -> Int -> Offsets
  advanceRotors' rs offsets 1 = advanceRotors offsets rs
  advanceRotors' rs offsets x = advanceRotors' rs (advanceRotors offsets rs) (x - 1)

  -- adds unique SteckerPairs to the stecker and returns Nothing if there is a contradiction
  uniqueSteckerPair :: SteckerPair -> Stecker -> Maybe Stecker
  uniqueSteckerPair (a, b) stecker 
                  | filteredStecker == [] = Just (concat [stecker, [(a,b)]])
                  | filter (\(x, y) -> ((x == a && y == b) || (x == b && y == a))) filteredStecker /= [] = Just stecker
                  | otherwise = Nothing
                    where filteredStecker = filter (\(x, y) -> (x == a || x == b || y == a || y == b)) stecker

  getCipherFromTuple :: [CribTuple] -> Integer -> Char
  getCipherFromTuple ((_,cipher,index):cs) m | index == m = cipher
                                             | otherwise = getCipherFromTuple cs m

  unMaybe :: Maybe a -> a 
  unMaybe (Just x) = x

  {- Useful definitions and functions -}

   -- substitution cyphers for the Enigma rotors
   -- as pairs of (wirings, knock-on position)
   -- knock-on position is where it will cause the next left wheel to
   -- advance when it moves past this position
 
   --     "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

  rotor1=("EKMFLGDQVZNTOWYHXUSPAIBRCJ",17::Int)
  rotor2=("AJDKSIRUXBLHWTMCQGZNPYFVOE",5::Int)
  rotor3=("BDFHJLCPRTXVZNYEIWGAKMUSQO",22::Int)
  rotor4=("ESOVPZJAYQUIRHXLNFTGKDCMWB",10::Int)
  rotor5=("VZBRGITYUPSDNHLXAWMJQOFECK",0::Int)

  {- the standard Enigma reflector (Reflector B)
    swapped A<->Y, B<->R, C<->U,D<->H, E<->Q, F<->S, G<->L, 
            I<->P, J<->X, K<->N, M<->O, T<->Z,V<->W
  -}
  reflectorB= [('A','Y'),
              ('B','R'),
              ('C','U'),
              ('D','H'),
              ('E','Q'),
              ('F','S'),
              ('G','L'),
              ('I','P'),
              ('J','X'),
              ('K','N'),
              ('M','O'),
              ('T','Z'),
              ('V','W')]

  {- alphaPos: given an uppercase letter, returns its index in the alphabet
     ('A' = position 0; 'Z' = position 25)
   -}
  alphaPos :: Char -> Int
  alphaPos c = (ord c) - ord 'A'  