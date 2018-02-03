{-# LANGUAGE LambdaCase #-}

module Marriage where

import Data.Array
import Data.Maybe (fromJust, isJust)
import Data.List (elemIndex, permutations)


data MatchingEnv m w
    = MatchingEnv
          (m, m)
          (w, w)
          -- earlier in the list => preferred
          (Array m [w])
          (Array w [m])

data MatchingProg m w
    = MatchingProg
          (Array m (Maybe w))
          (Array (m, w) Bool)
          m

stableMatch :: (Num man, Ix man, Ix woman)
            => MatchingEnv man woman
            -> [(man, woman)]
stableMatch env = -- TODO : no partial functions here
    let initial = newMatchingProg env
        result = until everyoneHasBeenMatched (iterateMatching env) initial
        result2 = untilJust extractMarriages3 (iterateMatching env) initial
    in result2

iterateMatching :: (Num man, Ix man, Ix woman)
            => MatchingEnv man woman
            -> MatchingProg man woman
            -> MatchingProg man woman
iterateMatching env@(MatchingEnv boundsM boundsW prefsM prefsW) prog@(MatchingProg currentEngs allProps pos) =
    case currentEngs ! pos of
        Just _  -> MatchingProg currentEngs allProps $ nextPos boundsM pos
        Nothing -> 
            let hd : freshWomen = filter (\w -> not $ allProps ! (pos, w)) $ prefsM ! pos
            in propose env prog hd
                    
propose :: (Num man, Ix man, Ix woman)
            => MatchingEnv man woman
            -> MatchingProg man woman
            -> woman
            -> MatchingProg man woman
propose (MatchingEnv boundsM _ _ prefsW) (MatchingProg currentEngs allProps pos) w =
    let newEngs = case filter ((== Just w) . snd) $ assocs currentEngs of
                    [] -> currentEngs // [(pos, Just w)]
                    (oldGuy, _) : [] -> case prefsW ! w of
                                          prefs -> if index' pos prefs < index' oldGuy prefs then
                                                       currentEngs // [(oldGuy, Nothing), (pos, Just w)] else
                                                       currentEngs
    in MatchingProg newEngs (allProps // [((pos, w), True)]) (nextPos boundsM pos)

index' :: Eq a => a -> [a] -> Int
index' a as = fromJust $ elemIndex a as
       
nextPos :: (Num a, Eq a) => (a, a) -> a -> a
nextPos (lower, upper) a =
    if upper == a then
        lower else
        a + 1

untilJust :: (a -> Maybe b) -> (a -> a) -> a -> b
untilJust f g a =
    case f a of
      Just b -> b
      Nothing -> untilJust f g $ g a
             
newMatchingProg :: (Ix man, Ix woman) => MatchingEnv man woman -> MatchingProg man woman
newMatchingProg (MatchingEnv boundsM boundsW _ _) =
    MatchingProg
        (listArray boundsM $ repeat Nothing) -- all Nothing
        (listArray ((fst boundsM, fst boundsW), (snd boundsM, snd boundsW)) $ repeat False) -- all False
        (fst boundsM)
                  
everyoneHasBeenMatched :: (Num man, Ix man, Ix woman) => MatchingProg man woman -> Bool
everyoneHasBeenMatched (MatchingProg arr _ _) =
    all isJust $ elems arr
                  
extractMarriages :: (Ix man, Ix woman) => MatchingProg man woman -> [(man, woman)]
extractMarriages (MatchingProg engMapping _ _) =
    fmap (\(a, Just b) -> (a, b)) $ assocs engMapping

extractMarriages2 :: (Ix man, Ix woman) => MatchingProg man woman -> Maybe [(man, woman)]
extractMarriages2 (MatchingProg engMapping _ _) =
    let engagements = fmap (\case
                                 (a, Just b) -> Just (a, b)
                                 (_, Nothing) -> Nothing
                           ) $ assocs engMapping
    in sequence engagements

extractMarriages3 :: (Ix man, Ix woman) => MatchingProg man woman -> Maybe [(man, woman)]
extractMarriages3 (MatchingProg engMapping _ _) =
    sequence $ sequence <$> assocs engMapping
       
simple :: MatchingEnv Int Integer
simple =
    let boundsM = (1, 10)
        boundsW = (1, 10)
    in MatchingEnv boundsM boundsW (listArray boundsM $ repeat [1 .. 10]) (listArray boundsW $ repeat [1 .. 10])

galeShapleyExample :: MatchingEnv Int Integer
galeShapleyExample =
    let prefsM = listArray (1, 3) [[1, 2, 3], [2, 3, 1], [3, 1, 2]]
        prefsW = listArray (1, 3) [[2, 3, 1], [3, 1, 2], [1, 2, 3]]
    in MatchingEnv (1, 3) (1, 3) prefsM prefsW
