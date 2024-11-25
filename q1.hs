main :: IO ()
main = do
    input <- readFile "./inputs/q1.txt"
    print $ foldr (\c acc -> cost c + acc) 0 input

    input2 <- readFile "./inputs/q1_1.txt"
    print . part2 $ input2

    input3 <- readFile "./inputs/q1_2.txt"
    print . part3 $ input3

part2 :: String -> Int
part2 str = foldr (\pair acc -> pairCost pair + acc) 0 $ (pairs . takeWhile (/= '\n')) str

part3 :: String -> Int
part3 str = foldr (\triple acc -> tripleCost triple + acc) 0 $ (triples . takeWhile (/= '\n')) str

cost :: Char -> Int
cost c  | c == 'B'  = 1
        | c == 'C'  = 3
        | c == 'D'  = 5
        | otherwise = 0

pairCost :: (Char, Char) -> Int
pairCost (a, b) = case (a, b) of
                    (a, 'x')    -> cost a
                    ('x', b)    -> cost b
                    (a, b)      -> cost a + cost b + 2

pairs :: [Char] -> [(Char, Char)]
pairs []       = []
pairs (a:b:cs) = (a, b) : pairs cs

tripleCost :: (Char, Char, Char) -> Int
tripleCost (a, b, c) = foldr (\c acc -> cost c + acc) 0 cs + (extras . length . filter (/= 'x')) cs
                        where
                            cs = [a, b, c]
                            extras n | n == 2    = n
                                     | n == 3    = n * 2
                                     | otherwise = 0

triples :: [Char] -> [(Char, Char, Char)]
triples []         = []
triples (a:b:c:cs) = (a, b, c) : triples cs
