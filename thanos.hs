import Data.List

thanos :: [Int] -> [Int]
thanos [] = []
thanos [x] = [x]
thanos xs = if (xs == sort xs) then xs else thanos $ take ((length xs) `div`  2) xs     
