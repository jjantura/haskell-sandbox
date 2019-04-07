main :: IO ()

delta :: Double -> Double -> Double -> Double
delta a b c = b * b - 4 * a * c

roots :: Double -> Double -> Double -> Maybe (Double, Double)
roots a b c | deltaValue < 0 = Nothing 
            | deltaValue == 0 = Just ((-b) / 2 * a, (-b) / 2 * a) 
            | deltaValue > 0 = Just (((-b) - sqrt(deltaValue)) / 2 * a, ((-b) + sqrt(deltaValue)) / 2 * a)
            where deltaValue = delta a b c

tfst :: (a, a, a) -> a
tfst (x, _, _) = x

tsnd :: (a, a, a) -> a
tsnd (_, y, _) = y

thrd :: (a, a, a) -> a 
thrd (_, _, z) = z

main = do
    putStrLn "Give a, b and c separated by comma: "
    params <- getLine
    let t = read $ "(" ++ params ++ ")" :: (Double, Double, Double) 
    print $ roots (tfst t) (tsnd t) (thrd t)  
