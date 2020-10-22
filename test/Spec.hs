main :: IO ()
main = do
  putStrLn "playground stuff"
  print mkseq

mkseq :: [(Int, Int)]
--mkseq = [(x,y) | x <- [1,2,3] , y <- [1,2,3], x /= y]
{-
-mkseq = do
  x <- [1, 2, 3]
  y <- [1, 2, 3]
  True <- return (x /= y)
  return (x, y)
-}

mkseq = [1,2,3] >>= (\ x -> [1,2,3] >>= (\y -> return (x/=y) >>=
   (\r -> case r of True -> return (x,y)
                    _    -> fail "")))

power :: Int -> Int
power x = x * x

sauce :: [Rational]
sauce = fmap (toRational . power . read) ["1", "2", "3"]

melt :: Rational
melt = foldl (*) 0 sauce

cstyle :: [String] -> IO Int
cstyle args = do
  let a = 1
  let b = 6
  return (a + b);
