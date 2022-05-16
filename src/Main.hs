
main = do
  putStrLn "Hello"
  putStrLn "World"


emptyLevel :: (Int,Int) -> [[Int]]
emptyLevel (_,0) = []
emptyLevel dim = replicate (fst dim) 0:emptyLevel (fst dim, snd dim - 1)
