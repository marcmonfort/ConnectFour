import System.Random

randInt :: Int -> Int -> IO Int
-- randInt low high is an IO action that returns a
-- pseudo-random integer between low and high (both included).

randInt low high = do
    random <- randomIO :: IO Int
    let result = low + random `mod` (high - low + 1)
    return result

main :: IO ()
-- main program that throws two dice.

main = do
    r1 <- randInt 1 6
    r2 <- randInt 1 6
    print (r1, r2)
