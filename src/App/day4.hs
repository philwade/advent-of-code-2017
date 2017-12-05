import Data.List
import System.IO

main :: IO ()
main =
    do
        inraw <- readFile "day4input"
        let inlines = lines inraw
        putStrLn $ show $ length $ filter (\x -> x == True) $ map isValid inlines
        putStrLn $ show $ isValid "aa bb cc dd ee"
        putStrLn $ show $ isValid "aa bb cc dd aa"
        putStrLn $ show $ isValid "aa bb cc dd aaa"
        putStrLn $ show $ length $ filter (\x -> x == True) $ map isValid2 inlines

isValid input =
    let
        inwords = words input
    in
        (length $ nub inwords) == (length inwords)

isValid2 input =
    let
        rawinwords = words input
        inwords = map (\x -> sort x) rawinwords
    in
        (length $ nub inwords) == (length inwords)
