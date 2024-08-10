--------------------------------
-- checking parseProduction
parseProduction :: String -> (String, [String])
parseProduction prod = 
    let (lhs, rhs) = break (== '-') (drop 2 (init prod)) -- "r( E -> T )" -> (" E ", ["T"])
        trimmedLhs = trim lhs   -- Remove leading/trailing spaces from lhs
        trimmedRhs = words (drop 2 rhs) -- `words` already handles trimming spaces between symbols in rhs
    in (trimmedLhs, trimmedRhs)

-- Helper function to trim spaces
trim :: String -> String
trim = unwords . words

----------------------------------


main :: IO ()
main = do
    ---------------------------
    -- checking parseProduction
    print "Checking parseProduction"
    let (lhs, rhs) = parseProduction "r( E -> T ^ R )"
    print lhs
    print rhs
    print "------------------------"
    ------------------------------

