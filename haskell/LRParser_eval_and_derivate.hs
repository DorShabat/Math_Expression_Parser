import qualified Data.Map as Map
import Data.Time.Clock


------------------------------------------------
-- tokenizer
data Token = NUMBER Double
           | VAR Char
           | FUNCTION String
           | OP Char
           | EndOfInput Char
           deriving (Show, Eq)
    
isDigit :: Char -> Bool
isDigit c = c >= '0' && c <= '9'

isAlpha :: Char -> Bool
isAlpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')

isOperator :: Char -> Bool
isOperator c = c `elem` "+-*/^()"

tokenize :: String -> [Token]
tokenize [] = [EndOfInput '$']
tokenize (c:cs)
    | isDigit c  = let (number, rest) = span (\x -> isDigit x || x == '.') (c:cs)
                   in NUMBER (read number) : tokenize rest
    | isAlpha c  = let (func, rest) = span isAlpha (c:cs)
                   in case func of
                        "X" -> VAR 'X' : tokenize rest
                        _   -> FUNCTION func : tokenize rest
    | isOperator c = OP c : tokenize cs
    | c == ' ' = tokenize cs  -- Skip whitespace
    | otherwise = error $ "Unexpected character: " ++ [c]

----------------------------------------------------------------------------

-- parsing table

type State = String
type Symbol = String
type Action = String

type ParsingTable = Map.Map State (Map.Map Symbol Action)

getParsingTable :: ParsingTable
getParsingTable = Map.fromList
    [ ("0",  Map.fromList 
        [ ("(", "s5"), ("-", "s6"), ("NUMBER", "s8"), ("X", "s9"), ("arccos", "s14"), 
          ("arcsin", "s13"), ("arctg", "s15"), ("cos", "s11"), ("exp", "s16"), ("ln", "s17"), 
          ("sin", "s10"), ("tg", "s12"), ("E", "1"), ("T", "2"), ("F", "3"), ("G", "4"), ("Func", "7") ]),
      ("1",  Map.fromList 
        [ ("+", "s18"), ("-", "s19"), ("$", "acc") ]),
      ("2",  Map.fromList 
        [ (")", "r( E -> T )"), ("*", "s20"), ("+", "r( E -> T )"), ("-", "r( E -> T )"), 
          ("/", "s21"), ("$", "r( E -> T )") ]),
      ("3",  Map.fromList 
        [ (")", "r( T -> F )"), ("*", "r( T -> F )"), ("+", "r( T -> F )"), ("-", "r( T -> F )"), 
          ("/", "r( T -> F )"), ("$", "r( T -> F )") ]),
      ("4",  Map.fromList 
        [ (")", "r( F -> G )"), ("*", "r( F -> G )"), ("+", "r( F -> G )"), ("-", "r( F -> G )"), 
          ("/", "r( F -> G )"), ("^", "s22"), ("$", "r( F -> G )") ]),
      ("5",  Map.fromList 
        [ ("(", "s5"), ("-", "s6"), ("NUMBER", "s8"), ("X", "s9"), ("arccos", "s14"), 
          ("arcsin", "s13"), ("arctg", "s15"), ("cos", "s11"), ("exp", "s16"), ("ln", "s17"), 
          ("sin", "s10"), ("tg", "s12"), ("E", "23"), ("T", "2"), ("F", "3"), ("G", "4"), ("Func", "7") ]),
      ("6",  Map.fromList 
        [ ("(", "s5"), ("-", "s6"), ("NUMBER", "s8"), ("X", "s9"), ("arccos", "s14"), 
          ("arcsin", "s13"), ("arctg", "s15"), ("cos", "s11"), ("exp", "s16"), ("ln", "s17"), 
          ("sin", "s10"), ("tg", "s12"), ("G", "24"), ("Func", "7") ]),
      ("7",  Map.fromList 
        [ ("(", "s25") ]),
      ("8",  Map.fromList 
        [ (")", "r( G -> NUMBER )"), ("*", "r( G -> NUMBER )"), ("+", "r( G -> NUMBER )"), 
          ("-", "r( G -> NUMBER )"), ("/", "r( G -> NUMBER )"), ("^", "r( G -> NUMBER )"), 
          ("$", "r( G -> NUMBER )") ]),
      ("9",  Map.fromList 
        [ (")", "r( G -> X )"), ("*", "r( G -> X )"), ("+", "r( G -> X )"), 
          ("-", "r( G -> X )"), ("/", "r( G -> X )"), ("^", "r( G -> X )"), 
          ("$", "r( G -> X )") ]),
      ("10", Map.fromList 
        [ ("(", "r( Func -> sin )") ]),
      ("11", Map.fromList 
        [ ("(", "r( Func -> cos )") ]),
      ("12", Map.fromList 
        [ ("(", "r( Func -> tg )") ]),
      ("13", Map.fromList 
        [ ("(", "r( Func -> arcsin )") ]),
      ("14", Map.fromList 
        [ ("(", "r( Func -> arccos )") ]),
      ("15", Map.fromList 
        [ ("(", "r( Func -> arctg )") ]),
      ("16", Map.fromList 
        [ ("(", "r( Func -> exp )") ]),
      ("17", Map.fromList 
        [ ("(", "r( Func -> ln )") ]),
      ("18", Map.fromList 
        [ ("(", "s5"), ("-", "s6"), ("NUMBER", "s8"), ("X", "s9"), ("arccos", "s14"), 
          ("arcsin", "s13"), ("arctg", "s15"), ("cos", "s11"), ("exp", "s16"), ("ln", "s17"), 
          ("sin", "s10"), ("tg", "s12"), ("T", "26"), ("F", "3"), ("G", "4"), ("Func", "7") ]),
      ("19", Map.fromList 
        [ ("(", "s5"), ("-", "s6"), ("NUMBER", "s8"), ("X", "s9"), ("arccos", "s14"), 
          ("arcsin", "s13"), ("arctg", "s15"), ("cos", "s11"), ("exp", "s16"), ("ln", "s17"), 
          ("sin", "s10"), ("tg", "s12"), ("T", "27"), ("F", "3"), ("G", "4"), ("Func", "7") ]),
      ("20", Map.fromList 
        [ ("(", "s5"), ("-", "s6"), ("NUMBER", "s8"), ("X", "s9"), ("arccos", "s14"), 
          ("arcsin", "s13"), ("arctg", "s15"), ("cos", "s11"), ("exp", "s16"), ("ln", "s17"), 
          ("sin", "s10"), ("tg", "s12"), ("F", "28"), ("G", "4"), ("Func", "7") ]),
      ("21", Map.fromList 
        [ ("(", "s5"), ("-", "s6"), ("NUMBER", "s8"), ("X", "s9"), ("arccos", "s14"), 
          ("arcsin", "s13"), ("arctg", "s15"), ("cos", "s11"), ("exp", "s16"), ("ln", "s17"), 
          ("sin", "s10"), ("tg", "s12"), ("F", "29"), ("G", "4"), ("Func", "7") ]),
      ("22", Map.fromList 
        [ ("(", "s5"), ("-", "s6"), ("NUMBER", "s8"), ("X", "s9"), ("arccos", "s14"), 
          ("arcsin", "s13"), ("arctg", "s15"), ("cos", "s11"), ("exp", "s16"), ("ln", "s17"), 
          ("sin", "s10"), ("tg", "s12"), ("F", "30"), ("G", "4"), ("Func", "7") ]),
      ("23", Map.fromList 
        [ (")", "s31"), ("+", "s18"), ("-", "s19") ]),
      ("24", Map.fromList 
        [ (")", "r( G -> - G )"), ("*", "r( G -> - G )"), ("+", "r( G -> - G )"), 
          ("-", "r( G -> - G )"), ("/", "r( G -> - G )"), ("^", "r( G -> - G )"), 
          ("$", "r( G -> - G )") ]),
      ("25", Map.fromList 
        [ ("(", "s5"), ("-", "s6"), ("NUMBER", "s8"), ("X", "s9"), ("arccos", "s14"), 
          ("arcsin", "s13"), ("arctg", "s15"), ("cos", "s11"), ("exp", "s16"), ("ln", "s17"), 
          ("sin", "s10"), ("tg", "s12"), ("E", "32"), ("T", "2"), ("F", "3"), ("G", "4"), ("Func", "7") ]),
      ("26", Map.fromList 
        [ (")", "r( E -> E + T )"), ("*", "s20"), ("+", "r( E -> E + T )"), 
          ("-", "r( E -> E + T )"), ("/", "s21"), ("$", "r( E -> E + T )") ]),
      ("27", Map.fromList 
        [ (")", "r( E -> E - T )"), ("*", "s20"), ("+", "r( E -> E - T )"), 
          ("-", "r( E -> E - T )"), ("/", "s21"), ("$", "r( E -> E - T )") ]),
      ("28", Map.fromList 
        [ (")", "r( T -> T * F )"), ("*", "r( T -> T * F )"), ("+", "r( T -> T * F )"), 
          ("-", "r( T -> T * F )"), ("/", "r( T -> T * F )"), ("$", "r( T -> T * F )") ]),
      ("29", Map.fromList 
        [ (")", "r( T -> T / F )"), ("*", "r( T -> T / F )"), ("+", "r( T -> T / F )"), 
          ("-", "r( T -> T / F )"), ("/", "r( T -> T / F )"), ("$", "r( T -> T / F )") ]),
      ("30", Map.fromList 
        [ (")", "r( F -> G ^ F )"), ("*", "r( F -> G ^ F )"), ("+", "r( F -> G ^ F )"), 
          ("-", "r( F -> G ^ F )"), ("/", "r( F -> G ^ F )"), ("$", "r( F -> G ^ F )") ]),
      ("31", Map.fromList 
        [ (")", "r( G -> ( E ) )"), ("*", "r( G -> ( E ) )"), ("+", "r( G -> ( E ) )"), 
          ("-", "r( G -> ( E ) )"), ("/", "r( G -> ( E ) )"), ("^", "r( G -> ( E ) )"), 
          ("$", "r( G -> ( E ) )") ]),
      ("32", Map.fromList 
        [ (")", "s33"), ("+", "s18"), ("-", "s19") ]),
      ("33", Map.fromList 
        [ (")", "r( G -> Func ( E ) )"), ("*", "r( G -> Func ( E ) )"), 
          ("+", "r( G -> Func ( E ) )"), ("-", "r( G -> Func ( E ) )"), 
          ("/", "r( G -> Func ( E ) )"), ("^", "r( G -> Func ( E ) )"), 
          ("$", "r( G -> Func ( E ) )") ])
    ]


lookupAction :: State -> Symbol -> ParsingTable -> Maybe Action
lookupAction state symbol table = 
    case Map.lookup state table of
        Just actions -> Map.lookup symbol actions
        Nothing -> Nothing


---------------------------------------------------------------------------

data LRParser = LRParser
    { parsingTable :: ParsingTable
    , xValue   :: Double  -- This represents the value of X
    }

data Mode = EVAL | DERIVATIVE deriving (Eq)

data Value = ValString String
           | ValDouble Double
           | ValNode Node
           deriving (Show, Eq)

data Node = Node
    { value    :: String
    , left     :: Maybe Node
    , right    :: Maybe Node
    }
    deriving (Show, Eq)


parse :: LRParser -> [Token] -> Mode -> Either String Value
parse parser tokens mode = parse' [0] tokens [] -- Initial state stack with state 0, empty value stack
    where
        parse' :: [Int] -> [Token] -> [Value] -> Either String Value
        parse' (state:states) (token:tokens) values =
            case lookupAction (show state) (tableColNames token) (parsingTable parser) of
                Just action ->
                    if take 1 action == "s" then
                        -- Shift operation
                        let nextState = read (drop 1 action) :: Int
                            newValues = if mode == EVAL 
                                then case token of
                                    NUMBER n    -> ValDouble n : values
                                    VAR 'X'     -> ValDouble (xValue parser) : values
                                    FUNCTION f  -> ValString f : values
                                    OP op       -> ValString [op] : values
                                    _           -> error "Unexpected token type"
                                else case token of -- DERIVATIVE mode
                                    NUMBER n    -> ValNode (Node (show n) Nothing Nothing) : values
                                    VAR 'X'     -> ValNode (Node "X" Nothing Nothing) : values
                                    FUNCTION f  -> ValString f : values
                                    OP op       -> ValString [op] : values
                                    _           -> error "Unexpected token type"
                        in parse' (nextState : state : states) tokens newValues
                    else if take 1 action == "r" then
                        -- Reduce operation
                        let (lhs, rhs) = parseProduction action
                            valuesToReduce = take (length rhs) values
                            remainingValues = drop (length rhs) values
                            remainingStats = drop (length rhs) (state : states)
                            result = if mode == EVAL
                                then evaluate parser lhs (reverse valuesToReduce)
                                else build_tree parser lhs (reverse valuesToReduce) -- mode == DERIVATIVE
                            -- Look up the new state after the reduction
                            newState = case lookupAction (show (head remainingStats)) lhs (parsingTable parser) of
                                Just nextAction -> read (nextAction) :: Int
                                Nothing         -> error "Failed to find next state during reduction"
                            newValues = result : remainingValues
                            -- Add newState to states stack
                            updatedStates = newState : remainingStats
                        in parse' updatedStates (token:tokens) newValues
                    else if action == "acc" then
                        if mode == EVAL 
                            then Right (head values)  -- Final result in EVAL mode
                            else case head values of  -- Mode DERIVATIVE
                                ValNode node -> Right (ValString (treeToString (differentiate node)))
                                _ -> error "Expected a node in DERIVATIVE mode"
                    else
                        Left "Unknown action"
                Nothing -> Left "Parsing error"          
        parse' _ [] _ = Left "Incomplete input"
                        

        tableColNames :: Token -> String
        tableColNames (NUMBER _) = "NUMBER"
        tableColNames (VAR _) = "X"
        tableColNames (FUNCTION func) = func
        tableColNames (OP op) = [op]
        tableColNames (EndOfInput _) = "$"


        parseProduction :: String -> (String, [String])
        parseProduction prod = 
            let (lhs, rhs) = break (== '-') (drop 2 (init prod)) -- "r( E -> T )" -> ("E", ["T"])
                trimmedLhs = trim lhs   -- Remove leading/trailing spaces from lhs
                trimmedRhs = words (drop 2 rhs) -- `words` already handles trimming spaces between symbols in rhs
            in (trimmedLhs, trimmedRhs)

        -- Helper function to trim spaces
        trim :: String -> String
        trim = unwords . words

evaluate :: LRParser -> String -> [Value] -> Value
evaluate _ "E" [v] = v
evaluate _ "E" [ValDouble v1, ValString "+", ValDouble v2] = ValDouble (v1 + v2)
evaluate _ "E" [ValDouble v1, ValString "-", ValDouble v2] = ValDouble (v1 - v2)

evaluate _ "T" [v] = v
evaluate _ "T" [ValDouble v1, ValString "*", ValDouble v2] = ValDouble (v1 * v2)
evaluate _ "T" [ValDouble v1, ValString "/", ValDouble v2] = ValDouble (v1 / v2)

evaluate _ "F" [v] = v
evaluate _ "F" [ValDouble v1, ValString "^", ValDouble v2] = ValDouble (v1 ** v2)

evaluate _ "G" [v] = v
evaluate _ "G" [ValString "-", ValDouble v] = ValDouble (-v)
evaluate _ "G" [ValString "(", v, _] = v
evaluate _ "G" [ValString func, ValString "(", ValDouble arg, ValString ")"] = ValDouble (applyFunction func arg)

-- If none of the patterns match, return the first value
evaluate _ _ (v:_) = v
evaluate _ _ _ = error "Unknown evaluation"


build_tree :: LRParser -> String -> [Value] -> Value
build_tree _ "E" [v] = v
build_tree _ "E" [ValNode n1, ValString "+", ValNode n2] = ValNode (Node "+" (Just n1) (Just n2))
build_tree _ "E" [ValNode n1, ValString "-", ValNode n2] = ValNode (Node "-" (Just n1) (Just n2))

build_tree _ "T" [v] = v
build_tree _ "T" [ValNode n1, ValString "*", ValNode n2] = ValNode (Node "*" (Just n1) (Just n2))
build_tree _ "T" [ValNode n1, ValString "/", ValNode n2] = ValNode (Node "/" (Just n1) (Just n2))

build_tree _ "F" [v] = v
build_tree _ "F" [ValNode n1, ValString "^", ValNode n2] = ValNode (Node "^" (Just n1) (Just n2))

build_tree _ "G" [v] = v
build_tree _ "G" [ValString "-", ValNode n] = ValNode (Node "-" Nothing (Just n))
build_tree _ "G" [ValString "(", v, ValString ")"] = v
build_tree _ "G" [ValString func, ValString "(", ValNode arg, ValString ")"] = ValNode (Node func Nothing (Just arg))

build_tree _ _ (v:_) = v
build_tree _ _ _ = error "Unknown tree construction"


differentiate :: Node -> Node
differentiate (Node "X" _ _) = Node "1" Nothing Nothing  -- Derivative of X is 1
differentiate (Node val _ _) 
    | isNumeric val = Node "0" Nothing Nothing  -- Derivative of a constant is 0
differentiate (Node "+" (Just l) (Just r)) = Node "+" (Just (differentiate l)) (Just (differentiate r))
differentiate (Node "-" Nothing (Just r)) = Node "-" Nothing (Just (differentiate r))
differentiate (Node "-" (Just l) (Just r)) = Node "-" (Just (differentiate l)) (Just (differentiate r))
differentiate (Node "*" (Just l) (Just r)) =
    Node "+" (Just (Node "*" (Just (differentiate l)) (Just r)))
             (Just (Node "*" (Just l) (Just (differentiate r))))
differentiate (Node "/" (Just l) (Just r)) =
    Node "/" (Just (Node "-" (Just (Node "*" (Just (differentiate l)) (Just r)))
                           (Just (Node "*" (Just l) (Just (differentiate r))))))
             (Just (Node "^" (Just r) (Just (Node "2" Nothing Nothing))))
differentiate (Node "^" (Just base) (Just exponent)) =
    Node "*" (Just (Node "^" (Just base) (Just exponent)))
             (Just (Node "+"
                   (Just (Node "*" (Just exponent)
                                  (Just (Node "/" (Just (differentiate base)) (Just base)))))
                   (Just (Node "*" (Just (Node "ln" Nothing (Just base)))
                                  (Just (differentiate exponent))))))
differentiate (Node "sin" Nothing (Just arg)) =
    Node "*" (Just (Node "cos" Nothing (Just arg))) (Just (differentiate arg))
differentiate (Node "cos" Nothing (Just arg)) =
    Node "*" (Just (Node "*" (Just (Node "-1" Nothing Nothing)) (Just (Node "sin" Nothing (Just arg))))) (Just (differentiate arg))
differentiate (Node "tg" Nothing (Just arg)) =
    Node "*" (Just (Node "/" (Just (Node "1" Nothing Nothing)) (Just (Node "^" (Just (Node "cos" Nothing (Just arg))) (Just (Node "2" Nothing Nothing)))))) (Just (differentiate arg))
differentiate (Node "arcsin" Nothing (Just arg)) =
    Node "*" 
        (Just (Node "/" (Just (Node "1" Nothing Nothing))
                            (Just (Node "^" (Just (Node "-" (Just (Node "1" Nothing Nothing)) (Just (Node "^" (Just arg) (Just (Node "2" Nothing Nothing)))))) (Just (Node "0.5" Nothing Nothing)))))) (Just (differentiate arg))
differentiate (Node "arccos" Nothing (Just right)) =
    Node "*" 
        (Just (Node "-1" Nothing Nothing)) 
        (Just (Node "*" 
                (Just (Node "/" 
                        (Just (Node "1" Nothing Nothing)) 
                        (Just (Node "^" 
                                (Just (Node "-" 
                                        (Just (Node "1" Nothing Nothing)) 
                                        (Just (Node "^" (Just right) (Just (Node "2" Nothing Nothing))))))
                                (Just (Node "0.5" Nothing Nothing))))))
                (Just (differentiate right))))

differentiate (Node "arctg" Nothing (Just right)) =
    Node "*" 
        (Just (Node "/" 
                (Just (Node "1" Nothing Nothing)) 
                (Just (Node "+" 
                        (Just (Node "1" Nothing Nothing)) 
                        (Just (Node "^" (Just right) (Just (Node "2" Nothing Nothing))))))))
        (Just (differentiate right))
differentiate (Node "exp" Nothing (Just arg)) =
    Node "*" (Just (Node "exp" Nothing (Just arg))) (Just (differentiate arg))
differentiate (Node "ln" Nothing (Just arg)) =
    Node "*" (Just (Node "/" (Just (Node "1" Nothing Nothing)) (Just arg))) (Just (differentiate arg))
differentiate node = error $ "Unsupported operation: " ++ value node

-- Helper function to check if a string is numeric
isNumeric :: String -> Bool
isNumeric = all (`elem` "0123456789.")

-- Function to convert a Node tree to a string
treeToString :: Node -> String
treeToString (Node val Nothing Nothing) = val
treeToString (Node val left right) =
    let leftStr  = maybe "" treeToString left
        rightStr = maybe "" treeToString right
    in if val `elem` ["cos", "sin", "ln", "tg", "arcsin", "arccos", "arctg", "exp"]
       then leftStr ++ " " ++ val ++ " (" ++ rightStr ++ ")"
       else "(" ++ leftStr ++ " " ++ val ++ " " ++ rightStr ++ ")"


applyFunction :: String -> Double -> Double
applyFunction "sin" arg = sin(arg)
applyFunction "cos" arg = cos(arg)
applyFunction "tg" arg  = tan(arg)
applyFunction "ln" arg  = log(arg)
applyFunction "exp" arg = exp(arg)
applyFunction "arcsin" arg = asin(arg)
applyFunction "arccos" arg = acos(arg)
applyFunction "arctg"  arg = atan(arg)
applyFunction funcName _ = error $ "Unknown function: " ++ funcName


-- More functions as needed
main :: IO ()
main = do
    tt_start <- getCurrentTime
    let parser = LRParser getParsingTable 2.0  -- Let's assume X = 2.0 for this example
    let tokens = tokenize "X^7+(X+11*2*X/4)+sin(X*2)+cos(sin(X))-(ln(81*X))*exp(2)+arctg(X/7)*(11-2*(X^3))-63*exp(X)/9+tg(18^(4*X))-(cos(X-6)/5*X)+X^X-X^X^X+X^2^X-X^X^X^X-2*X*17*sin(8*X-13)+87654"
    tt_end <- getCurrentTime
    let lrParserTokenizeRunTime =  diffUTCTime tt_end tt_start
    print ("LRParser + tokenize runtime = " ++ show lrParserTokenizeRunTime)
    tt_start <- getCurrentTime
    case parse parser tokens EVAL of
        Right result -> putStrLn $ "EVAL: " ++ show result
        Left errorMsg -> putStrLn $ "Error: " ++ errorMsg
    tt_end <- getCurrentTime
    let evalRunTime =  diffUTCTime tt_end tt_start
    print ("EVAL runtime = " ++ show evalRunTime)

    -- Then, evaluate in DERIVATIVE mode
    tt_start <- getCurrentTime
    case parse parser tokens DERIVATIVE of
        Right (ValString resultDERIVATIVE) -> do
            putStrLn $ "DERIVATIVE: " ++ resultDERIVATIVE
            tt_end <- getCurrentTime
            let derivativeRunTime =  diffUTCTime tt_end tt_start
            print ("DERIVATIVE runtime = " ++ show derivativeRunTime)
            -- Now parse the derivative result in EVAL mode
            let derivativeTokens = tokenize resultDERIVATIVE
            case parse parser derivativeTokens EVAL of
                Right evalDERIVATIVE -> putStrLn $ "evalDERIVATIVE: " ++ show evalDERIVATIVE
                Left errorMsg -> putStrLn $ "Error during evaluation of derivative: " ++ errorMsg
        Right _ -> putStrLn "DERIVATIVE did not produce a string."
        Left errorMsg -> putStrLn $ "Error: " ++ errorMsg
