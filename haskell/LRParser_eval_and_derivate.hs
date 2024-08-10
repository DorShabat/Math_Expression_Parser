import qualified Data.Map as Map
import Debug.Trace

---------------------------------------------------------------------------
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

getParsingTable3 :: ParsingTable
getParsingTable3 = Map.fromList
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
    trace (debugMessage state symbol result) result
  where
    result = case Map.lookup state table of
                Just actions -> Map.lookup symbol actions
                Nothing -> Nothing

    debugMessage :: State -> Symbol -> Maybe Action -> String
    debugMessage st sym res = "lookupAction called with State: " ++ show st ++ ", Symbol: " ++ show sym ++ ", Result: " ++ show res


---------------------------------------------------------------------------

data LRParser = LRParser
    { parsingTable :: ParsingTable
    , xValue   :: Double  -- This represents the value of X
    }

data Mode = Eval | Derivative

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
                -- Print the current state, token, and action
                trace ("State: " ++ show state ++ ", Token: " ++ show token ++ ", Action: " ++ action) $
                if take 1 action == "s" then
                    -- Shift operation
                    let nextState = read (drop 1 action) :: Int
                        newValues = case token of
                            NUMBER n    -> ValDouble n : values
                            VAR 'X'     -> ValDouble (xValue parser) : values
                            FUNCTION f  -> ValString f : values
                            OP op       -> ValString [op] : values
                            _           -> error "Unexpected token type"
                    in trace ("Shift to state: " ++ show nextState ++ ",(nextState : state : states): " ++ show (nextState : state : states) ++ ", Values: " ++ show newValues) $
                       parse' (nextState : state : states) tokens newValues
                else if take 1 action == "r" then
                    -- Reduce operation
                    let (lhs, rhs) = parseProduction action
                        valuesToReduce = take (length rhs) values
                        remainingValues = drop (length rhs) values
                        remainingStats = drop (length rhs) (state : states)

                        result = evaluate parser lhs (reverse valuesToReduce)
                        -- Look up the new state after the reduction
                        newState = case lookupAction (show (head remainingStats)) lhs (parsingTable parser) of
                            Just nextAction -> read (nextAction) :: Int
                            Nothing         -> error "Failed to find next state during reduction"
                        newValues = result : remainingValues
                        -- Add newState to states stack
                        
                        updatedStates = newState : remainingStats
                    -- Print the reduction and new state
                    in trace ("Reduce using " ++ action ++ ",valuesToReduce: " ++ show valuesToReduce ++ ",result: "++ show result ++ ", New state: " ++ show newState ++ ", updatedStates: " ++ show updatedStates ++ ", Values: " ++ show newValues) $
                       parse' updatedStates (token:tokens) newValues
                else if action == "acc" then
                    trace "Accept action reached, parsing complete." $
                    Right (head values)  -- Final result
                else
                    trace ("Unknown action: " ++ action) $
                    Left "Unknown action"
            Nothing -> trace "Parsing error: no action found." $
                       Left "Parsing error"
    parse' _ [] _ = trace "Incomplete input: no more tokens left." $
                    Left "Incomplete input"

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
evaluate _ "G" [ValString "(", v, ValString ")"] = v
evaluate _ "G" [ValString func, ValString "(", ValDouble arg, ValString ")"] = ValDouble (applyFunction func arg)

-- If none of the patterns match, return the first value
evaluate _ _ (v:_) = v
evaluate _ _ _ = error "Unknown evaluation"




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
    let parser = LRParser getParsingTable3 2.0  -- Let's assume X = 2.0 for this example
    --"X^2 + sin(X) - 3.14"
    --let tokens = tokenize "4*2 + 2^3 + 6/2 + sin(X)"
    let tokens = tokenize "(sin(2)+5*(2+3))"

    print tokens

    case parse parser tokens Eval of
        Right result -> putStrLn $ "Result: " ++ show result
        Left errorMsg -> putStrLn $ "Error: " ++ errorMsg

