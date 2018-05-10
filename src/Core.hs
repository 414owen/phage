module Core
    ( core
    ) where

{-
 - Core contains a small set of fucntions necessary to
 - bootstrap a useful language, the idea being that the
 - standard library will be written in phage, using core.
 -}

import Err
import Val
import Safe
import Parser
import Interpreter
import Safe
import Data.Tuple.Lazy
import Data.Map (insert)
import Data.Maybe
import Data.Monoid
import Control.Monad.Trans.Except
import Text.Megaparsec

typeMess = "wrong type supplied to function"

type PhageFuncRes = ExceptT PhageErr IO PhageVal
type PhageFunc = [PhageVal] -> SymTab -> PhageFuncRes
type SimFunc = [PhageVal] -> PhageFuncRes

quoteFunc :: PhageForm
quoteFunc [a] t = pure $ (a, t)
quoteFunc v _ = formErr v

defForm = PForm 0 [] (\t v -> pure (v, t)) quoteFunc Nothing False

mkBoundFunc :: Int -> PhageFunc -> SymTab -> Maybe String -> PhageVal
mkBoundFunc arity fn env name = PForm arity [] eval
        (\ps c -> (, c) <$> fn ps env) name True

mkFunc :: Int -> PhageFunc -> PhageVal
mkFunc arity fn = mkBoundFunc arity fn mempty Nothing

mkSimFunc ar fn = mkFunc ar (\ps tb -> fn ps)

-- these functions apply to their first argument, and
-- ignore subsequent arguments
unaryFunc ::
       (PhageVal -> Maybe a)
    -> (b -> PhageVal)
    -> (a -> b)
    -> PhageVal
unaryFunc from to fn = mkFunc 1 nf
    where
        nf (x : xs) _ = case from x of
            Just a -> pure $ to $ fn a
            _ -> throwE typeMess


-- these functions take the form (a -> b -> c)
-- but if more arguments are passed in, they are ignored
binFunc ::
       (PhageVal -> Maybe a)
    -> (PhageVal -> Maybe b)
    -> (c -> PhageVal)
    -> (a -> b -> c)
    -> PhageVal
binFunc fromA fromB toC fn = mkSimFunc 2 nf
    where
        nf :: SimFunc
        nf (a : b : xs) = case (fromA a, fromB b) of
            (Just a, Just b) -> return $ toC $ fn a b
            _ -> throwE typeMess


-- these functions continue passing values onwards, like folds,
-- but curry until two parameters are applied eg.
-- (+)         -> function
-- (+ 1)       -> function
-- (+ 1 2)     -> 3
-- (+ 1 2 3 4) -> 10
homogeneousBinFunc :: Eq a =>
       (PhageVal -> Maybe a)
    -> (a -> PhageVal)
    -> (a -> a -> a)
    -> PhageVal
homogeneousBinFunc from to fn = mkSimFunc 2 func
    where
        func vals =
            let fromvals = from <$> vals
                valid = not $ any (== Nothing) fromvals
                nvals = catMaybes fromvals
            in
                if valid
                    then pure $ to $
                        foldl (\a b -> fn a b) (head nvals) (tail nvals)
                    else throwE typeMess


fromNum :: (PhageVal -> Maybe Integer)
fromNum (PNum a) = Just a
fromNum _ = Nothing

truthy :: PhageVal -> Bool
truthy (PList []) = False
truthy (PBool a) = a
truthy (PNum 0) = False
truthy (PAtom "") = False
truthy _ = True

callErr a b = throwE ("Unexpected call to " <> a <> ": " <> show b)
formErr :: Show a => a -> ExceptT String IO b
formErr = callErr "form"
funcErr = callErr "func"

flat :: [([String], a)] -> [(String, a)]
flat = concat . fmap (\(ns, v) -> (, v) <$> ns)

anyVal :: [(String, PhageVal)]
anyVal =
    concat $
    [ [ ("=", equals)
      , ("!", notFunc)]
    , (fmap . mapSnd) booleyFunc
        [ ("&", (&&))
        , ("|", (||))
        ]
    ] where
        equals = mkFunc 2 (\v t -> pure $ PBool $ func v t) where
            func [a, b] _ = a == b
            func (a : b : xs) t = a == b && func (b : xs) t

        notFunc = unaryFunc (Just . truthy) PBool not

        booleyFunc = homogeneousBinFunc (Just . truthy) PBool

comparison :: [(String, PhageVal)]
comparison =
    (fmap . mapSnd) (binFunc fromNum fromNum PBool)
        [ ("<", (<))
        , (">", (>))
        , ("<=", (<=))
        , (">=", (>=))
        ]

arith :: [(String, PhageVal)]
arith =
    (fmap . mapSnd) (homogeneousBinFunc fromNum PNum)
        [ ("+", (+))
        , ("-", (-))
        , ("*", (*))
        -- TODO add exceptions on divide-by-zero
        , ("/", div)
        , ("%", mod)
        ]

bools :: [(String, PhageVal)]
bools =
    [ ("true", PBool True)
    , ("false", PBool False)
    ]

cardrs = concat $ fmap gen [1..6]
    where
        gen :: Int -> [String]
        gen 0 = ["r"]
        gen a = gen (a - 1) >>= \b -> ['a' : b, 'd' : b]

lists :: [(String, PhageVal)]
lists = concat
    [ (fmap . mapSnd) (uncurry mkFunc)
      [ ("cons", (2, consFunc)) ]
    , fmap mkardr cardrs
    ] where
        consFunc (x : PList xs : _) _ = pure $ PList (x : xs)
        consFunc v _ = funcErr v

        mkardrFunc :: String -> String -> PhageFunc
        mkardrFunc n ('a' : rst) [PList (x : xs)] t = mkardrFunc n rst [x] t
        mkardrFunc n ('d' : rst) [PList (x : xs)] t = mkardrFunc n rst [PList xs] t
        mkardrFunc _ "r" [val] _ = pure val
        mkardrFunc _ _ l v = formErr v

        mkardr :: String -> (String, PhageVal)
        mkardr adr = let name = 'c' : adr in
            (name, mkFunc 1 $ mkardrFunc name adr)

metaFuncs :: [(String, PhageVal)]
metaFuncs =
    [ ("arity", mkFunc 1 arFunc)
    , ("apply", defForm {arity=2, form=apFunc, paramap=eval})
    ] where
        arFunc :: PhageFunc
        arFunc (PForm{arity=ar} : _) _ = pure $ PNum $ toInteger $ ar

        apFunc :: PhageForm
        apFunc (f@PForm{} : PList args : xs) tab =
            apply tab f args
        apFunc v _ = funcErr v

specials :: [(String, PhageVal)]
specials =
    fmap (\(n, ar, fn) -> (n, defForm {arity=ar, form=fn, name=(Just n)}))
    [ ("if",     3, ifFunc)
    , ("cond",   0, condFunc)
    , ("\\",     2, funcFunc)
    , ("def",    2, def)
    , ("let",    1, letFunc)
    , ("fun",    2, namedFun)
    , ("eval",   1, evalFunc)
    , ("import", 1, importFunc)
    , ("quote",  1, quoteFunc)
    ] where

        importFunc :: PhageForm
        importFunc [PStr fname] t =
            ExceptT $ readFile fname >>= imp
            where imp s = case parse parseAst fname s of
                    Left s -> return $ Left $ show s
                    Right ast -> runExceptT $ interpret t ast
        importFunc v _ = formErr v

        evalFunc :: PhageForm
        evalFunc [a] t = eval t a >>= \(a, _) -> eval t a
        evalFunc v _ = formErr v

        param (PAtom str) = pure str
        param thing = throwE $ "Invalid parameter name: " <> show thing

        runFunc :: [PhageVal] -> [String] -> PhageFunc
        runFunc blk strs params oldtab =
            let entries = [ ("args", PList params)
                          , ("rest", PList (drop (length strs) params))
                          ]
                tab = newTab (entries <> zip strs params) oldtab
            in  lastDef (PList []) <$> block tab blk

        funcreate :: [PhageVal] -> SymTab -> [String] -> Maybe String -> PhageRes
        funcreate (PList strs : blk) tab selfrefs name = mapM param strs
            >>= \strs ->
                let srefs = selfrefs <> ["rec", "this"]
                    f = mkBoundFunc (length strs)
                        (runFunc blk strs)
                        (newTab (zip srefs (repeat f)) tab)
                        name
                in  pure (f, tab)
        funcreate v _ _ _ = formErr v

        -- named functions support recursion
        namedFun :: PhageForm
        namedFun (PAtom name : params) tab =
            (\(v, t) -> (v, Data.Map.insert name v t)) <$>
                funcreate params tab [name] (Just name)
        namedFunc v _ = formErr v

        letFunc' :: PhageVal -> PhageForm
        letFunc' res [] t = pure (res, t)
        letFunc' res (PList [PAtom str, val] : others) t = eval t val
            >>= \(val, ntab) -> letFunc' val others (insert str val t)
        letFunc' _ v _ = formErr v

        letFunc :: PhageForm
        letFunc = letFunc' (PList [])

        def :: PhageForm
        def [PAtom a, b] t = letFunc [PList [PAtom a, b]] t
        def v _ = formErr v

        funcFunc :: PhageForm
        funcFunc params tab = funcreate params tab [] Nothing

        condFunc :: PhageForm
        condFunc [] tab = throwE "condition not met"
        condFunc (PList [pred, val] : nodes) tab = eval tab pred
            >>= \(pred, t) -> if truthy pred
                then eval tab val
                else condFunc nodes tab
        condFunc v _ = formErr v

        ifFunc :: PhageForm
        ifFunc [a, b, c] t = condFunc [PList [a, b], PList [PNum 1, c]] t
        ifFunc v _ = formErr v

allVals :: [(String, PhageVal)]
allVals = fmap nameThings $ concat
    [ arith
    , comparison
    , bools
    , specials
    , lists
    , anyVal
    , metaFuncs
    , [ ("print", mkSimFunc 0 prnt)
      ]
    ]
    where
        nameThings (s, f@PForm{name=n}) = (s, f {name=Just s})
        nameThings a = a

        prnt :: SimFunc
        prnt lst =
            ret ((mapM (putStr . (<> " ") . show) lst) >> putStrLn "")
                (lastDef (PList []) lst)

        ret :: IO a -> b -> ExceptT PhageErr IO b
        ret a b = ExceptT $ const (Right b) <$> a

core :: SymTab
core = newTab allVals mempty
