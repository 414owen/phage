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
binFunc fromA fromB toC fn = mkFunc 2 nf
    where
        nf :: PhageFunc
        nf (a : b : xs) _ = case (fromA a, fromB b) of
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
homogeneousBinFunc from to fn = mkFunc 2 func
    where
        func vals _ =
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

mkFunc :: Int -> PhageFunc -> PhageVal
mkFunc arity = PFunc arity [] mempty

truthy :: PhageVal -> Bool
truthy (PList []) = False
truthy (PBool a) = a
truthy (PNum 0) = False
truthy (PAtom "") = False
truthy _ = True

callErr s = throwE ("Unexpected call to " <> s)
formErr a b = callErr ("form " <> show a <> " " <> show b)
funcErr a b = callErr ("func " <> show a <> " " <> show b)

anyVal :: [(String, PhageVal)]
anyVal =
    concat $
    [ [ ("= eq", equals)
      , ("not !", notFunc)]
    , (fmap . mapSnd) booleyFunc
        [ ("& and", (&&))
        , ("| or", (||))
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
        [ ("< lt", (<))
        , ("> gt", (>))
        , ("<= lte", (<=))
        , (">= gte", (>=))
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
    [ ("true t", PBool True)
    , ("false f", PBool False)
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
        consFunc [x, PList xs] _ = pure $ PList (x : xs)
        consFunc v _ = funcErr "cons" v

        mkardrFunc :: String -> String -> PhageFunc
        mkardrFunc n ('a' : rst) [PList (x : xs)] t = mkardrFunc n rst [x] t
        mkardrFunc n ('d' : rst) [PList (x : xs)] t = mkardrFunc n rst [PList xs] t
        mkardrFunc _ "r" [val] _ = pure val
        mkardrFunc name _ l v = formErr (name <> ": " <> show l) v

        mkardr :: String -> (String, PhageVal)
        mkardr adr = let name = 'c' : adr in
            (name, mkFunc 1 $ mkardrFunc name adr)

metaFuncs :: [(String, PhageVal)]
metaFuncs =
    (fmap . mapSnd) (uncurry mkFunc)
    [ ("arity", (1, arFunc))
    , ("apply", (2, apFunc))
    ] where
        arFunc :: PhageFunc
        arFunc [PFunc ar ps _ _] t = pure $
            PNum $ toInteger $ max (ar - (length ps)) 0
        arFunc [PForm ar _] t = pure $ PNum $ toInteger $ ar
        arFunc v _ = funcErr "arity" v

        apFunc :: PhageFunc
        apFunc [PForm ar fn, PList args] tab =
            fst <$> (apply tab (PForm ar fn) args)
        apFunc [PFunc ar bound tab fn, PList args] _ =
            fst <$> (apply tab (PFunc ar bound tab fn) args)
        apFunc v _ = funcErr "apply" v

specials :: [(String, PhageVal)]
specials =
    (fmap . mapSnd) (uncurry PForm)
    [ ("if", (3, ifFunc))
    , ("cond", (0, condFunc))
    , ("\\", (2, funcFunc))
    , ("def var define", (2, def))
    , ("let", (1, letFunc))
    , ("fun fn", (2, namedFun))
    , ("eval", (1, evalFunc))
    , ("import require using", (1, importFunc))
    , ("quote data", (1, quoteFunc))
    ] where
        quoteFunc :: PhageForm
        quoteFunc [a] t = pure $ (a, t)
        quoteFunc v _ = formErr "quote" v

        importFunc :: PhageForm
        importFunc [PStr fname] t =
            ExceptT $ readFile fname >>= imp
            where
                imp s = case parse parseAst fname s of
                    Left s -> return $ Left $ show s
                    Right ast -> runExceptT $ interpret t ast
        importFunc v _ = formErr "import" v

        evalFunc :: PhageForm
        evalFunc [a] t = eval t a >>= \(v, t) -> eval t v
        evalFunc v _ = formErr "eval" v

        param (PAtom str) = pure str
        param thing = throwE $ "Invalid parameter name: " <> show thing

        runFunc :: [PhageVal] -> [String] -> PhageFunc
        runFunc blk strs params oldtab =
            let entries = [ ("args", PList params)
                          , ("rest", PList (drop (length strs) params))
                          ]
                tab = newTab (entries <> zip strs params) oldtab
            in  lastDef (PList []) <$> block tab blk

        funcreate (PList strs : blk) tab selfrefs = mapM param strs
            >>= \strs ->
                let srefs = selfrefs <> ["rec", "this"]
                    f = PFunc (length strs) []
                        (newTab (zip srefs (repeat f)) tab)
                        (runFunc blk strs)
                in  pure f
        funcreate v _ _ = formErr "function" v

        -- named functions support recursion
        namedFun :: PhageForm
        namedFun (PAtom name : params) tab = funcreate params tab [name]
            >>= \f -> pure (f, insert name f tab)
        namedFunc v _ = formErr "fun" v

        letFunc' :: PhageVal -> PhageForm
        letFunc' res [] t = pure (res, t)
        letFunc' res (PList [PAtom str, val] : others) t = eval t val
            >>= \(val, ntab) -> letFunc' val others (insert str val t)
        letFunc' _ v _ = formErr "let" v

        letFunc :: PhageForm
        letFunc = letFunc' (PList [])

        def :: PhageForm
        def [PAtom a, b] tab = letFunc [PList [PAtom a, b]] tab
        def v _ = formErr "def" v

        funcFunc :: PhageForm
        funcFunc params tab = (,tab) <$> funcreate params tab []

        condFunc :: PhageForm
        condFunc [] tab = throwE "condition not met"
        condFunc (PList [pred, val] : nodes) tab = eval tab pred
            >>= \(pred, t) -> if truthy pred
                then eval tab val
                else condFunc nodes tab
        condFunc v _ = formErr "cond" v

        ifFunc :: PhageForm
        ifFunc [a, b, c] t = condFunc [PList [a, b], PList [PNum 1, c]] t
        ifFunc v _ = formErr "if" v

allVals :: [(String, PhageVal)]
allVals = concat
    [ arith
    , comparison
    , bools
    , specials
    , lists
    , anyVal
    , metaFuncs
    , [ ("print", mkFunc 0 prnt)
      ]
    ] >>= \(ss, val) -> (, val) <$> words ss
    where
        prnt :: PhageFunc
        prnt lst t =
            ret ((mapM (putStr . (<> " ") . show) lst) >> putStrLn "")
                (lastDef (PList []) lst)

        ret :: IO a -> b -> ExceptT PhageErr IO b
        ret a b = ExceptT $ const (Right b) <$> a

core :: SymTab
core = newTab allVals mempty
