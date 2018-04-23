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
import SymTab
import Safe
import Parser
import Interpreter
import Safe
import Data.Map (insert)
import Data.Maybe
import Data.Monoid
import Control.Monad.Trans.Except
import Text.Megaparsec

typeMess = "wrong type supplied to function"

binFunc ::
       (PhageVal -> Maybe a)
    -> (PhageVal -> Maybe b)
    -> (c -> PhageVal)
    -> (a -> b -> c)
    -> PhageVal
binFunc fromA fromB toC fn = mkFunc 2 nf
    where
        nf :: PhageFunc
        nf (a : b : xs) tab = case (fromA a, fromB b) of
            (Nothing, _) -> throwE $ typeMess
            (_, Nothing) -> throwE $ typeMess
            (Just a, Just b) -> return $ toC $ fn a b

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

mapSnd :: (a -> b) -> (c, a) -> (c, b)
mapSnd fn (a, b) = (a, fn b)

createOnInts = homogeneousBinFunc fromNum PNum

mkFunc :: Int -> PhageFunc -> PhageVal
mkFunc arity = PFunc arity [] mempty

truthy :: PhageVal -> Bool
truthy (PList []) = False
truthy (PBool a) = a
truthy (PNum 0) = False
truthy (PAtom "") = False
truthy _ = True

funcy :: PhageVal -> Bool
funcy (PFunc _ _ _ _) = True
funcy (PForm _ _) = True
funcy _ = False

fmapmapsnd = fmap . mapSnd

callErr s = throwE ("Unexpected call to " <> s)
formErr a b = callErr ("form " <> show a <> " " <> show b)
funcErr a b = callErr ("func " <> show a <> " " <> show b)

anyVal :: [(String, PhageVal)]
anyVal =
    concat $
    [ [ ("=", equals) ]
    , concat $ fmap (\(f, s) -> fmap (\w -> (w, booleyFunc s)) (words f))
        [ ("& and", (&&))
        , ("| or", (||))
        ]
    ] where
        equals = mkFunc 2 (\v t -> pure $ PBool $ func v t) where
            func [a, b] _ = a == b
            func (a : b : xs) t = a == b && func (b : xs) t

        booleyFunc = homogeneousBinFunc (Just . truthy) PBool

comparison :: [(String, PhageVal)]
comparison =
    fmapmapsnd (binFunc fromNum fromNum PBool)
        [ ("<", (<))
        , (">", (>))
        , ("<=", (<=))
        , (">=", (>=))
        ]

arith :: [(String, PhageVal)]
arith =
    fmapmapsnd (homogeneousBinFunc fromNum PNum)
        [ ("+", (+))
        , ("-", (-))
        , ("*", (*))
        -- TODO add exceptions on divide-by-zero
        , ("/", div)
        , ("%", mod)
        ]

consts :: [(String, PhageVal)]
consts =
    [ ("zero", PNum 0)
    , ("true", PBool True)
    , ("false", PBool False)
    ]

cardrs = concat $ fmap gen [1..6]
    where
        gen :: Int -> [String]
        gen 0 = ["r"]
        gen a = gen (a - 1) >>= \b -> ['a' : b, 'd' : b]

lists :: [(String, PhageVal)]
lists = concat
    [ fmapmapsnd (uncurry mkFunc)
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
    fmapmapsnd (uncurry mkFunc)
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
    fmapmapsnd (uncurry PForm)
    [ ("if", (3, ifFunc))
    , ("cond", (0, condFunc))
    , ("\\", (2, funcFunc))
    , ("def", (2, def))
    , ("let", (1, letFunc))
    , ("fun", (2, namedFun))
    , ("eval", (1, evalFunc))
    , ("import", (1, importFunc))
    , ("quote", (1, quoteFunc))
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
                let srefs = selfrefs <> ["rec"]
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
    , consts
    , specials
    , lists
    , anyVal
    , metaFuncs
    , [ ("print", mkFunc 0 prnt)
      ]
    ] where
        prnt :: PhageFunc
        prnt lst t =
            ret ((mapM (putStr . (<> " ") . show) lst) >> putStrLn "")
                (lastDef (PList []) lst)

        ret :: IO a -> b -> ExceptT PhageErr IO b
        ret a b = ExceptT $ const (Right b) <$> a

core :: SymTab PhageVal
core = newTab allVals mempty
