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
type PhageFunc = SymTab -> [PhageVal] -> PhageFuncRes
type SimFunc = [PhageVal] -> PhageFuncRes

quote t [v] = pure ([], v)
defForm = PForm 0 [] (\_ v -> pure ([], v)) quote Nothing False

mkBoundFunc :: Int -> PhageFunc -> SymTab -> Maybe String -> PhageVal
mkBoundFunc arity fn env name = PForm arity [] eval
         (funToForm fn env) name True

funToForm :: PhageFunc -> SymTab -> PhageForm
funToForm fn e c ps = ([],) <$> fn e ps

mkFunc :: Int -> PhageFunc -> PhageVal
mkFunc arity fn = mkBoundFunc arity fn mempty Nothing

mkSimFunc :: Int -> SimFunc -> PhageVal
mkSimFunc ar fn = mkFunc ar (\_ ps -> fn ps)

-- these functions apply to their first argument, and
-- ignore subsequent arguments
unaryFunc :: (PhageVal -> Maybe a) -> (b -> PhageVal) -> (a -> b) -> PhageVal
unaryFunc from to fn = mkSimFunc 1 nf
    where nf (x : xs) = case from x of
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
    where nf (a : b : xs) = case (fromA a, fromB b) of
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
        equals = mkFunc 2 (\t v -> pure $ PBool $ func v) where
            func [a, b] = a == b
            func (a : b : xs) = a == b && func (b : xs)

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
    [ [ ("cons", mkSimFunc 2 consFunc) ]
    , fmap mkardr cardrs
    ] where
        consFunc :: SimFunc
        consFunc (x : PList xs : _) = pure $ PList (x : xs)
        consFunc v = funcErr v

        mkardrFunc :: String -> SimFunc
        mkardrFunc ('a' : rst) [PList (x : xs)] = mkardrFunc rst [x]
        mkardrFunc ('d' : rst) [PList (x : xs)] = mkardrFunc rst [PList xs]
        mkardrFunc "r" [val] = pure val
        mkardrFunc s v = formErr v

        mkardr :: String -> (String, PhageVal)
        mkardr adr = let name = 'c' : adr in
            (name, mkSimFunc 1 $ mkardrFunc adr)

metaFuncs :: [(String, PhageVal)]
metaFuncs =
    [ ("arity", mkFunc 1 arFunc)
    , ("apply", defForm {arity=2, form=apFunc, paramap=eval})
    ] where
        arFunc :: PhageFunc
        arFunc _ (PForm{arity=ar} : _) = pure $ PNum $ toInteger $ ar

        apFunc :: PhageForm
        apFunc tab (f@PForm{} : PList args : xs) =
            apply tab f args
        apFunc _ v = funcErr v

specials :: [(String, PhageVal)]
specials =
    fmap (\(n, ar, fn) -> (n, defForm {arity=ar, form=fn, name=(Just n)}))
    [ ("if",     3, ifFunc)
    , ("cond",   0, condFunc)
    , ("def",    2, def)
    , ("let",    1, letFunc)
    , ("eval",   1, evalFunc)
    , ("import", 1, importFunc)


    -- Function / Form definition
    --
    -- takes a name
    -- evaluate args
    -- dynamically / lexically scoped
    -- scope escapes form / function
    , ("\\\\s",  2, formFunc False False False False)
    , ("\\\\",   2, formFunc False False False True)
    , ("\\\\sd", 2, formFunc False False True  False)
    , ("\\\\d",  2, formFunc False False True  True)
    , ("\\s",    2, formFunc False True  False False)
    , ("\\",     2, formFunc False True  False True)
    , ("\\sd",   2, formFunc False True  True  False)
    , ("\\d",    2, formFunc False True  True  True)

    -- TODO move to prelude
    , ("forms",  3, formFunc True  False False False)
    , ("form",   3, formFunc True  False False True)
    , ("formsd", 3, formFunc True  False True  False)
    , ("formd",  3, formFunc True  False True  True)
    , ("funs",   3, formFunc True  True  False False)
    , ("fun",    3, formFunc True  True  False True)
    , ("funsd",  3, formFunc True  True  True  False)
    , ("fund",   3, formFunc True  True  True  True)
    ] where

        importFunc :: PhageForm
        importFunc tab [PStr fname] =
            ExceptT $ readFile fname >>= imp
            where imp s = case parse parseAst fname s of
                    Left s -> return $ Left $ show s
                    Right ast -> runExceptT $ lastBlock tab ast
        importFunc _ v = formErr v

        evalFunc :: PhageForm
        evalFunc t [a] = eval t a >>= \(_, a) -> eval t a
        evalFunc _ v = formErr v

        param (PAtom str) = pure str
        param thing = throwE $ "Invalid parameter name: " <> show thing

        runForm :: Bool -> Bool -> [PhageVal] -> [String] -> SymTab -> PhageForm
        runForm scoped dyn blk strs bound oldtab params =
            let entries = [ ("args", PList params)
                          , ("rest", PList (drop (length strs) params))
                          ]
                tab = newTab (entries <> zip strs params)
                    (if dyn then oldtab else bound)
                res = lastBlock tab blk
            in  mapFst (if scoped then const [] else id) <$> res

        formcreate ::
            Maybe String ->
            -- evaluate arguments
            Bool ->
            -- scope escapes
            Bool ->
            -- dynamically scoped
            Bool ->
            SymTab ->
            [PhageVal] ->
            [String] ->
            ExceptT PhageErr IO PhageVal
        formcreate name argval dyn scoped tab (PList strs : blk) selfrefs =
            let t = newTabM (zip selfrefs $ repeat f) tab
                f = (\strs -> defForm
                    { arity=length strs
                    , form=runForm scoped dyn blk strs t
                    , paramap=if argval then eval else paramap defForm
                    , func=not argval
                    , name=name
                    }) <$> mapM param strs in f
        formcreate _ _ _ _ _ v _ = formErr v

        recs = ["rec", "this"]

        -- named, eval args, eval result
        formFunc :: Bool -> Bool -> Bool -> Bool -> PhageForm
        formFunc named argval dyn scoped tab (PAtom n : ns) | named =
            let v = formcreate (Just n) argval dyn scoped tab ns (n : recs) in
                (\v -> ([(n, pure v)], v)) <$> v
        formFunc named argval dyn scoped tab ns | not named =
            ([],) <$> formcreate Nothing argval dyn scoped tab ns recs
        formFunc _ _ _ _ _ v = formErr v

        letFunc' :: PhageVal -> PhageForm
        letFunc' res t [] = pure ([], res)
        letFunc' res t (PList [PAtom str, val] : others) = eval t val
            >>= \(_, val) -> letFunc' val (newTab [(str, val)] t) others
        letFunc' _ _ v = formErr v

        letFunc :: PhageForm
        letFunc = letFunc' (PList [])

        def :: PhageForm
        def tab val@[PAtom a, b] =
            -- we're using binding so as not to repeat IO actions
            -- on variable accesses
            let v = snd <$> eval (newTabM [(a, v)] tab) b in
                v >>= \v -> pure ([(a, pure v)], v)
        def _ v = formErr v

        condFunc :: PhageForm
        condFunc tab [] = throwE "condition not met"
        condFunc tab (PList [pred, val] : nodes) = eval tab pred
            >>= \(t, pred) -> if truthy pred
                then eval tab val
                else condFunc tab nodes
        condFunc _ v = formErr v

        ifFunc :: PhageForm
        ifFunc t [a, b, c] = condFunc t [PList [a, b], PList [PNum 1, c]]
        ifFunc _ v = formErr v

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
core = newTab allVals $ mempty
