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
import Data.Map (insert)
import Data.Maybe
import Data.Monoid
import Control.Monad.Trans.Except
import Text.Megaparsec

typeMess :: Integer -> String -> PhageVal -> PhageErr
typeMess n exp val = "Parameter " ++ show n ++ " has the wrong type,\
    \ expecting '" ++ exp ++ "' but got '" ++ typeName val ++ "'"

arityMess :: Int -> Int -> PhageErr
arityMess a b = show a <> " arguments applied to function of arity " <> show b

-- create a binary function over phage values
createBinFunc ::
    (PhageVal -> Maybe a, String)
    -> (PhageVal -> Maybe b, String)
    -> (c -> PhageVal)
    -> (a -> b -> c)
    -> PhageVal
createBinFunc (fromA, aStr) (fromB, bStr) toC fn = mkFunc 2 nf
    where
        nf :: PhageFunc
        nf [a, b] tab = case (fromA a, fromB b) of
            (Nothing, _) -> throwE $ typeMess 1 aStr a
            (_, Nothing) -> throwE $ typeMess 2 bStr b
            (Just a, Just b) -> return $ toC $ fn a b
        nf l _ = throwE $ arityMess 2 (length l)

fromNum :: (PhageVal -> Maybe Integer)
fromNum (PNum a) = Just a
fromNum _ = Nothing

fromNumTup = (fromNum, "Number")

mapSnd :: (a -> b) -> (c, a) -> (c, b)
mapSnd fn (a, b) = (a, fn b)

createOnTwoInts = createBinFunc fromNumTup fromNumTup

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

err = ExceptT . return . Left

callErr s = err ("Unexpected call to " <> s)
formErr = callErr . ("form " <>)
funcErr = callErr . ("func " <>)

arith :: [(String, PhageVal)]
arith =
    fmapmapsnd (createOnTwoInts PNum)
        [ ("+", (+))
        , ("-", (-))
        , ("*", (*))
        , ("/", div)
        , ("%", mod)
        ]

comparison :: [(String, PhageVal)]
comparison =
    fmapmapsnd (createOnTwoInts PBool)
        [ ("<", (<))
        , (">", (>))
        , ("<=", (<=))
        , (">=", (>=))
        ]

anyVal :: [(String, PhageVal)]
anyVal =
    concat $
    fmap (\(f, s) -> fmap (\w -> (w, mkFunc 2 $ binFunc PBool s)) (words f))
    [ ("= eq", (==))
    , ("& and", andFunc)
    , ("| or", orFunc)
    ] where
        binFunc constr f [a, b] t = pure $ constr $ f a b
        andFunc a b = truthy a && truthy b
        orFunc a b = truthy a || truthy b

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
        consFunc _ _ = funcErr "cons"

        mkardrFunc :: String -> String -> PhageFunc
        mkardrFunc n ('a' : rst) [PList (x : xs)] t = mkardrFunc n rst [x] t
        mkardrFunc n ('d' : rst) [PList (x : xs)] t = mkardrFunc n rst [PList xs] t
        mkardrFunc _ "r" [val] _ = pure val
        mkardrFunc name _ l _ = formErr (name <> ": " <> show l)

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
        arFunc _ _ = funcErr "arity"

        apFunc :: PhageFunc
        apFunc [PFunc _ ps t fn, PList args] _ = fn (reverse ps <> args) t
        apFunc _ _ = funcErr "apply"

specials :: [(String, PhageVal)]
specials =
    fmapmapsnd (uncurry PForm)
    [ ("if", (3, ifFunc))
    , ("cond", (0, condFunc))
    , ("\\", (2, funcFunc))
    , ("def", (2, def))
    , ("let", (1, letFunc))
    , ("fun", (2, namedFun))
    , ("quote", (1, quoteFunc))
    , ("eval", (1, evalFunc))
    , ("import", (1, importFunc))
    ] where
        importFunc :: PhageForm
        importFunc [PStr fname] t =
            ExceptT $ readFile fname >>= imp
            where
                imp s = case parse parseAst fname s of
                    Left s -> return $ Left $ show s
                    Right ast -> runExceptT $ interpret t ast
        importFunc _ _ = formErr "import"

        quoter (PList a) = PList $ quoter <$> a
        quoter a = a

        quoteFunc :: PhageForm
        quoteFunc [a] t = pure $ (quoter a, t)
        quoteFunc _ _ = formErr "quote"

        evalFunc :: PhageForm
        evalFunc [a] t = eval t a >>= \(v, t) -> eval t v
        evalFunc _ _ = formErr "eval"

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
        funcreate _ _ _ = formErr "function"

        -- named functions support recursion
        namedFun :: PhageForm
        namedFun (PAtom name : params) tab = funcreate params tab [name]
            >>= \f -> pure (f, insert name f tab)
        namedFunc _ _ = formErr "fun"

        letFunc' :: PhageVal -> PhageForm
        letFunc' res [] t = pure (res, t)
        letFunc' res (PList [PAtom str, val] : others) t = eval t val
            >>= \(val, ntab) -> letFunc' val others (insert str val t)
        letFunc' _ a _ = formErr "let"

        letFunc :: PhageForm
        letFunc = letFunc' (PList [])

        def :: PhageForm
        def [PAtom a, b] tab = letFunc [PList [PAtom a, b]] tab
        def a _ = formErr "def"

        funcFunc :: PhageForm
        funcFunc params tab = (,tab) <$> funcreate params tab []

        condFunc :: PhageForm
        condFunc [] tab = ExceptT $ return $ Left "Condition not met"
        condFunc (PList [pred, val] : nodes) tab = eval tab pred
            >>= \(pred, t) -> if truthy pred
                then eval tab val
                else condFunc nodes tab
        condFunc _ _ = formErr "cond"

        ifFunc :: PhageForm
        ifFunc [a, b, c] t = condFunc [PList [a, b], PList [PNum 1, c]] t
        ifFunc _ t = formErr "if"

allVals :: [(String, PhageVal)]
allVals = concat
    [ arith
    , comparison
    , consts
    , specials
    , lists
    , anyVal
    , metaFuncs
    , [("print", mkFunc 0 prnt)]
    ] where
        prnt :: PhageFunc
        prnt lst t =
            ret ((mapM (putStr . (<> " ") . show) lst) >> putStrLn "")
                (lastDef (PList []) lst)

        ret :: IO a -> b -> ExceptT PhageErr IO b
        ret a b = ExceptT $ const (Right b) <$> a

core :: SymTab PhageVal
core = newTab allVals mempty
