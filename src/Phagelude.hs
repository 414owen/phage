module Phagelude
    ( phagelude
    ) where

import Ast
import Err
import Val
import SymTab
import Safe
import Interpreter
import Data.Map
import Data.Maybe
import Data.Monoid
import Control.Monad.Trans.Except

newTab :: [(String, PhageVal)] -> SymTab PhageVal -> SymTab PhageVal
newTab vs tab = Prelude.foldl (\m (k, v) -> insert k v m) tab vs

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

fmapmapsnd = fmap . mapSnd

err = ExceptT . return . Left
ret = ExceptT . return . Right

callErr s = err ("Unexpected call to " <> s <> ":")
formErr s = callErr ("form " <> s)
funcErr s = callErr ("func " <> s)

arith :: [(String, PhageVal)]
arith =
    fmapmapsnd (createOnTwoInts PNum)
        [ ("+", (+))
        , ("-", (-))
        , ("*", (*))
        , ("/", div)
        , ("%", mod)
        , ("min", min)
        , ("max", max)
        ]

comparison :: [(String, PhageVal)]
comparison =
    fmapmapsnd (createOnTwoInts PBool)
        [ ("=", (==))
        , ("<", (<))
        , (">", (>))
        , ("<=", (<=))
        , (">=", (>=))
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
        gen 0 = [""]
        gen a = gen (a - 1) >>= \b -> ['a' : b, 'd' : b]

lists :: [(String, PhageVal)]
lists = concat
    [ fmapmapsnd (uncurry mkFunc)
      [ ("list", (1, listFunc))
      , ("cons", (2, consFunc))
      ]
    , fmap mkardr cardrs
    ] where
        listFunc vals _ = ret $ PList vals
        consFunc [x, PList xs] _ = ret $ PList (x : xs)
        consFunc _ _ = funcErr "cons"

        mkardrFunc :: String -> String -> PhageFunc
        mkardrFunc _ [] [val] _ = ret val
        mkardrFunc n ('a' : rst) [PList (x : xs)] t = mkardrFunc n rst [x] t
        mkardrFunc n ('d' : rst) [PList (x : xs)] t = mkardrFunc n rst [PList xs] t
        mkardrFunc name _ _ _ = formErr name

        mkardr :: String -> (String, PhageVal)
        mkardr s = let name = 'c' : s <> "r" in
            (name, mkFunc 1 $ mkardrFunc name s)

specials :: [(String, PhageVal)]
specials =
    [ ("if", PForm 3 ifFunc)
    , ("cond", PForm 0 condFunc)
    , ("\\", PForm 2 funcFunc)
    , ("def", PForm 2 def)
    , ("let", PForm 1 letFunc)
    , ("fun", PForm 2 namedFun)
    ] where
        param (AAtom str) = ret str
        param thing = throwE $ "Invalid parameter name: " <> show thing

        runFunc :: [AstNode] -> [String] -> PhageFunc
        runFunc blk strs params oldtab =
            let tab = newTab (zip strs params) oldtab in
                lastDef (PList []) <$> block tab blk

        -- named functions support recursion
        namedFun :: PhageForm
        namedFun (AAtom name : AList strs : blk) tab = mapM param strs
            >>= \strs ->
                let fn = PFunc (length strs) [] (insert name fn tab) (runFunc blk strs)
                in  ret (fn, insert name fn tab)

        letFunc' :: PhageVal -> PhageForm
        letFunc' res [] t = ret (res, t)
        letFunc' res (AList [AAtom str, val] : others) t = eval t val
            >>= \(val, ntab) -> letFunc' val others (insert str val t)
        letFunc' _ a _ = formErr "let"

        letFunc :: PhageForm
        letFunc = letFunc' (PList [])

        def :: PhageForm
        def [AAtom a, b] tab = letFunc [AList [AAtom a, b]] tab
        def a _ = formErr "def"

        funcFunc :: PhageForm
        funcFunc (AList atoms : blk) tab = mapM param atoms
            >>= \strs ->
                ret (PFunc (length strs) [] tab (runFunc blk strs), tab)
        funcFunc _ _ = formErr "fun"

        condFunc :: PhageForm
        condFunc [] tab = ExceptT $ return $ Left "Condition not met"
        condFunc (AList [pred, val] : nodes) tab = eval tab pred
            >>= \(pred, t) -> if truthy pred
                then eval tab val
                else condFunc nodes tab
        condFunc _ _ = formErr "cond"

        ifFunc :: PhageForm
        ifFunc [a, b, c] = condFunc [AList [a, b], AList [ANum 1, c]]

allVals :: [(String, PhageVal)]
allVals = concat
    [ arith
    , comparison
    , consts
    , specials
    , lists
    , [("print", mkFunc 1 prnt)]
    ] where
        prnt :: PhageFunc
        prnt []  t = ret (putStrLn "") (PList [])
        prnt lst t = ret (mapM print lst) (last lst)

        ret :: IO a -> b -> ExceptT PhageErr IO b
        ret a b = ExceptT $ const (Right b) <$> a

phagelude :: Map String PhageVal
phagelude = newTab allVals mempty
