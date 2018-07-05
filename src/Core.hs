module Core
    ( core
    ) where

{-
 - Core contains a small set of fucntions necessary to
 - bootstrap a useful language, the idea being that the
 - standard library will be written in phage, using core.
 -}

import Control.Monad.Trans.Except
import Data.Tuple.Lazy
import Data.Map.Lazy (toList)
import Data.Monoid
import Lens.Micro ((&), (^.), (.~))
import Text.Megaparsec

import Err
import Val
import Parser
import Interpreter

typeMess :: String
typeMess = "wrong type supplied to function"

type PhageFuncRes = ExceptT PhageErr IO PhageVal
type PhageFunc = SymTab -> [PhageVal] -> PhageFuncRes
type SimFunc = [PhageVal] -> PhageFuncRes

quote :: Applicative f => t -> [v] -> f ([a], v)
quote _ []      = error "quote is non-total"
quote _ (v : _) = pure ([], v)

defForm :: FormVal
defForm = FormVal 0 [] (\_ v -> pure ([], v)) quote Nothing False

mkBoundFunc :: Int -> PhageFunc -> SymTab -> Maybe String -> PhageVal
mkBoundFunc a fn env n =
  PForm $ FormVal a [] eval (funToForm fn env) n True

funToForm :: PhageFunc -> SymTab -> PhageForm
funToForm fn _ c ps = ([],) <$> fn c ps

mkFunc :: Int -> PhageFunc -> PhageVal
mkFunc a fn = mkBoundFunc a fn mempty Nothing

mkSimFunc :: Int -> SimFunc -> PhageVal
mkSimFunc ar fn = mkFunc ar (\_ ps -> fn ps)

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
      nf (a : b : _) =
        case (fromA a, fromB b) of
            (Just a', Just b') -> return $ toC $ fn a' b'
            _ -> throwE typeMess

      nf _ = throwE typeMess


fromNum :: (PhageVal -> Maybe Integer)
fromNum (PNum a) = Just a
fromNum _ = Nothing

truthy :: PhageVal -> Bool
truthy (PList []) = False
truthy (PBool a) = a
truthy (PNum 0) = False
truthy (PAtom "") = False
truthy _ = True

stringy :: PhageVal -> Maybe String
stringy (PList []) = Just []
stringy (PList (PChar x : xs)) = (x:) <$> stringy (PList xs)
stringy (PList (_ : xs)) = stringy (PList xs)
stringy _ = Nothing

callErr :: (Show a, Monad m) => String -> a -> ExceptT String m b
callErr a b = throwE ("Unexpected call to " <> a <> ": " <> show b)

formErr :: Show a => a -> ExceptT String IO b
formErr = callErr "form"

funcErr :: Show a => a -> ExceptT String IO b
funcErr = callErr "func"

anyVal :: [(String, PhageVal)]
anyVal =
    [ ("=", equals)
    ] where
        equals = mkFunc 2 (\_ v -> pure $ PBool $ fn v)
        fn [a, b] = a == b
        fn (a : b : xs) = a == b && fn (b : xs)
        fn _ = error "fn is non-total in anyVal"

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
    (fmap . mapSnd) (binFunc fromNum fromNum PNum)
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

cardrs :: [String]
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
        mkardrFunc ('a' : rst) [PList (x : _)] = mkardrFunc rst [x]
        mkardrFunc ('d' : rst) [PList (_ : xs)] = mkardrFunc rst [PList xs]
        mkardrFunc "r" [val] = pure val
        mkardrFunc _ v = formErr v

        mkardr :: String -> (String, PhageVal)
        mkardr adr = let name' = 'c' : adr in
            (name', mkSimFunc 1 $ mkardrFunc adr)

metaFuncs :: [(String, PhageVal)]
metaFuncs =
    [ ("arity", mkFunc 1 arFunc)
    , ("apply", PForm $ defForm & arity .~ 2 & form .~ apFunc & paramap .~ eval)
    , ("call", PForm $ defForm & arity .~ 2 & form .~ callFunc & paramap .~ eval)
    ] where
        arFunc :: PhageFunc
        arFunc _ (PForm fv : _) = pure $ PNum $ toInteger $ fv ^. arity
        arFunc _ _              = error "metaFuncs is non-total in call to arFunc"

        apFunc :: PhageForm
        apFunc tab (f@PForm{} : PList a1 : PList a2 : xs) =
            apFunc tab (f : PList (a1 <> a2) : xs)
        apFunc tab (f@PForm{} : PList args : []) =
            apply tab f args
        apFunc _ v = funcErr v

        callFunc :: PhageForm
        callFunc tab (x : xs) = apFunc tab [x, PList xs]
        callFunc _   _        = error "metaFuncs is non-total in call to callFunc"

specials :: [(String, PhageVal)]
specials =
    fmap (\(n, ar, fn) -> (n, PForm $ defForm & arity .~ ar & form .~ fn & name .~ (Just n)))
    [ ("if",     3, ifFunc)
    , ("def",    2, def)
    , ("eval",   1, evalFunc)
    , ("import", 1, importFunc)


    -- Function / Form definition
    --
    -- takes a name
    -- evaluate args
    -- dynamically / lexically scoped
    -- scope escapes form / function
    , ("s\\\\",  2, formFunc False False False False)
    , ("\\\\",   2, formFunc False False False True)
    , ("ds\\\\", 2, formFunc False False True  False)
    , ("d\\\\",  2, formFunc False False True  True)
    , ("s\\",    2, formFunc False True  False False)
    , ("\\",     2, formFunc False True  False True)
    , ("ds\\",   2, formFunc False True  True  False)
    , ("d\\",    2, formFunc False True  True  True)
    ] where
        importFunc :: PhageForm
        importFunc tab [v] = eval tab v >>= \(_, v') -> case stringy v' of
            Nothing -> formErr v
            Just fname -> ExceptT $ readFile fname >>= imp
                where imp s = case parse parseAst fname s of
                        Left e -> return $ Left $ show e
                        Right ast -> runExceptT $ lastBlock tab ast
        importFunc _ v = formErr v

        evalFunc :: PhageForm
        evalFunc t [v@(PQList _)] = eval t v
        evalFunc t [a] = eval t a >>= \(eds, a') -> eval (newTabM eds t) a'
        evalFunc _ v = formErr v

        param (PAtom str) = pure str
        param thing = throwE $ "Invalid parameter name: " <> show thing

        runForm :: Bool -> Bool -> [PhageVal] -> [String] -> SymTab -> PhageForm
        runForm scoped dyn blk strs bctx cctx params =
            let entries = [ ("args", PList params)
                          , ("rest", PList (drop (length strs) params))
                          ]
                tab = newTab (entries <> zip strs params)
                    (if dyn then cctx else bctx)
                res = lastBlock tab blk
            in  mapFst (if scoped then const [] else id) <$> res

        formcreate
          :: Maybe String
          -> Bool
          -> Bool
          -> Bool
          -> SymTab
          -> [PhageVal]
          -> [String]
          -> PhageFuncRes
        formcreate newName argval dyn scoped tab (PList strs : blk) selfrefs =
            let t = newTabM (zip selfrefs $ repeat f) tab
                f = (\strs' -> PForm $ defForm
                                & arity   .~ length strs
                                & form    .~ runForm scoped dyn blk strs' t
                                & paramap .~ (if argval then eval else defForm ^. paramap)
                                & func    .~ not argval
                                & name    .~ newName
                    ) <$> mapM param strs in f
        formcreate _ _ _ _ _ v _ = formErr v

        recs = ["rec", "this"]

        -- named, eval args, eval result
        formFunc :: Bool -> Bool -> Bool -> Bool -> PhageForm
        formFunc named argval dyn scoped tab (PAtom n : ns) | named =
            let v = formcreate (Just n) argval dyn scoped tab ns (n : recs) in
                (\v' -> ([(n, pure v')], v')) <$> v
        formFunc named argval dyn scoped tab ns | not named =
            ([],) <$> formcreate Nothing argval dyn scoped tab ns recs
        formFunc _ _ _ _ _ v = formErr v

        def :: PhageForm
        def tab [PAtom a, b] =
            -- we're using binding so as not to repeat IO actions
            -- on variable accesses
            let v = snd <$> eval (newTabM [(a, v)] tab) b in
                v >>= \v' -> pure ([(a, pure v')], v')
        def _ v = formErr v

        ifFunc :: PhageForm
        ifFunc tab [a, b, c] = eval tab a
            >>= \(eds, pred') -> eval (newTabM eds tab)
                    (if truthy pred' then b else c)
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
    , [ ("print", mkSimFunc 1 prnt)
      , ("atom", mkFunc 1 atom)
      , ("str", PForm $ defForm & arity .~ 1 & form .~ str)
      , ("env", mkFunc 0 env)
      ]
    ]
    where
        atom :: PhageFunc
        atom _ [v] = case stringy v of
            Nothing -> funcErr [v]
            Just str' -> pure $ PAtom str'
        atom _ v = funcErr v

        str :: PhageForm
        str _ [PAtom a] = pure ([], PList (PChar <$> a))
        str _ v = funcErr v

        nameThings (s, PForm fv) = (s, PForm $ fv & name .~ Just s)
        nameThings a = a

        prnt :: SimFunc
        prnt [v] = ret (putStr $ show v) v
        prnt v = funcErr v

        env :: PhageFunc
        env tab _ = ExceptT $ const (Right $ PList []) <$> print (fmap fst $ toList tab)

        ret :: IO a -> b -> ExceptT PhageErr IO b
        ret a b = ExceptT $ const (Right b) <$> a

core :: SymTab
core = newTab allVals $ mempty
