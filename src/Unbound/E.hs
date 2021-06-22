{-# LANGUAGE MultiParamTypeClasses, DeriveGeneric, DeriveDataTypeable #-}


module Unbound.E () where
  
import Unbound.Generics.LocallyNameless
import Data.Typeable (Typeable)
import GHC.Generics (Generic)

import Unbound.Generics.LocallyNameless.Unsafe (unsafeUnbind)

import Unbound.Generics.LocallyNameless.Name 
import Unbound.Generics.LocallyNameless.Bind 


type Var = Name Expr

data Expr = V Var | Lam (Bind Var Expr) | I Int | App Expr Expr
  deriving (Generic, Typeable, Show)

instance Alpha Expr

instance Subst Expr Expr where
  isvar (V x) = Just (SubstName x)
  isvar _     = Nothing

smallStep :: Expr -> Maybe Expr
smallStep ((Lam bndBod) `App` a) = Just $ substBind bndBod a
smallStep (f `App` a) = 
  case (smallStep f, smallStep a) of
    (Nothing, Nothing) -> Nothing
    (Just f', _) -> Just $ f' `App` a
    (Nothing, Just a') -> Just $ f `App` a'
smallStep (Lam (B xx bod)) = Lam <$> B xx <$> smallStep bod
-- smallStep (Lam bndBod) = Lam <$> underBinder smallStep bndBod
smallStep _ = Nothing -- no step

smallStep' :: (Fresh m) => Expr -> m (Maybe Expr)
smallStep' ((Lam bndBod) `App` a) = do
  (name, bod) <- unbind bndBod
  pure $ Just $ subst name a bod
smallStep' (f `App` a) = do
  mf' <- smallStep' f
  ma' <- smallStep' a
  case (mf', ma') of
    (Nothing, Nothing) -> pure Nothing
    (Just f', _) -> pure $ Just $ f' `App` a
    (Nothing, Just a') -> pure $ Just $ f `App` a'
smallStep' (Lam bndBod) = do
  (name, bod) <- unbind bndBod
  mbod' <- smallStep' bod
  case mbod' of
    Nothing -> pure Nothing
    Just bod' -> pure $ Just $ Lam $ bind name bod'
smallStep' _ = pure Nothing -- no step



x,y,z :: Var
x = s2n "x"
y = s2n "y"
z = s2n "z"


e1 = Lam $ B x $ V x

ee1 = substBind (B x $ V x) $ I 0

badsubstbindexample  = let
  ex = (bind y $ bind x (V x,V y)) :: Bind Var ( Bind Var (Expr, Expr))
  ex' = substBind ex $ V x
  ex'' = substBind ex' $ I 99
  in ex''

e2 = App (Lam (B z $ V z)) (I 0)
ee2 = smallStep e2

-- note this is acutally bugy, since the free var is not accounted for!
ee2' = runFreshM $ smallStep' e2


e3 = has' initialCtx (AnyName z) $ App (Lam (B z $ V z)) (I 0)


e41 = has' initialCtx (AnyName z) $ x
e42 = has' initialCtx (AnyName z) $ AnyName x
e43 = has' initialCtx (AnyName z) $ V x
e44 = has' initialCtx (AnyName z) $ (B z $ V x)
e4 = has' initialCtx (AnyName z) $ App (Lam (B z $ V x)) (I 0) -- TODO needs to ignore binding positions
e50 = has' initialCtx (AnyName $ (Bn 0 0 :: Var)) $ (Bn 0 0 :: Var)
e51 = has' initialCtx (AnyName $ (Bn 1 0 :: Var)) $ (Bn 0 0 :: Var)
e52 = has' initialCtx (AnyName $ (Bn 0 0 :: Var)) $ (Bn 1 0 :: Var)
e53 = has' initialCtx (AnyName $ (Bn 0 1 :: Var)) $ (Bn 0 1 :: Var)

e54 = has' initialCtx (AnyName $ (Bn 0 0 :: Var)) $ B z $ (V $ Bn 0 0)
e55 = has' initialCtx (AnyName $ (Bn 0 0 :: Var)) $ B z $ (V $ Bn 1 0)

