substPat is not compatible with SubstCoerce?

TODO test subst bind over patterns


```haskell
-- -- see https://github.com/lambdageek/unbound-generics/issues/16
-- substBind :: (Typeable a, Alpha b, Subst a b) => Bind (Name a) b -> a -> b
-- substBind bndb a = let (x , b) = unsafeUnbind bndb in subst x a b
-- -- substBind bndb a = runFreshM $ do (x, b) <- unbind bndb; pure $ subst x a b

data E' = 
  V' (Name E') | C' String | B' (Bind (Name E') E')
  deriving (
    Show,
   Generic, Typeable)

instance Alpha E'
instance Subst E' E' where
  -- `isvar` identifies the variable case in your AST.
  isvar (V' x) = Just (SubstName x)
  isvar _     = Nothing


badsubstbindexample  = let
  x = s2n "x"
  y = s2n "y"
  ex = (bind y $ bind x (V' x,V' y)) :: Bind (Name E') ( Bind (Name E') (E', E'))
  ex' = substBind ex $ V' x
  ex'' = substBind ex' $ C' "should only apear once"
  in ex''

cansubstboundvars = let
  x = s2n "x"
  y = s2n "y"
  ex = (B' $ B x $ V' $ Bn 1 0  ) :: E'
  in subst (Bn 0 0) (C' "hi") ex  

cansubstboundvars' = let
  x = s2n "x"
  y = s2n "y"
  ex = (B' $ B x $ V' $ Bn 1 0  ) :: E'
  in subst (Bn 0 0) (C' "hi") ex  
```


