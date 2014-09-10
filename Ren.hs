import Control.Monad
import Control.Monad.Logic
import Control.Monad.State hiding (lift)

newtype Var = Var Int
 deriving (Eq, Show)
type Gensym = [Var]
type Context s = [(Var, s)]
type Store s = (Context s,Gensym)
type Kanren s a = StateT (Store s) Logic a

data P = String :- [Either Var P]
 deriving (Eq, Show)

initial :: Store s
initial = ([],map Var [0..])

fresh :: Kanren s Var
fresh = do (store,x:xs) <- get
           put (store,xs)
           return x

associate (var,val) = do (store,xs) <- get
                         put ((var,val):store,xs)

-- returns a fresh variable or a thing
-- todo: worry about infinite loop
walk (Left v) = do (store,_) <- get
                   case lookup v store of
                     Nothing -> return $ Left v
                     Just val -> walk val
walk (Right p) = return $ Right p

unify v1 v2 = do w1 <- walk v1
                 w2 <- walk v2
                 case (w1, w2) of
                   (Left v1, _) -> associate (v1,v2)
                   (_, Left v2) -> associate (v2,v1)
                   (Right p, Right q) -> unifyTerms p q

unifyTerms (s1 :- a1) (s2 :- a2)
  | s1 == s2 && length a1 == length a2 = mapM_ (uncurry unify) (zip a1 a2)
  | otherwise = mzero

reify t = do w <- walk t
             case w of
               Left v -> return $ Left v
               Right (s :- a) -> do aw <- mapM reify a
                                    return $ Right (s :- aw)

run' :: Kanren (Either Var P) (Either Var P) -> [Either Var P]
run' k = observeAll (evalStateT (k >>= reify) initial)
