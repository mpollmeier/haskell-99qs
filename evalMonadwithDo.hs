--- Using a monad to implement the evaluation example but this time using "do"

import Control.Applicative
import Control.Monad

data Expr = C Float |
            Expr :+ Expr| 
            V String |
            Let String Expr Expr
            deriving Show

type Env = [(String, Float)]

--- Yes, this could be done using the pre-defined State monad...but this is more useful for learning from....
newtype Compute a = Comp{ compExpr :: Env -> (Env , a)}

instance Monad Compute where
 
  return x = Comp (\env -> (env , x))
 
  e >>= f = Comp(\env ->  let  (env', v) = compExpr e env in compExpr (f v) env')

instance Functor Compute where
  fmap = liftM

instance Applicative Compute where
  pure = return
  (<*>) = ap


eval :: Expr -> Compute Float
eval (C x)         = return x

eval (e1 :+ e2)    = do 
                       v1 <- eval e1
                       v2 <- eval e2
                       return (v1 + v2)

eval (V v)         = find' v

eval (Let v e1 e2) = do
                       v1 <- eval e1
                       extend' v v1
                       v2 <- eval e2
                       return v2

---Find a variable's value by looking in the environment
find :: String -> Env -> Float
find v  []          = error ("Unbound variable: " ++ v)
find v1 ((v2,e):es) = if v1 == v2 then e else find v1 es

---Use find properly
---find' :: String -> Env -> (Env, Float)
---find' v env = (env, find v env)
find' :: String -> Compute Float
find' v = Comp(\env -> (env, find v env))
                     
---We extend with variables that may already appear in
---the environment so as to have a sensible block
---structure, so, for example,
---evaluate (Let “x” (C 5) (Let “x” (C 4) (V “x”))
---gives 4.0 and not 5.0

extend :: String -> Float -> Env -> Env
extend v e env = (v,e):env

---Use extend properly
---extend' :: String -> Float -> Env -> (Env, Float)
---extend' v e env = (extend v e env, e)
extend' :: String -> Float -> Compute Float
extend' v e = Comp(\env -> (extend v e env, e))

---Finally answer to start the computation with an empty environment, and returns final answer
---answer :: Expr -> Float
answer e =  (compExpr (eval e) [])

e0 = Let "x" (C 2.0) (V "x" :+ C 3.0)

e1 = Let "x" (C 2.0) (Let "y" (C 3.0) (V "x" :+ V "y"))

e2 = Let "x" (C 2.0) (Let "y" (C 3.0) (V "x" :+ V "x"))