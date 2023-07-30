module Backend.Language where

data LambdaExpr = Add LambdaExpr LambdaExpr
                | Var String
                | Const Int
                | Lam String LambdaExpr
                | App LambdaExpr LambdaExpr deriving Show

data Program = Named    String LambdaExpr Program
             | Command  LambdaExpr deriving Show

type Env = [(String, LambdaExpr)]

interpret :: Env -> Program -> Maybe LambdaExpr
interpret env (Command expr) = evalExpr env expr
interpret env (Named var expr rest) =
    let newEnv = (var, expr) : env
    in interpret newEnv rest

evalExpr :: Env -> LambdaExpr -> Maybe LambdaExpr
evalExpr env (Var v) = lookup v env
evalExpr env (Lam var expr) = evalExpr ((var,expr):env) expr
evalExpr env (App expr1 expr2) =
    let
       fun = evalExpr env expr1
       arg = evalExpr env expr2
    in
       case (fun,arg) of
            (Just (Lam var body),Just rest) -> evalExpr ((var, rest) : env) body
            _ -> Nothing
evalExpr env (Const i) = Just $ (Const i)
evalExpr env (Add exp1 exp2) =
    let
       val1 = evalExpr env exp1
       val2 = evalExpr env exp2
    in
        case (val1, val2) of
            (Just (Const i), Just (Const j)) -> Just $ (Const (i + j))
            _ -> Nothing

-- Example:
-- interpret [] (Named "f" (Lam "x" (LExpr $ Add (Var "x") (LExpr $ Const 3))) (Command (App (Var "f") (LExpr $ Const 20))))
