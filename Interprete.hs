--  Auteur : Honoré NINTUNZE

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Parser
import Data.Maybe
import Data.Either
import System.IO (isEOF,getLine,putStr)

type Nom = String

data Expression = Lam Nom Expression
                | App Expression Expression
                | Var Nom
                | Lit Litteral
                deriving (Show,Eq)

data Litteral = Entier Integer
              | Bool   Bool
              deriving (Show,Eq)
              

--  Q1
espacesP :: Parser ()
espacesP = do   zeroOuPlus (car ' ')
                return ()

--  Q2
nomP :: Parser Nom
nomP = do   cs <- unOuPlus lettre
            espacesP
            return cs
    where lettre = carCond (flip elem ['a'..'z'])
    
--  Q3
varP :: Parser Expression
varP = do   n <- nomP
            return (Var n) 
            
--  Q4
applique :: [Expression] -> Expression
applique    = foldl1 (\a b -> App a b)

applique' :: [Expression] -> Expression
applique' []         = undefined
applique' [e1,e2]    = (App e1 e2)
applique' (e1:e2:es) = applique (app:es)
                where app = (App e1 e2)

--  Q5 et Q7
exprP :: Parser Expression
exprP = exprParentheseeP ||| lambdaP ||| varP  ||| booleenP ||| nombreP

exprsP :: Parser Expression
exprsP = do expr <- unOuPlus exprP
            return (applique expr)

--  Q6
fleche :: Parser ()
fleche = do chaine "->"
            espacesP
            return ()

lambdaP :: Parser Expression
lambdaP = do    car '\\'
                espacesP
                nom <- nomP
                fleche
                exp <- exprsP
                return (Lam nom exp)

--  Q8
parentheseOuvrante :: Parser Char
parentheseOuvrante = car '('

parentheseFermante :: Parser Char
parentheseFermante = car ')'


exprParentheseeP :: Parser Expression
exprParentheseeP = do   parentheseOuvrante
                        exp <- exprsP
                        parentheseFermante
                        espacesP
                        return exp

--  Q9
nombreP :: Parser Expression
nombreP = do    cs <- unOuPlus nombre
                espacesP
                return (Lit (Entier (read cs)))
    where nombre = carCond (flip elem ['0'..'9'])


--  Q10
booleenP :: Parser Expression
booleenP = do   cs <- (chaine "True" ||| chaine "False")
                espacesP
                case cs of
                    "True"      -> return (Lit (Bool True))
                    "False"     -> return (Lit (Bool False))
                    otherwise   -> echoue
                        
--  Q11
expressionP :: Parser Expression
expressionP = do    espacesP
                    e <- exprsP
                    espacesP
                    return e

--  Q12
ras :: String -> Expression
ras s = case complet r of   True  -> fst (fromJust r)
                            False -> error "Erreur d’analyse syntaxique"
            where r = parse expressionP s

--  Q13
data ValeurA = VLitteralA Litteral
             | VFonctionA (ValeurA -> ValeurA)
        --deriving Show ne marche pas parce que show ne sait pas afficher le type fonctionnel ValeurA -> ValeurA
        
--  Q14
instance Show ValeurA where
    show (VFonctionA _)          = "λ"
    show (VLitteralA (Entier e)) = show e
    show (VLitteralA (Bool b))   = show b

type Environnement a = [(Nom, a)]

--  Q15
interpreteA :: Environnement ValeurA -> Expression -> ValeurA
interpreteA _   (Lit l)     = VLitteralA l
interpreteA env (Var n)     = fromJust (lookup n env)
interpreteA env (Lam n e)   = VFonctionA f
                where f v = interpreteA ((n,v) : env) e
interpreteA env (App e1 e2) = f (interpreteA env e2)
                where VFonctionA f = interpreteA env e1


--  Q16
negA :: ValeurA
negA = VFonctionA f
    where f e = case e of   
                    (VLitteralA (Entier n)) -> VLitteralA (Entier (negate n))
                    _                       -> error "negA s'applique à un litteral entier"


--  Q17
addA :: ValeurA
addA = VFonctionA f
    where   f (VLitteralA (Entier n1)) = VFonctionA h
                            where h (VLitteralA (Entier n2)) = VLitteralA (Entier (n1 + n2))


--  Q18
releveBinOpEntierA :: (Integer -> Integer -> Integer) -> ValeurA
releveBinOpEntierA op = VFonctionA f
            where   f (VLitteralA (Entier n1)) = VFonctionA h
                            where h (VLitteralA (Entier n2)) = VLitteralA (Entier (op n1 n2))

envA :: Environnement ValeurA
envA = [ ("neg",   negA)
       , ("if",    ifthenelseA)
       , ("add",   releveBinOpEntierA (+))
       , ("soust", releveBinOpEntierA (-))
       , ("mult",  releveBinOpEntierA (*))
       , ("quot",  releveBinOpEntierA quot) ]

--  Q19
ifthenelseA :: ValeurA
ifthenelseA = VFonctionA f
            where f (VLitteralA (Bool b)) = VFonctionA (\(VLitteralA l1) ->
                                                           VFonctionA (\(VLitteralA l2) ->
                                                                           case b of   True  -> (VLitteralA l1)
                                                                                       False -> (VLitteralA l2)))

--  Q20
affichePrompt :: IO ()
affichePrompt = putStr "minilang> "

traiteLigne :: IO ()
traiteLigne = do affichePrompt
                 b <- isEOF
                 if b
                 then return ()
                 else do  exp <- getLine
                          print (interpreteA envA (ras exp))
                          traiteLigne        

main :: IO ()
main = traiteLigne

--  Q21
data ValeurB = VLitteralB Litteral
             | VFonctionB (ValeurB -> ErrValB)

type MsgErreur = String
type ErrValB   = Either MsgErreur ValeurB

instance Show ValeurB where
    show (VFonctionB _)          = "λ"
    show (VLitteralB (Entier e)) = show e
    show (VLitteralB (Bool b))   = show b

--  Q22
interpreteB :: Environnement ValeurB -> Expression -> ErrValB
interpreteB _   (Lit l)     = Right (VLitteralB l)
interpreteB env (Var n)     =  let j = lookup n env
                               in case isJust j of  True  -> Right (fromJust j)
                                                    False -> Left ("variable "++ n ++" non definie")
interpreteB env (Lam n e)   = Right (VFonctionB f)
                where f v = interpreteB ((n,v) : env) e                                                   
interpreteB env (App e1 e2) = case interpreteB env e1 of
                                Right (VFonctionB f) -> case interpreteB env e2 of  Right r -> f r
                                                                                    Left  m -> Left m
                                Right (VLitteralB (Entier n)) -> Left (show n ++ " n'est pas une fonction, application impossible")
                                Right (VLitteralB (Bool b))   -> Left (show b ++ " n'est pas une fonction, application impossible")
                                Left  m              -> Left m
                
--  Q23
addB :: ValeurB
addB = VFonctionB f
    where   f (VLitteralB (Entier n1)) = Right (VFonctionB h)
                            where  h (VLitteralB (Entier n2)) = Right (VLitteralB (Entier (n1 + n2)))
                                   h (VLitteralB (Bool b))    = Left (show b ++ " n'est pas un entier")
                                   h (VFonctionB _)           = Left ("λ n'est pas un entier")
            f (VLitteralB (Bool b))    = Left (show b ++ " n'est pas un entier")
            f (VFonctionB _)           = Left ("λ n'est pas un entier")
                
--  Q24
quotB :: ValeurB                
quotB = VFonctionB f
    where   f (VLitteralB (Entier n1)) = Right (VFonctionB h)
                            where  h (VLitteralB (Entier 0))  = Left "division par zero"
                                   h (VLitteralB (Entier n2)) = Right (VLitteralB (Entier (quot n1 n2)))
                                   h (VLitteralB (Bool b))    = Left (show b ++ " n'est pas un entier")
                                   h (VFonctionB _)           = Left ("λ n'est pas un entier")
            f (VLitteralB (Bool b))    = Left (show b ++ " n'est pas un entier")
            f (VFonctionB _)           = Left ("λ n'est pas un entier")                

--  Q25                
data ValeurC = VLitteralC Litteral
             | VFonctionC (ValeurC -> OutValC)

type Trace   = String
type OutValC = (Trace, ValeurC)                
                
instance Show ValeurC where
    show (VFonctionC _)          = "λ"
    show (VLitteralC (Entier e)) = show e
    show (VLitteralC (Bool b))   = show b                

--  Q26
interpreteC :: Environnement ValeurC -> Expression -> OutValC
interpreteC _   (Lit l)     = ("",VLitteralC l)
interpreteC env (Var n)     = ("",fromJust (lookup n env))
interpreteC env (Lam n e)   = ("",VFonctionC f)
                where   f v = interpreteC ((n,v) : env) e
interpreteC env (App e1 e2) = (t1 ++ t2 ++ "." ++ t,vc)
                where   (t1,VFonctionC f) = interpreteC env e1
                        (t2,v)            = interpreteC env e2  
                        (t,vc)            = f v

--  Q26
pingC :: ValeurC
pingC = VFonctionC f
    where f v = ("p",v)

--  Q28
data ValeurM m = VLitteralM Litteral
               | VFonctionM (ValeurM m -> m (ValeurM m))

instance Show (ValeurM m) where
    show (VFonctionM _)          = "λ"
    show (VLitteralM (Entier e)) = show e
    show (VLitteralM (Bool b))   = show b 

data SimpleM v = S v
               deriving Show

--  Q29               
interpreteSimpleM :: Environnement (ValeurM SimpleM) -> Expression -> SimpleM (ValeurM SimpleM)
interpreteSimpleM _   (Lit l)     = S (VLitteralM l)
interpreteSimpleM env (Var n)     = S (fromJust (lookup n env))
interpreteSimpleM env (Lam n e)   = S (VFonctionM f)
                where f v = interpreteSimpleM ((n,v) : env) e
interpreteSimpleM env (App e1 e2) = f e
                where   S (VFonctionM f) = interpreteSimpleM env e1
                        S e              = interpreteSimpleM env e2

instance Monad SimpleM where
    return      = S
    (S v) >>= f = f v

--  Q30
interpreteM :: Monad m => Environnement (ValeurM m) -> Expression -> m (ValeurM m)
interpreteM _   (Lit l)     = return (VLitteralM l)
interpreteM env (Var n)     = return (fromJust (lookup n env))
interpreteM env (Lam n e)   = return (VFonctionM f)
                where f v = interpreteM ((n,v) : env) e
interpreteM env (App e1 e2) = do    VFonctionM f <- interpreteM env e1
                                    v <- interpreteM env e2
                                    f v

--  Q31
type InterpreteM m = Environnement (ValeurM m) -> Expression -> m (ValeurM m)

interpreteS :: InterpreteM SimpleM
interpreteS = interpreteM

--  Q32
data TraceM v = T (Trace, v)
              deriving Show

instance Monad TraceM where
    return v    = T ("",v)
    (T (t,v)) >>= f =   let T (t',v') = f v
                        in T (t ++ t',v')                
                
interpreteMT :: InterpreteM TraceM
interpreteMT = interpreteM

pingM :: ValeurM TraceM
pingM = VFonctionM (\v -> T ("p", v))                

--  Q33
interpreteMT' :: InterpreteM TraceM
interpreteMT' _   (Lit l)     = return (VLitteralM l)
interpreteMT' env (Var n)     = return (fromJust (lookup n env))
interpreteMT' env (Lam n e)   = return (VFonctionM f)
                where f v = interpreteMT' ((n,v) : env) e
interpreteMT' env (App e1 e2) = do  VFonctionM f <- interpreteMT' env e1
                                    v           <- interpreteMT' env e2
                                    let T (t'',v') = f v in T ("." ++ t'',v')

--  Q34
data ErreurM v = Succes v
               | Erreur String
               deriving Show

instance Monad ErreurM where
    fail e = Erreur e
    return v = Succes v
    (Succes v) >>= f = f v
    (Erreur m) >>= _ = fail m

--  Q35
interpreteE :: InterpreteM ErreurM
interpreteE _   (Lit l)     = return (VLitteralM l)
interpreteE env (Var n)     = case isJust r of True  -> return (fromJust r)
                                               False -> fail ("variable "++ n ++" non definie")
                where r = lookup n env
interpreteE env (Lam n e)   = return (VFonctionM f)
                where f v = interpreteE ((n,v) : env) e
interpreteE env (App e1 e2) = do    r <- interpreteE env e1
                                    case r of   VFonctionM f          -> do     v <- interpreteE env e2
                                                                                f v
                                                VLitteralM (Entier n) -> fail (show n ++ " n'est pas une fonction, application impossible")
                                                VLitteralM (Bool b)   -> fail (show b ++ " n'est pas une fonction, application impossible")

--  Q36
class Monad m => Injectable m t where
    injecte :: t -> ValeurM m

instance Monad m => Injectable m Bool where
    injecte = VLitteralM . Bool
    
instance Monad m => Injectable m Integer where
    injecte = VLitteralM . Entier    
    
--  Q37
instance (Monad m, Injectable m t) => Injectable m (Bool -> t) where
    injecte f = VFonctionM g
            where g (VLitteralM (Bool b)) = return (injecte (f b))

--instance (Monad m, Injectable m t) => Injectable m (Integer -> t) where
  --  injecte f = VFonctionM f

