import PA1Helper
import System.Environment (getArgs)
import qualified Data.Map as Map

-- Haskell representation of lambda expression
-- data Lexp = Atom String | Lambda String Lexp | Apply Lexp  Lexp 

-- Given a filename and function for reducing lambda expressions,
-- reduce all valid lambda expressions in the file and output results.
-- runProgram :: String -> (Lexp -> Lexp) -> IO ()

-- This is the identity function for the Lexp datatype, which is
-- used to illustrate pattern matching with the datatype. "_" was
-- used since I did not need to use bound variable. For your code,
-- however, you can replace "_" with an actual variable name so you
-- can use the bound variable. The "@" allows you to retain a variable
-- that represents the entire structure, while pattern matching on
-- components of the structure.
id' :: Lexp -> Lexp
id' v@(Atom _) = v
id' lexp@(Lambda _ _) = lexp
id' lexp@(Apply _ _) = lexp 


-- You will need to write a reducer that does something more than
-- return whatever it was given, of course!
--may have to declare tar as atom
findAtom :: Lexp -> Lexp -> Lexp -> Lexp
findAtom tar org@(Atom s) rep = 
    if ((Atom s) == tar)
        then rep
        else (Atom s)
findAtom tar org@(Lambda v exp) rep = (Lambda v (findAtom tar exp rep))
findAtom tar org@(Apply exp1 exp2) rep = (Apply (findAtom tar exp1 rep) (findAtom tar exp2 rep))

bHelper :: Lexp -> Lexp -> Lexp
bHelper org@(Atom v) lexp = Apply org lexp
bHelper org@(Lambda v exp) lexp = 
    if (findAtom (Atom v) exp lexp) == org
        then org
        else betaReduction (findAtom (Atom v) exp lexp)
bHelper org@(Apply exp1 exp2) lexp = 
    if (Apply (betaReduction org) lexp) == (Apply org lexp)
        then Apply org lexp
        else betaReduction (Apply (betaReduction org) lexp)

eHelper :: String -> Lexp -> Lexp
eHelper a org@(Atom v)  = 
    if a == v
        then Lambda a (Atom v)
        else org
eHelper a org@(Lambda v exp) = 
    if (eHelper v exp) == org
        then org
        else eHelper a (eHelper v exp)
eHelper a org@(Apply exp1 exp2) = 
    if (afinder a exp1)
        then Lambda a (etaReduction org)
        else etaReduction exp1

afinder :: String -> Lexp -> Bool
afinder a org@(Atom v) = 
    if a == v
        then True
        else False
afinder a org@(Lambda v exp) = afinder a exp
afinder a org@(Apply exp1 exp2) = (afinder a exp1) || (afinder a exp2)

-- will replace any bound variables with a new variable
aHelper :: Lexp -> Lexp -> Map.Map String String -> Lexp
aHelper var@(Atom a) org@(Atom v) varReplace =
    if a == v
        then Lambda (Map.findWithDefault "+" a varReplace) (Atom (Map.findWithDefault "+" a varReplace))
        else Lambda (Map.findWithDefault "+" a varReplace) (Atom v)
aHelper var@(Atom a) org@(Lambda v exp) varReplace = 
    if Map.member v varReplace
        then Lambda (Map.findWithDefault "+" a varReplace) (aHelper (Atom v) exp (Map.insert v ((Map.findWithDefault "+" v varReplace) ++ v) varReplace))
        else Lambda (Map.findWithDefault "+" a varReplace) (aHelper (Atom v) exp (Map.insert v (v ++ v) varReplace))
aHelper var@(Atom a) org@(Apply exp1 exp2) varReplace = var 


-- rename all variables that are bounded
alphaRenaming :: Lexp -> Lexp
alphaRenaming org@(Atom a) = (Atom a)
alphaRenaming org@(Lambda a exp) = aHelper (Atom a) exp (Map.fromList [(a, a ++ a)])
alphaRenaming org@(Apply exp1 exp2) = Apply (alphaRenaming exp1) (exp2)

betaReduction :: Lexp -> Lexp
betaReduction org@(Atom v) = org
betaReduction org@(Lambda v exp) = (Lambda v (betaReduction exp) )
betaReduction org@(Apply exp1 exp2) = (bHelper exp1 exp2)

--η-reduction can only be applied if x does not appear free in E
etaReduction :: Lexp -> Lexp
etaReduction org@(Atom v) = (Atom v)
etaReduction org@(Lambda v exp) = eHelper v exp
etaReduction org@(Apply exp1 exp2) = (Apply (etaReduction exp1) (etaReduction exp2) )

reducer :: Lexp -> Lexp
reducer lexp = alphaRenaming lexp
-- reducer lexp = etaReduction(betaReduction(alphaRenaming(lexp)))

-- Entry point of program
main = do
    args <- getArgs
    let inFile = case args of { x:_ -> x; _ -> "input.lambda" }
    let outFile = case args of { x:y:_ -> y; _ -> "output.lambda"}
    -- id' simply returns its input, so runProgram will result
    -- in printing each lambda expression twice. 
    runProgram inFile outFile reducer