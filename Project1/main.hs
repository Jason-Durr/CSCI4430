import PA1Helper
import System.Environment (getArgs)
import qualified Data.Set as Set

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

-- Will find all variables in a Lexp
findAllVars :: Lexp -> Set.Set String
findAllVars org@(Atom v) = Set.fromList [v]
findAllVars org@(Lambda v exp) = Set.union (Set.fromList [v]) (findAllVars exp)
findAllVars org@(Apply exp1 exp2) = Set.union (findAllVars exp1) (findAllVars exp2) 

-- Will find all the free variables in a Lexp
findFreeVars :: Lexp -> Set.Set String
findFreeVars org@(Atom v) = Set.fromList [v]
findFreeVars org@(Lambda v exp) = Set.difference (findFreeVars exp) (Set.fromList [v]) 
findFreeVars org@(Apply exp1 exp2) = Set.union (findFreeVars exp1) (findFreeVars exp2) 

-- List of variable names we can use
varsToUse :: Set.Set String
varsToUse = Set.fromList ["aa","bb","cc","dd","ee","ff","gg","hh","ii","jj","kk","ll","mm","nn","oo","pp","qq","rr","ss","tt","uu","vv","ww","xx","yy","zz"]

-- Get next new variable name to use
nextVarToUse :: Set.Set String -> String
nextVarToUse setOfVars = Set.elemAt 0 (Set.difference varsToUse setOfVars)

-- Will recurse all the way down a Lexp and replace each occurence of a variable with a new one
replaceSingleVar :: String -> String -> Lexp -> Set.Set String -> Lexp
replaceSingleVar new replace org@(Atom v) = 
    if v == replace
        then Atom new
        else org
replaceSingleVar new replace org@(Lambda v exp) = Lambda (nextVarToUse (Set.))
replaceSingleVar new replace org@(Apply exp1 exp2) = org
 
-- rename all variables that are bounded
alphaRenaming :: Lexp -> Set.Set String -> Lexp
alphaRenaming org@(Atom v) varsUsed = (Atom v)
alphaRenaming org@(Lambda v exp) varsUsed = Lambda (nextVarToUse varsUsed) (replaceSingleVar (nextVarToUse varsUsed) v exp varsUsed)
alphaRenaming org@(Apply exp1 exp2) varsUsed = Apply (alphaRenaming exp1 varsUsed) exp2

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
reducer lexp = alphaRenaming lexp Set.empty
-- reducer lexp = etaReduction(betaReduction(alphaRenaming(lexp)))

-- Entry point of program
main = do
    args <- getArgs
    let inFile = case args of { x:_ -> x; _ -> "input.lambda" }
    let outFile = case args of { x:y:_ -> y; _ -> "output.lambda"}
    -- id' simply returns its input, so runProgram will result
    -- in printing each lambda expression twice. 
    runProgram inFile outFile reducer