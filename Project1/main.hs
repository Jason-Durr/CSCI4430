import PA1Helper
import System.Environment (getArgs)

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
findAtom tar Lexp@(Atom s) rep= 
    if (s == tar)
        then rep
        else s
findAtom tar Lexp@(Lambda v exp) rep = findAtom tar exp rep
findAtom tar Lexp@(Apply exp1 exp2) rep = (Apply (findAtom tar exp1 rep) (findAtom tar exp2 rep))

bHelper :: Lexp -> Lexp -> Lexp
bHelper Lexp@((Atom v) lexp) = Apply v lexp
bHelper Lexp@((Lambda v exp) lexp) = findAtom v exp lexp

bHelper Lexp@((Apply exp1 exp2) lexp) = 


-- rename all variables that are bounded
alphaRenaming :: Lexp -> Lexp
alphaRenaming Lexp@(Atom v) = v
-- alphaRenaming (Lambda exp1 exp2) = 
-- alphaRenaming (Apply exp1 exp2) = 

betaReduction :: Lexp -> Lexp
betaReduction Lexp@(Atom v) = v
betaReduction Lexp@(Lambda v exp) = (Lambda v betaReduction exp )
betaReduction Lexp@(Apply exp1 exp2) = do 
    return (bHelper exp1 exp2)

--η-reduction can only be applied if x does not appear free in E
etaReduction :: Lexp -> Lexp
etaReduction Lexp@(Atom v) = v
-- etaReduction (Lambda exp1 exp2) = 
-- etaReduction (Apply exp1 exp2) = 

reducer :: Lexp -> Lexp
reducer lexp = etaReduction(betaReduction(alphaRenaming(lexp)))

-- Entry point of program
main = do
    args <- getArgs
    let inFile = case args of { x:_ -> x; _ -> "input.lambda" }
    let outFile = case args of { x:y:_ -> y; _ -> "output.lambda"}
    -- id' simply returns its input, so runProgram will result
    -- in printing each lambda expression twice. 
    runProgram inFile outFile reducer