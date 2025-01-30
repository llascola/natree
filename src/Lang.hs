module Lang where


-- AST para Prop


type Name = String

data SProp =
    Not SProp
  | And SProp SProp
  | Or SProp SProp
  | Implies SProp SProp
  | Atom Name
  | Bottom
  | Def Name
    deriving (Show, Eq)


data Pos = First | Second

data NaTree = 
    -- Regla trivial
      Trivial NaTree SProp 
    | Hip Integer SProp
    -- Reglas de introducción
    | AndInt NaTree NaTree SProp 
    | OrInt Pos NaTree SProp 
    | ImpliesInt Integer NaTree SProp 
    | BottomInt NaTree NaTree SProp 
    | NotInt NaTree SProp 
    --Reglas de eliminación
    | AndElim Pos NaTree SProp 
    | OrElim NaTree Integer NaTree Integer NaTree SProp 
    | ImpliesElim NaTree NaTree SProp 
    | BottomElim NaTree SProp 
    | DobNotElim NaTree SProp 



data ProofTree =
    -- Regla trivial
      PTrivial SProp ProofTree
    | PHip SProp
    -- Reglas de introducción
    | PAndInt SProp ProofTree ProofTree
    | POrInt SProp Pos ProofTree 
    | PImpliesInt SProp (ProofTree -> ProofTree)
    | PBottomInt SProp ProofTree ProofTree
    | PNotInt SProp ProofTree
    -- Reglas de eliminación
    | PAndElim SProp Pos ProofTree
    | POrElim SProp ProofTree (ProofTree -> ProofTree) (ProofTree -> ProofTree)
    | PImpliesElim SProp ProofTree ProofTree 
    | PBottomElim SProp SProp
    | PDobNotElim SProp ProofTree
    
    
tnd :: ProofTree
tnd = 
  PDobNotElim (Or (Atom "0") (Not (Atom "0"))) 
    (PNotInt (Not (Not (Or (Atom "0") (Not (Atom "0"))))) 
      (PBottomInt Bottom 
        (POrInt (Or (Atom "0") (Not (Atom "0"))) Second 
          (PNotInt (Not (Atom "0")) 
            (PBottomInt Bottom 
            (POrInt (Or (Atom "0") (Not (Atom "0"))) First
              (PHip (Atom "0")))
            (PHip (Not (Or (Atom "0") (Not (Atom "0"))))))))
        (PHip (Not (Or (Atom "0") (Not (Atom "0")))) ) ) )