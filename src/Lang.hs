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

data ProofTree =
    -- Regla trivial
      Trivial SProp ProofTree
    | Hip SProp
    -- Reglas de introducción
    | AndInt SProp ProofTree ProofTree
    | OrInt SProp Pos ProofTree 
    | ImpliesInt SProp (ProofTree -> ProofTree)
    | BottomInt SProp ProofTree ProofTree
    | NotInt SProp ProofTree
    -- Reglas de eliminación
    | AndElim SProp Pos ProofTree
    | OrElim SProp ProofTree (ProofTree -> ProofTree) (ProofTree -> ProofTree)
    | ImpliesElim SProp ProofTree ProofTree 
    | BottomElim SProp SProp
    | DobNotElim SProp ProofTree
    
    
tnd :: ProofTree
tnd = 
  DobNotElim (Or (Atom "0") (Not (Atom "0"))) 
    (NotInt (Not (Not (Or (Atom "0") (Not (Atom "0"))))) 
      (BottomInt Bottom 
        (OrInt (Or (Atom "0") (Not (Atom "0"))) Second 
          (NotInt (Not (Atom "0")) 
            (BottomInt Bottom 
            (OrInt (Or (Atom "0") (Not (Atom "0"))) First
              (Hip (Atom "0")))
            (Hip (Not (Or (Atom "0") (Not (Atom "0"))))))))
        (Hip (Not (Or (Atom "0") (Not (Atom "0")))) ) ) )