module Lang where


-- AST para Prop


data Prop =
    Not Prop
  | And Prop Prop
  | Or Prop Prop
  | Implies Prop Prop
  | Atom Int
  | Bottom

data Pos = First | Second

data ProofTree =
    -- Regla trivial
      Trivial Prop ProofTree
    | Hip Prop
    -- Reglas de introducción
    | AndInt Prop ProofTree ProofTree
    | OrInt Prop Pos ProofTree 
    | ImpliesInt Prop (ProofTree -> ProofTree)
    | BottomInt Prop ProofTree ProofTree
    | NotInt Prop ProofTree
    -- Reglas de eliminación
    | AndElim Prop Pos ProofTree
    | OrElim Prop ProofTree (ProofTree -> ProofTree) (ProofTree -> ProofTree)
    | ImpliesElim Prop ProofTree ProofTree 
    | BottomElim Prop Prop
    | DobNotElim Prop ProofTree
    
    
tnd :: ProofTree
tnd = 
  DobNotElim (Or (Atom 0) (Not (Atom 0))) 
    (NotInt (Not (Not (Or (Atom 0) (Not (Atom 0))))) 
      (BottomInt Bottom 
        (OrInt (Or (Atom 0) (Not (Atom 0))) Second 
          (NotInt (Not (Atom 0)) 
            (BottomInt Bottom 
            (OrInt (Or (Atom 0) (Not (Atom 0))) First
              (Hip (Atom 0)))
            (Hip (Not (Or (Atom 0) (Not (Atom 0))))))))
        (Hip (Not (Or (Atom 0) (Not (Atom 0)))) ) ) )