module Lang (someFunc) where


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
    | NotElim Prop ProofTree
    
    
