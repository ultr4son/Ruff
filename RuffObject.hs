module RuffObject where
  import Data.Map
  import RuffModel


  data Object r = Object r (Map String ([FArg]->FArg)) (Map String (r->[FArg]->(FArg, r)))
