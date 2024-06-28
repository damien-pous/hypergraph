open Types

class virtual generic: 
  object
    (* float*float are in device space, leaving points/vectors for canvas space *)
    method virtual private dpointer: float*float
    method virtual private dsize: float*float
    (* refresh called on ensure/zoom/move, but not on resize *)
    inherit arena    
  end
