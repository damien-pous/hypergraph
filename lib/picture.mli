open Types

class basic_canvas: canvas
class void_canvas: canvas

class virtual virtual_arena:
  object
    (* float*float are in device space, leaving points/vectors for canvas space *)
    method virtual private dpointer: float*float
    method virtual private dsize: float*float
    method virtual private refresh: unit (* called on ensure/zoom/move, but not on resize *)
    inherit arena    
  end
