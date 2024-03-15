open Types

val create:
  width:int ->
  height:int ->
  ?window:< misc : #GDraw.misc_ops; .. > ->
  GMisc.drawing_area ->
  canvas ->
  unit -> 
  <arena; refresh: unit; enable_moves: unit>
