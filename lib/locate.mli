open Types

type term = Types.positionned Term.t

class virtual locate: arena ->
  object
    method virtual entry: string
    method virtual set_entry: 'a. ('a, Format.formatter, unit, unit) format4 -> 'a
    method virtual warning: 'a. ('a, Format.formatter, unit, unit) format4 -> 'a
    method virtual info: 'a. ('a, Format.formatter, unit, unit) format4 -> 'a
    method virtual help: 'a. ('a, Format.formatter, unit, unit) format4 -> 'a

    method private virtual read: string -> term list
    method private virtual write: string -> term list -> unit
    method private virtual export: string -> term list -> unit
    
    method on_button_press: unit
    method on_button_release: unit
    method on_motion: unit
    method on_key_press: string -> unit
    method on_entry_changed: unit

    method undo: unit -> unit
    method redo: unit -> unit
    method normalise_term: unit -> unit
    method desugar_term: unit -> unit

    method load_from: string -> unit
    method save_to: string -> unit

    method init: string list -> unit
  end

