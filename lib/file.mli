open Types

type term = positionned Term.t

(* non-empty lists of equivalent terms, the last of which is used for rendering *)
type t

(* starting value: a single term *)
val single: term -> t 

(* is a new term compatible with those in the file? *)
val compatible: t -> term -> bool

(* append a new term (a priori a properly placed one),
   which is assumed to be equivalent to the previous ones *)
val append: t -> term -> t

(* first and last elements *)
val first: t -> term
val last: t -> term

(* reading from / writing to files
   first argument is the basename, extension ".hg" is added *)
val read: string -> t
val write: string -> t -> unit

(* exporting to both PDF & SVG 
   first argument is the basename, extensions ".pdf" and ".svg" are added *)
val export_term: string -> term -> unit
val export: string -> t -> unit

(* does the given HG file already exists *)
val exists: string -> bool
