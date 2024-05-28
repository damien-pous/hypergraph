open Hypergraphs
open Types

(* exporting lists of images as multipage documents *)
val multi_pdf: (image * box) list -> string -> unit
val multi_svg: (image * box) list -> string -> unit (* broken? *)

(* single image versions *)
val pdf: image -> box -> string -> unit
val svg: image -> box -> string -> unit

(* alternative for single page SVG documents, via vg.cairo *)
val svg_via_vg: image -> box -> string -> unit


(* below, files are given by their basenames,
   extensions ".hg", ".pdf" or ".svg" are automativally added *)

type term = positionned Term.t

(* reading from / writing to files
   first argument is the basename, extension ".hg" is added *)
val read: string -> term list
val write: string -> term list -> unit

(* exporting to both PDF & SVG *)
val export: string -> term list -> unit

(* does the given HG file already exists *)
val exists: string -> bool
