open Common

(** graphs with inner elements decorated with 'a values *)
include INITIAL_ALGEBRA                (* TODO: INITIAL *)

type ivertex
type edge
type vertex = Src of int | Inn of ivertex

val iter_edges: (edge -> 'a -> vertex seq -> unit) -> 'a t -> unit
val iter_ivertices: (ivertex -> 'a -> unit) -> 'a t -> unit

val neighbours: 'a t -> edge -> vertex seq
val einfo: 'a t -> edge -> 'a
val iinfo: 'a t -> ivertex -> 'a

val map': ('a -> 'b) -> ((ivertex -> 'b) -> 'a -> vertex seq -> 'b) -> 'a t -> 'b t

(* isomorphim check, using the given function to compare edge infos *)
val iso : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool

(* printing in dot format *)
val pp_dot: Format.formatter -> info t -> unit

(** graphs with *all* elements decorated with 'a values *)
type 'a graph = 'a t
module Sourced: sig
  type 'a t = 'a seq * 'a graph
  val vpos: info t -> vertex -> p2
  val center_edge: info t -> edge -> unit
  val map :
    fs:('a -> 'b) ->
    fi:('a -> 'b) ->
    fe:('a -> 'b) ->
    'a t -> 'b t
  val pp_gen :
    pps:(Format.formatter -> 'a -> unit) ->
    ppi:(Format.formatter -> 'a -> unit) ->
    ppe:(Format.formatter -> 'a -> unit) ->
    Format.formatter -> 'a t -> unit
end
type 'a sgraph = 'a Sourced.t

(** dynamic graphs, which can be modified *)
class ['a] dyn: 
  object
    method set: 'a sgraph -> unit

    method sources: 'a seq
    method sgraph: 'a sgraph
    method graph: 'a graph
    method arity: int

    method neighbours: edge -> vertex seq
    method einfo: edge -> 'a
    method sinfo: int -> 'a
    method iinfo: ivertex -> 'a
    method vinfo: vertex -> 'a

    method iter_sources: (int -> 'a -> unit) -> unit
    method iter_ivertices: (ivertex -> 'a -> unit) -> unit
    method iter_vertices: (vertex -> 'a -> unit) -> unit
    method iter_edges: (edge -> 'a -> vertex seq -> unit) -> unit
    method iter_infos: ('a -> unit) -> unit

    method add_edge: 'a -> vertex seq -> edge
    method add_ivertex: 'a -> ivertex
    method rem_edge: edge -> unit
    method rem_ivertex: ivertex -> unit
    method rem_source: int -> unit
    method rem_vertex: vertex -> unit
    method promote: ivertex -> unit
    method permute: Perm.t -> unit
    method forget: int -> unit
    method lift: 'a -> unit

    method find: ('a -> bool) -> [`V of vertex * 'a | `E of edge * 'a | `N]
  end
