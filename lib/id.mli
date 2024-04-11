module Make(_: sig val prefix: string end): sig
  type t
  val fresh: unit -> t
  val to_int: t -> int
  val pp: Format.formatter -> t -> unit
  
  type 'a map
  val get: 'a map -> t -> 'a
  val empty: 'a map
  val add: t -> 'a -> 'a map -> 'a map
  val single: t -> 'a -> 'a map
  val rem: t -> 'a map -> 'a map
  val union: 'a map -> 'a map -> 'a map
  val iter: (t -> 'a -> unit) -> 'a map -> unit
  val map: ('a -> 'b) -> 'a map -> 'b map
  val imap: (t -> 'a -> 'b) -> 'a map -> 'b map
  val omap: ('a -> 'b option) -> 'a map -> 'b map
  val find: (t -> 'a -> bool) -> 'a map -> (t*'a) option
  val size: 'a map -> int
  
  val keys: 'a map -> t MSet.t
  val values: 'a map -> 'a MSet.t
end
