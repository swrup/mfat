module Sfn : sig
  type t

  val compare : t -> t -> int
  val equal : t -> t -> bool
  val root : t
  val of_string : string -> (t, [> `Msg of string ]) result
  val pp : Format.formatter -> t -> unit
end

module Spath : sig
  type t

  val compare : t -> t -> int
  val equal : t -> t -> bool
  val root : t
  val add : t -> Sfn.t -> t
  val ( / ) : t -> Sfn.t -> t
  val of_string : string -> (t, [> `Msg of string ]) result
  val pp : Format.formatter -> t -> unit
end

type 'blk t
type entry = { name: Sfn.t; is_dir: bool; size: int32 }

module type BLOCK = sig
  type t

  val pagesize : t -> int
  val read : t -> src_off:int -> ?dst_off:int -> Bstr.t -> unit
  val write : t -> ?src_off:int -> dst_off:int -> Bstr.t -> unit
end

module Make (Blk : BLOCK) : sig
  val create : Blk.t -> (Blk.t t, [> `Msg of string ]) result
  val ls : Blk.t t -> Spath.t -> (entry list, [> `Msg of string ]) result
  val read : Blk.t t -> Spath.t -> (string, [> `Msg of string ]) result
  val write : Blk.t t -> Spath.t -> string -> (unit, [> `Msg of string ]) result
  val mkdir : Blk.t t -> Spath.t -> (unit, [> `Msg of string ]) result
  val remove : Blk.t t -> Spath.t -> (unit, [> `Msg of string ]) result
  val exists : Blk.t t -> Spath.t -> bool
  val stat : Blk.t t -> Spath.t -> (entry, [> `Msg of string ]) result
end
