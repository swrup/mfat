type sfn
type spath
type 'blk t
type entry = { name: sfn; is_dir: bool; size: int32 }

module Sfn : sig
  type t = sfn

  val root : t
  val base : t -> string
  val ext : t -> string
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val of_string : string -> (t, [> `Msg of string ]) result
  val to_string : t -> string
  val pp : Format.formatter -> t -> unit
end

module Spath : sig
  type t = spath

  val root : t
  val v : sfn -> t
  val add : t -> sfn -> t
  val ( / ) : t -> sfn -> t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val of_string : string -> (t, [> `Msg of string ]) result
  val to_string : t -> string
  val pp : Format.formatter -> t -> unit
end

module type BLOCK = sig
  type t

  val pagesize : t -> int
  val read : t -> src_off:int -> ?dst_off:int -> Bstr.t -> unit
  val write : t -> ?src_off:int -> dst_off:int -> Bstr.t -> unit
end

module Make (Blk : BLOCK) : sig
  val create : Blk.t -> (Blk.t t, [> `Msg of string ]) result
  val ls : Blk.t t -> spath -> (entry list, [> `Msg of string ]) result
  val read : Blk.t t -> spath -> (string, [> `Msg of string ]) result
  val write : Blk.t t -> spath -> string -> (unit, [> `Msg of string ]) result
  val mkdir : Blk.t t -> spath -> (unit, [> `Msg of string ]) result
  val remove : Blk.t t -> spath -> (unit, [> `Msg of string ]) result
  val exists : Blk.t t -> spath -> bool
  val stat : Blk.t t -> spath -> (entry, [> `Msg of string ]) result
end
