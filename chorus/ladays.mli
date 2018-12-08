
open Result

val version: string


module Type: sig


  type 'a t
  (** The type for runtime representation of values of type ['a]. *)

  (** {1:primitives Primitives} *)

  val unit: unit t
  (** [unit] is a representation of the unit type. *)

  val bool: bool t
  (** [bool] is a representation of the boolean type. *)

  val char: char t
  (** [char] is a representation of the character type. *)

  val int32: int32 t
  (** [int32] is a representation of the 32-bit integers type. *)

  val int64: int64 t
  (** [int64] is a representation of the 64-bit integer type. *)

  val float: float t
  (** [float] is a representation of the float type. *)

  val string: string t
  (** [string] is a representation of the string type. *)

  val cstruct: Cstruct.t t
  (** [cstruct] is a representation of the [Cstruct.t] type. *)

  val list: 'a t -> 'a list t
  (** [list t] is a representation of list of values of type [t]. *)

  val array: 'a t -> 'a array t
  (** [array t] is a representation of array of values of type [t]. *)

  val option: 'a t -> 'a option t
  (** [option t] is a representation of value of type [t option]. *)

  val pair: 'a t -> 'b t -> ('a * 'b) t
  (** [pair x y] is a representation of values of type [x * y]. *)

  val triple: 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
  (** [triple x y z] is a representation of values of type [x * y *
      z]. *)

  val result: 'a t -> 'b t -> ('a, 'b) result t
  (** [result a b] is a representation of values of type [(a, b)
      result]. *)

  (** {1:records Records} *)

  type ('a, 'b) field
  (** The type for fields holding values of type ['b] and belonging to a
      record of type ['a]. *)

  val field: string -> 'a t -> ('b -> 'a) -> ('b, 'a) field
  (** [field n t g] is the representation of the field [n] of type [t]
      with getter [g].

      For instance:

      {[
        type manuscript = { title : string option }

        let manuscript = field "title" (option string) (fun t -> t.title)]}
  *)

  type ('a, 'b, 'c) open_record
  (** The type for representing open records of type ['a] with
      constructors of type ['b]. ['c] represents the fields missings to
      the record, e.g. an open record initially holds ['c = 'b] and it
      can can be {{!sealr}sealed} when ['c = 'a]. *)

  val sealr: ('a, 'b, 'a) open_record -> 'a t
  (** [sealr r] seal the open record [r]. *)

  val (|+):
    ('a, 'b, 'c -> 'd) open_record -> ('a, 'c) field -> ('a, 'b, 'd) open_record
  (** [r |+ f] adds the field [f] to the open record [r]. *)

  val record: string -> 'b -> ('a, 'b, 'b) open_record
  (** [record n f fs] is the representation of the record called [n] of
      type ['a] using [f] as constructor and with the fields [fs].

      Putting all together:

      {[
        type menu = { restaurant: string; items: (string * int32) list; }

        let t =
          record "t" (fun restaurant items -> {restaurant; items})
          |+ field "restaurant" string (fun t -> t.restaurant)
          |+ field "items" (list (pair string int32)) (fun t -> t.items)
          |> sealr]}
  *)

  (** {1:variants Variants} *)

  type ('a, 'b) case
  (** The type for representing variant cases of type ['a] with
      patterns of type ['b]. *)

  type 'a case_p
  (** The type for representing patterns for a variant of type ['a]. *)

  val case0: string -> 'a -> ('a, 'a case_p) case
  (** [case0 n v] is a representation of a variant case [n] with no
      argument and a singleton pattern. e.g.

      {[
        type t = Foo

        let foo = case0 "Foo" Foo]}
  *)

  val case1: string -> 'b t -> ('b -> 'a) -> ('a, 'b -> 'a case_p) case
  (** [case1 n t c] is a representation of a variant case [n] with 1
      argument of type [t] and a pattern [c] an function with one argument
      of type [t]. e.g.

      {[
        type t = Foo of string

        let foo = case1 "Foo" string (fun s -> Foo s)]}
  *)

  type ('a, 'b, 'c) open_variant
  (** The type for representing open variants of type ['a] with pattern
      matching of type ['b]. ['c] represents the missing cases for the
      variant, e.g. initially variant hols [c' = 'b] and it can be
      {{!sealv}sealed} when ['c = 'a].  *)

  val sealv: ('a, 'b, 'a -> 'a case_p) open_variant -> 'a t
  (** [sealv v] seals the open variant [v]. *)

  val (|~):
    ('a, 'b, 'c -> 'd) open_variant -> ('a, 'c) case -> ('a, 'b, 'd) open_variant
  (** [v |~ c] is the map [v] augmented with the case [c]. *)

  val variant: string -> 'b -> ('a, 'b, 'b) open_variant
  (** [variant n c p] is a representation of a variant type containing
      the cases [c] and using [p] to deconstruct values.

      Putting all together:

      {[
        type t = Foo | Bar of string

        let t =
          variant "t" (fun foo bar -> function
              | Foo   -> foo
              | Bar s -> bar s)
          |~ case0 "Foo" Foo
          |~ case1 "Bar" string (fun x -> Bar x)
          |> sealr]}
  *)

  val enum: string -> (string * 'a) list -> 'a t
  (** [enum n l] is a representation of the variant type which has
      only constant variant case. e.g.

      {[
        type t = Foo | Bar | Toto

        let t = enum "t" ["Foo", Foo; "Bar", Bar; "Toto", Toto]]}
  *)

  (** {1:recursive Recursive definitions}

      [Type] allows to create a limited form of recursive records and
      variants.

      {b TODO}: describe the limitations, e.g. only regular recursion
      and no use of the generics inside the [mu*] functions and the
      usual caveats with recursive values (such as infinite loops on
      most of the generics which don't check sharing).

  *)

  val mu: ('a t -> 'a t) -> 'a t
  (** [mu f] is the representation [r] such that [r = mu r].

      For instance:

      {[
        type x = { x: x option }

        let x = mu (fun x ->
            record "x" (fun x -> { x })
            |+ field "x" x (fun x -> x.x)
            |> sealr)]}
  *)

  val mu2: ('a t -> 'b t -> 'a t * 'b t) -> 'a t * 'b t
  (** [mu2 f] is the representations [r] and [s] such that [r, s = mu2 r
      s].

      For instance:

      {[
        type r = { foo: int; bar: string list; z: z option }
        and z = { x: int; r: r list }

        (* Build the representation of [r] knowing [z]'s. *)
        let mkr z =
          record "r" (fun foo bar z -> { foo; bar; z })
          |+ field "foo" int (fun t -> t.foo)
          |+ field "bar" (list string) (fun t -> t.bar)
          |+ field "z" (option z) (fun t -> t.z)
          |> sealr

        (* And the representation of [z] knowing [r]'s. *)
        let mkz r =
          record "z" (fun x r -> { x; r })
          |+ field "x" int (fun t -> t.x)
          |+ field "r" (list r) (fun t -> t.r)
          |> sealr

        (* Tie the loop. *)
        let r, z = mu2 (fun r z -> mkr z, mkz y)]}
  *)

  (** {1:proj Bijections}

      Sometimes it is not always possible to describe precisely a type
      (or it could be too tedious) and it is easier to describe the
      relation with an other know type. This is what bijections are
      about.
  *)

  val like: 'a t -> ('a -> 'b) -> ('b -> 'a) -> 'b t
  (** [like x f g] is the description of a type which looks like [x]
      using the bijetion [(f, g)]. *)

  (** {1:generics Generic Operations}

      Given a value ['a t], it is possible to define generic operations
      on value of type ['a] such as pretty-printing, parsing and
      unparsing.
  *)

  val dump: 'a t -> 'a Fmt.t
  (** [dump t] dumps the values of type [t] as a parsable OCaml
      expression. *)

  val equal: 'a t -> 'a -> 'a -> bool
  (** [equal t] is the equality function between values of type [t]. *)

  val compare: 'a t -> 'a -> 'a -> int
  (** [compare t] compares values of type [t]. *)

  (** {2 JSON converters} *)

  val pp_json: ?minify:bool -> 'a t -> 'a Fmt.t
  (** Similar to {!dump} but pretty-prints the JSON representation instead
      of the OCaml one. See {!encode_json} for details about the encoding.

      For instance:

      {[
        type t = { foo: int option; bar: string list };;

        let t =
          record "r" (fun foo bar -> { foo; bar })
          |+ field "foo" (option int) (fun t -> t.foo)
          |+ field "bar" (list string) (fun t -> t.bar)
          |> sealr

        let s = Fmt.strf "%a\n" (pp t) { foo = None; bar = ["foo"] }
        (* s is "{ foo = None; bar = [\"foo\"]; }" *)

        let j = Fmt.strf "%a\n" (pp_json t) { foo = None; bar = ["foo"] }
        (* j is "{ \"bar\":[\"foo\"] }" *)]}

      {b NOTE:} this will automatically convert JSON fragments to valid
      JSON objects by adding an enclosing array if necessary. *)

  val encode_json: 'a t -> Jsonm.encoder -> 'a -> unit
  (** [encode_json t e] encodes [t] into the
      {{:http://erratique.ch/software/jsonm}jsonm} encoder [e]. The
      encoding is a relatively straightforward translation of the OCaml
      structure into JSON. The main highlights are:

      {ul
      {- OCaml [ints] are translated into JSON floats.}
      {- OCaml strings are translated into JSON strings. You must then
         ensure that the OCaml strings contains only valid UTF-8
         characters.}
      {- OCaml record fields of type ['a option] are automatically
         unboxed in their JSON representation. If the value if [None],
         the field is removed from the JSON object.}
      {- variant cases built using {!case0} are represented as strings.}
      {- variant cases built using {!case1} are represented as a record
         with one field; the field name is the name of the variant.}
      }

      {b NOTE:} this can be used to encode JSON fragments. That's the
      responsibility of the caller to ensure that the encoded JSON
      fragment fits properly into a well-formed JSON object. *)

  val decode_json: 'a t -> Jsonm.decoder -> ('a, [`Msg of string]) result
  (** [decode_json t e] decodes values of type [t] from the
      {{:http://erratique.ch/software/jsonm}jsonm} decoder [e]. *)

  val decode_json_lexemes: 'a t -> Jsonm.lexeme list ->
    ('a, [`Msg of string]) result
  (** [decode_json_lexemes] is similar to {!decode_json} but use an
      already decoded list of JSON lexemes instead of a decoder. *)

  val encode_cstruct: 'a t -> 'a -> Cstruct.t
  (** [encode_cstruct t e] encodes [t] into a `Cstruct.t`. The size of
      the returned buffer is precomputed and the buffer is allocated
      at once.

      {b NOTE:} There is a special case when the parameter [t] is a
      single [cstruct]: the original value is returned as is, without
      being copied. *)

  val decode_cstruct: 'a t -> Cstruct.t -> ('a, [`Msg of string]) result
  (** [decode_cstruct t buf] decodes values of type [t] as produced by
      [encode_cstruct t v].

      {b NOTE:} When the parameter [t] is a single [cstruct], the
      original buffer is returned as is, otherwise sub-[cstruct] are
      copied. *)

end

(** Commit info are used to keep track of the origin of write
    operations in the stores. [Info] model the metadata associated
    with commit objects in Git. *)
module Info: sig

  (** {1 Commit Info} *)

  type t
  (** The type for commit info. *)

  val v: date:int64 -> author:string -> string -> t
  (** Create a new commit info. *)

  val date: t -> int64
  (** [date t] is [t]'s commit date.

      The date provided by the user when calling the {{!Info.v}create}
      function. Rounding [Unix.gettimeofday ()] (when available) is a
      good value for such date. On more esoteric platforms, any
      monotonic counter is a fine value as well. On the Git backend,
      the date is translated into the commit {e Date} field and is
      expected to be the number of POSIX seconds (thus not counting
      leap seconds) since the Epoch. *)

  val author: t -> string
  (** [author t] is [t]'s commit author.

      The author identifies the entity (human, unikernel, process,
      thread, etc) performing an operation. For the Git backend, this
      will be directly translated into the {e Author} field. *)

  val message: t -> string
  (** [message t] is [t]'s commit message. *)

  val empty: t
  (** The empty commit info. *)

  (** {1 Info Functions} *)

  type f = unit -> t
  (** Alias for functions which can build commit info. *)

  val none: f
  (** The empty info function. [none ()] is [empty] *)

  (** {1 Value Types} *)

  val t: t Type.t
  (** [t] is the value type for {!t}. *)

end
