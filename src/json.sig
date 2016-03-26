signature JSON =
  sig
    datatype ('n, 's) t =
      OBJECT of ('s * ('n, 's) t) list
    | ARRAY of ('n, 's) t list
    | NUMBER of 'n
    | STRING of 's
    | FALSE
    | TRUE
    | NULL

    type number
    type string

    val decodeFile : String.string -> (number, string) t option
    val decodeString : String.string -> (number, string) t option
    val encodeString : { spaces : String.string } -> (number, string) t -> String.string
  end
