signature JSON_FORMAT =
  sig
    type 'inner stream

    val stream : 'inner -> 'inner stream

    val format :
         {
           numberFormat : ('num -> string),
           stringFormat : ('str -> string),
           indentChars : string
         }
      -> (('num, 'str) Token.t, 'inner) Reader.t
      -> (string, 'inner stream) Reader.t
  end
