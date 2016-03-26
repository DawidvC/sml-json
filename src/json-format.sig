signature JSON_FORMAT =
  sig
    datatype indent_char = SPACE | TAB

    type 'inner stream

    val stream : 'inner -> 'inner stream

    val format :
         {
           numberFormat : ('num -> string),
           stringFormat : ('str -> string),
           indentWidth : int,
           indentChar : indent_char
         }
      -> (('num, 'str) Token.t, 'inner) Reader.t
      -> (string, 'inner stream) Reader.t
  end
