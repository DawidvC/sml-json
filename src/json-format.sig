signature JSON_FORMAT =
  sig
    datatype indent_char = SPACE | TAB

    type 'inner stream

    val stream : 'inner -> 'inner stream

    val format :
         {
           numberFormat : ('n -> string),
           stringFormat : ('s -> string),
           indentWidth : int,
           indentChar : indent_char
         }
      -> (('n, 's) Token.t, 'inner) Reader.t
      -> (string, 'inner stream) Reader.t
  end
