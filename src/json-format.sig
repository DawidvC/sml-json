signature JSON_FORMAT =
  sig
    type outer

    type 'inner stream = 'inner * outer

    datatype indent_char = SPACE | TAB

    val stream : 'inner -> 'inner stream

    val format :
         {
           numberFormat : ('n -> string),
           stringFormat : ('s -> string),
           indentWidth : int,
           indentChar : indent_char
         }
      -> (('n, 's) Token.t, 'inner stream) Reader.t
      -> (string, 'inner stream) Reader.t
  end
