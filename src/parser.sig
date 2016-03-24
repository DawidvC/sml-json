signature PARSER =
  sig
    val parse : (('num, 'str) Token.t, 'stream) Reader.t -> (('num, 'str) Value.t, 'stream) Reader.t
  end
