signature PARSER =
  sig
    val parse : (('num, 'str) Token.t, 'stream) Reader.t -> (('num, 'str) JSON.t, 'stream) Reader.t
  end
