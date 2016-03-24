structure Encoding :> ENCODING =
  struct
    datatype t = datatype Encoding.t

    (**
     * The original JSON specification, RFC 4627, presents a method of finding
     * the encoding of a JSON stream by looking at the pattern of nulls in the
     * first four bytes. It is described in §3 as such:
     *
     * > Since the first two characters of a JSON text will always be ASCII
     * > characters [RFC0020], it is possible to determine whether an octet
     * > stream is UTF-8, UTF-16 (BE or LE), or UTF-32 (BE or LE) by looking
     * > at the pattern of nulls in the first four octets.
     * >
     * >        00 00 00 xx  UTF-32BE
     * >        00 xx 00 xx  UTF-16BE
     * >        xx 00 00 00  UTF-32LE
     * >        xx 00 xx 00  UTF-16LE
     * >        xx xx xx xx  UTF-8
     *
     * However, RFC 4627 assumed that the root value of a JSON stream is either
     * an object or an arrat. Later on, RFC 7159 allowed any scalar value to be
     * used as a JSON text, which means that a standalone JSON string is now a
     * valid JSON text. For this reason, we no longer have the guarantee that
     * the first two characters in a JSON text are always ASCII-encodable. For
     * example, when the JSON text is a string whose first character is an emoji
     * we clearly have a non-ASCII-encodable character after the opening quote
     * of the string.
     *
     * Because of this, we're relaxing the patterns for determining UTF-16
     * encoded values by looking only at the first two octets.
     *)
    fun guess (reader : (Word8.word, 's) StringCvt.reader) stream =
      let
        fun fromPattern stream =
          case Reader.grouped 4 reader stream of
            NONE => NONE
          | SOME (0w0 :: 0w0 :: 0w0 :: _   :: _, _) => SOME UTF32BE
          | SOME (_   :: 0w0 :: 0w0 :: 0w0 :: _, _) => SOME UTF32LE
          | SOME (0w0 :: _   :: _, _) => SOME UTF16BE
          | SOME (_   :: 0w0 :: _, _) => SOME UTF16LE
          | SOME _ => SOME UTF8
      in
        case Encoding.fromBOM reader stream of
          NONE => fromPattern stream
        | SOME (encoding, _) => SOME encoding
      end
  end
