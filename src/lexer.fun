functor Lexer(A : LEXER_ARG) :> LEXER
  where type NumberLexer.repr = A.NumberLexer.repr
    and type StringLexer.repr = A.StringLexer.repr =
  struct
    open A
    open Token

    fun lex encoding reader stream =
      let
        val decode =
          case encoding of
            Encoding.UTF8 => UTF8.decode reader
          | Encoding.UTF16BE => UTF16BE.decode reader
          | Encoding.UTF16LE => UTF16LE.decode reader
          | Encoding.UTF32BE => UTF32BE.decode reader
          | Encoding.UTF32LE => UTF32LE.decode reader

        fun skipWS stream =
          case decode stream of
            SOME (0wx20, stream) => skipWS stream
          | SOME (0wx09, stream) => skipWS stream
          | SOME (0wx0A, stream) => skipWS stream
          | SOME (0wx0D, stream) => skipWS stream
          | _ => stream

        fun constant points token stream =
          let
            fun recur [] stream = SOME (token, stream)
              | recur (a :: rest) stream =
                case decode stream of
                  NONE => NONE
                | SOME (b, stream) => if a = b then recur rest stream else NONE
          in
            recur points stream
          end

        fun false stream =
          constant [0wx61, 0wx6c, 0wx73, 0wx65] FALSE stream

        fun null stream =
          constant [0wx75, 0wx6c, 0wx6c] NULL stream

        fun true stream =
          constant [0wx72, 0wx75, 0wx65] TRUE stream
      in
        case decode (skipWS stream) of
          NONE => NONE
        | SOME (0wx5B, stream) => SOME (LSQUARE, stream)
        | SOME (0wx7B, stream) => SOME (LCURLY, stream)
        | SOME (0wx5D, stream) => SOME (RSQUARE, stream)
        | SOME (0wx7D, stream) => SOME (RCURLY, stream)
        | SOME (0wx3A, stream) => SOME (COLON, stream)
        | SOME (0wx2C, stream) => SOME (COMMA, stream)
        | SOME (0wx66, stream) => false stream
        | SOME (0wx6E, stream) => null stream
        | SOME (0wx74, stream) => true stream
        | SOME (0wx22, _) => Reader.map STRING (StringLexer.lex decode) stream
        | SOME (point, _) => Reader.map NUMBER (NumberLexer.lex decode) stream
      end
  end
