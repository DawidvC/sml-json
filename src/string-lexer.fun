signature STRING_LEXER_ARG =
  sig
    structure Char : CHAR
    structure String : STRING where type char = Char.char
    structure Charset : CHARSET
  end

functor StringLexer(A : STRING_LEXER_ARG) : PARALEXER =
  struct
    structure Char = A.Char
    structure String = A.String
    structure Charset = A.Charset
    structure W8V = Word8Vector

    type repr = String.string

    fun encode codePoint =
      case Charset.encode (Reader.const codePoint) () of
        NONE => raise Fail "BUG: invalid Unicode code point."
      | SOME (vector, _) => vector

    fun asString vector =
      let
        fun asChar (word, result) = (Char.chr (Word8.toInt word)) :: result
      in
        String.implode (W8V.foldr asChar [] vector)
      end

    fun lex decode stream =
      let
        fun hexdig stream result =
          let
            fun add acc word = Word.orb (Word.<< (acc, 0w4), word)
            fun loop stream acc n =
              if n = 0
              then inside stream (W8V.concat [result, encode acc])
              else
                case decode stream of
                  SOME (0wx30, stream) => loop stream (add acc 0wx0) (n - 1)
                | SOME (0wx31, stream) => loop stream (add acc 0wx1) (n - 1)
                | SOME (0wx32, stream) => loop stream (add acc 0wx2) (n - 1)
                | SOME (0wx33, stream) => loop stream (add acc 0wx3) (n - 1)
                | SOME (0wx34, stream) => loop stream (add acc 0wx4) (n - 1)
                | SOME (0wx35, stream) => loop stream (add acc 0wx5) (n - 1)
                | SOME (0wx36, stream) => loop stream (add acc 0wx6) (n - 1)
                | SOME (0wx37, stream) => loop stream (add acc 0wx7) (n - 1)
                | SOME (0wx38, stream) => loop stream (add acc 0wx8) (n - 1)
                | SOME (0wx39, stream) => loop stream (add acc 0wx9) (n - 1)
                | SOME (0wx41, stream) => loop stream (add acc 0wxA) (n - 1)
                | SOME (0wx42, stream) => loop stream (add acc 0wxB) (n - 1)
                | SOME (0wx43, stream) => loop stream (add acc 0wxC) (n - 1)
                | SOME (0wx44, stream) => loop stream (add acc 0wxD) (n - 1)
                | SOME (0wx45, stream) => loop stream (add acc 0wxE) (n - 1)
                | SOME (0wx46, stream) => loop stream (add acc 0wxF) (n - 1)
                | SOME (0wx61, stream) => loop stream (add acc 0wxA) (n - 1)
                | SOME (0wx62, stream) => loop stream (add acc 0wxB) (n - 1)
                | SOME (0wx63, stream) => loop stream (add acc 0wxC) (n - 1)
                | SOME (0wx64, stream) => loop stream (add acc 0wxD) (n - 1)
                | SOME (0wx65, stream) => loop stream (add acc 0wxE) (n - 1)
                | SOME (0wx66, stream) => loop stream (add acc 0wxF) (n - 1)
                | _ => NONE
          in
            loop stream 0w0 4
          end

        and escape stream result =
          case decode stream of
            SOME (0wx22, stream) => inside stream (W8V.append (result, 0wx22))
          | SOME (0wx5C, stream) => inside stream (W8V.append (result, 0wx5C))
          | SOME (0wx2F, stream) => inside stream (W8V.append (result, 0wx2F))
          | SOME (0wx62, stream) => inside stream (W8V.append (result, 0wx08))
          | SOME (0wx66, stream) => inside stream (W8V.append (result, 0wx0C))
          | SOME (0wx6E, stream) => inside stream (W8V.append (result, 0wx0A))
          | SOME (0wx72, stream) => inside stream (W8V.append (result, 0wx0D))
          | SOME (0wx74, stream) => inside stream (W8V.append (result, 0wx09))
          | SOME (0wx75, stream) => hexdig stream result
          | _ => NONE

        and inside stream result =
          case decode stream of
            NONE => SOME (asString result, stream)
          | SOME (0wx22, stream) => SOME (asString result, stream)
          | SOME (0wx5C, stream) => escape stream result
          | SOME (point, stream) =>
              if point = 0wx20
                 orelse point = 0wx21
                 orelse (0wx23 <= point andalso point <= 0wx5B)
                 orelse (0wx5D <= point andalso point <= 0wx10FFFF)
              then inside stream (W8V.concat [result, encode point])
              else NONE
      in
        case decode stream of
          SOME (0wx22, stream) => inside stream (W8V.fromList [])
        | _ => NONE
      end
  end
