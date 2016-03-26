structure JSONFormat :> JSON_FORMAT =
  struct
    datatype indent_char = SPACE | TAB

    type 'inner stream = 'inner * { level : int, isMember : bool }

    fun stream inner =
      (inner, { level = 0, isMember = false })

    fun innerStream (inner, _) = inner

    fun outerStream (_, outer) = outer

    fun incLevel { level, isMember } =
      { level = level + 1, isMember = isMember }

    fun decLevel { level, isMember } =
      { level = level - 1, isMember = isMember }

    fun setMember { level, isMember } =
      { level = level, isMember = true }

    fun unsetMember { level, isMember } =
      { level = level, isMember = false }

    fun composeReaders reader f stream =
      case reader (innerStream stream) of
        NONE => NONE
      | SOME (a, inner) =>
        case f a (outerStream stream) of
          (b, outer) => SOME (b, (inner, outer))

    fun format { numberFormat, stringFormat, indentWidth, indentChar } nextToken stream =
      let
        val indentChar = case indentChar of SPACE => " " | TAB => "\t"
        val newLine = if indentWidth = 0 then "" else "\n"
        val colon = if indentWidth = 0 then ":" else ": "

        fun repeat n = String.repeat (indentWidth * n) indentChar

        fun formatToken token (stream as { level, isMember }) =
          let
            open Token
            fun pad n = if isMember then "" else repeat n
            fun belowDedent str = newLine ^ pad (level - 1) ^ str
            val indent = pad level
          in
            case token of
              NUMBER n => (indent ^ numberFormat n, unsetMember stream)
            | STRING s => (indent ^ stringFormat s, unsetMember stream)
            | NULL     => (indent ^ "null", unsetMember stream)
            | TRUE     => (indent ^ "true", unsetMember stream)
            | FALSE    => (indent ^ "false", unsetMember stream)
            | COLON    => (colon, setMember stream)
            | COMMA    => ("," ^ newLine, unsetMember stream)
            | LSQUARE  => (indent ^ "[" ^ newLine, unsetMember (incLevel stream))
            | LCURLY   => (indent ^ "{" ^ newLine, unsetMember (incLevel stream))
            | RSQUARE  => (belowDedent "]", unsetMember (decLevel stream))
            | RCURLY   => (belowDedent "}", unsetMember (decLevel stream))
          end
      in
        composeReaders nextToken formatToken stream
      end
  end
