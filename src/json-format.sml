structure JSONFormat :> JSON_FORMAT =
  struct
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

    fun seqCompositeReaders readerA readerB stream =
      case readerA (innerStream stream) of
        NONE => NONE
      | SOME (a, inner) =>
        case readerB a (outerStream stream) of
          (b, outer) => SOME (b, (inner, outer))

    fun format { numberFormat, stringFormat, indentChars } nextToken stream =
      let
        val newLine = if indentChars = "" then "" else "\n"
        val colon = if indentChars = "" then ":" else ": "

        fun repeat n = String.repeat n indentChars

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
        seqCompositeReaders nextToken formatToken stream
      end
  end
