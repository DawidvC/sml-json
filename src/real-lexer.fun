signature REAL_LEXER_ARG =
  sig
    type real
    val scan : (char, 'a) Reader.t -> (real, 'a) Reader.t
  end

functor RealLexer(Real : REAL_LEXER_ARG) : PARALEXER =
  struct
    type repr = Real.real

    datatype state = NEG | INT | INT' | FRAC | DEC | DEC' | EXP | EXP'

    fun lex decode stream =
      let
        fun exp' point =
          case point of
            0wx30 => SOME (#"0", EXP')
          | 0wx31 => SOME (#"1", EXP')
          | 0wx32 => SOME (#"2", EXP')
          | 0wx33 => SOME (#"3", EXP')
          | 0wx34 => SOME (#"4", EXP')
          | 0wx35 => SOME (#"5", EXP')
          | 0wx36 => SOME (#"6", EXP')
          | 0wx37 => SOME (#"7", EXP')
          | 0wx38 => SOME (#"8", EXP')
          | 0wx39 => SOME (#"9", EXP')
          | _ => NONE

        fun exp point =
          case point of
            0wx2B => SOME (#"+", EXP')
          | 0wx2D => SOME (#"-", EXP')
          | 0wx30 => SOME (#"0", EXP')
          | 0wx31 => SOME (#"1", EXP')
          | 0wx32 => SOME (#"2", EXP')
          | 0wx33 => SOME (#"3", EXP')
          | 0wx34 => SOME (#"4", EXP')
          | 0wx35 => SOME (#"5", EXP')
          | 0wx36 => SOME (#"6", EXP')
          | 0wx37 => SOME (#"7", EXP')
          | 0wx38 => SOME (#"8", EXP')
          | 0wx39 => SOME (#"9", EXP')
          | _ => NONE

        fun dec' point =
          case point of
            0wx30 => SOME (#"0", DEC')
          | 0wx31 => SOME (#"1", DEC')
          | 0wx32 => SOME (#"2", DEC')
          | 0wx33 => SOME (#"3", DEC')
          | 0wx34 => SOME (#"4", DEC')
          | 0wx35 => SOME (#"5", DEC')
          | 0wx36 => SOME (#"6", DEC')
          | 0wx37 => SOME (#"7", DEC')
          | 0wx38 => SOME (#"8", DEC')
          | 0wx39 => SOME (#"9", DEC')
          | 0wx65 => SOME (#"e", EXP)
          | 0wx45 => SOME (#"E", EXP)
          | _ => NONE

        fun dec point =
          case point of
            0wx30 => SOME (#"0", DEC')
          | 0wx31 => SOME (#"1", DEC')
          | 0wx32 => SOME (#"2", DEC')
          | 0wx33 => SOME (#"3", DEC')
          | 0wx34 => SOME (#"4", DEC')
          | 0wx35 => SOME (#"5", DEC')
          | 0wx36 => SOME (#"6", DEC')
          | 0wx37 => SOME (#"7", DEC')
          | 0wx38 => SOME (#"8", DEC')
          | 0wx39 => SOME (#"9", DEC')
          | _ => NONE

        fun frac point =
          if point = 0wx2E then SOME (#".", DEC) else NONE

        fun int' point =
          case point of
            0wx30 => SOME (#"0", INT')
          | 0wx31 => SOME (#"1", INT')
          | 0wx32 => SOME (#"2", INT')
          | 0wx33 => SOME (#"3", INT')
          | 0wx34 => SOME (#"4", INT')
          | 0wx35 => SOME (#"5", INT')
          | 0wx36 => SOME (#"6", INT')
          | 0wx37 => SOME (#"7", INT')
          | 0wx38 => SOME (#"8", INT')
          | 0wx39 => SOME (#"9", INT')
          | 0wx2E => SOME (#".", DEC)
          | 0wx65 => SOME (#"e", EXP)
          | 0wx45 => SOME (#"E", EXP)
          | _ => NONE

        fun int point =
          case point of
            0wx30 => SOME (#"0", FRAC)
          | 0wx31 => SOME (#"1", INT')
          | 0wx32 => SOME (#"2", INT')
          | 0wx33 => SOME (#"3", INT')
          | 0wx34 => SOME (#"4", INT')
          | 0wx35 => SOME (#"5", INT')
          | 0wx36 => SOME (#"6", INT')
          | 0wx37 => SOME (#"7", INT')
          | 0wx38 => SOME (#"8", INT')
          | 0wx39 => SOME (#"9", INT')
          | _ => NONE

        fun neg point =
          case point of
            0wx2D => SOME (#"-", INT)
          | 0wx30 => SOME (#"0", FRAC)
          | 0wx31 => SOME (#"1", INT')
          | 0wx32 => SOME (#"2", INT')
          | 0wx33 => SOME (#"3", INT')
          | 0wx34 => SOME (#"4", INT')
          | 0wx35 => SOME (#"5", INT')
          | 0wx36 => SOME (#"6", INT')
          | 0wx37 => SOME (#"7", INT')
          | 0wx38 => SOME (#"8", INT')
          | 0wx39 => SOME (#"9", INT')
          | _ => NONE

        fun transform stream f =
          case decode stream of
            NONE => NONE
          | SOME (point, stream) =>
            case f point of
              NONE => NONE
            | SOME (char, state) => SOME (char, (stream, state))

        fun reader (stream, state) =
          case state of
            NEG  => transform stream neg
          | INT  => transform stream int
          | INT' => transform stream int'
          | FRAC => transform stream frac
          | DEC  => transform stream dec
          | DEC' => transform stream dec'
          | EXP  => transform stream exp
          | EXP' => transform stream exp'
      in
        case Real.scan reader (stream, NEG) of
          NONE => NONE
        | SOME (real, (stream, _)) => SOME (real, stream)
      end
  end
