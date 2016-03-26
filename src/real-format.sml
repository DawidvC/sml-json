(*
 * Adapted from an original version by Emden Gansner and John Reppy located at:
 * http://www.mpi-sws.org/~joshua/stardust/current/src/nj-util/real-format.sml
 *)
structure RealFormat =
  struct
    exception BadPrecision

    fun zeroLPad (s, w) = StringCvt.padLeft #"0" w s
    fun mkDigit i = String.sub ("0123456789", i)

    (*
     * Decompose a non-zero real into a list of at most maxPrec significant
     * digits (the first digit non-zero), and integer exponent. The return
     * value `(a :: b :: c ..., exp)` is produced from real argument
     * `a.bc ... * (10 ^^ exp)`. If the list would consist of all 9's, the
     * list consisting of 1 followed by all 0's is returned instead.
     *)
    val maxPrec = 15

    fun decompose (f, e, precisionFn) =
      let
        fun scaleUp (x, e) =
          if x < 1.0 then scaleUp (10.0 * x, e - 1) else (x, e)

        fun scaleDown (x, e) =
          if x >= 10.0 then scaleDown (0.1 * x, e + 1) else (x, e)

        fun mkdigits (f, 0) = ([], if f < 5.0 then 0 else 1)
          | mkdigits (f, i) =
            let
              val d = floor f
              val (digits, carry) = mkdigits (10.0 * (f - real d), i - 1)
              val (digit, c) =
                case (d, carry) of
                  (9, 1) => (0, 1)
                | _ => (d + carry, 0)
            in
              (digit :: digits, c)
            end

        val (f, e) =
          if f < 1.0
          then scaleUp (f, e)
          else
            if f >= 10.0
            then scaleDown (f, e)
            else (f, e)

        val (digits, carry) =
          mkdigits (f, Int.max (0, Int.min (precisionFn e, maxPrec)))
      in
        case carry of
          0 => (digits, e)
        | _ => (1 :: digits, e + 1)
      end

    fun formatWithPrecision { precision = prec } r =
      let
        val () = if prec < 1 then raise BadPrecision else ()

        fun pf _ = prec

        fun rtoa (sign, (digits, e)) =
          let
            fun mkRes (w, f, e) = { sign = sign, whole = w, frac = f, exp = e }

            fun doFrac [] = []
              | doFrac (0 :: tl) =
                let in
                  case doFrac tl of
                    [] => []
                  | rest => #"0" :: rest
                end
              | doFrac (hd :: tl) = (mkDigit hd) :: (doFrac tl)

            fun doWhole ([], e, wh) =
                  if e >= 0
                  then doWhole ([], e - 1, #"0" :: wh)
                  else mkRes (implode (rev wh), "", NONE)
              | doWhole (arg as (hd :: tl), e, wh) =
                  if e >= 0
                  then doWhole (tl, e - 1, (mkDigit hd) :: wh)
                  else mkRes (implode (rev wh), implode (doFrac arg), NONE)
          in
            if e < ~4 orelse e >= prec
            then
              mkRes (
                String.str (mkDigit (hd digits)),
                implode (doFrac (tl digits)),
                SOME e
              )
            else
              if e >= 0
              then doWhole (digits, e, [])
              else
                let
                  val frac = implode (doFrac digits)
                in
                  mkRes ("0", zeroLPad (frac, (size frac) + (~1 - e)), NONE)
                end
          end
      in
        if r < 0.0
        then rtoa (true, decompose (~r, 0, pf))
        else
          if r > 0.0
          then rtoa (false, decompose (r, 0, pf))
          else { sign = false, whole = "0", frac = "", exp = NONE }
      end

    fun format n =
      let
        val { sign, whole, frac, exp } = formatWithPrecision { precision = 15 } n
      in
        String.concat [
          if sign then "-" else "",
          whole,
          frac,
          Option.getOpt (Option.map Int.toString exp, "")
        ]
      end
  end
