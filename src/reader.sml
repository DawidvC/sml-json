structure Reader =
  struct
    type ('a, 's) t = 's -> ('a * 's) option

    fun map f reader stream =
      Option.map (fn (a, s) => (f a, s)) (reader stream)

    fun grouped n reader stream =
      let
        fun return result stream =
        case result of
          [] => NONE
        | _ => SOME (List.rev result, stream)

        fun loop stream 0 result = return result stream
          | loop stream n result =
              case reader stream of
                NONE => return result stream
              | SOME (a, stream) =>
                  loop stream (n - 1) (a :: result)
      in
        loop stream n []
      end

    fun const a stream = SOME (a, stream)
  end
