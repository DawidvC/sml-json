structure Reader =
  struct
    type ('a, 's) t = 's -> ('a * 's) option

    fun map f reader stream =
      Option.map (fn (a, s) => (f a, s)) (reader stream)

    fun const a stream = SOME (a, stream)
  end
