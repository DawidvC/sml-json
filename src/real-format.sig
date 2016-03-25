(*
 * Adapted from an original version by Emden Gansner and John Reppy located at:
 * http://www.mpi-sws.org/~joshua/stardust/current/src/nj-util/real-format.sml
 *)
signature REAL_FORMAT =
  sig
    val format : { precision : int } -> real ->
      { sign : bool, whole : string, frac : string, exp : int option }
  end
