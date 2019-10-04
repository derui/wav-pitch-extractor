val dft : float array -> Complex.t array
(** [dft data] gets the result of Discrete Fourier Transform.
                                   This function has computation [O(n^2)]. *)

val fft : float array -> Complex.t array
(** [fft data] gets the result of Fast Fourier Transform.
                                   This function has computation [O(N log N)]. *)
