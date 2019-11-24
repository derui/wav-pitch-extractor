val dft : Complex.t array -> Complex.t array
(** [dft data] gets the result of Discrete Fourier Transform.
    This function has computation [O(n^2)]. *)

val dft_float : float array -> Complex.t array
(** [dft_float data] gets the result of Discrete Fourier Transform.
    This function is a special version of [dft]
    This function has computation [O(n^2)]. *)

val fft : Complex.t array -> Complex.t array
(** [fft data] gets the result of Fast Fourier Transform.
    This function has computation [O(N log N)]. *)

val fft_float : float array -> Complex.t array
(** [fft_float data] gets the result of Fast Fourier Transform via float array.
    This function is the special version of [fft] pre-defined.*)

val fft_general : converter:('a -> Complex.t) -> 'a array -> Complex.t array
(** [fft data] gets the result of Fast Fourier Transform.
    This function has computation [O(N log N)]. *)

val ifft : Complex.t array -> Complex.t array
(** [ifft data] gets the result of Inverse Fast Fourier Transform.
    This function has computation [O(N log N)]. *)
