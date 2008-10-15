(*
 * Copyright 2003-2006 Savonet team
 *
 * This file is part of Ocaml-mad.
 *
 * Ocaml-mad is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * Ocaml-mad is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Ocaml-mad; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

(**
  * Functions for decoding mp3 files using the libmad.
  *
  * @author Samuel Mimram
  *)

(* $Id$ *)

(** {1 Exceptions} *)

(** An error occured with libmad. *)
exception Mad_error of string

(** An error occured while reading a file. *)
exception Read_error of string

(** The end of the mp3 stream was reached. *)
exception End_of_stream

(** Could not open a file. *)
exception Openfile_error of string

(** Could not close a file. *)
exception Closefile_error of string

(** {1 Parameters of the output format} *)

(** Number of channels of the output. *)
val wav_output_channels : int

(** Sample size of the output in bits. *)
val wav_output_sample_size : int

(** Are the output samples in big endian? *)
val wav_output_big_endian : bool

(** Are the output samples signed? *)
val wav_output_signed : bool

(** {1 Decoding files} *)

(** An mp3 file opened for decoding. *)
type mad_file

(**
  * Open an mp3 file.
  *
  * @raise Openfile_error if an error occured while trying to open the file.
  *)
val openfile : string -> mad_file

(**
  * [openstream read_func] opens a stream where [read_func n] should be a
  * function which returns [n] bytes of data or less, the second component of
  * the result being the number of bytes to read in the fist component.
  *)
val openstream : (int -> (string * int)) -> mad_file

(**
  * Close an mp3 file previously opened with [openfile].
  *
  * @raise Closefile_error if an error occured while trying to close the file.
  *)
val close : mad_file -> unit

(**
  * Get the current position (in bytes) of the decoder in the mp3 file which
  * should have been opened with [openfile].
  *)
val get_current_position : mad_file -> int

(** Decode an mp3 frame. *)
val decode_frame : mad_file -> string

(** Decode an mp3 frame. *)
val decode_frame_float : mad_file -> float array array

(*
 * Get the samplerate, number of channels and samples per channel currently in
 * the synth. This should be called after [decode_frame] or
 * [decode_frame_float].
 *)
val get_output_format : mad_file -> int * int * int

(** {1 Some info about MP3 files} *)

(** Get the samplefreq of a file.
  * This is a dirty hack, you should prefer get_output_format. *)
val samplefreq : string -> int

(** Compute the duration of a file, in seconds.
  * Never raises any exception, but returns 0. in case of error.
  * The returned duration is exact but its computation is not efficient,
  * as it proceeds by completely decoding the file. *)
val duration : string -> float
