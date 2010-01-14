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
  * Functions for decoding mp3 files using libmad.
  *
  * @author Samuel Mimram
  *)

(* $Id$ *)

type mad_file

exception Mad_error of string
exception Read_error of string
exception End_of_stream
exception Openfile_error of string
exception Closefile_error of string

let _ =
  Callback.register_exception "mad_exn_mad_error" (Mad_error "");
  Callback.register_exception "mad_exn_read_error" (Read_error "");
  Callback.register_exception "mad_exn_end_of_stream" End_of_stream;
  Callback.register_exception "mad_exn_openfile_error" (Openfile_error "");
  Callback.register_exception "mad_exn_closefile_error" (Closefile_error "")

external openfile : string -> mad_file = "ocaml_mad_openfile"

external openstream : (int -> (string * int)) -> mad_file = "ocaml_mad_openstream"

external close : mad_file -> unit = "ocaml_mad_close"

external get_current_position : mad_file -> int = "ocaml_mad_get_current_position"

external decode_frame : mad_file -> string = "ocaml_mad_decode_frame"

external decode_frame_float : mad_file -> float array array = "ocaml_mad_decode_frame_float"

external get_output_format : mad_file -> int * int * int = "ocaml_mad_get_synth_pcm_format"

let duration file =
  let mf = openfile file in
  let close () = 
    try
      close mf
    with _ -> ()
  in
  try
    let duration = 
      (* Decode some data *)
      let decode_samples () = 
      let data = decode_frame_float mf in
        Array.length data.(0)
      in
      let samples = decode_samples () in
      (* Get data information *)
      let (samplefreq,_,_) = get_output_format mf in
      (* The decoding loop *)
      let rec decode_loop samples =
        try
          let samples = samples + decode_samples () in
          decode_loop samples
        with _ -> samples
      in
      let decoded_samples = decode_loop samples in
      (float decoded_samples) /. (float samplefreq)
    in
    close ();
    duration
  with
    | _ -> close (); 0.
