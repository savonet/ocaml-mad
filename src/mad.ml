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

let wav_output_channels = 2
let wav_output_sample_size = 16
let wav_output_big_endian = false
let wav_output_signed = true

let get_samplefreq f =
  let bits_in i pos length =
    ((int_of_char i) lsr pos) mod (1 lsl length)
  in
  let samplefreq_array =
    [|
      [| 11025; 12000; 8000; 0 |];
      [| |];
      [| 22050; 24000; 16000; 0 |];
      [| 44100; 48000; 32000; 0 |];
    |]
  in
  let read n =
    let ans = String.create n in
      assert (n = Unix.read f ans 0 n) ;
      ans
  in
  let read_byte () =
    int_of_char (read 1).[0]
  in
  let read_size () =
    let buf = read 4 in
    let b0 = int_of_char buf.[0] in
    let b1 = int_of_char buf.[1] in
    let b2 = int_of_char buf.[2] in
    let b3 = int_of_char buf.[3] in
      (* TODO* : lsl 7 -> overlapping ?? *)
      (((((b0 lsl 7) lor b1) lsl 7) lor b2) lsl 7) lor b3
  in
    if read 3 = "ID3" then
      (
        ignore (Unix.lseek f 3 Unix.SEEK_CUR);
        ignore (Unix.lseek f (read_size ()) Unix.SEEK_CUR);
      )
    else
      ignore (Unix.lseek f 0 Unix.SEEK_SET);
    (* Try to seek to the next frame. *)
    while read_byte () <> 0xff do () done;
    let buf = read 3 in
    let version = bits_in buf.[0] 3 2 in
    let samplefreq_i = bits_in buf.[1] 2 2 in
      samplefreq_array.(version).(samplefreq_i)

let samplefreq fname =
  let f = Unix.openfile fname [Unix.O_RDONLY] 0o400 in
  try
    let freq = get_samplefreq f in
      Unix.close f ;
      freq
  with
    | e -> Unix.close f ; raise e

let duration file =
  let bytes mf =
    let bytes = ref 0 in
      try
        while true do
          bytes := !bytes + String.length (decode_frame mf)
        done ;
        assert false
      with _ -> !bytes
  in
  try
    let mf = openfile file in
    let ret = 
      try
        let samplefreq = samplefreq file in
          (float (8 * bytes mf)) /.
          (float (wav_output_channels*wav_output_sample_size*samplefreq))
      with _ -> 0.
    in
    try 
      close mf ;
      ret
    with _ -> ret
  with
    | _ -> 0.
