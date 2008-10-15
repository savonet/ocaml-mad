
let info file =
  Printf.printf "Samplefreq:\t%dHz\nDuration:\t%fs\n"
    (Mad.samplefreq file) (Mad.duration file)

let _ = info Sys.argv.(1)
