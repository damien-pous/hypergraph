open Hypergraphs

let check f =
  ignore(File.read f)

let place f =
  let l = File.read f in
  let t = File.last l in
  let g = Conversions.graph_of_raw t in
  let _ = Place.sources_on_circle g in
  let _ = Place.graphviz g in
  File.save f (File.append l t);
  File.export_term f t

let export f = File.export (File.read f)

let _ =
  Arg.(parse
         [ "-check", String check, "f\tcheck file f.hg (no output)";
           "-place", String place, "f\tplace file f.hg (before output)" ]
         export
         "hg [-check file, -place file, file]*")
