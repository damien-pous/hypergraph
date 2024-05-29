open Hypergraphs

let check f =
  ignore(File.read f)

let place f =
  let l = File.read f in
  let l = List.map (fun t ->
              let g = Graph.of_term t in
              let _ = Place.sources_on_circle g in
              let _ = Place.graphviz g in
              Graph.to_term g) l in  
  File.write f l;
  File.export f l

let export f = File.export f (File.read f)

let _ =
  Arg.(parse
         [ "-check", String check, "f\tcheck file f.hg (no output)";
           "-place", String place, "f\tplace file f.hg (before output)" ]
         export
         "hg [-check file, -place file, file]*")
