open Hypergraphs

open Js_of_ocaml
open Gg
open Vg


module Html = Dom_html

(* let (>>=) = Lwt.bind *)
	
let app p mk =
  let e = mk Html.document in
  Dom.appendChild p e;
  e

let add_string p s = 
  Dom.appendChild p (Html.document##createTextNode (Js.string s))

let clear par = 
  let rec aux () = 
    match Js.Opt.to_option par##.firstChild with 
      | Some c -> Dom.removeChild par c; aux() 
      | None -> ()
  in aux ()

(* let print par = clear par; add_string (app par Html.createDiv) *)
let eprint par =
  clear par;
  let d = app par Html.createDiv in
  d##.style##.cssText := (Js.string ("color:red"));
  add_string d

let get s = 
  Js.Opt.get (Html.document##getElementById(Js.string s))
    (fun () -> assert false)

let onload _ =
  let log = get "log" in
  let canvas = app (get "canvas") Html.createCanvas in
  let l = Lexing.from_string "#<pos=0,0>,<pos=200,0> a<pos=50,0>.<pos=100,0>b<pos=150,0>" in
  let t = Parser.sterm Lexer.token l in
  let t = Term.map Info.kvl_to_positionned t in
  let g = Graph.of_term t in
  let view = Graph.bbox g in
  let size = Box2.size view in
  let image = Graph.draw ~iprops:true g in
  let vgr = Vgr.create (Vgr_htmlc.target canvas) `Other in
  ignore (Vgr.render vgr (`Image (size, view, image)));
  ignore (Vgr.render vgr `End);
  let onclick e =
    eprint log (string_of_int (e##.offsetX));
    Js._false
  in
  canvas##.onclick := Html.handler onclick;    
  Js._false
  

let _ =
  Html.window##.onload := Html.handler onload
