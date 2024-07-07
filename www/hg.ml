open Hypergraphs

open Js_of_ocaml
open Gg
open Vg


let initial_terms =
  [
    "a.b|fc|ld"
  ;
    "#<pos='0,0'>,<pos='400,0'> f<pos='100,100';label='x';radius='6'>f<pos='100,-100';label='y';radius='6'>({234}f<pos='300,100';label='z';radius='6'>({234}-?<pos='166.667,33.3333';color='red';split='s3'> | {14}r-<pos='350,50';color='violet'>) | {234}f<pos='300,-100';label='t';radius='6'>({14}r-<pos='350,-50';color='blue'> | {234}-?<pos='166.667,-33.3333';color='orange';split='s3'>) | {134}-?<pos='66.6667,0';color='yellow';radius='25'>)"
  ;
    "#<pos='0,0'>,<pos='400,0'> f<pos='100,100';label='x';radius='6'>f<pos='100,-100';label='y';radius='6'>({234}f<pos='200,0'>({134}f<pos='300,-100';label='t';radius='6'>({234}r-?<pos='200,-66.6667';color='orange';split='s1'> | {134}(13)-?<pos='300,-33.3333';color='blue';radius='25';split='1'>) | {124}f<pos='300,100';label='z';radius='6'>({234}r-?<pos='200,66.6667';color='red';split='s1'> | {134}(13)-?<pos='300,33.3333';color='violet';radius='25';split='1'>) | {234}-?<pos='133.333,0';color='yellow';radius='25';split='s'>) | {134}-?<pos='66.6667,0';color='green';radius='25';split='s1'>)"
  ;
    "#<pos='0,0'>,<pos='400,0'> f<pos='100,-100';label='y';radius='6'>f<pos='100,100';label='x';radius='6'>({234}f<pos='200,0'>({124}f<pos='300,-100';label='t';radius='6'>({234}r-?<pos='200,-66.6667';color='orange';split='s2'> | {134}(13)-?<pos='300,-33.3333';color='blue';radius='25';split='s3'>) | {134}f<pos='300,100';label='z';radius='6'>({234}r-?<pos='200,66.6667';color='red';split='s2'> | {134}(13)-?<pos='300,33.3333';color='violet';radius='25';split='s3'>)) | {134}r-?<pos='66.6667,0';color='green';radius='25';split='s1'>)"
  ]

module Html = Dom_html
	
let app p mk =
  let e = mk Html.document in
  Dom.appendChild p e;
  e

let println par s = 
  Dom.appendChild par (Html.document##createTextNode (Js.string s));
  ignore(app par Html.createBr)

let clear par = 
  let rec aux () = 
    match Js.Opt.to_option par##.firstChild with 
      | Some c -> Dom.removeChild par c; aux() 
      | None -> ()
  in aux ()

let print par s =
  clear par; 
  let l = String.split_on_char '\n' s in
  List.iter (println par) l

let get s = 
  Js.Opt.get (Html.document##getElementById(Js.string s))
    (fun () -> assert false)

let add_listener elt evt f =
  ignore(Html.addEventListener elt evt (Html.handler f) Js._true)
  

class arena (canvas: Html.canvasElement Js.t) =
  object(self)
    inherit Arena.generic
    
    method private dsize =
      float_of_int canvas##.width,
      float_of_int canvas##.height

    val mutable dpointer = (0,0)
    method private dpointer =
      let x,y = dpointer in
      float_of_int x,
      float_of_int y
    
    method! refresh =
      let size = Box2.size self#view in
      let vgr = Vgr.create (Vgr_htmlc.target ~resize:false canvas) `Other in 
      let image = I.blend self#canvas#get (I.const Color.white) in
      ignore (Vgr.render vgr (`Image (size, self#view, image)));
      ignore (Vgr.render vgr `End);
      ignore self#dsize

    method private scroll ev =
      if Js.to_bool ev##.ctrlKey then
        ((if ev##.wheelDelta > 0 then self#zoom 0.95
          else self#zoom 1.05); Js._false)
      else Js._true

    val mutable mode = None
    method private mouseup ev =
      if Js.to_bool ev##.ctrlKey then
        (mode <- None; Js._false)
      else Js._true
    method private mousedown ev =
      if Js.to_bool ev##.ctrlKey then
        (mode <- Some dpointer; Js._false)
      else Js._true
    method private mousemove ev =
      dpointer <- ev##.offsetX, ev##.offsetY; 
      match Js.to_bool ev##.ctrlKey, mode with
      | true,Some(x0,y0) ->
         let x,y = dpointer in
         self#move (float_of_int (x0-x), float_of_int (y0-y)); mode <- Some dpointer;
         Js._false
      | _ -> Js._true
    
    initializer
      canvas##.onwheel := Html.handler self#scroll;
      add_listener canvas Html.Event.mousedown self#mousedown;
      add_listener canvas Html.Event.mousemove self#mousemove;
      add_listener canvas Html.Event.mouseup self#mouseup;
      ()

  end

class locate arena entry infos warnings =
  object
    inherit Locate.locate arena 
    method entry = Js.to_string (entry##.value)
    method set_entry fmt = Format.kasprintf (fun s -> entry##.value := Js.string s) fmt
    method warning fmt = Format.kasprintf (print warnings) fmt
    method info fmt = Format.kasprintf (print infos) fmt
    method help fmt = Format.kasprintf (print infos) fmt
    method private read _ = assert false
    method private write _ _ = assert false
    method private export _ _ = assert false
  end

let std_evt f ev =
  if not (Js.to_bool ev##.ctrlKey) then
    (f ev; Js._false)
  else Js._true

let onload _ =
  let canvas = app (get "canvas") Html.createCanvas in
  let entry = app (get "entry") Html.createTextarea in
  let strokes = app (get "strokes") Html.createTextarea in
  strokes##.value := Js.string "type commands here";
  let infos = get "infos" in
  let warnings = get "warnings" in
  let arena = new arena canvas in
  let self = new locate arena entry infos warnings in
  (* let entryfocused = ref true in *)
  let onkeypress ev =
    (* strokes##.value := Js.string ""; *)
    (match Js.to_string (Js.Optdef.get ev##.key (fun _ -> assert false)) with
      | "Control" | "Alt" | "Shift" | "Meta" | "Tab" -> ()
      | s -> self#on_key_press s);
    Js.bool (Js.to_string (Js.Optdef.get ev##.key (fun _ -> assert false)) = "Tab")
  in
  let onkeyup ev =
    ignore ev;
    self#on_entry_changed;
    Js._true
  in
  
  entry##.style##.width := Js.string "80%";
  canvas##.style##.width := Js.string "80%";
  entry##.tabIndex := 1;
  strokes##.tabIndex := 2;
  warnings##.style##.cssText := Js.string "color:red";
  add_listener canvas Html.Event.mousedown (std_evt (fun _ -> self#on_button_press));
  add_listener canvas Html.Event.mousemove (std_evt (fun _ -> self#on_motion));
  add_listener canvas Html.Event.mouseup (std_evt (fun _ -> self#on_button_release));      
  add_listener strokes Html.Event.keydown onkeypress;
  add_listener entry Html.Event.keyup onkeyup;
  (* add_listener entry Html.Event.focus (fun _ -> entryfocused := true; Js._true); *)
  (* add_listener entry Html.Event.blur (fun _ -> entryfocused := false; Js._true); *)
  self#init initial_terms;
  (* infos##focus; *)
  Js._false

let _ =
  Html.window##.onload := Html.handler onload;

