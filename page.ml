open Core.Std
open Cow
open Types
open Conf

 
let page_uri item = "/" ^ item.page
;;

let item_uri item = "/" ^ item.filepath
;;

let thumb_uri item = "/" ^ item.thumbpath
;;


let prev_html = function
    None -> <:html< >>
  | Some i ->  <:html< <a id="prev" href="$str:page_uri i$">previous</a> >>

let next_html = function
    None -> <:html< >>
  | Some i ->  <:html< <a id="next" href="$str:page_uri i$">next</a> >>


(* belong to an Item module *)
let it_to_s = function Photo -> "photos" | Video -> "videos"

let embed_item_html item =
  match item.media with
    Photo -> <:html< <img src="$str:item_uri item$" />&>>
  | Video -> <:html< <video src="$str:item_uri item$" width="480" height="360" 
                     controls="controls" autoplay="autoplay"> </video> &>>
;;

(* year page, part of Item module *)
let current_year_page year item_type = 
  let typ = match item_type with Photo -> "photos" | Video -> "videos" in
  typ ^ "_" ^ string_of_int year ^ ".html"
;;

let items_in_year_link year item_type = 
  "/" ^ Filename.concat html_dir (current_year_page year item_type)
;;


let title = "My website..."

let h1 = <:html< <h1>$str:title</h1> &>>
;;

let head = 
<:html< 
  <title>$str:title</title>
  <link rel="stylesheet" href="/css/styles.css" type="text/css" />
  <script type="text/javascript" src="/js/jquery-1.3.2.js"> </script>
  <script type="text/javascript" src="/js/main.js"> </script>
&>>
;;


let page_html date prev_html next_html year_link year embed_item caption =
  <:html<
  <html>
  <head>$head$</head>
  <body>
    $h1$
    <p class="menu">
      <a href="photos.html">photos</a> | <a href="videos.html">videos</a>
    </p>
    <p class="date">$date$</p> 
    <div id="photo"> 
      <div class="caption">
         $caption$
      </div>
      <p id="nav">
        $prev_html$ 
        $next_html$
      </p>
      <p>
        <a href="$str:year_link$">$str:year$</a> 
     </p>
     <p>$embed_item$</p>
    </div>
  </body>
  </html>&>>
;;


let year_links_html years item_type = 
  let html_of_year year = 
    let year_link = items_in_year_link year item_type in 
    <:html< <a href="$str:year_link$">$str:(string_of_int year)$</a> &>> 
  in <:html< $list:List.map years html_of_year$ &>>
;;

let thumbs_html items =
  let li_of_item item = 
    let item_link = page_uri item in
    let item_thumb_link = thumb_uri item in
    <:html< <li><a href="$str:item_link$"><img src="$str:item_thumb_link$" /></a></li> &>> 
  in <:html< $list:List.map items li_of_item$ &>>
;;

(* all items in year page *)
let year_page_html years items item_type =
  match List.last years with                     (* most recent year *)
  None -> <:html< &>>
  | Some year ->
    let year_links = year_links_html years item_type in
    let thumbs = thumbs_html items in
 <:html< 
  <html>
  <head>$head$</head>
  <body>
  $h1$
  <p class="menu">
    <a href="photos.html">photos</a> | <a href="videos.html">videos</a>
  </p>
  <p>$year_links$</p>
	<ul id="photos">$thumbs$</ul>
</body>
</html>
&>>
;;


(*  

Core.Std.Unix.strftime (Unix.gmtime 14564564.) "%A %d-%m-%Y";;
*)

let time_to_string (t: Unix.tm) : string  = 
  Unix.strftime t "%A %d-%m-%Y %H:%M:%S (%z)"
;;

let write_page curr prev next site_dir = 
  let date = Xml.of_string (time_to_string curr.time) in
  let prev_html = prev_html prev in
  let next_html = next_html next in
  let year = curr.year in 
  let year_str = string_of_int year in
  let year_link = items_in_year_link year curr.media in
  let caption = curr.caption in
  let embed_item = embed_item_html curr in 
  let html = Html.to_string (page_html date prev_html 
      next_html year_link year_str embed_item caption) in 
  let page_name = Filename.concat site_dir curr.page in 
  let oc = open_out page_name in
  let page = Printf.sprintf "<!DOCTYPE html>%s" html in
  Out_channel.output_string oc page;
  Out_channel.close oc;
  Printf.printf "wrote: %s\n" page_name
;;

let write_year year years item_type items site_dir = 
  let page_name = Filename.concat site_dir 
      (Filename.concat html_dir (current_year_page year item_type)) in
  let html = Html.to_string (year_page_html years items item_type) in 
  let page = Printf.sprintf "<!DOCTYPE html>%s" html in
  let oc = open_out page_name in
  Out_channel.output_string oc page;
  Out_channel.close oc;
  Printf.printf "wrote: %s\n" page_name
;;


let items_index year item_type site_dir =
  let src = current_year_page year item_type in
  let dest = Filename.concat html_dir (it_to_s item_type ^ ".html") in
  let cmd = Printf.sprintf "cd %s; ln -s %s %s" site_dir src dest in  
  begin 
    ignore (Sys.command cmd);
    Printf.printf "wrote: %s\n" dest
  end
;;

let index year site_dir =
  let src = current_year_page year Photo in
  let dest = Filename.concat html_dir "index.html" in
  let cmd = Printf.sprintf "cd %s; ln -s %s %s" site_dir src dest in
  begin 
    ignore (Sys.command cmd);
    Printf.printf "wrote: %s\n" dest
  end 
;;


let delete_old_html site_dir = 
  let html_path = Filename.concat site_dir (Filename.concat html_dir "*.html") in
  let cmd = Printf.sprintf "rm %s" html_path in
  begin
    ignore (Sys.command cmd);
    Printf.printf "old html deleted\n"  
  end 
;;

