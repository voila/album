open Core.Std
open Cow
open Types
open Conf


let tm_compare t1 t2 = 
  let open Unix in
  match Int.compare t1.tm_year t2.tm_year with
    0 -> (match Int.compare t1.tm_mon t2.tm_mon with
        0 -> (match Int.compare t1.tm_mday t2.tm_mday with
            0 -> (match Int.compare t1.tm_hour t2.tm_hour with
                0 -> (match Int.compare t1.tm_min t2.tm_min with
                    0 -> Int.compare t1.tm_sec t2.tm_sec
                  | n -> n)
              | n -> n)
          | n -> n)
      | n -> n)
  | n -> n
;;


let get_time (fname:string) : Unix.tm =
  let open Str in
  let pat = regexp (".+_\\([a-zA-Z0-9]+\\)\\..*$")  in
  let tmstamp = if string_match pat fname 0 then matched_group 1 fname else "" in
  try Unix.gmtime (Float.of_string tmstamp)
  with Invalid_argument(_) -> raise (Failure ("cannot date: " ^ fname))
;;

let get_caption (bname:string) (captions_dir:string) : Xml.t =
  let caption_file = (Filename.concat captions_dir  (Filename.chop_extension bname ^ ".md")) in 
  Markdown.of_string (
  try
    In_channel.read_all caption_file
  with Sys_error msg -> " "
  )
;;

let item_dir = function
    Photo -> photos_dir
  | Video -> videos_dir
    
let make_item (item_type:media_type) (captions_dir:string) (bname:string) : item = 
  let open Filename in
  let open Unix in 
  let caption = get_caption bname captions_dir in
  let filepath = concat (item_dir item_type) bname in
  let thumbpath = concat thumbs_dir (chop_extension bname ^ ".jpg") in
  let page = concat html_dir (chop_extension bname ^ ".html") in
  let time : Unix.tm = get_time bname in {
    filename = bname; 
    filepath = filepath;
    thumbpath = thumbpath;
    page = page;
    caption = caption; 
    time = time; 
    year = time.tm_year + 1900;
    media = item_type;
  }
;;


let years (item_coll: item coll) : int list = 
  let ys = List.map ~f:(fun (x,y) -> x) item_coll in 
  List.sort ~cmp:(Int.compare) ys
;;


let scan_items item_type site_dir : item list =
  let items_dir = Filename.concat site_dir (item_dir item_type) in
  let filenames = Array.to_list (Sys.readdir items_dir) in
  let captions_dir = Filename.concat site_dir captions_dir in
  (* avoid files starting with a dot *)
  let itemfiles = List.filter filenames ~f:(fun str -> str.[0] <> '.') in
  List.map itemfiles ~f:(make_item item_type captions_dir) 
;;


(* builds an associative list : [(2011, list_of_items); ...] *)
let items_by_year (items: item list) : item coll = 
  let item_cmp i1 i2 = tm_compare i1.time i2.time in 
  let step lst item = 
    let year = item.year in
    match List.Assoc.find lst year with
      Some items -> 
      let items' = List.sort item_cmp (item::items) in
      List.Assoc.add lst year items'
    | None -> List.Assoc.add lst year [item]
  in 
  List.fold_left items ~init:[] ~f:step
;;


let rec generate_pages coll_items site_dir =
  match coll_items with
      (year, items)::rest ->
      let zitems = Zipper.from_list items in 
      let rec each_item zitems = 
        match Zipper.cursor zitems with
          Some curr -> 
          let prev = Zipper.prev zitems in
          let next = Zipper.next zitems in
          Page.write_page curr prev next site_dir; 
          each_item (Zipper.go_right zitems) 
        | None -> ()
      in each_item zitems; generate_pages rest site_dir 
    | [] -> ()
;;
 
(* this also generates (symlink) the items index *)
let rec generate_year_pages item_type item_coll site_dir = 
  let years = years item_coll in
  let write_year_page year = 
    match List.Assoc.find item_coll year  with
      Some items -> Page.write_year year years item_type items site_dir
    | None -> ()
  in List.iter years ~f:write_year_page;
  Option.value_map (List.last years) ~default:()
    ~f:(fun y -> Page.items_index y item_type site_dir)
;;


let last_year years = match List.last years with
  Some y -> y
  | None -> raise (Failure "Empty item collection!")



let generate_site () =
  Page.delete_old_html site_dir;
  let photos = scan_items Photo site_dir in
  let photos_by_year = items_by_year photos  in
  generate_pages photos_by_year site_dir;
  generate_year_pages Photo photos_by_year site_dir;
  Page.index (last_year (years photos_by_year)) site_dir;
  let videos = scan_items Video site_dir in
  let videos_by_year = items_by_year videos  in
  generate_pages videos_by_year site_dir;
  generate_year_pages Video videos_by_year site_dir
;;


let _ = generate_site ()
;;
