open Cow
open Core.Std


type media_type = Photo | Video

type item = {
  filename : string; (* boat.jpg *)
  filepath : string; (* some/path/boat.jpg *)
  thumbpath : string; (* some/path/thumb/boat.jpg *)
  page: string;      (* html/boat.html *)
  caption : Xml.t;
  time: Unix.tm;
  year : int;
  media : media_type;
}


type 'a coll = (int, 'a list) List.Assoc.t
