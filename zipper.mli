type 'a t 
val from_list : 'a list -> 'a t
val to_list : 'a t -> 'a list
val go_left : 'a t -> 'a t
val go_right : 'a t -> 'a t
val to_start : 'a t -> 'a t
val to_end : 'a t -> 'a t
val at_start : 'a t -> bool
val at_end : 'a t -> bool
val cursor : 'a t -> 'a option
val prev : 'a t -> 'a option
val next : 'a t -> 'a option
