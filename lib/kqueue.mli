include Kqueue_intf.S

val file_descr_to_int : Unix.file_descr -> int
val file_descr_of_int : int -> Unix.file_descr
