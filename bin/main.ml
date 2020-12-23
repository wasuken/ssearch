open Core_kernel

let fileContentsSearch ptn path =
  let l = Core_kernel.In_channel.read_lines path
  and r = Str.regexp ptn in
  List.filter ~f:(fun x -> Str.string_match r x 0) l

let singleFileSearch ptn path =
  let lines = fileContentsSearch ptn path in
  List.iter ~f:(fun x -> print_endline x) lines

let () =
  let ptn = Sys.argv.(1) in
  List.iter ~f:(fun path -> printf "[%s]\n" path; singleFileSearch ptn path)
    (List.drop (Array.to_list Sys.argv) 2)
