open Core_kernel

let indexing path =
  let contents = In_channel.read_all path in
  let m = Mecab.Tagger.create [|""|] in
  let nodes = Mecab.Tagger.sparse_tonode m contents in
  let filteredNodes = List.filter
                        ~f:(fun node -> String.length node.surface > 2)
                        nodes in
  List.iter ~f:(fun node -> print_endline node.surface) filteredNodes

let fileContentsSearch ptn path =
  let l = Core_kernel.In_channel.read_lines path
  and r = Str.regexp ptn in
  List.filter ~f:(fun x -> Str.string_match r x 0) l

let singleFileSearch ptn path =
  let lines = fileContentsSearch ptn path in
  List.iter ~f:(fun x -> print_endline x) lines

let () =
  let cmd = Sys.argv.(1)
  and query = Sys.argv.(2) in
  if (String.compare cmd "index") = 0 then indexing Sys.argv.(2)
  else if (String.compare cmd "index") = 0 then List.iter
                                                  ~f:(fun path -> printf "[%s]\n" path; singleFileSearch query path)
                                                  (List.drop (Array.to_list Sys.argv) 3)
