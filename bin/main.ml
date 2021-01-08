open Core_kernel

(* util *)
let escape str =
  let r = Str.regexp "[%|'|\"|:|\\*|;|~|\\.|_|$|&|\\(|\\)|\\s|\\|^]" in
  Str.global_replace r "" str

let parsing contents =
  let m = Mecab.Tagger.create [|""|] in
  let nodes = Mecab.Tagger.sparse_tonode m contents in
  let surfaces = List.map ~f:(fun node ->
                     escape node.surface) nodes in
  List.map ~f:(fun x -> String.lowercase x)
    (List.filter
       ~f:(fun surface ->
         (String.length surface > 2) &&
           (Pcre.pmatch ~rex:(Pcre.regexp "^\\w+$") surface))
       surfaces)

let spit path contents =
  let f = Out_channel.create path in
  let _ = fprintf f "%s\n" contents in
  Out_channel.close f;
  ()

let index_dir_path = "./index/"

let if_not_exists_then_mkdir path =
  if not (Sys.file_exists path)
  then Core__Core_unix.mkdir path

let index_slurp word =
  let index_path = sprintf "%s%s" index_dir_path word in
  In_channel.read_lines index_path

let index_spit word =
  let index_path = sprintf "%s%s" index_dir_path word in
  spit index_path

(* とにかく追記方式。 *)
(* どっかのタイミングで調整するか、逐一調整する処理を追記する。 *)
let add_index word linum path =
  let word_hash = Sha1.to_hex (Sha1.string word) in
  let index_path = sprintf "%s%s" index_dir_path word_hash in
  let out = Out_channel.create ~append:true index_path in
  let _ = fprintf out "%s %i\n" path linum in
  Out_channel.close out

(* インデックスファイル生成 *)
let indexing path =
  let lines = In_channel.read_lines path in
  let linum_words = List.mapi lines
                      ~f:(fun i line -> (i, parsing line)) in
  List.iter
    linum_words
    ~f:(fun (i,words) ->
      List.iter words
        ~f:(fun word ->
          add_index word i path))

let path_lines_tbl = Hashtbl.create
                       ~growth_allowed:true
                       ~size:1000
                       (module String)

let mem_path_line path linum =
  let line = match Hashtbl.find path_lines_tbl path with
    | Some(lines) -> List.nth lines linum
    | _ -> let lines = Core_kernel.In_channel.read_lines path in
           let _ = Hashtbl.add path_lines_tbl ~key:path ~data:lines in
           List.nth lines linum
  in match line with
     | Some(l) -> l
     | _ -> ""

let search word =
  let word_hash = Sha1.to_hex (Sha1.string word) in
  let index_path = sprintf "%s%s" index_dir_path word_hash in
  if Sys.file_exists index_path
  then List.iter
         (In_channel.read_lines index_path)
         ~f:(fun line ->
           let spl = String.split line ~on:' ' in
           let path = (List.nth_exn spl 0) in
           let linum = int_of_string (List.nth_exn spl 1) in
           printf "path: %s, linum: %i, line: %s\n"
             path
             linum
             (mem_path_line path linum)
         )
  else printf "[%s]: not indexed." word

let () =
  if_not_exists_then_mkdir index_dir_path;
  let cmd = Sys.argv.(1) in
  match cmd with
  | "index" -> let path_lst = List.drop (Array.to_list Sys.argv) 3 in
               let total = List.length path_lst in
               List.iteri path_lst ~f:(fun i path ->
                   printf "[%i/%i]%s: indexing..." (i+1) total path;
                   indexing path;
                   printf "indexed.\n"
                 )
  | "search" -> let query = Sys.argv.(2) in
                search query
  | _ -> print_endline "not supported command."
