open Core_kernel
open Sqlite3

(* DB系 *)

let app_db = "./ssearch.sqlite3"
let index_path = "./index/"

let escape str =
  let r = Str.regexp "[%|'|\"|:|\\*|;|~|\\.|_|$|&|\\(|\\)|\\s|\\|^]" in
  Str.global_replace r "" str

let generate_insert_docs_sql path contents_hash =
  sprintf "insert into docs(path,contents_hash) values('%s','%s');" path contents_hash

let generate_insert_doc_words_sql word doc_id linum =
  sprintf "insert into doc_words(word,doc_id,linum) values('%s',%i,%i);" word doc_id linum

let create_table_sql =
  sprintf "%s;%s;"
    "create table docs(id integer primary key, path text, contents_hash text, unique(path))"
    "create table doc_words(doc_id integer, word text, linum integer, foreign key(doc_id) references docs(id))"

let generate_find_docs_id path =
  sprintf "select id,path,contents_hash from docs where path = '%s';" path

let exec_db sql suc_msg =
  let db = Sqlite3.db_open app_db in
  match Sqlite3.exec db sql with
  | Sqlite3.Rc.OK -> print_endline suc_msg
  | r -> prerr_endline (Sqlite3.Rc.to_string r); prerr_endline (errmsg db)

let docs_find_id path =
  let db = db_open app_db in
  let id = ref 0 in
  let sql = generate_find_docs_id path in
  let _u = Sqlite3.exec_not_null_no_headers db
            ~cb:(fun row -> id.contents <- (int_of_string row.(0)))
            sql in
  !id

let create_db_when_not_exists path =
  if not (Sys.file_exists path) then exec_db create_table_sql "success create query."

let generate_words_where_word_get_id words =
  let single_words = List.map ~f:(fun x -> sprintf "'%s'" x) words in
  let words_query_text = String.concat ~sep:"," single_words in
  sprintf "select id from words where name in (%s)" words_query_text

let words_where_word_get_id words =
  let id_lst = ref [] in
  let db = db_open app_db in
  let _u = Sqlite3.exec_not_null_no_headers
             db
             ~cb:(fun row -> id_lst.contents <- List.cons (int_of_string row.(0)) id_lst.contents)
             (generate_words_where_word_get_id words) in
  !id_lst

(* 操作系 *)

let saving linum_words path =
  let _u = create_db_when_not_exists app_db in
  let contents_hash = Sha1.to_hex (Sha1.file path) in
  let dsql = generate_insert_docs_sql path contents_hash in
  exec_db dsql "success words insert query.";
  let doc_id = docs_find_id path in
  let dw_sql = List.fold linum_words
                 ~init:""
                 ~f:(fun acm (i, words) ->
                   sprintf "%s%s" acm
                     (List.fold words ~init:""
                        ~f:(fun ac word -> sprintf "%s%s" ac
                                             (generate_insert_doc_words_sql
                                                word
                                                doc_id
                                                i))))
  in
  exec_db dw_sql "success doc_words insert query."

let parsing contents =
  let m = Mecab.Tagger.create [|""|] in
  let nodes = Mecab.Tagger.sparse_tonode m contents in
  let surfaces = List.map ~f:(fun node ->
                     escape node.surface) nodes in
  List.filter
    ~f:(fun surface -> String.length surface > 2)
    surfaces

let indexing path =
  let lines = In_channel.read_lines path in
  let linum_words = List.mapi ~f:(fun i line -> (i, (parsing line))) lines in
  let _u = saving linum_words path in
  ()

let multiIndexing path_lst =
  List.iter ~f:(fun path -> indexing path) path_lst

let fileContentsMatch ptn path =
  let l = Core_kernel.In_channel.read_lines path
  and r = Str.regexp ptn in
  List.filter ~f:(fun x -> Str.string_match r x 0) l

let singleFileGrep ptn path =
  printf "[%s]\n" path;
  let lines = fileContentsMatch ptn path in
  List.iter ~f:(fun x -> print_endline x) lines

let multiFileGrep ptn path_lst =
  List.iter ~f:(fun path -> singleFileGrep ptn path) path_lst

let generate_where_doc_words_linum word =
  sprintf
    "select d.path,dw.linum,dw.word,dw.linum,d.id,dw.doc_id from docs as d join doc_words as dw on d.id = dw.doc_id where dw.word like '%s';"
    word

let getFileLine path linum =
  let l = Core_kernel.In_channel.read_lines path in
  match List.nth l linum with
  | Some(a) -> a
  | _a -> ""

let matchLineFromIndex query =
  let _c = create_db_when_not_exists app_db in
  let sql = generate_where_doc_words_linum query in
  let db = db_open app_db in
  let linum_lst = ref [] in
  let _u = exec_not_null_no_headers db
             ~cb:(fun row -> linum_lst.contents <- List.cons (row.(0), row.(1)) linum_lst.contents)
             sql in
  List.iter
    ~f:(fun (path, linum) ->
      printf "[%s: %s] %s\n" path linum (getFileLine path (int_of_string linum)))
    !linum_lst

let () =
  let cmd = Sys.argv.(1) in
  if (String.compare cmd "index") = 0 then multiIndexing (List.drop (Array.to_list Sys.argv) 2)
  else
    if (String.compare cmd "grep") = 0
    then multiFileGrep Sys.argv.(2) (List.drop (Array.to_list Sys.argv) 3)
    else
      if (String.compare cmd "search") = 0
      then matchLineFromIndex (String.lowercase Sys.argv.(2))
