let ( let* ) = Result.bind

let show_entry entry =
  let open Mfat in
  if entry.Mfat.is_dir then Fmt.pr "%a/\n%!" Sfn.pp entry.name
  else Fmt.pr "%a (%ld)\n%!" Sfn.pp entry.name entry.Mfat.size

let run fpath path =
  Fs.with_image fpath @@ fun t ->
  let* path = Mfat.Spath.of_string path in
  let* entries = Fs.ls t path in
  List.iter show_entry entries;
  Ok ()

open Cmdliner

let image = Arg.(required & pos 0 (some string) None & info [] ~docv:"IMAGE")
let path = Arg.(value & pos 1 string "/" & info [] ~docv:"PATH")

let term =
  let open Term in
  term_result ~usage:false (const run $ image $ path)

let cmd =
  let doc = "List directory contents" in
  let info = Cmd.info "ls" ~doc in
  Cmd.v info term
