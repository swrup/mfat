let ( let* ) = Result.bind

let run fpath path =
  Fs.with_image fpath @@ fun t ->
  let* path = Mfat.Spath.of_string path in
  let* data = Fs.read t path in
  print_string data; Ok ()

open Cmdliner

let image = Arg.(required & pos 0 (some string) None & info [] ~docv:"IMAGE")
let path = Arg.(value & pos 1 string "/" & info [] ~docv:"PATH")

let term =
  let open Term in
  term_result (const run $ image $ path)

let cmd =
  let doc = "Read a file" in
  let info = Cmd.info "cat" ~doc in
  Cmd.v info term
