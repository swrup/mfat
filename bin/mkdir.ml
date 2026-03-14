let ( let* ) = Result.bind

let run fpath path =
  Fs.with_image fpath @@ fun t ->
  let* path = Mfat.Spath.of_string path in
  Fs.mkdir t path

open Cmdliner

let image = Arg.(required & pos 0 (some string) None & info [] ~docv:"IMAGE")
let path = Arg.(value & pos 1 string "/" & info [] ~docv:"PATH")

let term =
  let open Term in
  term_result (const run $ image $ path)

let cmd =
  let doc = "Create a directory" in
  let info = Cmd.info "mkdir" ~doc in
  Cmd.v info term
