let ( let* ) = Result.bind

let run fpath path =
  Fs.with_image fpath @@ fun t ->
  let* path = Mfat.Spath.of_string path in
  Fs.remove t path

open Cmdliner

let image = Arg.(required & pos 0 (some string) None & info [] ~docv:"IMAGE")
let path = Arg.(required & pos 1 (some string) None & info [] ~docv:"PATH")

let term =
  let open Term in
  term_result (const run $ image $ path)

let cmd =
  let doc = "Remove a file or empty directory" in
  let info = Cmd.info "rm" ~doc in
  Cmd.v info term
