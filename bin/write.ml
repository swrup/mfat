let ( let* ) = Result.bind
let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt

let run filename path contents =
  let str =
    match contents with
    | None ->
        let buf = Buffer.create 0x7ff in
        let tmp = Bytes.create 0x7ff in
        let rec go () =
          match input stdin tmp 0 (Bytes.length tmp) with
          | 0 -> Buffer.contents buf
          | len ->
              Buffer.add_subbytes buf tmp 0 len;
              go ()
          | exception End_of_file -> Buffer.contents buf
        in
        go ()
    | Some fpath ->
        let ic = open_in_bin fpath in
        let finally () = close_in ic in
        Fun.protect ~finally @@ fun () ->
        let len = in_channel_length ic in
        really_input_string ic len
  in
  Fs.with_image filename @@ fun fs ->
  let* path = Mfat.Spath.of_string path in
  Fs.write fs path str

open Cmdliner

let image = Arg.(required & pos 0 (some string) None & info [] ~docv:"IMAGE")
let path = Arg.(required & pos 1 (some string) None & info [] ~docv:"PATH")

let contents =
  let doc = "The document to store into the FAT32 image." in
  let parser str =
    match str with
    | "-" -> Ok None
    | fpath when Sys.file_exists fpath && Sys.is_regular_file fpath ->
        Ok (Some fpath)
    | fpath -> error_msgf "%s does not exist (or is not a regular file)" fpath
  in
  let pp ppf = function
    | None -> Fmt.string ppf "-"
    | Some fpath -> Fmt.string ppf fpath
  in
  let contents = Arg.conv (parser, pp) in
  let open Arg in
  required & pos 2 (some contents) None & info [] ~doc ~docv:"FILE"

let term =
  let open Term in
  term_result (const run $ image $ path $ contents)

let cmd =
  let doc = "Write a file" in
  let info = Cmd.info "write" ~doc in
  Cmd.v info term
