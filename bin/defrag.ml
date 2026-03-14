type t = File of Mfat.Sfn.t * string | Dir of Mfat.Sfn.t * t list

let bytes_per_sector = 512
let ( let* ) = Result.bind

let rec collect_tree t path =
  let* entries = Fs.ls t path in
  let fn acc entry =
    let abs = Mfat.Spath.(path / entry.Mfat.name) in
    match acc with
    | Error _ as err -> err
    | Ok acc when entry.Mfat.is_dir ->
        let* vs = collect_tree t abs in
        let entry = Dir (entry.Mfat.name, vs) in
        Ok (entry :: acc)
    | Ok acc ->
        let* contents = Fs.read t abs in
        Ok (File (entry.Mfat.name, contents) :: acc)
  in
  let* entries = List.fold_left fn (Ok []) entries in
  Ok (List.rev entries)

let rec restore_tree t base nodes =
  let fn acc node =
    match (acc, node) with
    | (Error _ as err), _ -> err
    | Ok (), File (name, contents) ->
        let abs = Mfat.Spath.(base / name) in
        Fs.write t abs contents
    | Ok (), Dir (name, children) ->
        let abs = Mfat.Spath.(base / name) in
        let* () = Fs.mkdir t abs in
        restore_tree t abs children
  in
  List.fold_left fn (Ok ()) nodes

let rec count_files = function
  | File _ -> (1, 0)
  | Dir (_, children) ->
      let fn (af, ad) node =
        let f, d = count_files node in
        (af + f, ad + d)
      in
      let f, d = List.fold_left fn (0, 0) children in
      (f, d + 1)

let count_tree nodes =
  let fn (af, ad) node =
    let f, d = count_files node in
    (af + f, ad + d)
  in
  List.fold_left fn (0, 0) nodes

let read_total_sectors fd =
  let buf = Bstr.create bytes_per_sector in
  Fs.Blk.read fd ~src_off:0 buf;
  Int32.to_int (Bstr.get_int32_le buf 32)

let run fpath =
  let fd = Unix.openfile fpath [ Unix.O_RDWR ] 0 in
  let finally () = Unix.close fd in
  Fun.protect ~finally @@ fun () ->
  let* t = Fs.create fd in
  let* tree = collect_tree t Mfat.Spath.root in
  let n_files, n_dirs = count_tree tree in
  let total_sectors = read_total_sectors fd in
  let total_size = total_sectors * bytes_per_sector in
  Unix.ftruncate fd total_size;
  Make.format fd ~total_sectors;
  let* t = Fs.create fd in
  let* () = restore_tree t Mfat.Spath.root tree in
  Fmt.pr "Defragmented %s: %d file(s), %d directory(ies)\n" fpath n_files n_dirs;
  Ok ()

open Cmdliner

let image = Arg.(required & pos 0 (some string) None & info [] ~docv:"IMAGE")

let term =
  let open Term in
  term_result (const run $ image)

let cmd =
  let doc = "Defragment the filesystem" in
  let info = Cmd.info "defrag" ~doc in
  Cmd.v info term
