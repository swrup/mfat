type entry = { name: string; is_dir: bool; size: int32 }

(* BPB (BIOS Parameter Block) parsed from sector 0 *)
type bpb = {
    bytes_per_sector: int
  ; sectors_per_cluster: int
  ; reserved_sectors: int
  ; num_fats: int
  ; total_sectors_32: int32
  ; fat_size_32: int32
  ; root_cluster: int32
}

type 'blk t = { blk: 'blk; cache: 'blk Cachet.t; bpb: bpb }

let error_msgf fmt = Fmt.kstr (fun s -> Error (`Msg s)) fmt
let ( let* ) = Result.bind

(* BPB parsing *)

module Bpb = struct
  let parse cache =
    let bytes_per_sector = Cachet.get_uint16_le cache 11 in
    let sectors_per_cluster = Cachet.get_uint8 cache 13 in
    let reserved_sectors = Cachet.get_uint16_le cache 14 in
    let num_fats = Cachet.get_uint8 cache 16 in
    let total_sectors_32 = Cachet.get_int32_le cache 32 in
    let fat_size_32 = Cachet.get_int32_le cache 36 in
    let root_cluster = Cachet.get_int32_le cache 44 in
    if
      bytes_per_sector <> 512
      && bytes_per_sector <> 1024
      && bytes_per_sector <> 2048
      && bytes_per_sector <> 4096
    then error_msgf "invalid bytes_per_sector: %d" bytes_per_sector
    else if sectors_per_cluster = 0 then error_msgf "sectors_per_cluster is 0"
    else if num_fats = 0 then error_msgf "num_fats is 0"
    else
      Ok
        {
          bytes_per_sector
        ; sectors_per_cluster
        ; reserved_sectors
        ; num_fats
        ; total_sectors_32
        ; fat_size_32
        ; root_cluster
        }

  let cluster_size bpb = bpb.bytes_per_sector * bpb.sectors_per_cluster
  let fat_offset bpb = bpb.reserved_sectors * bpb.bytes_per_sector

  let data_offset bpb =
    (bpb.reserved_sectors + (bpb.num_fats * Int32.to_int bpb.fat_size_32))
    * bpb.bytes_per_sector

  let cluster_offset bpb cluster =
    let cluster_num = Int32.to_int cluster - 2 in
    data_offset bpb + (cluster_num * cluster_size bpb)
end

(* FAT table operations *)

module type BLOCK = sig
  type t

  val pagesize : t -> int
  val read : t -> src_off:int -> ?dst_off:int -> Bstr.t -> unit
  val write : t -> ?src_off:int -> dst_off:int -> Bstr.t -> unit
end

module Fat (Blk : BLOCK) = struct
  let eoc = 0x0FFFFFFFl
  let eoc_min = 0x0FFFFFF8l
  let free = 0x00000000l
  let mask = 0x0FFFFFFFl

  let read_entry cache bpb cluster =
    let off = Bpb.fat_offset bpb + (Int32.to_int cluster * 4) in
    Int32.logand (Cachet.get_int32_le cache off) mask

  let write_entry blk cache bpb cluster value =
    let off = Bpb.fat_offset bpb + (Int32.to_int cluster * 4) in
    let pagesize = Blk.pagesize blk in
    let page_off = off / pagesize * pagesize in
    let buf = Bstr.create pagesize in
    Blk.read blk ~src_off:page_off buf;
    let rel = off - page_off in
    (* Write the 32-bit LE value, preserving high 4 bits *)
    let old_val = Bstr.get_int32_le buf rel in
    let high_bits = Int32.logand old_val 0xF0000000l in
    let new_val = Int32.logor high_bits (Int32.logand value mask) in
    Bstr.set_int32_le buf rel new_val;
    Blk.write blk ~dst_off:page_off buf;
    Cachet.invalidate cache ~off:page_off ~len:pagesize

  let follow_chain cache bpb start =
    let rec go acc cluster =
      if Int32.unsigned_compare cluster eoc_min >= 0 then List.rev acc
      else if cluster = free then List.rev acc
      else
        let next = read_entry cache bpb cluster in
        go (cluster :: acc) next
    in
    go [] start

  let alloc_cluster blk cache bpb =
    let total_data_clusters =
      (Int32.to_int bpb.total_sectors_32
      - bpb.reserved_sectors
      - (bpb.num_fats * Int32.to_int bpb.fat_size_32))
      / bpb.sectors_per_cluster
    in
    let max_cluster = total_data_clusters + 1 in
    let rec search i =
      if i > max_cluster then error_msgf "no free cluster"
      else
        let v = read_entry cache bpb (Int32.of_int i) in
        if v = free then (
          write_entry blk cache bpb (Int32.of_int i) eoc;
          Ok (Int32.of_int i))
        else search (i + 1)
    in
    search 2

  let free_chain blk cache bpb start =
    let rec go cluster =
      if Int32.unsigned_compare cluster eoc_min >= 0 then ()
      else if cluster = free then ()
      else
        let next = read_entry cache bpb cluster in
        write_entry blk cache bpb cluster free;
        go next
    in
    go start
end

module Dir (Blk : BLOCK) = struct
  let entry_size = 32
  let attr_directory = 0x10
  let attr_long_name = 0x0f
  let deleted_marker = 0xe5

  module Fat = Fat (Blk)

  type raw_entry = {
      name: string
    ; ext: string
    ; attr: int
    ; first_cluster: int32
    ; file_size: int32
    ; long_name: string option
  }

  (* LFN checksum: computed from the 8.3 short name (11 bytes) *)
  let lfn_checksum name_8 ext_3 =
    let ( .![] ) str idx = Char.code str.[idx] in
    let chk = 0 in
    let chk = (((chk land 1) lsl 7) + (chk lsr 1) + name_8.![0]) land 0xff in
    let chk = (((chk land 1) lsl 7) + (chk lsr 1) + name_8.![1]) land 0xff in
    let chk = (((chk land 1) lsl 7) + (chk lsr 1) + name_8.![2]) land 0xff in
    let chk = (((chk land 1) lsl 7) + (chk lsr 1) + name_8.![3]) land 0xff in
    let chk = (((chk land 1) lsl 7) + (chk lsr 1) + name_8.![4]) land 0xff in
    let chk = (((chk land 1) lsl 7) + (chk lsr 1) + name_8.![5]) land 0xff in
    let chk = (((chk land 1) lsl 7) + (chk lsr 1) + name_8.![6]) land 0xff in
    let chk = (((chk land 1) lsl 7) + (chk lsr 1) + ext_3.![0]) land 0xff in
    let chk = (((chk land 1) lsl 7) + (chk lsr 1) + ext_3.![1]) land 0xff in
    chk

  (* Extract UCS-2 characters from an LFN entry and convert to UTF-8 bytes.
     LFN stores 13 UCS-2 characters per entry at specific offsets:
       bytes 1-10  : chars 0-4  (5 chars, 2 bytes each)
       bytes 14-25 : chars 5-10 (6 chars, 2 bytes each)
       bytes 28-31 : chars 11-12 (2 chars, 2 bytes each)

     UCS-2 is the predecessor of UTF-16, so it's a UTF-16 to UTF-8 function. *)
  let extract_lfn_chars cache off =
    let offsets =
      [
        off + 1; off + 3; off + 5; off + 7; off + 9; off + 14; off + 16
      ; off + 18; off + 20; off + 22; off + 24; off + 28; off + 30
      ]
    in
    let buf = Buffer.create 13 in
    let stop = ref false in
    let fn offset =
      if not !stop then begin
        let lo = Cachet.get_uint8 cache offset in
        let hi = Cachet.get_uint8 cache (offset + 1) in
        let code = lo lor (hi lsl 8) in
        if code = 0x0000 || code = 0xffff then stop := true
        else if code < 0x80 then Buffer.add_char buf (Char.chr code)
          (* 1 byte *)
        else if code < 0x800 then begin
          (* 2 bytes *)
          Buffer.add_char buf (Char.chr (0xc0 lor (code lsr 6)));
          Buffer.add_char buf (Char.chr (0x80 lor (code land 0x3f)))
        end
        else begin
          (* 3 bytes *)
          Buffer.add_char buf (Char.chr (0xe0 lor (code lsr 12)));
          Buffer.add_char buf (Char.chr (0x80 lor ((code lsr 6) land 0x3f)));
          Buffer.add_char buf (Char.chr (0x80 lor (code land 0x3f)))
        end
      end
    in
    List.iter fn offsets; Buffer.contents buf

  let parse_entry cache off =
    let first_byte = Cachet.get_uint8 cache off in
    if first_byte = 0x00 then `End
    else if first_byte = deleted_marker then `Deleted
    else
      let attr = Cachet.get_uint8 cache (off + 11) in
      if attr land attr_long_name <> 0 then
        let ord = first_byte land 0x3f in
        let chk = Cachet.get_uint8 cache (off + 13) in
        let chars = extract_lfn_chars cache off in
        `LongName (ord, chk, chars)
      else
        (* SFN *)
        let name = Cachet.get_string cache ~len:8 off in
        let ext = Cachet.get_string cache ~len:3 (off + 8) in
        let cluster_high = Cachet.get_uint16_le cache (off + 20) in
        let cluster_low = Cachet.get_uint16_le cache (off + 26) in
        let first_cluster =
          Int32.logor
            (Int32.shift_left (Int32.of_int cluster_high) 16)
            (Int32.of_int cluster_low)
        in
        let file_size = Cachet.get_int32_le cache (off + 28) in
        `Entry { name; ext; attr; first_cluster; file_size; long_name= None }

  let format_name raw =
    let name = String.trim raw.name in
    let ext = String.trim raw.ext in
    if ext = "" then name else name ^ "." ^ ext

  let to_entry raw =
    let name =
      match raw.long_name with
      | Some ln -> ln
      | None -> String.uppercase_ascii (format_name raw)
    in
    let is_dir = raw.attr land attr_directory <> 0 in
    { name; is_dir; size= raw.file_size }

  let reconstruct_long_name lfn_parts raw =
    match lfn_parts with
    | [] -> None
    | parts ->
        let expected_cksum = lfn_checksum raw.name raw.ext in
        let valid =
          List.for_all (fun (_, cksum, _) -> cksum = expected_cksum) parts
        in
        if valid then begin
          let fn (a, _, _) (b, _, _) = compare a b in
          let sorted = List.sort fn parts in
          Some (String.concat "" (List.map (fun (_, _, c) -> c) sorted))
        end
        else None

  let read_dir cache bpb cluster =
    let clusters = Fat.follow_chain cache bpb cluster in
    let cluster_sz = Bpb.cluster_size bpb in
    let entries = ref [] in
    let stop = ref false in
    let lfn_parts = ref [] in
    let fn cl =
      if not !stop then
        let base = Bpb.cluster_offset bpb cl in
        let n = cluster_sz / entry_size in
        for i = 0 to n - 1 do
          if not !stop then
            let off = base + (i * entry_size) in
            match parse_entry cache off with
            | `End -> stop := true
            | `Deleted -> lfn_parts := []
            | `LongName (ord, chk, chars) ->
                lfn_parts := (ord, chk, chars) :: !lfn_parts
            | `Entry raw ->
                if raw.name <> ".       " && raw.name <> "..      " then begin
                  let long_name = reconstruct_long_name !lfn_parts raw in
                  entries := { raw with long_name } :: !entries
                end;
                lfn_parts := []
        done
    in
    List.iter fn clusters; List.rev !entries

  let find_in_dir cache bpb cluster name =
    let target = String.uppercase_ascii name in
    let entries = read_dir cache bpb cluster in
    let matches raw =
      match raw.long_name with
      | Some ln -> String.uppercase_ascii ln = target
      | None ->
          let formatted = String.uppercase_ascii (format_name raw) in
          formatted = target
    in
    match List.find_opt matches entries with
    | Some e -> Ok e
    | None -> error_msgf "%s: not found" name

  (* Convert a filename to 8.3 format (padded with spaces) *)
  let to_8_3 name =
    let name = String.uppercase_ascii name in
    let name_8, ext_3 =
      match String.rindex_opt name '.' with
      | None -> (name, "")
      | Some idx ->
          let len = String.length name in
          let name_8 = String.sub name 0 idx in
          let ext_3 = String.sub name (idx + 1) (len - idx - 1) in
          (name_8, ext_3)
    in
    (* cap to 8 bytes and 3 bytes *)
    let name_8 =
      let buf = Bytes.make 8 ' ' in
      let len = Int.min 8 (String.length name_8) in
      Bytes.blit_string name_8 0 buf 0 len;
      Bytes.to_string buf
    in
    let ext_3 =
      let buf = Bytes.make 3 ' ' in
      let len = Int.min 3 (String.length ext_3) in
      Bytes.blit_string ext_3 0 buf 0 len;
      Bytes.to_string buf
    in
    (name_8, ext_3)

  let needs_lfn name =
    let len = String.length name in
    if len = 0 then false
    else
      let has_lower = ref false in
      String.iter (function 'a' .. 'z' -> has_lower := true | _ -> ()) name;
      if !has_lower then true
      else
        match String.rindex_opt name '.' with
        | None -> len > 8
        | Some idx ->
            let base_len = idx in
            let ext_len = len - idx - 1 in
            base_len > 8 || ext_len > 3

  (* Generate a unique 8.3 short name with numeric tail (~1, ~2, etc.)
     For LFN entries, the short name is a basis name like "LONGFI~1.TXT" *)
  let generate_short_name _cache _bpb _dir_cluster name =
    let name = String.uppercase_ascii name in
    let name_8, ext_3 =
      match String.rindex_opt name '.' with
      | None -> (name, "")
      | Some idx ->
          let len = String.length name in
          let name_8 = String.sub name 0 idx
          and ext_3 = String.sub name (idx + 1) (len - idx - 1) in
          (name_8, ext_3)
    in
    let strip str =
      let buf = Buffer.create (String.length str) in
      let fn = function ' ' | '.' -> () | chr -> Buffer.add_char buf chr in
      String.iter fn str; Buffer.contents buf
    in
    let name_8 = strip name_8 in
    let ext_3 = String.sub ext_3 0 (Int.min 3 (String.length ext_3)) in
    (* truncate base to 6 chars and add ~1 *)
    let name_8 = String.sub name_8 0 (Int.min 6 (String.length name_8)) in
    let name_8 = name_8 ^ "~1" in
    (* cap to 8 bytes and 3 bytes *)
    let name_8 =
      let buf = Bytes.make 8 ' ' in
      let len = Int.min 8 (String.length name_8) in
      Bytes.blit_string name_8 0 buf 0 len;
      Bytes.to_string buf
    in
    let ext_3 =
      let buf = Bytes.make 3 ' ' in
      let len = Int.min 3 (String.length ext_3) in
      Bytes.blit_string ext_3 0 buf 0 len;
      Bytes.to_string buf
    in
    (name_8, ext_3)

  let write_lfn_entry_at blk cache off ~ord ~checksum ~chars ~is_last =
    let pagesize = Blk.pagesize blk in
    let page_off = off / pagesize * pagesize in
    let buf = Bstr.create pagesize in
    Blk.read blk ~src_off:page_off buf;
    let rel = off - page_off in
    Bstr.memset buf ~off:rel ~len:32 '\000';
    let ord_byte = if is_last then ord lor 0x40 else ord in
    Bstr.set_uint8 buf rel ord_byte;
    Bstr.set_uint8 buf (rel + 11) attr_long_name;
    Bstr.set_uint8 buf (rel + 12) 0;
    Bstr.set_uint8 buf (rel + 13) checksum;
    Bstr.set_uint16_le buf (rel + 26) 0;
    let ucs2_offsets =
      [
        rel + 1; rel + 3; rel + 5; rel + 7; rel + 9; rel + 14; rel + 16
      ; rel + 18; rel + 20; rel + 22; rel + 24; rel + 28; rel + 30
      ]
    in
    let chars_idx = ref 0 in
    let chars_len = String.length chars in
    let finished = ref false in
    let fn off =
      if !finished then begin
        Bstr.set_uint8 buf off 0xff;
        Bstr.set_uint8 buf (off + 1) 0xff
      end
      else if !chars_idx >= chars_len then begin
        Bstr.set_uint8 buf off 0x00;
        Bstr.set_uint8 buf (off + 1) 0x00;
        finished := true
      end
      else begin
        (* see [extract_lfn_chars] *)
        let byte0 = Char.code chars.[!chars_idx] in
        let code, advance =
          if byte0 < 0x80 then (byte0, 1)
          else if byte0 < 0xe0 && !chars_idx + 1 < chars_len then
            let byte1 = Char.code chars.[!chars_idx + 1] in
            (((byte0 land 0x1f) lsl 6) lor (byte1 land 0x3f), 2)
          else if byte0 < 0xf0 && !chars_idx + 2 < chars_len then
            let byte1 = Char.code chars.[!chars_idx + 1] in
            let byte2 = Char.code chars.[!chars_idx + 2] in
            let int24 =
              ((byte0 land 0x0f) lsl 12)
              lor ((byte1 land 0x3f) lsl 6)
              lor (byte2 land 0x3f)
            in
            (int24, 3)
          else (0xfffd, 1)
        in
        Bstr.set_uint8 buf off (code land 0xff);
        Bstr.set_uint8 buf (off + 1) ((code lsr 8) land 0xff);
        chars_idx := !chars_idx + advance
      end
    in
    List.iter fn ucs2_offsets;
    Blk.write blk ~dst_off:page_off buf;
    Cachet.invalidate cache ~off:page_off ~len:pagesize

  let split_lfn_name name =
    let len = String.length name in
    let chunks = ref [] in
    let current = Buffer.create 26 in
    let count = ref 0 in
    let i = ref 0 in
    while !i < len do
      let byte0 = Char.code name.[!i] in
      let advance =
        if byte0 < 0x80 then 1
        else if byte0 < 0xe0 then 2
        else if byte0 < 0xf0 then 3
        else 4
      in
      let advance = min advance (len - !i) in
      Buffer.add_string current (String.sub name !i advance);
      incr count;
      i := !i + advance;
      if !count = 13 then begin
        chunks := Buffer.contents current :: !chunks;
        Buffer.clear current;
        count := 0
      end
    done;
    if Buffer.length current > 0 then
      chunks := Buffer.contents current :: !chunks;
    List.rev !chunks

  (* Write a 32-byte directory entry at the given offset *)
  let write_entry_at blk cache off ~name_8 ~ext_3 ~attr ~first_cluster
      ~file_size =
    let pagesize = Blk.pagesize blk in
    let page_off = off / pagesize * pagesize in
    let buf = Bstr.create pagesize in
    Blk.read blk ~src_off:page_off buf;
    let rel = off - page_off in
    Bstr.blit_from_string name_8 ~src_off:0 buf ~dst_off:rel ~len:8;
    Bstr.blit_from_string ext_3 ~src_off:0 buf ~dst_off:(rel + 8) ~len:3;
    Bstr.set_uint8 buf (rel + 11) attr;
    (* Bytes 12-19: reserved/time fields, zero them *)
    Bstr.memset buf ~off:(rel + 12) ~len:8 '\000';
    Bstr.set_uint16_le buf (rel + 20)
      (Int32.to_int (Int32.shift_right_logical first_cluster 16));
    (* Bytes 22-25: time fields, zero them *)
    Bstr.memset buf ~off:(rel + 22) ~len:4 '\000';
    Bstr.set_uint16_le buf (rel + 26)
      (Int32.to_int (Int32.logand first_cluster 0xFFFFl));
    Bstr.set_int32_le buf (rel + 28) file_size;
    Blk.write blk ~dst_off:page_off buf;
    Cachet.invalidate cache ~off:page_off ~len:pagesize

  (* Find N contiguous free slots in a directory, returns offset of the first *)
  let find_free_slots blk cache bpb dir_cluster n =
    let clusters = Fat.follow_chain cache bpb dir_cluster in
    let cluster_sz = Bpb.cluster_size bpb in
    let result = ref None in
    let consecutive = ref 0 in
    let first_off = ref 0 in
    let fn cl =
      if !result = None then
        let base = Bpb.cluster_offset bpb cl in
        let count = cluster_sz / entry_size in
        for i = 0 to count - 1 do
          if !result = None then begin
            let off = base + (i * entry_size) in
            let first_byte = Cachet.get_uint8 cache off in
            if first_byte = 0x00 || first_byte = deleted_marker then begin
              if !consecutive = 0 then first_off := off;
              incr consecutive;
              if !consecutive >= n then result := Some !first_off
            end
            else consecutive := 0
          end
        done
    in
    List.iter fn clusters;
    match !result with
    | Some off -> Ok off
    | None -> begin
        (* Need to extend the directory: allocate a new cluster *)
        let last_cluster = List.nth clusters (List.length clusters - 1) in
        match Fat.alloc_cluster blk cache bpb with
        | Error _ as e -> e
        | Ok new_cl ->
            Fat.write_entry blk cache bpb last_cluster new_cl;
            (* Zero out the new cluster *)
            let pagesize = Blk.pagesize blk in
            let base = Bpb.cluster_offset bpb new_cl in
            let n_pages = cluster_sz / pagesize in
            for p = 0 to n_pages - 1 do
              let off = base + (p * pagesize) in
              let buf = Bstr.create pagesize in
              Bstr.fill buf '\000';
              Blk.write blk ~dst_off:off buf;
              Cachet.invalidate cache ~off ~len:pagesize
            done;
            (* If still not enough room, we'd need to allocate more,
               but a single cluster should hold enough for reasonable names *)
            if cluster_sz / entry_size >= n then Ok base
            else error_msgf "name too long for directory extension"
      end

  let add_entry blk cache bpb dir_cluster ~name ~attr ~first_cluster ~file_size
      =
    if needs_lfn name then begin
      let name_8, ext_3 = generate_short_name cache bpb dir_cluster name in
      let chk = lfn_checksum name_8 ext_3 in
      let chunks = split_lfn_name name in
      let num_lfn = List.length chunks in
      let total_slots = num_lfn + 1 in
      let* base_off = find_free_slots blk cache bpb dir_cluster total_slots in
      (* Write LFN entries in reverse order (highest ordinal first) *)
      let fn idx chunk =
        let ord = num_lfn - idx in
        let is_last = idx = 0 in
        let off = base_off + (idx * entry_size) in
        write_lfn_entry_at blk cache off ~ord ~checksum:chk ~chars:chunk
          ~is_last
      in
      (* LFN entries are stored in reverse order on disk.
         Entry with highest ordinal comes first, lowest ordinal last,
         then the 8.3 entry follows. *)
      List.iteri fn (List.rev chunks);
      (* Write the 8.3 entry after all LFN entries *)
      let sfn_off = base_off + (num_lfn * entry_size) in
      write_entry_at blk cache sfn_off ~name_8 ~ext_3 ~attr ~first_cluster
        ~file_size;
      Ok ()
    end
    else begin
      let name_8, ext_3 = to_8_3 name in
      let* off = find_free_slots blk cache bpb dir_cluster 1 in
      write_entry_at blk cache off ~name_8 ~ext_3 ~attr ~first_cluster
        ~file_size;
      Ok ()
    end

  (* Mark a byte as deleted at the given offset *)
  let mark_deleted blk cache off =
    let pagesize = Blk.pagesize blk in
    let page_off = off / pagesize * pagesize in
    let buf = Bstr.create pagesize in
    Blk.read blk ~src_off:page_off buf;
    Bstr.set_uint8 buf (off - page_off) deleted_marker;
    Blk.write blk ~dst_off:page_off buf;
    Cachet.invalidate cache ~off:page_off ~len:pagesize

  let remove_entry blk cache bpb dir_cluster name =
    let target_upper = String.uppercase_ascii name in
    let clusters = Fat.follow_chain cache bpb dir_cluster in
    let cluster_sz = Bpb.cluster_size bpb in
    let found = ref false in
    let lfn_offsets = ref [] in
    let lfn_parts = ref [] in
    let fn cl =
      if not !found then
        let base = Bpb.cluster_offset bpb cl in
        let n = cluster_sz / entry_size in
        for i = 0 to n - 1 do
          if not !found then
            let off = base + (i * entry_size) in
            match parse_entry cache off with
            | `End -> ()
            | `Deleted ->
                lfn_offsets := [];
                lfn_parts := []
            | `LongName (ord, chk, chars) ->
                lfn_offsets := off :: !lfn_offsets;
                lfn_parts := (ord, chk, chars) :: !lfn_parts
            | `Entry raw ->
                let long_name = reconstruct_long_name !lfn_parts raw in
                let matches =
                  match long_name with
                  | Some ln -> String.uppercase_ascii ln = target_upper
                  | None ->
                      String.uppercase_ascii (format_name raw) = target_upper
                in
                if matches then begin
                  List.iter (mark_deleted blk cache) !lfn_offsets;
                  mark_deleted blk cache off;
                  found := true
                end;
                lfn_offsets := [];
                lfn_parts := []
        done
    in
    List.iter fn clusters;
    if !found then Ok () else error_msgf "%s: not found" name

  (* Update the file_size and first_cluster of an existing directory entry *)
  let update_entry blk cache bpb dir_cluster name ~first_cluster ~file_size =
    let target_upper = String.uppercase_ascii name in
    let clusters = Fat.follow_chain cache bpb dir_cluster in
    let cluster_sz = Bpb.cluster_size bpb in
    let found = ref false in
    (* Accumulate LFN parts to reconstruct long names *)
    let lfn_parts = ref [] in
    let fn cl =
      if not !found then
        let base = Bpb.cluster_offset bpb cl in
        let n = cluster_sz / entry_size in
        for i = 0 to n - 1 do
          if not !found then
            let off = base + (i * entry_size) in
            match parse_entry cache off with
            | `End -> ()
            | `Deleted -> lfn_parts := []
            | `LongName (ord, chk, chars) ->
                lfn_parts := (ord, chk, chars) :: !lfn_parts
            | `Entry raw ->
                let long_name = reconstruct_long_name !lfn_parts raw in
                let matches =
                  match long_name with
                  | Some ln -> String.uppercase_ascii ln = target_upper
                  | None ->
                      String.uppercase_ascii (format_name raw) = target_upper
                in
                if matches then begin
                  write_entry_at blk cache off ~name_8:raw.name ~ext_3:raw.ext
                    ~attr:raw.attr ~first_cluster ~file_size;
                  found := true
                end;
                lfn_parts := []
        done
    in
    List.iter fn clusters;
    if !found then Ok () else error_msgf "%s: not found" name
end

module Path (Blk : BLOCK) = struct
  module Dir = Dir (Blk)

  let split path =
    let path =
      if String.length path > 0 && path.[0] = '/' then
        String.sub path 1 (String.length path - 1)
      else path
    in
    if path = "" then [] else String.split_on_char '/' path

  let parent_and_name path =
    let parts = split path in
    match List.rev parts with
    | [] -> error_msgf "empty path"
    | name :: rev_parent -> Ok (List.rev rev_parent, name)

  let resolve_dir cache bpb parts =
    let rec go cluster = function
      | [] -> Ok cluster
      | name :: rest ->
          let* raw = Dir.find_in_dir cache bpb cluster name in
          if raw.Dir.attr land Dir.attr_directory <> 0 then
            go raw.Dir.first_cluster rest
          else error_msgf "%s: not a directory" name
    in
    go bpb.root_cluster parts

  let resolve cache bpb path =
    let parts = split path in
    match List.rev parts with
    | [] -> Ok (bpb.root_cluster, None)
    | name :: rev_parent ->
        let* dir_cluster = resolve_dir cache bpb (List.rev rev_parent) in
        let* raw = Dir.find_in_dir cache bpb dir_cluster name in
        Ok (dir_cluster, Some raw)
end

module Make (Blk : BLOCK) = struct
  module Fat = Fat (Blk)
  module Dir = Dir (Blk)
  module Path = Path (Blk)

  let create blk =
    let pagesize = Blk.pagesize blk in
    let map blk ~pos _len =
      let buf = Bstr.create pagesize in
      let src_off = pos / pagesize * pagesize in
      Blk.read blk ~src_off buf; buf
    in
    let cache = Cachet.make ~pagesize ~map blk in
    let* bpb = Bpb.parse cache in
    Ok { blk; cache; bpb }

  let ls t path =
    let parts = Path.split path in
    let* cluster = Path.resolve_dir t.cache t.bpb parts in
    let raws = Dir.read_dir t.cache t.bpb cluster in
    Ok (List.map Dir.to_entry raws)

  let read t path =
    let* _, v = Path.resolve t.cache t.bpb path in
    match v with
    | None -> error_msgf "%s: is root directory" path
    | Some raw ->
        if raw.Dir.attr land Dir.attr_directory <> 0 then
          error_msgf "%s: is a directory" path
        else
          let size = Int32.to_int raw.Dir.file_size in
          if size = 0 then Ok ""
          else
            let clusters =
              Fat.follow_chain t.cache t.bpb raw.Dir.first_cluster
            in
            let cluster_sz = Bpb.cluster_size t.bpb in
            let buf = Buffer.create size in
            let remaining = ref size in
            List.iter
              (fun cl ->
                if !remaining > 0 then (
                  let base = Bpb.cluster_offset t.bpb cl in
                  let to_read = min !remaining cluster_sz in
                  let s = Cachet.get_string t.cache ~len:to_read base in
                  Buffer.add_string buf s;
                  remaining := !remaining - to_read))
              clusters;
            Ok (Buffer.contents buf)

  let write_data_to_clusters blk cache bpb clusters data =
    let cluster_sz = Bpb.cluster_size bpb in
    let pagesize = Blk.pagesize blk in
    let data_len = String.length data in
    let offset = ref 0 in
    let fn cl =
      if !offset < data_len then begin
        let base = Bpb.cluster_offset bpb cl in
        let to_write = min (data_len - !offset) cluster_sz in
        (* Write page by page *)
        let written = ref 0 in
        while !written < to_write do
          let page_base = base + !written in
          let page_aligned = page_base / pagesize * pagesize in
          let buf = Bstr.create pagesize in
          (* Read existing page if partial write *)
          if page_base <> page_aligned || to_write - !written < pagesize then
            Blk.read blk ~src_off:page_aligned buf
          else Bstr.fill buf '\000';
          let in_page_off = page_base - page_aligned in
          let in_page_len =
            min (pagesize - in_page_off) (to_write - !written)
          in
          Bstr.blit_from_string data ~src_off:(!offset + !written) buf
            ~dst_off:in_page_off ~len:in_page_len;
          Blk.write blk ~dst_off:page_aligned buf;
          Cachet.invalidate cache ~off:page_aligned ~len:pagesize;
          written := !written + in_page_len
        done;
        offset := !offset + to_write
      end
    in
    List.iter fn clusters

  let write t path data =
    let* parent_parts, name = Path.parent_and_name path in
    let* dir_cluster = Path.resolve_dir t.cache t.bpb parent_parts in
    let data_len = String.length data in
    let cluster_sz = Bpb.cluster_size t.bpb in
    let needed_clusters =
      if data_len = 0 then 0 else (data_len + cluster_sz - 1) / cluster_sz
    in
    (* Check if file already exists *)
    let existing = Dir.find_in_dir t.cache t.bpb dir_cluster name in
    let existing = Result.to_option existing in
    (* Free old clusters if file exists *)
    let fn raw =
      if raw.Dir.first_cluster <> 0l then
        Fat.free_chain t.blk t.cache t.bpb raw.Dir.first_cluster
    in
    Option.iter fn existing;
    (* Allocate new clusters *)
    let rec alloc_n n acc =
      if n = 0 then Ok (List.rev acc)
      else
        let* cl = Fat.alloc_cluster t.blk t.cache t.bpb in
        alloc_n (n - 1) (cl :: acc)
    in
    let* clusters = alloc_n needed_clusters [] in
    (* Chain the clusters together *)
    let rec chain = function
      | [] | [ _ ] -> ()
      | a :: (b :: _ as rest) ->
          Fat.write_entry t.blk t.cache t.bpb a b;
          chain rest
    in
    chain clusters;
    (* Write data *)
    if data_len > 0 then
      write_data_to_clusters t.blk t.cache t.bpb clusters data;
    let first_cluster = match clusters with [] -> 0l | c :: _ -> c in
    let file_size = Int32.of_int data_len in
    match existing with
    | Some _ ->
        Dir.update_entry t.blk t.cache t.bpb dir_cluster name ~first_cluster
          ~file_size
    | None ->
        Dir.add_entry t.blk t.cache t.bpb dir_cluster ~name ~attr:0x20
          ~first_cluster ~file_size

  let mkdir t path =
    let* parent_parts, name = Path.parent_and_name path in
    let* dir_cluster = Path.resolve_dir t.cache t.bpb parent_parts in
    (* Check if already exists *)
    match Dir.find_in_dir t.cache t.bpb dir_cluster name with
    | Ok _ -> error_msgf "%s: already exists" name
    | Error _ ->
        (* Allocate a cluster for the new directory *)
        begin match Fat.alloc_cluster t.blk t.cache t.bpb with
        | Error _ as e -> e
        | Ok new_cl ->
            (* Zero out the new cluster *)
            let cluster_sz = Bpb.cluster_size t.bpb in
            let pagesize = Blk.pagesize t.blk in
            let base = Bpb.cluster_offset t.bpb new_cl in
            let n_pages = cluster_sz / pagesize in
            for p = 0 to n_pages - 1 do
              let off = base + (p * pagesize) in
              let buf = Bstr.create pagesize in
              Bstr.fill buf '\000';
              Blk.write t.blk ~dst_off:off buf;
              Cachet.invalidate t.cache ~off ~len:pagesize
            done;
            (* Create . entry *)
            Dir.write_entry_at t.blk t.cache base ~name_8:".       "
              ~ext_3:"   " ~attr:Dir.attr_directory ~first_cluster:new_cl
              ~file_size:0l;
            (* Create .. entry *)
            Dir.write_entry_at t.blk t.cache (base + 32) ~name_8:"..      "
              ~ext_3:"   " ~attr:Dir.attr_directory ~first_cluster:dir_cluster
              ~file_size:0l;
            (* Add entry in parent directory *)
            Dir.add_entry t.blk t.cache t.bpb dir_cluster ~name
              ~attr:Dir.attr_directory ~first_cluster:new_cl ~file_size:0l
        end

  let remove t path =
    let* parent_parts, name = Path.parent_and_name path in
    let* dir_cluster = Path.resolve_dir t.cache t.bpb parent_parts in
    let* raw = Dir.find_in_dir t.cache t.bpb dir_cluster name in
    (* If directory, check it's empty *)
    if raw.Dir.attr land Dir.attr_directory <> 0 then
      let entries = Dir.read_dir t.cache t.bpb raw.Dir.first_cluster in
      if entries <> [] then error_msgf "%s: directory not empty" name
      else begin
        Fat.free_chain t.blk t.cache t.bpb raw.Dir.first_cluster;
        Dir.remove_entry t.blk t.cache t.bpb dir_cluster name
      end
    else begin
      if raw.Dir.first_cluster <> 0l then
        Fat.free_chain t.blk t.cache t.bpb raw.Dir.first_cluster;
      Dir.remove_entry t.blk t.cache t.bpb dir_cluster name
    end

  let exists t path =
    match Path.resolve t.cache t.bpb path with Ok _ -> true | Error _ -> false

  let stat t path =
    let parts = Path.split path in
    if parts = [] then Ok { name= "/"; is_dir= true; size= 0l }
    else
      match Path.resolve t.cache t.bpb path with
      | Error _ as e -> e
      | Ok (_, None) -> Ok { name= "/"; is_dir= true; size= 0l }
      | Ok (_, Some raw) -> Ok (Dir.to_entry raw)
end
