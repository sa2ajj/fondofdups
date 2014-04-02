(*
 * A simple finder of duplicate files.
 *
 * Copyright (C) 2014, Mikhail Sobolev <mss@mawhrin.net>
 *)

module FileNames = Set.Make(struct
    type t = string
    let compare : string -> string -> int = compare
end)

module FilesBySize = Map.Make(struct
    type t = int
    let compare : int -> int -> int = compare
end)

module WithContent = Map.Make(struct
    type t = string
    let compare : string -> string -> int = compare
end)

module WithDigests = Map.Make(struct
    type t = Digest.t
    let compare = Digest.compare
end)

type large_file = Single of string
                | Various of FileNames.t WithDigests.t

type kind = Empty of FileNames.t
          | Smallish of FileNames.t WithContent.t
          | Largish of large_file WithContent.t

let small_file_size = 64

let fold_left = List.fold_left

let printf = Printf.printf
let eprintf = Printf.eprintf

let find_or_default search default =
    try
        search ()
    with Not_found ->
        default ();;

let handle_file info filename =
    let read_bytes name size =
        let data = open_in_bin name in
        let result = Buffer.create size
        in
            Buffer.add_channel result data size;
            close_in data;
            Buffer.contents result in
    let get_digest filename = Digest.file filename in
    let find_by_size size info =
        find_or_default (fun () -> FilesBySize.find size info)
                        (fun () -> if size <= small_file_size
                                   then Smallish WithContent.empty
                                   else Largish WithContent.empty) in
    let update_filenames search filename =
        try
            let filenames = search () in
            FileNames.add filename filenames
        with Not_found ->
            FileNames.singleton filename in
    let size = (Unix.stat filename).Unix.st_size in
    match find_by_size size info with
    | Empty empties ->
        FilesBySize.add 0 (Empty (FileNames.add filename empties)) info
    | Smallish small_files ->
        let bytes = read_bytes filename size in
        let more_names = update_filenames (fun () -> WithContent.find bytes small_files) filename in
        let small_files' = WithContent.add bytes more_names small_files in
        FilesBySize.add size (Smallish small_files') info
    | Largish large_files ->
        let check_1st_level bytes =
            find_or_default (fun () -> Some (WithContent.find bytes large_files))
                            (fun () -> None) in
        let bytes = read_bytes filename size in
        let large_files' = match check_1st_level bytes with
            | None ->
                WithContent.singleton bytes (Single filename)
            | Some value ->
                let various = match value with
                            | Single previous ->
                                WithDigests.singleton (get_digest previous)
                                                      (FileNames.singleton previous)
                            | Various many ->
                                many in
                let digest = get_digest filename in
                let more_names = update_filenames (fun () -> WithDigests.find digest various) filename in
                let various' = WithDigests.add digest more_names various
                in
                    WithContent.add bytes (Various various') large_files
        in
            FilesBySize.add size (Largish large_files') info;;

let rec handle_dir info directory =
    try
        let dirs, files = Sys.readdir directory
                        |> Array.to_list
                        |> List.map (Filename.concat directory)
                        |> List.partition Sys.is_directory
        in
            fold_left handle_dir
                      (fold_left handle_file info files)
                      dirs
    with Sys_error _ ->
        eprintf "Directory %s not found.  Skipped.\n" directory;
        info;;

let print_result info =
    let print_group title filenames =
        if FileNames.cardinal filenames > 1 then begin
            printf "%s:\n" title;
            FileNames.iter (printf "  %s\n") filenames
        end in
    let print_dups _ filenames =
        print_group "Found a group of duplicates:" filenames in
    let print_one _ item =
        match item with
        | Empty empties ->
            print_group "Empty files" empties
        | Smallish small_files ->
            WithContent.iter print_dups small_files
        | Largish large_files ->
            let print_multi_only _ candidate =
                match candidate with
                | Single _ ->
                    ()
                | Various to_check ->
                    WithDigests.iter print_dups to_check in
            WithContent.iter print_multi_only large_files
    in FilesBySize.iter print_one info;;

let () =
    match Array.to_list Sys.argv with
    | [_] | [] -> eprintf "Wrong usage.\n"
    | _ :: dirs ->
        let info = FilesBySize.singleton 0 (Empty FileNames.empty) in
        let result = fold_left handle_dir info dirs in
        print_result result;;
