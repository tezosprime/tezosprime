(* Copyright (c) 2016 The Qeditas developers *)
(* Copyright (c) 2017-2018 The Dalilcoin developers *)
(* Distributed under the MIT software license, see the accompanying
   file COPYING or http://www.opensource.org/licenses/mit-license.php. *)

let exitfn : (int -> unit) ref = ref (fun n -> exit n);;

let log : out_channel ref = ref stderr

let openlog () =
  log := open_out_gen [Open_wronly;Open_creat;Open_append] 0o644 (!Config.datadir ^ (if !Config.testnet then "/testnet/debug.log" else "/debug.log"))

let closelog () =
  close_out !log

let log_string x =
  output_string !log x;
  flush !log;
  if pos_out !log > 100000000 then (*** prevent debug.log from becoming more than 100MB ***)
    begin
      closelog ();
      let prevlog = (!Config.datadir ^ (if !Config.testnet then "/testnet/debug.log.1" else "/debug.log.1")) in
      if Sys.file_exists prevlog then (*** if the previous log file has not been moved or deleted, then stop the node; this is to prevent the log files from piling up ***)
	!exitfn 9
      else
	begin
	  Sys.rename (!Config.datadir ^ (if !Config.testnet then "/testnet/debug.log" else "/debug.log")) prevlog;
	  openlog()
	end
    end

(***
 era ranges from 1 and 43 (roughly 1 + 41*4 = 165 years until final era when reward drops to 0
 era 1 is for the first 70000 blocks (1 to 70000)
 era 2 is for the next 210000 blocks (70001 to 280000)
 and so on until 8680001 when the final (unlimited) era of 43 begins.
 ***)
let era blkh =
  if blkh < 8680001L then
    let blki = Int64.to_int blkh in
    ((blki + 349999) / 210000) (*** start counting here at blkh = 1L, corresponding to Bitcoin block at height 350000 ***)
  else
    43

(*** the max block size is 500K during era 1 and doubles with each era, with a final max block size of 512M. ***)
let maxblockdeltasize blkh = 250000 lsl (era blkh)

let random_initialized : bool ref = ref false;;

let hexchar_inv x =
  match x with
  | '0' -> 0
  | '1' -> 1
  | '2' -> 2
  | '3' -> 3
  | '4' -> 4
  | '5' -> 5
  | '6' -> 6
  | '7' -> 7
  | '8' -> 8
  | '9' -> 9
  | 'A' -> 10
  | 'B' -> 11
  | 'C' -> 12
  | 'D' -> 13
  | 'E' -> 14
  | 'F' -> 15
  | 'a' -> 10
  | 'b' -> 11
  | 'c' -> 12
  | 'd' -> 13
  | 'e' -> 14
  | 'f' -> 15
  | _ -> raise (Failure("not a hexit: " ^ (string_of_int (Char.code x))))

let initialize_random_seed () =
  match !Config.randomseed with
  | Some(r) ->
      if not (String.length r = 64) then raise (Failure "bad randomseed given, should be 32 bytes as 64 hex chars");
      let a = Array.make 32 0 in
      for i = 0 to 31 do
	a.(i) <- 16 * hexchar_inv (r.[2*i]) + hexchar_inv (r.[2*i+1])
      done;
      Random.full_init a;
      random_initialized := true
  | None ->
      if Sys.file_exists "/dev/random" then
	let r = open_in_bin "/dev/random" in
	let a = Array.make 32 0 in
	Printf.printf "Computing random seed, this may take a while.\n"; flush stdout;
	for i = 0 to 31 do
	  a.(i) <- input_byte r
	done;
	close_in r;
	Random.full_init a;
	random_initialized := true
      else
	begin
	  raise (Failure("Since /dev/random is not on your system (Windows?), you must give some random seed with -randomseed\nMake sure the seed is really random or serious problems could result.\n"))
	end
	  
let rand_bit () =
  if not !random_initialized then initialize_random_seed();
  Random.bool()

let rand_int32 () =
  if not !random_initialized then initialize_random_seed();
  Int32.logor (Int32.shift_left (Random.int32 65536l) 16) (Random.int32 65536l)

let rand_int64 () =
  if not !random_initialized then initialize_random_seed();
  Int64.logor (Int64.shift_left (Random.int64 4294967296L) 32) (Random.int64 4294967296L)

