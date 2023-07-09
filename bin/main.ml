open Printf
open Unix


let get_first_arg () =
    try Sys.argv.(1)
    with Invalid_argument _ -> ""

let get_display_time () =
    input_line (Unix.open_process_in "tmux show-options -g | grep display-time | cut -d ' ' -f 2")

let get_time seconds =
  let hours = seconds / 3600 in
  let minutes = (seconds mod 3600) / 60 in
  let remaining_seconds = seconds mod 60 in
  let format_number n =
    if n < 10 then "0" ^ string_of_int n else string_of_int n
  in
  if hours > 0 then 
      format_number hours ^ ":" ^ format_number minutes ^ ":" ^ format_number remaining_seconds
  else 
      format_number minutes ^ ":" ^ format_number remaining_seconds

let validate_arg arg =
    let stripped_arg = String.trim arg in
    let arg_len = String.length stripped_arg in
    if arg_len = 0 then
    if arg_len = 0 then begin
        eprintf "arg_len is 0\n";
        exit 1
    end else
        ()

let rec pomodoro_start minutes remaining_seconds break =
    let _ = Sys.command "clear" in
    if remaining_seconds <= 0 then begin
        let _ = Sys.command "tmux set-option -g display-time 5000" in
        if break then begin
            let _  = Sys.command "tmux display-message \"Break is over\"" in
            print_endline "----------------------------------";
            print_endline "Break is over";
            print_endline "----------------------------------";
            let _ = Sys.command ("tmux set-option -g display-time " ^ get_display_time ()) in
            pomodoro_start minutes (minutes * 60) false
        end else begin
            let _  = Sys.command "tmux display-message \"Start your break\"" in
            print_endline "----------------------------------";
            print_endline "Start your break";
            print_endline "----------------------------------";
            let _ = Sys.command ("tmux set-option -g display-time " ^ get_display_time ()) in
            pomodoro_start minutes (minutes * 60) true
        end
    end else begin
        print_endline (get_time remaining_seconds);
        sleep 1;
        pomodoro_start minutes (remaining_seconds - 1) break
    end

let pomodoro time = 
    print_endline "Starting pomodoro";
    ignore (pomodoro_start time (time * 60) false);
    ()


let check_arg arg time = 
    match arg with
    | "start" -> pomodoro time
    | _ -> begin
        eprintf "Invalid argument\n"; 
        exit 1
    end

let () = 
    let arg = get_first_arg () in
    validate_arg arg;
    check_arg arg 25
