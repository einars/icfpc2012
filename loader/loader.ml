(* vim: set tw=0 : *)
open Printf
open ExtLib

(* {{{ common utility stuff *)

let ($) a b = b a
let (&) a b = a b
let (<<) a b c = a (b c)
let (>>) a b c = b (a c)

let default_logger msg = try printf "%s\n%!" msg with _ -> ()
let log msg = kprintf default_logger msg
let failwithf msg = kprintf (fun s -> (log "%s" s; failwith s) ) msg

let lines_of_file = Std.input_list << open_in

type coords = int * int

module CoordMap = Map.Make(struct type t = coords let compare = compare end)


let (^+) c1 c2 =
  let x1, y1 = c1 and x2, y2 = c2
  in (x1 + x2), (y1 + y2)

let (^-) c1 c2 =
  let x1, y1 = c1 and x2, y2 = c2
  in (x1 - x2), (y1 - y2)

let string_of_coords c = sprintf "%d:%d" (fst c) (snd c)

let make_2d_array n m init =
  let res = Array.make n (Array.make m init) in
  for i = 1 to n - 1 do
    res.(i) <- Array.make m init
  done;
  res

(* }}} *)

type elem = Wall | Rock | Lift | Earth | Empty | Lambda | Robot

let initial_robot_coordinates = ref (0,0)

type field =
  { f: elem CoordMap.t
  ; dimensions: coords
  ; score: int
  ; lambdas: int
  ; lambdas_eaten: int
  ; robot: coords
  ; is_complete: bool
  ; moves_taken: string list


  ; water: int
  ; flooding: int
  ; waterproof: int

  ; cur_wetness: int
  ; cur_water: int

  }

let rec count_lambdas f =
  let n = ref 0 in
  CoordMap.iter (fun coords elem -> if elem = Lambda then n := !n + 1) f;
  !n


(* {{{ load/print field *)
let load_char = function
  | '#' -> Wall, false
  | '.' -> Earth, false
  | ' ' -> Empty, false
  | 'R' -> Empty, true
  | 'L' -> Lift, false
  | 'O' -> Lift, false
  | '0' -> Lambda, false
  | '*' -> Rock, false
  | '\\'-> Lambda, false
  | c -> failwithf "Unknown char %c" c

let char_of_elem ?(lambdas = 1) = function
  | Wall -> "▓"
  | Earth -> "·"
  | Empty -> " "
  | Lift -> if lambdas = 0 then "O" else "L"
  | Rock -> "*"
  | Lambda -> "λ"
  | Robot -> "E"


let load_line pos s accum =
  let rec loop ps inner_accum =
    let index = (fst ps) - 1 in
    if String.length s <= index then inner_accum
    else (
      let elem, robo = load_char & String.get s index
      and rest = loop (ps ^+ (1,0)) inner_accum in

      if robo then initial_robot_coordinates := ps;
      match elem with
      | Wall -> CoordMap.add ps Wall rest
      | Rock -> CoordMap.add ps Rock rest
      | Lift -> CoordMap.add ps Lift rest
      | Earth -> CoordMap.add ps Earth rest
      | Lambda -> CoordMap.add ps Lambda rest
      | Robot -> failwith "I certainly didn't expect Robot in response to load_char"
      | Empty -> rest
    )
  in
  loop pos accum

let get_dimensions f =
  CoordMap.fold (fun (x,y) elem (max_x, max_y) -> (max x max_x), (max y max_y)) f (0,0)

let invert_y dimensions pos =
  let _, h = dimensions and x, y = pos in
  x, h - y + 1

let parse_metadata lines =
  let rec loop water flooding waterproof = function
    | l :: rest -> let k, v = String.split (String.strip l) " " in
    if k = "Flooding" then loop water (String.to_int v) waterproof rest
    else if k = "Waterproof" then loop water flooding (String.to_int v) rest
    else if k = "Water"      then loop (String.to_int v) flooding water rest
    else failwithf "Unknown metadata key %s" k
  | _ -> water, flooding, waterproof
  in loop 0 0 10 lines

let split_lines lines =
  let rec loop is_metadata field metadata = function
  | "" :: t -> loop true field metadata t
  | l :: t -> loop is_metadata (if is_metadata then field else l::field) (if is_metadata then l::metadata else metadata) t
  | _ -> (List.rev field), metadata
  in loop false [] [] lines

let load_field lines =
  let rec loop pos = function
    | line::rest -> load_line pos line ( loop (pos ^+ (0,1) ) rest )
    | _ -> CoordMap.empty
  in
  let l_field, l_meta = split_lines lines in
  let loaded_field = loop (1, 1) l_field in
  let water, flooding, waterproof = parse_metadata l_meta in
  let dimensions = get_dimensions loaded_field in
  { f = CoordMap.fold
      (fun coords elem new_map -> CoordMap.add (invert_y dimensions coords) elem new_map)
      loaded_field
      CoordMap.empty
  ; is_complete = false
  ; score = 0
  ; lambdas = count_lambdas loaded_field
  ; lambdas_eaten = 0
  ; dimensions = dimensions
  ; robot = invert_y dimensions !initial_robot_coordinates
  ; moves_taken = []

  ; waterproof = waterproof
  ; flooding = flooding
  ; water = water
  ; cur_wetness = 0
  ; cur_water = 0
  }


let print_field f =
  (*
  log "Robot @ %s" & string_of_coords f.robot;
  log "Field dimensions: %s" & string_of_coords f.dimensions;
  *)
  let w, h = f.dimensions in
  let repr = make_2d_array (h + 1) (w + 1) " " in

  printf "\x1b[2J\x1b[;H";

  List.iter (fun c -> printf "%s" c) (List.rev f.moves_taken);
  log "\nscore=%d eaten=%d/%d\nwater=%d/%d wetness=%d/%d" f.score f.lambdas_eaten (f.lambdas_eaten + f.lambdas) f.water f.cur_water f.cur_wetness f.waterproof;

  let rx, ry = f.robot in repr.(ry).(rx) <- "R";


  CoordMap.iter (fun (x,y) elem -> repr.(y).(x) <- char_of_elem ~lambdas:f.lambdas elem) f.f;

  for j = 1 to h do
    printf "%s" (if (h - j + 1 >= f.water) then " " else "~");
    for k = 1 to w do
      printf "%s" repr.(h - j + 1).(k);
    done;
    printf "%s" (if (h - j + 1 >= f.water) then " " else "~");
    printf "\n%!";
  done;

  log ""

(* }}} *)


let peek_location ?(robot=(-1,-1)) f l =
  if robot = l then Some Robot else
    if CoordMap.mem l f.f then Some (CoordMap.find l f.f) else None

let string_of_elem_at f l =
  match peek_location f l with
  | Some l -> char_of_elem l
  | None -> " "


let rec is_empty field c =
  match peek_location field c with
  | None | Some Empty -> true
  | _ -> false

let rec is_walkable field direction c =
  match peek_location field c with
  | Some Wall  -> false
  | Some Lift  -> field.lambdas = 0
  | Some Rock  ->
      (* rocks can be pushed? *)
      if direction = "L" then
        is_empty field (c ^- (1, 0))
      else if direction = "R" then
        is_empty field (c ^+ (1, 0))
      else false
  | _ -> true


let update_robot coords = function
  | "A" -> coords
  | "L" -> coords ^+ (-1,0)
  | "R" -> coords ^+ (+1,0)
  | "U" -> coords ^+ (0,1)
  | "D" -> coords ^+ (0,-1)
  | "W" -> coords
  | s -> failwithf "Unknown robo-action %s" s

exception Bork_unwalkable
exception Bork_fallen_rock
exception Bork_finished
exception Bork_drowned

exception Bork_no_backtrack_hack

exception Finished of int

let exec_action field ?(backtrack=true) action =

  if field.is_complete then raise Bork_finished;

  (* if action = "A" then raise (Finished (field.score + 25 * field.lambdas_eaten)); *)
  if action = "A" then raise (Finished field.score);

  if not backtrack && field.moves_taken <> [] then (
    let last_action = List.hd field.moves_taken in
    let subsub = update_robot field.robot last_action in
    if last_action = "R" && action = "L" && is_walkable field "X" subsub then raise Bork_no_backtrack_hack;
    if last_action = "L" && action = "R" && is_walkable field "X" subsub then raise Bork_no_backtrack_hack;
    if last_action = "U" && action = "D" && is_walkable field "X" subsub then raise Bork_no_backtrack_hack;
    if last_action = "D" && action = "U" && is_walkable field "X" subsub then raise Bork_no_backtrack_hack;
  );

  let new_robo_coords = update_robot field.robot action in
  (* nevar paiet, stāvi uz vietas *)
  if not & is_walkable field action new_robo_coords then raise Bork_unwalkable;

  let peek = peek_location ~robot:new_robo_coords field in

  let score_diff, lambda_diff =
  match peek_location field new_robo_coords with
  | Some Earth -> -1, 0
  | Some Lambda -> 49, 1
  (* | Some Lift -> 50 * field.lambdas_eaten - 1, 0 (* handled via internal raise Finished *)*)
  | _ -> -1, 0
  in


  let rec update_location coords loc =
    if loc = Wall then coords, Some loc else
    if coords = new_robo_coords then (

      (* robots uzkāpj uz zemes, zeme pazūd: skipojam cellu *)
      if loc = Earth then coords, None
      else

      (* robots apēd lambdu *)
      if loc = Lambda then coords, None
      else

      (* robots iekāpj liftā *)
      if loc = Lift then
        (* XXX raise (Finished (field.score + 50 * field.lambdas_eaten - 1)) *)
        raise (Finished (field.score + 25 * field.lambdas_eaten - 1))
      else

      (* robots grūž akmeni *)
      if loc = Rock then
        let delta = (if action = "L" then (-1, 0) else (1, 0)) in
        update_location (coords ^+ delta) loc
      else (coords, Some loc)

    ) else (

      if loc = Rock then begin

        let c_bottom = coords ^+ (+0, -1)
        and c_left   = coords ^+ (-1, +0)
        and c_right  = coords ^+ (+1, +0)
        and c_bottom_left  = coords ^+ (-1, -1)
        and c_bottom_right = coords ^+ (+1, -1) in
        if peek c_bottom = None && c_bottom <> new_robo_coords then (
          (* akmens krīt lejā vertikāli *)
          if new_robo_coords = coords ^+ (+0, -2) then raise Bork_fallen_rock;
          (c_bottom, Some loc)
        ) else
        if peek c_bottom = Some Rock && peek c_right = None && peek c_bottom_right = None then (
            (* uz leju pa labi *)
            if new_robo_coords = coords ^+ (+1, -2) then raise Bork_fallen_rock;
            (c_bottom_right, Some loc)
        ) else
        if peek c_bottom = Some Lambda && peek c_right = None && peek c_bottom_right = None then (
            (* uz leju pa labi *)
            if new_robo_coords = coords ^+ (+1, -2) then raise Bork_fallen_rock;
            (c_bottom_right, Some loc)
        ) else
        if peek c_bottom = Some Rock && (peek c_left = None && peek c_bottom_left = None) && (peek c_right <> None || peek c_bottom_right <> None) then (
          if new_robo_coords = coords ^+ (-1, -2) then raise Bork_fallen_rock;
          (c_bottom_left, Some loc)
        ) else
          (coords, Some loc)
      end
        else
        (coords, Some loc)
    )
  in

  let nf = CoordMap.fold
    (fun coords elem new_map -> match update_location coords elem with
    | new_coords, Some elem -> CoordMap.add new_coords elem new_map
    | _, None -> new_map)
    field.f
    CoordMap.empty in

  let cur_water = if field.cur_water + 1 > field.flooding then 0 else field.cur_water + 1 in
  let water = if cur_water = 0 && field.flooding > 0 then field.water + 1 else field.water in
  let robo_y = snd new_robo_coords in
  let cur_wetness = if robo_y <= water then field.cur_wetness + 1 else 0 in
  if cur_wetness > field.waterproof then raise Bork_drowned;
  (* if cur_wetness > 0 then log "Achievement achieved: got wet %d" cur_wetness; *)

  { field with
    f = nf;
    score = field.score + score_diff;
    moves_taken = action :: field.moves_taken;
    lambdas = count_lambdas nf;
    lambdas_eaten = field.lambdas_eaten + lambda_diff;
    robot = new_robo_coords;

    cur_water = cur_water;
    cur_wetness = cur_wetness;
    water = water;
  }

let do_action field ?(backtrack=true) action = (
  try
    exec_action field ~backtrack:backtrack action
  with
    | Finished total_score-> { field with
        is_complete = true;
        score = total_score;
        moves_taken = action :: field.moves_taken;
    }
  )


let animate_solution field winning_moves =
  let rec loop cf = function
    | action::actions ->
        let nf = do_action cf action in
        ignore(Unix.select [] [] [] 0.2);
        print_field nf;
        loop nf actions
    | _ -> ()
  in
  List.rev winning_moves $ loop field

(* {{{ run_tests *)

let run_tests () =

  let waffle ?(print=true) cmds f =
    let grr = ref f in
    if print then print_field !grr;
    for i = 0 to pred & String.length cmds do
      grr := do_action !grr (String.get cmds i $ Std.string_of_char);
      if print then print_field !grr;
    done;
    !grr

  in

  let print = false
  and f = load_field
  [ "#**#L#"
  ; "#R* 0#"
  ; "# # ##"
  ; "######"
  ] in

  let walk_test = is_walkable f " "
  and peek_test = peek_location f in
  assert( not & walk_test (1,2) );
  assert(       walk_test (2,3) );
  assert( not & walk_test (3,4) );
  assert( peek_test (2,3) = None );
  assert( peek_test (1,4) = Some Wall );
  assert( peek_test (2,4) = Some Rock );

  if print then print_field f;

  let f = do_action f "W" in
  assert( f.moves_taken = ["W"] );
  assert( f.robot = (2,3) );

  if print then print_field f;

  let f = do_action f "R" in
  assert( f.moves_taken = ["R"; "W"] );
  assert( f.robot = (3,3) );

  if print then print_field f;

  let f = do_action f "R" in
  assert( f.robot = (4,3) );

  if print then print_field f;

  let f = do_action f "W" in
  assert( f.robot = (4,3) );

  if print then print_field f;

  let f = do_action f "L" in
  if print then print_field f;
  assert( f.robot = (3,3) );

  let f = do_action f "R" in
  if print then print_field f;
  let f = do_action f "R" in
  if print then print_field f;
  assert (f.score = 18);

  let f_lift = do_action f "U" in
  if print then print_field f_lift;
  assert (f_lift.score = 18 + 50 - 1);

  let f_abort = do_action f "A" in
  if print then print_field f_abort;
  assert (f_abort.score = 18 + 25);

  ignore(waffle ~print:false "WWWWW" &
  load_field
  [ "#    ****     R"
  ; "#    ****      "
  ; "#    ****      "
  ; "#    0000      "
  ; "###############"
  ]);

  let f = waffle ~print:false "LDRDDUULLLDDL" & load_field & lines_of_file "../maps/contest1.map" in
  assert (f.score = 212);
  let f = waffle ~print:false "RRUDRRULURULLLLDDDL" & load_field & lines_of_file "../maps/contest2.map" in
  assert (f.score = 281);
  let f = waffle ~print:false "LDDDRRRRDDLLLLLDURRRUURRR" & load_field & lines_of_file "../maps/contest3.map" in
  assert (f.score = 275);




  log "Tests passed"

(* }}} *)

let heuristics_score f = 1000 * f.score + List.length f.moves_taken
let heuristics_comparator a b = (-) (heuristics_score b) (heuristics_score a)

let best n fs = List.sort ~cmp:heuristics_comparator fs $ List.take n


let solve f =

  let h = (snd f.dimensions) and w = (fst f.dimensions) in

  let astar = make_2d_array (h + 1) (w + 1) [] in


  let tolerance = 15 in


  let rec loop_astar tries =
    let astar_put_score f =
      let x,y = f.robot in
      if astar.(y).(x) = [] || (List.fold_left (fun a b -> min a (heuristics_score b)) 999999 astar.(y).(x) < heuristics_score f) then (
        astar.(y).(x) <- best tolerance (f :: astar.(y).(x));
        1
      ) else (
        0
      )
    and allow_backtrack = true
    in
    match tries with
    | h::t -> if h.is_complete $ not then (
      (try ( astar_put_score & do_action h ~backtrack:allow_backtrack "U") with _ -> 0) +
      (try ( astar_put_score & do_action h ~backtrack:allow_backtrack "D") with _ -> 0) +
      (try ( astar_put_score & do_action h ~backtrack:allow_backtrack "L") with _ -> 0) +
      (try ( astar_put_score & do_action h ~backtrack:allow_backtrack "R") with _ -> 0) (* +
      (try ( astar_put_score & do_action f "A") with _ -> 0) *) + loop_astar t
    ) else loop_astar t
    | _ -> 0
  in

  let rec process_frontier () =
    printf ".%!";
    let had_modifications = ref 0 in
    for j = 1 to h do
      for k = 1 to w do
        match astar.(j).(k) with
        | [] -> ()
        | h::[] -> had_modifications := !had_modifications + (loop_astar [h])
        | fs ->
            had_modifications := !had_modifications + (List.sort ~cmp:heuristics_comparator fs $ loop_astar)
      done;
    done;
    if !had_modifications > 0 then process_frontier ()
  in


  Sys.set_signal Sys.sigint (Sys.Signal_handle (fun _ ->
    let maximum = ref f in

    for j = 1 to h do
      for k = 1 to w do
        match astar.(j).(k) with
        | [] -> ()
        | fs -> let f = List.hd & best 1 fs in
            if f.score > !maximum.score then maximum := f
      done;
    done;
    let solution = if !maximum.is_complete
    then !maximum.moves_taken
    else "A" :: !maximum.moves_taken in
    animate_solution f solution;
    exit 0;
  ));

  let x,y = f.robot in astar.(y).(x) <- [f];
  process_frontier ();

  Sys.set_signal Sys.sigint Sys.Signal_default;

  let maximum = ref f in

  for j = 1 to h do
    for k = 1 to w do
      match astar.(j).(k) with
      | [] -> ()
      | fs -> let f = List.hd & best 1 fs in
          if f.score > !maximum.score then maximum := f
    done
  done;
  if !maximum.is_complete
  then !maximum.moves_taken
  else "A" :: !maximum.moves_taken



let _ =

  (* run_tests (); *)

  if Array.length Sys.argv = 1 then begin

    ();

  end else if Array.length Sys.argv = 2 then begin

    let f = load_field & lines_of_file & Sys.argv.(1) in
    (* solve f $ String.join "" $  log "%s"; *)
    solve f $ animate_solution f;

  end else begin

    let f = load_field & lines_of_file & Sys.argv.(1) in
    String.explode Sys.argv.(2) $ List.map String.of_char $ List.rev $ animate_solution f;

  end;


  ()
