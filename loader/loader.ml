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

  ; mutable solver_touched: bool

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
  | Lift -> if lambdas = 0 then "@" else "#"
  | Rock -> "O"
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
    if k = "Flooding"        then loop water (String.to_int v) waterproof rest
    else if k = "Waterproof" then loop water flooding (String.to_int v) rest
    else if k = "Water"      then loop (String.to_int v) flooding waterproof rest
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
  ; solver_touched = false
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
  log "\nscore=%d eaten=%d/%d\nwater=%d/%d flooding=%d wetness=%d/%d" f.score f.lambdas_eaten (f.lambdas_eaten + f.lambdas) f.water f.cur_water f.flooding f.cur_wetness f.waterproof;

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

exception Finished of int

let exec_action field ?(backtrack=true) ?(allow_unwalkable=false) action =

  if field.is_complete then raise Bork_finished;

  if action = "A" then raise (Finished field.score);

  let new_robo_coords = update_robot field.robot action in
  (* nevar paiet, stāvi uz vietas *)
  let new_robo_coords =
  if not & is_walkable field action new_robo_coords then (
    if allow_unwalkable then field.robot else raise Bork_unwalkable;
  ) else new_robo_coords in

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
  if cur_wetness > field.waterproof then (
    raise Bork_drowned;
  );
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

let do_action field ?(backtrack=true) ?(allow_unwalkable=false) action = (
  try
    exec_action field ~backtrack:backtrack ~allow_unwalkable:allow_unwalkable action
  with
    | Finished total_score-> { field with
        is_complete = true;
        score = total_score;
        moves_taken = action :: field.moves_taken;
    }
  )

let animate_solution field winning_moves =
  let exploded_moves = String.explode winning_moves $ List.map String.of_char $ List.rev in
  List.fold_right
      (fun action field ->
        let nf = do_action field ~allow_unwalkable:true action in
        ignore(Unix.select [] [] [] 0.1);
        print_field nf;
        nf
      ) exploded_moves field $ ignore

let apply_solution field some_moves =
  let exploded_moves = String.explode some_moves $ List.map String.of_char $ List.rev in
  List.fold_right (fun action field -> do_action ~allow_unwalkable:true field action) exploded_moves field

let manhattan_distance (ax, ay) (bx, by) = (1 + bx - ax $ abs) + (1 + by - ay $ abs)

let get_rocks f =
  let lambda_map = CoordMap.filter (fun k v -> v = Rock) f.f in
  List.map (fun (k,v) -> k) (CoordMap.bindings lambda_map)

let get_lambdas f =
  let lambda_map = CoordMap.filter (fun k v -> v = Lambda || v = Lift) f.f in
  List.map (fun (k,v) -> k) (CoordMap.bindings lambda_map)

let rockability_score f =
  let height = snd f.dimensions in
  List.fold_left (fun sum (rock_x, rock_y) -> sum - rock_y * height) 0 (get_rocks f)

let lambda_manhattan_score f =
  + (rockability_score f) / 4
  - 25 * f.lambdas * (List.fold_left (fun sum lambda_coords -> sum + (manhattan_distance lambda_coords f.robot)) 0  (get_lambdas f))
  + 3 * f.score

let scorer = lambda_manhattan_score

let proper_solution_from_move_list move_list =
  String.join "" (List.rev move_list)


let solve ?(quiet=false) ?(use_signals=true) f =

  let h = (snd f.dimensions) and w = (fst f.dimensions) in

  let astar = make_2d_array (h + 1) (w + 1) [] in

  let rec process_astar ff =
    let astar_put_score f =
      let x,y = f.robot in let best = astar.(y).(x) in
      if best = [] || (scorer (List.hd best) < (scorer f)) then (
        astar.(y).(x) <- [ { f with solver_touched = false } ];
        1
      ) else (
        0
      )
    in
    if ff.solver_touched $ not then (
      let res =
      (try ( astar_put_score & do_action ff "U") with _ -> 0) +
      (try ( astar_put_score & do_action ff "D") with _ -> 0) +
      (try ( astar_put_score & do_action ff "L") with _ -> 0) +
      (try ( astar_put_score & do_action ff "R") with _ -> 0)
      in
      ff.solver_touched <- true;
      res
    ) else 0
  in

  let rec process_frontier () =
    if not quiet then printf ".%!";
    let had_modifications = ref 0 in
    for j = 1 to h do
      for k = 1 to w do
        match astar.(j).(k) with
        | [] -> ()
        | h::[] -> had_modifications := !had_modifications + process_astar h;
        | _ -> failwithf "astar coma"
      done;
    done;
    if !had_modifications > 0 then process_frontier ()
  in

  if use_signals then Sys.set_signal Sys.sigint (Sys.Signal_handle (fun _ ->
      Sys.set_signal Sys.sigint Sys.Signal_default;
      let maximum = ref f in

      for j = 1 to h do
        for k = 1 to w do
          match astar.(j).(k) with
          | [] -> ()
          | f::t -> if f.score > !maximum.score then maximum := f
        done;
      done;
      let solution = if !maximum.is_complete
      then !maximum.moves_taken
      else "A" :: !maximum.moves_taken in
      animate_solution f (proper_solution_from_move_list solution);
      exit 0;
  ));

  let x,y = f.robot in astar.(y).(x) <- [f];
  process_frontier ();

  if use_signals then Sys.set_signal Sys.sigint Sys.Signal_default;

  let maximum = ref f in

  for j = 1 to h do
    for k = 1 to w do
      match astar.(j).(k) with
      | [] -> ()
      | f::t -> if f.score > !maximum.score then maximum := f
    done
  done;
  let solution =
    if !maximum.is_complete
    then !maximum.moves_taken
    else "A" :: !maximum.moves_taken
  in
  (proper_solution_from_move_list solution)


(* {{{ torture / scoring *)

let torture_chambers =
  [ "/home/w/projekti/icfp12/maps/contest1.map", 212
  ; "/home/w/projekti/icfp12/maps/contest2.map", 281
  ; "/home/w/projekti/icfp12/maps/contest3.map", 275
  ; "/home/w/projekti/icfp12/maps/contest4.map", 575
  ; "/home/w/projekti/icfp12/maps/contest5.map", 1303
  ; "/home/w/projekti/icfp12/maps/contest6.map", 1177
  ; "/home/w/projekti/icfp12/maps/contest7.map", 869
  ; "/home/w/projekti/icfp12/maps/contest8.map", 1973
  ; "/home/w/projekti/icfp12/maps/contest9.map", 3093
  ; "/home/w/projekti/icfp12/maps/contest10.map", 3634
  ; "/home/w/projekti/icfp12/maps/flood1.map", 945
  ; "/home/w/projekti/icfp12/maps/flood2.map", 281
  ; "/home/w/projekti/icfp12/maps/flood3.map", 1303
  ; "/home/w/projekti/icfp12/maps/flood4.map", 1592
  ; "/home/w/projekti/icfp12/maps/flood5.map", 575
  ]

let torture torture_fn =
  let tortured_total, best_possible_total =
  List.fold_left (fun  (tortured_total, best_possible_total) (map, best_possible_score) ->
    let field = load_field & lines_of_file & map
    and solution = torture_fn map in
    let solved_field = apply_solution field solution in
    log "%s: %3d%% %4d of %4d" map (solved_field.score * 100 / best_possible_score) solved_field.score best_possible_score;
    (tortured_total + solved_field.score, best_possible_total + best_possible_score)
  ) (0,0) torture_chambers in
  log "TOTAL: %4d of %d, %d%%" tortured_total best_possible_total (tortured_total * 100 / best_possible_total)



let self_torture map_file =
  solve ~quiet:true ~use_signals:false & load_field & lines_of_file & map_file

let alien_torture executable map_file =
  let syscall cmd =
    let ic, oc = Unix.open_process cmd in
    let buf = Buffer.create 16 in
    (try
      while true do
        Buffer.add_channel buf ic 1
      done
    with End_of_file -> ());
    let _ = Unix.close_process (ic, oc) in
    (Buffer.contents buf)
in
    sprintf "%s %s" executable map_file $ syscall $ String.strip


(* }}} *)


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
  let f = waffle ~print:false "LDRDDUULLLDDL" & load_field & lines_of_file "../maps/contest1.map" in
  assert (f.score = 212);
  let f = waffle ~print:false "RRUDRRULURULLLLDDDL" & load_field & lines_of_file "../maps/contest2.map" in
  assert (f.score = 281);
  let f = waffle ~print:false "LDDDRRRRDDLLLLLDURRRUURRR" & load_field & lines_of_file "../maps/contest3.map" in
  assert (f.score = 275);

  let n = proper_solution_from_move_list ["A"; "B"; "C"] in
  assert (n = "CBA");

  log "Tests passed"

(* }}} *)

let _ =

  run_tests ();

  if Array.length Sys.argv = 1 then begin

    ();

  end else if Array.length Sys.argv = 2 && Sys.argv.(1) = "torture" then begin

    self_torture $ torture;

  end else if Array.length Sys.argv = 3 && Sys.argv.(1) = "torture" then begin

    alien_torture Sys.argv.(2) $ torture

  end else if Array.length Sys.argv = 2 then begin

    let f = load_field & lines_of_file & Sys.argv.(1) in
    (* solve f $ String.join "" $  log "%s"; *)
    solve f $ animate_solution f;

  end else begin

    let f = load_field & lines_of_file Sys.argv.(1) in
    animate_solution f Sys.argv.(2)

  end;


  ()
