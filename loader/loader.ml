(* vim: set tw=0 : *)
(*
  * ICFP-2012 robot solver by Raging Mushrooms Team
  * Written by Einar Lielmanis
  *
  *
  * Usage:
  *
  * Contest mode:
  * ./loader <../maps/some.map
  *
  * Solve map and animate solution:
  * ./loader ../maps/some.map
  *
  * Manual mode, animate given solution:
  * ./loader ../maps/some.map UDUDULRLR
  *
  * To turn off animation, set QUIET environment variable,
  * e.g QUIET=1 ./loader ../maps/some.map
  *
  *)

open Printf
open ExtLib

(* {{{ common utility stuff *)

let ($) a b = b a
let (&) a b = a b
let (<<) a b c = a (b c)

let default_logger msg = try printf "%s\n%!" msg with _ -> ()
let log msg = kprintf default_logger msg
let failwithf msg = kprintf (fun s -> (log "%s" s; failwith s) ) msg

let lines_of_file = Std.input_list << open_in

type coords = int * int

module CoordMap = Map.Make(struct type t = coords let compare = compare end)

let rstrip ?(chars=" \t\r\n") s =
  let p = 0
  and l = ref ( (String.length s) - 1) in
  while !l >= p && String.contains chars (String.unsafe_get s !l) do
    decr l;
  done;
  String.sub s p (!l - p + 1)


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

type elem = Wall | Rock | Grimrock | Lift | Earth | Empty | Lambda | Beard of int | Razor | Target of int | Trampoline of int | Robot | SysTrampoline of string

let initial_robot_coordinates = ref (0,0)

type field =
  { f: elem CoordMap.t
  ; dimensions: coords
  ; score: int
  ; total_lambdas: int
  ; lambdas_eaten: int
  ; robot: coords
  ; is_complete: bool
  ; moves_taken: string

  ; water: int
  ; flooding: int
  ; waterproof: int

  ; cur_wetness: int
  ; cur_water: int

  ; growth: int
  ; razors: int

  ; path_excitement: int

  }

let rec count_lambdas f =
  let n = ref 0 in
  CoordMap.iter (fun coords elem -> if elem = Lambda || elem = Grimrock then n := !n + 1) f;
  !n


(* {{{ load/print field *)
let load_char = function
  | '#' -> Wall
  | '.' -> Earth
  | ' ' -> Empty
  | 'R' -> Robot
  | 'L' -> Lift
  | 'O' -> Lift
  | '0' -> Lambda
  | '*' -> Rock
  | '\\'-> Lambda
  | 'W'-> Beard 0
  | '!'-> Razor
  | 'A'-> SysTrampoline "A"
  | 'B'-> SysTrampoline "B"
  | 'C'-> SysTrampoline "C"
  | 'D'-> SysTrampoline "D"
  | 'E'-> SysTrampoline "E"
  | 'F'-> SysTrampoline "F"
  | 'G'-> SysTrampoline "G"
  | 'H'-> SysTrampoline "H"
  | 'I'-> SysTrampoline "I"
  | '1'-> Target 1
  | '2'-> Target 2
  | '3'-> Target 3
  | '4'-> Target 4
  | '5'-> Target 5
  | '6'-> Target 6
  | '7'-> Target 7
  | '8'-> Target 8
  | '9'-> Target 9
  | '@' -> Grimrock
  | c -> log "Unknown char %c, treating as a wall" c; Wall

let char_of_elem ?(exit_open = false) = function
  | Wall -> "▓"
  | Earth -> "·"
  | Empty -> " "
  | Lift -> if exit_open then "@" else "#"
  | Rock -> "O"
  | Grimrock -> "@"
  | Lambda -> "λ"
  | Beard _ -> "W"
  | Razor _ -> "!"
  | Target _ -> "t"
  | Trampoline _ -> "T"
  | SysTrampoline _ -> failwith "char_of_elem certainly didn't expect any SysTrampoline"
  | Robot -> failwith "char_of_elem certainly didn't expect any Robot"

let rec decode_trammap s = function
  | h::t -> if fst h = s then snd h else decode_trammap s t
  | _ -> failwithf "Unknown trampoline %s" s

let load_line ~trammap pos s accum =
  let rec loop ps inner_accum =
    let index = (fst ps) - 1 in
    if String.length s <= index then inner_accum
    else (
      let elem = load_char & String.get s index
      and rest = loop (ps ^+ (1,0)) inner_accum in

      match elem with
      | Wall -> CoordMap.add ps Wall rest
      | Razor -> CoordMap.add ps Razor rest
      | Rock -> CoordMap.add ps Rock rest
      | Lift -> CoordMap.add ps Lift rest
      | Earth -> CoordMap.add ps Earth rest
      | Lambda -> CoordMap.add ps Lambda rest
      | Beard _ -> CoordMap.add ps (Beard 0) rest
      | Target n -> CoordMap.add ps (Target n) rest
      | Grimrock -> CoordMap.add ps Grimrock rest
      | SysTrampoline s -> CoordMap.add ps (Trampoline (decode_trammap s trammap)) rest
      | Empty -> rest
      | Robot -> initial_robot_coordinates := ps; rest
      | Trampoline _ -> failwith "I certainly didn't expect an usual Trampoline in response to load_char"
    )
  in
  loop pos accum

let get_dimensions f =
  CoordMap.fold (fun (x,y) elem (max_x, max_y) -> (max x max_x), (max y max_y)) f (0,0)

let invert_y dimensions pos =
  let _, h = dimensions and x, y = pos in
  x, h - y + 1

let make_trammap s =
  let tramp_def, ss = String.split s " " in
  let _, target = String.split ss " " in
  tramp_def, (String.to_int target)

let parse_metadata lines =


  let rec loop water flooding waterproof growth razors trammap = function
  | l :: rest -> let k, v = String.split (String.strip l) " " in
         if k = "Water"      then loop (String.to_int v)  flooding waterproof growth razors trammap rest
    else if k = "Flooding"   then loop water (String.to_int v)     waterproof growth razors trammap rest
    else if k = "Waterproof" then loop water flooding (String.to_int v)       growth razors trammap rest
    else if k = "Growth"     then loop water flooding waterproof (String.to_int v)   razors trammap rest
    else if k = "Razors"     then loop water flooding waterproof growth (String.to_int v)   trammap rest
    else if k = "Trampoline" then loop water flooding waterproof growth razors  ((make_trammap v) :: trammap) rest
    else ( log "Unknown metadata key %s" k; loop water flooding waterproof growth razors trammap rest );
  | _ -> water, flooding, waterproof, growth, razors, trammap
  in loop 0 0 10 25 0 [] lines

let split_lines_to_field_and_metadata lines =
  let rec loop is_metadata field metadata = function
  | "" :: t -> loop true field metadata t
  | l :: t -> loop is_metadata (if is_metadata then field else l::field) (if is_metadata then l::metadata else metadata) t
  | _ -> (List.rev field), metadata
  in loop false [] [] lines

let load_field lines =
  let rec loop ~trammap pos = function
    | line::rest -> load_line ~trammap:trammap pos (rstrip line) ( loop ~trammap:trammap (pos ^+ (0,1) ) rest )
    | _ -> CoordMap.empty
  in
  let l_field, l_meta = split_lines_to_field_and_metadata lines in
  let water, flooding, waterproof, growth, razors, trammap = parse_metadata l_meta in
  let loaded_field = loop ~trammap:trammap (1, 1) l_field in
  let dimensions = get_dimensions loaded_field in

  { f = CoordMap.fold
      (fun coords elem new_map -> CoordMap.add (invert_y dimensions coords) elem new_map)
      loaded_field
      CoordMap.empty
  ; is_complete = false
  ; score = 0
  ; total_lambdas = count_lambdas loaded_field
  ; lambdas_eaten = 0
  ; dimensions = dimensions
  ; robot = invert_y dimensions !initial_robot_coordinates
  ; moves_taken = ""

  ; waterproof = waterproof
  ; flooding = flooding
  ; water = water
  ; growth = growth
  ; razors = razors

  ; cur_wetness = 0
  ; cur_water = 0

  ; path_excitement = 0
  }


let print_field ?(clear=true) ?(heuristics=0) f =
  let w, h = f.dimensions in
  let repr = make_2d_array (h + 1) (w + 1) " " in

  if clear then printf "\x1b[2J\x1b[;H";

  log "";

  let rx, ry = f.robot in repr.(ry).(rx) <- "R";


  CoordMap.iter (fun (x,y) elem -> repr.(y).(x) <- char_of_elem ~exit_open:(f.total_lambdas = f.lambdas_eaten) elem) f.f;

  for j = 1 to h do
    printf "%s" (if (h - j >= f.water) then " " else "~");
    for k = 1 to w do
      printf "%s" repr.(h - j + 1).(k);
    done;
    printf "%s" (if (h - j >= f.water) then " " else "~");
    printf "\n%!";
  done;

  log "";
  log "\nscore=%d eaten=%d/%d razors=%d" f.score f.lambdas_eaten f.total_lambdas f.razors;
  log "water=%d/%d flooding=%d wetness=%d/%d growth=%d" f.water f.cur_water f.flooding f.cur_wetness f.waterproof f.growth;
  if heuristics <> 0 then log "Heuristics = %d" heuristics;
  log "%s" f.moves_taken;
  ()

(* }}} *)


let peek_map ?(robot=(-1,-1)) f l =
  if robot = l then Robot
  else try CoordMap.find l f with _ -> Empty

let is_blocked field c =
  let cx, cy = c
  and fw, fh = field.dimensions in
  if cx < 1 || cy < 1 || cx > fw || cy > fh then true
  else
  match peek_map field.f c with
  | Wall  -> true
  | Beard _ -> false (* sic! not tragic, maybe we'll get a razor *)
  | Razor -> false
  | Lambda -> false
  | Empty -> false
  | Earth -> false
  | Target _ -> false
  | Trampoline _ -> false
  | Lift  -> false
  | Rock  -> true
  | Grimrock  -> true
  | Robot -> failwith "is_blocked got some Robot, that should not happen"
  | SysTrampoline _ -> failwith "is_blocked got some Robot, that should not happen"


let turns_grimrock_to_lambda map c =
  match peek_map map c with
  | Wall  -> true
  | Beard _ -> true
  | Razor -> true
  | Lambda -> true
  | Empty -> false
  | Earth -> true
  | Target _ -> true
  | Trampoline _ -> true
  | Lift  -> true
  | Rock  -> true
  | Grimrock  -> true
  | Robot -> failwith "turns_grimrock_to_lambda got some Robot, that should not happen"
  | SysTrampoline _ -> failwith "turns_grimrock_to_lambda got some SysTrampoline, that should not happen"


let update_robot coords = function
  | "A" -> coords
  | "L" -> coords ^+ (-1,0)
  | "R" -> coords ^+ (+1,0)
  | "U" -> coords ^+ (0,1)
  | "D" -> coords ^+ (0,-1)
  | "W" -> coords
  | "S" -> coords
  | s -> failwithf "Unknown robo-action %s" s

exception Bork_unwalkable
exception Bork_fallen_rock
exception Bork_finished
exception Bork_drowned
exception Bork_worthless
exception Bork_impossible

exception Finished of int
exception CoordsFound of coords


let shave_map_at (sx, sy) map =
  let shaven_map = CoordMap.filter (fun (cx, cy) e -> match e with Beard n -> if (cx - sx $ abs) < 2 && (cy - sy $ abs) < 2 then false else true | _ -> true ) map in
  if CoordMap.cardinal shaven_map = CoordMap.cardinal map then raise Bork_worthless;
  shaven_map


let exec_action field action =

  if field.is_complete then raise Bork_finished;

  if action = "A" then raise (Finished field.score);
  if action = "S" && field.razors = 0 then raise Bork_impossible;

  let new_robo_coords = update_robot field.robot action in

  let score_diff, lambda_diff =
  match peek_map field.f new_robo_coords with
  | Earth -> -1, 0
  | Lambda -> 49, 1
  | _ -> -1, 0
  in

  let get_coords_of_target target map = (
    try
      CoordMap.iter (fun k e -> match e with | Target t -> if t = target then raise (CoordsFound k) | _ -> ()) map;
      failwithf "Target %d not found on map" target
    with CoordsFound c -> c
  ) in



  let map_with_targets_removed target_to_remove map =
    CoordMap.filter (fun k e -> match e with
    | Trampoline t | Target t -> t <> target_to_remove | _ -> true) map
  in

  let excitement_empty = 0
  and excitement_earth = 1
  and excitement_rock = 5
  and excitement_lambda = 100
  and excitement_razor = 10
  and excitement_trampoline = 1
  and excitement_grimrock = 1
  and excitement_shave = 5
  in

  (* do robot action *)
  let do_robo_stuff map = match peek_map map new_robo_coords with
  | Wall -> raise Bork_unwalkable
  | Beard _ -> raise Bork_unwalkable
  | Target _ -> raise Bork_unwalkable
  | Trampoline t -> (
    (get_coords_of_target t field.f), (map_with_targets_removed t field.f), excitement_trampoline
  )
  | Empty ->
      if action = "S"
      then new_robo_coords, (shave_map_at new_robo_coords map), excitement_shave
      else new_robo_coords, map, excitement_empty
  | Razor -> new_robo_coords, CoordMap.remove new_robo_coords map, excitement_razor
  | Earth -> new_robo_coords, CoordMap.remove new_robo_coords map, excitement_earth
  | Lambda -> new_robo_coords, CoordMap.remove new_robo_coords map, excitement_lambda
  | Lift -> if field.total_lambdas = field.lambdas_eaten then raise (Finished (field.score + 25 * field.lambdas_eaten - 1)) else raise Bork_unwalkable
  | Rock ->
        let rock_moved_to = (if action = "L" then new_robo_coords ^+ (-1, 0) else if action = "R" then new_robo_coords ^+ (1, 0) else raise Bork_unwalkable) in
        if not & CoordMap.mem rock_moved_to map then (
          new_robo_coords, (CoordMap.add rock_moved_to Rock (CoordMap.remove new_robo_coords map)), excitement_rock
        ) else (
          raise Bork_unwalkable
        )
  | Grimrock ->
        let rock_moved_to = (if action = "L" then new_robo_coords ^+ (-1, 0) else if action = "R" then new_robo_coords ^+ (1, 0) else raise Bork_unwalkable) in
        if not & CoordMap.mem rock_moved_to map then (
          new_robo_coords, (CoordMap.add rock_moved_to Grimrock (CoordMap.remove new_robo_coords map)), excitement_grimrock
        ) else (
          raise Bork_unwalkable
        )
  | Robot -> failwith "Robot in do_robo_stuff"
  | SysTrampoline _ -> failwith "SysTrampoline in do_robo_stuff"

  in

  let new_robo_coords, map_after_robot_movement, excitement_score = do_robo_stuff field.f in
  let peek = peek_map ~robot:new_robo_coords map_after_robot_movement in

  let rec transform_location coords loc map =
    match loc with
    | Rock | Grimrock -> begin
      let c_bottom = coords ^+ (+0, -1)
      and c_left   = coords ^+ (-1, +0)
      and c_right  = coords ^+ (+1, +0)
      and c_bottom_left  = coords ^+ (-1, -1)
      and c_bottom_right = coords ^+ (+1, -1) in
      let p_bottom = peek c_bottom in
      if p_bottom = Empty then (
        (* stone falling down *)
        if new_robo_coords = coords ^+ (+0, -2) then raise Bork_fallen_rock;
        if loc = Grimrock && turns_grimrock_to_lambda map (coords ^+ (+0, -2)) then
          CoordMap.add c_bottom Lambda (CoordMap.remove coords map)
        else
          CoordMap.add c_bottom loc (CoordMap.remove coords map);
      ) else
      if (p_bottom = Rock || p_bottom = Grimrock) && peek c_right = Empty && peek c_bottom_right = Empty then (
          (* down right, rock on rock *)
          if new_robo_coords = coords ^+ (+1, -2) then raise Bork_fallen_rock;
          if loc = Grimrock && turns_grimrock_to_lambda map (coords ^+ (+1, -2))
            then CoordMap.add c_bottom_right Lambda (CoordMap.remove coords map)
            else CoordMap.add c_bottom_right loc (CoordMap.remove coords map)
      ) else
      if p_bottom = Lambda && peek c_right = Empty && peek c_bottom_right = Empty then (
          (* down right, rock on lambda *)
          if new_robo_coords = coords ^+ (+1, -2) then raise Bork_fallen_rock;
          if loc = Grimrock && turns_grimrock_to_lambda map (coords ^+ (+1, -2))
            then CoordMap.add c_bottom_right Lambda (CoordMap.remove coords map)
            else CoordMap.add c_bottom_right loc (CoordMap.remove coords map)
      ) else
      if (p_bottom = Rock || p_bottom = Grimrock) && (peek c_left = Empty && peek c_bottom_left = Empty) && (peek c_right <> Empty || peek c_bottom_right <> Empty) then (
        (* down left *)
        if new_robo_coords = coords ^+ (-1, -2) then raise Bork_fallen_rock;
        if loc = Grimrock && turns_grimrock_to_lambda map (coords ^+ (-1, -2))
          then CoordMap.add c_bottom_left Lambda (CoordMap.remove coords map)
          else CoordMap.add c_bottom_left loc (CoordMap.remove coords map)
      ) else
      map
    end
    | Beard n -> begin
      let m = CoordMap.remove coords map in
      if n = field.growth - 1 then (
        let m = (let c = (-1, -1) ^+ coords in if (not & CoordMap.mem c map) && c <> new_robo_coords then CoordMap.add c (Beard 0) m else m) in
        let m = (let c = (+0, -1) ^+ coords in if (not & CoordMap.mem c map) && c <> new_robo_coords then CoordMap.add c (Beard 0) m else m) in
        let m = (let c = (+1, -1) ^+ coords in if (not & CoordMap.mem c map) && c <> new_robo_coords then CoordMap.add c (Beard 0) m else m) in
        let m = (let c = (-1, +0) ^+ coords in if (not & CoordMap.mem c map) && c <> new_robo_coords then CoordMap.add c (Beard 0) m else m) in

        let m = (let c = (+1, +0) ^+ coords in if (not & CoordMap.mem c map) && c <> new_robo_coords then CoordMap.add c (Beard 0) m else m) in
        let m = (let c = (-1, +1) ^+ coords in if (not & CoordMap.mem c map) && c <> new_robo_coords then CoordMap.add c (Beard 0) m else m) in
        let m = (let c = (+0, +1) ^+ coords in if (not & CoordMap.mem c map) && c <> new_robo_coords then CoordMap.add c (Beard 0) m else m) in
        let m = (let c = (+1, +1) ^+ coords in if (not & CoordMap.mem c map) && c <> new_robo_coords then CoordMap.add c (Beard 0) m else m) in
        CoordMap.add coords (Beard 0) m
      ) else
        CoordMap.add coords (Beard (n + 1)) m
    end
    | _ -> map
  in

  let new_razors = field.razors
    + if peek_map field.f new_robo_coords = Razor then 1 else 0
    + if action = "S" then -1 else 0
  in


  let cur_water = if field.cur_water + 1 >= field.flooding then 0 else field.cur_water + 1 in
  let water = if cur_water = 0 && field.flooding > 0 then field.water + 1 else field.water in
  let robo_y = snd new_robo_coords in
  let cur_wetness = if robo_y <= water then field.cur_wetness + 1 else 0 in
  if cur_wetness > field.waterproof then raise Bork_drowned;

  { field with
    f             = CoordMap.fold transform_location map_after_robot_movement map_after_robot_movement;
    robot         = new_robo_coords;
    score         = field.score + score_diff;
    moves_taken   = field.moves_taken ^ action;
    lambdas_eaten = field.lambdas_eaten + lambda_diff;

    cur_water   = cur_water;
    cur_wetness = cur_wetness;
    water       = water;
    razors      = new_razors;

    path_excitement = field.path_excitement + excitement_score;
  }

let do_action field action = (
  try
    exec_action field action
  with
    | Finished total_score-> { field with
        is_complete = true;
        score = total_score;
        moves_taken = field.moves_taken ^ action;
    }
  )


(* {{{ Heuristics *)

let manhattan_distance (ax, ay) (bx, by) = (1 + bx - ax $ abs) + (1 + by - ay $ abs)

let get_lambdas f =
  let lambda_map = CoordMap.filter (fun k v -> v = Lambda || v = Lift) f.f in
  List.map (fun (k,v) -> k) (CoordMap.bindings lambda_map)

let n_beards f =
  CoordMap.cardinal & CoordMap.filter (fun k v -> match v with Beard _ -> true | _ -> false) f.f

let n_earth f =
  let n_earth = ref 0 in
  CoordMap.iter (fun _ e -> if e = Earth then n_earth := !n_earth + 1) f.f;
  !n_earth

let lift_coords f =
  let coords = ref (0,0) in
  CoordMap.iter (fun c e -> if e = Lift then coords := c) f.f;
  !coords

let heuristic_score f =
  let lift = lift_coords f in
  let lift_is_stoned =
    (is_blocked f (lift ^+ (1,0))) &&
    (is_blocked f (lift ^- (1,0))) &&
    (is_blocked f (lift ^+ (0,1))) &&
    (is_blocked f (lift ^- (0,1))) in

  - 25 * (f.total_lambdas - f.lambdas_eaten) * (List.fold_left (fun sum lambda_coords -> sum + (manhattan_distance lambda_coords f.robot)) 0  (get_lambdas f))
  + f.path_excitement
  + if lift_is_stoned then (-9000) else 0
  + (if f.is_complete then 10000000 else 0)
  + (if f.total_lambdas = f.lambdas_eaten && (not lift_is_stoned) then 1000000 else 0)
  - 5 * (if f.total_lambdas = f.lambdas_eaten && (not lift_is_stoned) then manhattan_distance lift f.robot else 0)
  - (n_beards f) * 5
  (*
  - 50 * (if f.total_lambdas = f.lambdas_eaten then manhattan_distance (lift_coords f) f.robot else 0)
  *)
  (*
  - 25 * (f.total_lambdas - f.lambdas_eaten) * (List.fold_left (fun sum lambda_coords -> sum + (manhattan_distance lambda_coords f.robot)) 0  (get_lambdas f))
  - (n_beards f) * 5
  + f.razors * 10
  - (n_earth f) * 5
  + f.score
  *)

(* }}} *)

(* {{{ animate_solution *)
let animate_solution field winning_moves =
  ignore & String.explode winning_moves $ List.map String.of_char $
  List.fold_left
      (fun field action ->
        let nf = do_action field action in
        ignore(Unix.select [] [] [] 0.05);
        print_field ~heuristics:(heuristic_score nf) nf;
        nf
      ) field

let apply_solution field some_moves =
  List.fold_left do_action field & String.explode some_moves $ List.map String.of_char

(* }}} *)

(* {{{ Coma solver *)

type solver_record = Blank | JustSolution of string * int * int | History of field * int

let coma = ref CoordMap.empty
let coma_ongoing = ref true

let solve ?(quiet=false) ?(use_signals=true) f =

  let rec induce_coma map field action = (
    try
      let morphed = do_action field action in
      let morphed_hscore = (heuristic_score morphed) in
      if not (CoordMap.mem morphed.robot map) then (
        coma_ongoing := true;
        CoordMap.add morphed.robot (History (morphed, (heuristic_score morphed))) map
      ) else match CoordMap.find morphed.robot map with
        | JustSolution (_, hscore, _) ->
          if hscore < morphed_hscore then (
            coma_ongoing := true;
            CoordMap.add morphed.robot (History (morphed, (heuristic_score morphed))) map
          ) else map
        | History (nice_state, hscore) ->
          (* overwriting a fresh, perfectly nice solution *)
            if hscore < morphed_hscore then (
              (*
              log "--------------------------------------------------";
              log "**** OVERWRITE h=%d" hscore;
              print_field ~heuristics:hscore ~clear:false nice_state;
              log "**** With %d" morphed_hscore;
              print_field ~heuristics:morphed_hscore ~clear:false morphed;
              *)
              CoordMap.add morphed.robot (History (morphed, morphed_hscore)) map
            ) else map
      | Blank ->
          coma_ongoing := true;
          CoordMap.add morphed.robot (History (morphed, (heuristic_score morphed))) map
    with _ -> map
  )

  in



  let process_coma coords slv new_coma =
    match slv with
    | JustSolution _ -> new_coma
    | Blank -> new_coma
    | History (old_field, old_score) -> (
        let m = new_coma in
        let m = induce_coma m old_field "D" in
        let m = induce_coma m old_field "L" in
        let m = induce_coma m old_field "U" in
        let m = induce_coma m old_field "R" in
        let m = if old_field.razors > 0 then induce_coma m old_field "S" else m in
        begin match CoordMap.find coords m with
        | Blank -> m
        | JustSolution (_, score_check, _)
        | History (_, score_check) ->
            if score_check = old_score
            then CoordMap.add coords (JustSolution (old_field.moves_taken, (heuristic_score old_field), old_field.score)) m
            else m
        end
    )
  in
  let retrieve_best_solution () =
    let top_score = ref (-999999)
    and top_hscore = ref (-999999)
    and top_solution = ref "A" in
      CoordMap.iter (fun coords elem ->
          match elem with
          | History (current_best, hscore) ->
              if current_best.score > !top_score then (
                top_solution := current_best.moves_taken;
                top_score := current_best.score;
                top_hscore := hscore;
              )
          | JustSolution (solution, hscore, score) ->
              if score > !top_score then (
                top_solution := solution;
                top_score := score;
                top_hscore := hscore;
              )
          | Blank -> ()
      ) !coma;

    (* log "Retrieved %s as best (score=%d, hscore=%d)" !top_solution !top_score !top_hscore; *)

    !top_solution

  in


  if use_signals then Sys.set_signal Sys.sigint (Sys.Signal_handle (fun _ ->
      Sys.set_signal Sys.sigint Sys.Signal_default;
      log "%s" & retrieve_best_solution ();
      exit 0;
  ));

  coma := CoordMap.add f.robot (History (f, heuristic_score f)) CoordMap.empty;

  coma_ongoing := true;
  while !coma_ongoing do
    if not quiet then printf ".%!";
    coma_ongoing := false;
    coma := CoordMap.fold process_coma !coma !coma;
  done;

  if use_signals then Sys.set_signal Sys.sigint Sys.Signal_default;

  retrieve_best_solution ()

(* }}} *)

(* {{{ torture / scoring *)

let torture_chambers =
  [ "../maps/contest1.map", 212
  ; "../maps/contest2.map", 281
  ; "../maps/contest3.map", 275
  ; "../maps/contest4.map", 575
  ; "../maps/contest5.map", 1303
  ; "../maps/contest6.map", 1177
  ; "../maps/contest7.map", 869
  ; "../maps/contest8.map", 1973
  ; "../maps/contest9.map", 3093
  ; "../maps/contest10.map", 3634

  ; "../maps/flood1.map", 945
  ; "../maps/flood2.map", 281
  ; "../maps/flood3.map", 1303
  ; "../maps/flood4.map", 1592
  ; "../maps/flood5.map", 575

  ; "../maps/beard1.map", 860
  ; "../maps/beard2.map", 4522
  ; "../maps/beard3.map", 1789
  ; "../maps/beard4.map", 3103
  ; "../maps/beard5.map", 665

  ; "../maps/trampoline1.map", 426
  ; "../maps/trampoline2.map", 1742
  ; "../maps/trampoline3.map", 5477

  ; "../maps/horock1.map", 758
  ; "../maps/horock2.map", 747
  ; "../maps/horock3.map", 2406
  ]

let torture torture_fn =
  let tortured_total, best_possible_total =
  List.fold_left (fun  (tortured_total, best_possible_total) (map, best_possible_score) ->

    let solved_score = (try
      let field = load_field & lines_of_file & map in
      let solved_field = apply_solution field & torture_fn map in
      solved_field.score
     with _ -> 0
    ) in

    log "%s: %3d%% %4d of %4d" map (solved_score * 100 / best_possible_score) solved_score best_possible_score;
    (tortured_total + solved_score, best_possible_total + best_possible_score)
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

  let c1 = load_field & lines_of_file "../maps/contest1.map"
  and c2 = load_field & lines_of_file "../maps/contest2.map"
  and c3 = load_field & lines_of_file "../maps/contest3.map" in
  let f = apply_solution c1 "LDRDDUULLLDDL" in assert (f.score = 212);
  let f = apply_solution c2 "RRUDRRULURULLLLDDDL" in assert (f.score = 281);
  let f = apply_solution c3 "LDDDRRRRDDLLLLLDURRRUURRR" in assert (f.score = 275);

  assert (" ABC" = rstrip " ABC ");
  assert ( ("A", 1) = make_trammap "A targets 1");

  log "Tests passed"

(* }}} *)

let _ =

  let quiet_mode = (try ignore(Sys.getenv "QUIET"); true with _ -> false)
  and do_tests = (try ignore(Sys.getenv "NOTEST"); false with _ -> true)
  in

  if Array.length Sys.argv = 1 then begin

    let f = load_field & Std.input_list stdin in
    log "%s" & solve ~quiet:true f

  end else if Array.length Sys.argv = 2 && Sys.argv.(1) = "torture" then begin

    self_torture $ torture;

  end else if Array.length Sys.argv = 3 && Sys.argv.(1) = "torture" then begin

    alien_torture Sys.argv.(2) $ torture

  end else if Array.length Sys.argv = 2 then begin

    if (do_tests) then run_tests ();

    let f = load_field & lines_of_file & Sys.argv.(1) in
    (* solve f $ String.join "" $  log "%s"; *)

    let solution = solve f in
    if quiet_mode then apply_solution f solution $ print_field
    else animate_solution f solution;

  end else begin

    if (do_tests) then run_tests ();

    let f = load_field & lines_of_file Sys.argv.(1) in

    if quiet_mode then apply_solution f Sys.argv.(2) $ print_field
    else animate_solution f Sys.argv.(2) $ ignore;

  end;


  ()
