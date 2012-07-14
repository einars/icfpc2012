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

type location =
  { coords: coords
  ; stuff: elem
}


type field =
  { f: location list
  ; dimensions: coords
  ; score: int
  ; lambdas: int
  ; lambdas_eaten: int
  ; robot: coords
  ; is_complete: bool
  ; moves_taken: string list
  }

let rec count_lambdas n = function
  | loc::t ->
      count_lambdas (if loc.stuff = Lambda then n + 1 else n) t
  | _ -> n


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
  | Earth -> "."
  | Empty -> " "
  | Lift -> if lambdas = 0 then "O" else "L"
  | Rock -> "*"
  | Lambda -> "λ"
  | Robot -> "E"


let load_line pos s (accum:location list) =
  let rec loop ps =
    let index = (fst ps) - 1 in
    if String.length s <= index then accum
    else (
      let elem, robo = load_char & String.get s index in
      let rest =  ps ^+ (1,0) $ loop in
      if robo then initial_robot_coordinates := ps;
      match elem with
      | Wall -> { coords = ps; stuff = Wall } :: rest
      | Rock -> { coords = ps; stuff = Rock } :: rest
      | Lift -> { coords = ps; stuff = Lift } :: rest
      | Earth -> { coords = ps; stuff = Earth } :: rest
      | Lambda -> { coords = ps; stuff = Lambda } :: rest
      | Robot -> failwith "I certainly didn't expect Robot in response to load_char"
      | Empty -> rest
    )
  in
  loop pos

let get_dimensions =
  let rec loop max_x max_y = function
    | loc :: rest ->
        let x, y = loc.coords in
        loop (max x max_x) (max y max_y) rest
     | _ -> max_x, max_y
  in
  loop 0 0

let invert_y dimensions pos =
  let _, h = dimensions and x, y = pos in
  x, h - y + 1

let load_field lines =
  let rec loop pos = function
    | line::rest -> load_line pos line ( loop (pos ^+ (0,1) ) rest )
    | _ -> []
  in
  let loaded_field = loop (1, 1) lines in
  let dimensions = get_dimensions loaded_field in
  { f = List.map
    (fun elem -> { elem with coords = invert_y dimensions elem.coords })
    loaded_field
  ; is_complete = false
  ; score = 0
  ; lambdas = count_lambdas 0 loaded_field
  ; lambdas_eaten = 0
  ; dimensions = dimensions
  ; robot = invert_y dimensions !initial_robot_coordinates
  ; moves_taken = []
  }


let print_field f =
  (*
  log "Robot @ %s" & string_of_coords f.robot;
  log "Field dimensions: %s" & string_of_coords f.dimensions;
  *)
  let w, h = f.dimensions in
  let repr = make_2d_array (h + 1) (w + 1) " " in

  List.iter (fun c -> printf "%s" c) (List.rev f.moves_taken);
  log " score=%d eaten=%d (%d rem)" f.score f.lambdas_eaten f.lambdas;

  let rec loop n_elems = function
    | h::t ->
      let hx, hy = h.coords in
      repr.(hy).(hx) <- char_of_elem ~lambdas:f.lambdas h.stuff;
      loop (succ n_elems) t;
    | _ -> n_elems
  in

  let rx, ry = f.robot in repr.(ry).(rx) <- "R";

  ignore( loop 0 f.f );
  for j = 1 to h do
    for k = 1 to w do
      printf "%s%!" repr.(h - j + 1).(k);
    done;
    printf "\n";
  done;

  log ""

(* }}} *)


let peek_location ?(robot=(-1,-1)) f l =
  let rec loop = function
  | h::t -> if h.coords = l then Some h.stuff else loop t
  | _ -> None
  in
  if robot = l then Some Robot else loop f.f

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
exception Finished of int

let exec_action field action =

  if field.is_complete then raise Bork_finished;

  if action = "A" then raise (Finished (field.score + 25 * field.lambdas_eaten));

  let new_robo_coords = update_robot field.robot action in
  (* nevar paiet, stāvi uz vietas *)
  if not & is_walkable field action new_robo_coords then raise Bork_unwalkable;


  (* bork: fall on head? *)

  let peek = peek_location ~robot:new_robo_coords field in

  let score_diff, lambda_diff =
  match peek_location field new_robo_coords with
  | Some Earth -> -1, 0
  | Some Lambda -> 24, 1
  (* | Some Lift -> 50 * field.lambdas_eaten - 1, 0 (* handled via internal raise Finished *)*)
  | _ -> -1, 0
  in


  let rec loop = function
    | loc::t ->
        if loc.coords = new_robo_coords then (

          (* robots uzkāpj uz zemes, zeme pazūd: skipojam cellu *)
          if loc.stuff = Earth then loop t else

          (* robots apēd lambdu *)
          if loc.stuff = Lambda then loop t else

          (* robots iekāpj liftā *)
          if loc.stuff = Lift then (
            raise (Finished (field.score + 50 * field.lambdas_eaten - 1))
          ) else

          (* robots grūž akmeni *)
          if loc.stuff = Rock then
            let delta = (if action = "L" then (-1, 0) else (1, 0)) in
            { loc with coords = loc.coords ^+ delta } :: t $ loop (* retry -- it may fall *)
          else loc :: (loop t)

        ) else (

          if loc.stuff = Rock then begin
            let c_bottom = loc.coords ^+ (+0, -1)
            and c_left   = loc.coords ^+ (-1, +0)
            and c_right  = loc.coords ^+ (+1, +0)
            and c_bottom_left  = loc.coords ^+ (-1, -1)
            and c_bottom_right = loc.coords ^+ (+1, -1) in
            let peek_bottom = peek c_bottom
            and peek_right = peek c_right
            and peek_left = peek c_left
            in
            if peek_bottom = None && c_bottom <> new_robo_coords then (
              (* akmens krīt lejā vertikāli *)
              if new_robo_coords = loc.coords ^+ (+0, -2) then raise Bork_fallen_rock;
              { loc with coords = c_bottom } :: (loop t)
            ) else
            if peek_bottom = Some Rock && peek_right = None && peek c_bottom_right = None then (
                (* uz leju pa labi *)
                if new_robo_coords = loc.coords ^+ (+1, -2) then raise Bork_fallen_rock;
                { loc with coords = c_bottom_right } :: (loop t)
            ) else
            if peek_bottom = Some Lambda && peek_right = None && peek c_bottom_right = None then (
                (* uz leju pa labi *)
                if new_robo_coords = loc.coords ^+ (+1, -2) then raise Bork_fallen_rock;
                { loc with coords = c_bottom_right } :: (loop t)
            ) else
            if peek_bottom = Some Rock && (peek_left = None && peek c_bottom_left = None) && (peek_right <> None || peek c_bottom_right <> None) then (
              if new_robo_coords = loc.coords ^+ (-1, -2) then raise Bork_fallen_rock;
              { loc with coords = c_bottom_left } :: (loop t)
            ) else
              loc :: (loop t)
          end
            else loc :: (loop t)
        )
    | _ -> []
  in

  let nf = loop (List.sort ~cmp:(fun a b -> let ax, ay = a.coords and bx, by = b.coords in if ay == by then ax - bx else ay - by) field.f) in
  { field with
    f = nf;
    score = field.score + score_diff;
    moves_taken = action :: field.moves_taken;
    lambdas = count_lambdas 0 nf;
    lambdas_eaten = field.lambdas_eaten + lambda_diff;
    robot = new_robo_coords;
  }

let do_action field action = (
  try
    exec_action field action
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
        ignore(Unix.select [] [] [] 0.1);
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


let get_lambdas f =
  let rec loop = function
    | h::t -> let rest = loop t in if h.stuff = Lambda then h.coords :: rest else rest
    | _ -> []
  in loop f.f

let heuristics_score f =
  f.score
  (* 100 * f.score + 10 * f.lambdas_eaten - (1 * List.length f.moves_taken) *)

let best n fs =
  List.sort ~cmp:(fun a b -> (-) (heuristics_score b) (heuristics_score a)) fs $ List.take n


let solve f =

  let h = (snd f.dimensions) and w = (fst f.dimensions) in

  let astar = make_2d_array (h + 1) (w + 1) [] in


  let tolerance = 5 in


  let rec loop_astar tries =
    let astar_put_score f = 
      let x,y = f.robot in
      if astar.(y).(x) = [] || (List.fold_left (fun a b -> min a (heuristics_score b)) 999999 astar.(y).(x) < heuristics_score f) then (
        astar.(y).(x) <- best tolerance (f :: astar.(y).(x));
        1
      ) else (
        0
      )
    in
    match tries with
    | h::t -> if h.is_complete $ not then (
      (try ( astar_put_score & do_action h "U") with _ -> 0) +
      (try ( astar_put_score & do_action h "D") with _ -> 0) +
      (try ( astar_put_score & do_action h "L") with _ -> 0) +
      (try ( astar_put_score & do_action h "R") with _ -> 0) (* +
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
        | fs ->
            had_modifications := !had_modifications + (loop_astar fs)
      done;
    done;
    if !had_modifications > 0 then process_frontier ()
  in

    (*
    new_frontier := [];
    let rec loop_frontier = function
      | f::t ->
        if f.is_complete $ not then (
        (try ( astar_put_score & do_action f "U") with _ -> ());
        (try ( astar_put_score & do_action f "D") with _ -> ());
        (try ( astar_put_score & do_action f "L") with _ -> ());
        (try ( astar_put_score & do_action f "R") with _ -> ());
        (try ( astar_put_score & do_action f "A") with _ -> ());
        (* (try ( astar_put_score & do_action f "W") with _ -> () ); *)
        );
        (* if f.robot = (3,4) then print_field & do_action f "L"; *)
        loop_frontier t
      | _ -> ()
    in
    loop_frontier frontier;
    let ff = !new_frontier in
    if !new_frontier <> [] then process_frontier ff
    *)


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

  run_tests ();

  if Array.length Sys.argv = 1 then begin

    ();

  end else if Array.length Sys.argv = 1 then begin

    let f = load_field & lines_of_file & Sys.argv.(1) in
    solve f $ animate_solution f;

  end else begin

    let f = load_field & lines_of_file & Sys.argv.(1) in
    String.explode Sys.argv.(2) $ List.map String.of_char $ List.rev $ animate_solution f;

  end;


  ()
