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

type elem = Wall | Rock | Lift | Earth | Empty | Lambda

let initial_robot_coordinates = ref (0,0)

type location =
  { coords: coords
  ; stuff: elem
}

type field =
  { f: location list
  ; dimensions: coords
  ; score: int
  ; robot: coords
  ; robot_alive: bool
  ; actions_so_far: string list
  }


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

let char_of_elem = function
  | Wall -> "#"
  | Earth -> "."
  | Empty -> " "
  | Lift -> "L"
  | Rock -> "*"
  | Lambda -> "λ"


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
  ; robot_alive = true
  ; score = 0
  ; dimensions = dimensions
  ; robot = invert_y dimensions !initial_robot_coordinates
  ; actions_so_far = []
  }


let print_field f =
  (*
  log "Robot @ %s" & string_of_coords f.robot;
  log "Field dimensions: %s" & string_of_coords f.dimensions;
  *)
  let w, h = f.dimensions in
  let repr = make_2d_array (h + 1) (w + 1) " " in

  List.iter (fun c -> printf "%s" c) (List.rev f.actions_so_far);
  log " score=%d" f.score;

  let rec loop n_elems = function
    | h::t ->
      let hx, hy = h.coords in
      repr.(hy).(hx) <- char_of_elem h.stuff;
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

let peek_location f l =
  let rec loop = function
  | h::t -> if h.coords = l then Some h.stuff else loop t
  | _ -> None
  in loop f.f

let string_of_elem_at f l =
  match peek_location f l with
  | Some l -> char_of_elem l
  | None -> " "


let rec is_walkable field direction c =
  match peek_location field c with
  | Some Wall  -> false
  | Some Rock  ->
      (* rocks can be pushed? *)
      if direction = "L" then
        is_walkable field "X" (c ^- (1, 0))
      else if direction = "R" then
        is_walkable field "X" (c ^+ (1, 0))
      else false
  | _ -> true


let do_action field action =

  let new_robo_coords = (match action with
  | "L" -> field.robot ^+ (-1,0)
  | "R" -> field.robot ^+ (+1,0)
  | "U" -> field.robot ^+ (0,1)
  | "D" -> field.robot ^+ (0,-1)
  | "W" -> field.robot
  | s -> failwithf "Unknown robo-action %s" s
  ) in
  (* nevar paiet, stāvi uz vietas *)
  let new_robo_coords = if not & is_walkable field action new_robo_coords then begin
    log "Tried to move %s from %s but can't" action (string_of_coords new_robo_coords);
    field.robot
  end else new_robo_coords in

  (* bork: fall on head? *)

  let peek = peek_location field in

  let (>>>) elem a = let l,n = a in ((elem::l), n) in

  let rec loop score = function
    | loc::t ->
        if loc.coords = new_robo_coords then (

          (* robots uzkāpj uz zemes, zeme pazūd: skipojam cellu *)
          if loc.stuff = Earth then loop score t else

          if loc.stuff = Lambda then loop 25 t else

          (* robots grūž akmeni *)
          if loc.stuff = Rock then
            let delta = (if action = "L" then (-1, 0) else (1, 0)) in
            { loc with coords = loc.coords ^+ delta } :: t $ loop score (* retry -- it may fall *)
          else loc >>> (loop score t)

        ) else (

          if loc.stuff = Rock then begin
            let c_bottom = loc.coords ^+ (+0, -1)
            and c_left   = loc.coords ^+ (-1, +0)
            and c_right  = loc.coords ^+ (+1, +0)
            and c_bottom_left  = loc.coords ^+ (-1, -1)
            and c_bottom_right = loc.coords ^+ (+1, -1)
            in
            if peek c_bottom = None && c_bottom <> new_robo_coords then
              (* akmens krīt lejā vertikāli *)
              (* bork: var uzkrist uz robota galvas *)
              { loc with coords = c_bottom } >>> (loop score t)
            else
            if peek c_bottom = Some Rock && peek c_right = None && peek c_bottom_right = None then
                (* uz leju pa labi *)
                { loc with coords = c_bottom_right } >>> (loop score t)
            else
            if peek c_bottom = Some Lambda && peek c_right = None && peek c_bottom_right = None then
                (* uz leju pa labi *)
                { loc with coords = c_bottom_right } >>> (loop score t)
            else
            if peek c_bottom = Some Rock && (peek c_left = None && peek c_bottom_left = None) && (peek c_right <> None || peek c_bottom_right <> None) then
              { loc with coords = c_bottom_left } >>> (loop score t)
            else
              loc >>> (loop score t)
          end
            else loc >>> (loop score t)
        )
    | _ -> [], score
  in

  (* bork: lifts *)


  let nf, new_score = loop 0 (List.sort ~cmp:(fun a b -> let ax, ay = a.coords and bx, by = b.coords in if ay == by then ax - bx else ay - by) field.f) in
  { field with
    f = nf;
    score = field.score + (if new_score = 0 then -1 else new_score);
    actions_so_far = action :: field.actions_so_far;
    robot = new_robo_coords;
  }

(* {{{ run_tests *)

let run_tests () =

  let rec waffle cmds f =
    let grr = ref f in
    print_field !grr;
    for i = 1 to pred & String.length cmds do
      grr := do_action !grr (String.get cmds i $ Std.string_of_char);
      print_field !grr;
    done;
    !grr
  in

  let print = true
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
  assert( f.actions_so_far = ["W"] );
  assert( f.robot = (2,3) );

  if print then print_field f;

  let f = do_action f "R" in
  assert( f.actions_so_far = ["R"; "W"] );
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

  if false then (
  let _ = waffle "WWWWW" &
  load_field
  [ "#    ****     R"
  ; "#    ****      "
  ; "#    ****      "
  ; "#    0000      "
  ; "###############"
  ] in ());

  if false then (
  let _ = waffle "WW" &
  load_field
  [ "#* *#"
  ; "#* *# R"
  ; "###############"
  ] in ());

  log "Tests passed"

(* }}} *)



let _ =
  run_tests ();
  (* print_field & load_field & lines_of_file "../maps/contest1.map"; *)
  ()
