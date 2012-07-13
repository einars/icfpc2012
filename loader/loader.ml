
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
  let x1, y1 = c1
  and x2, y2 = c2
  in (x1 + x2), (y1 + y2)

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
  { pos: coords
  ; stuff: elem
}

type field =
  { f: location list
  ; dimensions: coords
  ; robot: coords
  }

let load_char = function
  | '#' -> Wall, false
  | '.' -> Earth, false
  | ' ' -> Empty, false
  | 'R' -> Empty, true
  | 'L' -> Lift, false
  | 'O' -> Lift, false
  | '*' -> Rock, false
  | '\\'-> Lambda, false
  | c -> failwithf "Unknown char %c" c

let char_of_elem = function
  | Wall -> "#"
  | Earth -> "."
  | Empty -> " "
  | Lift -> "L"
  | Rock -> "*"
  | Lambda -> "0"

let load_line pos s (accum:location list) =
  let rec loop ps =
    let index = (fst ps) - 1 in
    if String.length s <= index then accum
    else (
      let elem, robo = load_char & String.get s index in
      let rest =  ps ^+ (1,0) $ loop in
      if robo then initial_robot_coordinates := ps;
      match elem with
      | Wall -> { pos = ps; stuff = Wall } :: rest
      | Rock -> { pos = ps; stuff = Rock } :: rest
      | Lift -> { pos = ps; stuff = Lift } :: rest
      | Earth -> { pos = ps; stuff = Earth } :: rest
      | Lambda -> { pos = ps; stuff = Lambda } :: rest
      | Empty -> rest
    )
  in
  loop pos

let get_dimensions =
  let rec loop max_x max_y = function
    | loc :: rest ->
        let x, y = loc.pos in
        loop (max x max_x) (max y max_y) rest
     | _ -> max_x, max_y
  in
  loop 0 0

let load_field lines =
  let rec loop pos = function
    | line::rest -> load_line pos line ( loop (pos ^+ (0,1) ) rest )
    | _ -> []
  in
  let loaded_field = loop (1, 1) lines in
  { f = loaded_field
  ; dimensions = get_dimensions loaded_field
  ; robot = !initial_robot_coordinates
  }


let print_field f =
  log "Robot @ %s" & string_of_coords f.robot;
  log "Field dimensions: %s" & string_of_coords f.dimensions;
  let w, h = f.dimensions in
  let repr = make_2d_array (h + 1) (w + 1) " " in

  let rec loop n_elems = function
    | h::t ->
      let hx, hy = h.pos in
      repr.(hy).(hx) <- char_of_elem h.stuff;
      loop (succ n_elems) t;
    | _ -> n_elems
  in

  let rx, ry = f.robot in repr.(ry).(rx) <- "R";

  let n_elems = loop 0 f.f in
  for j = 1 to h do
    for k = 1 to w do
      printf "%s%!" repr.(j).(k);
    done;
    printf "\n";
  done;
  log "%d elems" n_elems

let _ =
  print_field & load_field & lines_of_file "../maps/contest1.map";
  ()
