open OCADml
open OSCADml
open Syntax

type bump_loc =
  | Thumb of Idx.t * Idx.t * v2 option
  | Body of Idx.t * Idx.t * v2 option
  | Point of v2

let thumb ?loc col row = Thumb (col, row, loc)
let body ?loc col row = Body (col, row, loc)
let point p = Point p

let locate (key : Key.t) = function
  | Some (loc : v2) ->
    let xaxis = key.faces.east.normal
    and yaxis = key.faces.north.normal in
    let xshift = V3.(xaxis *$ (key.config.outer_w *. (V2.x loc -. 0.5)))
    and yshift = V3.(yaxis *$ (key.config.outer_h *. (V2.y loc -. 0.5))) in
    V3.(to_v2 @@ (key.origin +@ xshift +@ yshift))
  | None -> V2.of_v3 key.origin

let locate_bump (plate : Plate.t) = function
  | Thumb (c, k, loc) ->
    let* col = Idx.to_find c plate.thumb in
    let+ key = Idx.to_find k col.keys in
    locate key loc
  | Body (c, k, loc) ->
    let* col = Idx.to_find c plate.body in
    let+ key = Idx.to_find k col.keys in
    locate key loc
  | Point p -> Some p

let default_bumps =
  [ thumb Last First
  ; thumb Last Last
  ; body First Last
  ; body (Idx 3) Last
  ; body Last Last
  ; body Last First
  ]

let make
    ?(thickness = 1.65)
    ?fastener
    ?(bumpon_rad = 5.5)
    ?(bumpon_inset = 0.8)
    ?(bump_locs = default_bumps)
    ?(upper_left_hole = true)
    ?(upper_left_hole_diameter = 30.0)
    case
  =
  let screw_config =
    match case.Case.eyelets with
    | [] -> Eyelet.m4_config (* dummy *)
    | eye :: _ -> eye.config
  in
  let thickness, screw_hole =
    let fastener =
      match fastener with
      | None ->
        ( match screw_config with
        | { hole = Through; _ } -> Eyelet.screw_fastener ()
        | _ -> SameMagnet )
      | Some fastener -> fastener
    and magnetize rad h =
      let thickness = Float.max thickness (h +. 0.6) in
      ( thickness
      , Scad.union
          [ Scad.ztrans (thickness -. h -. 0.05)
            @@ Scad.cylinder ~fn:32 ~height:(h +. 0.1) rad
          ; Scad.ztrans (-0.05)
            @@ Scad.cylinder ~fn:32 ~height:(thickness -. h +. 0.1) (rad /. 2.)
          ] )
    in
    match fastener with
    | Screw { head_rad; shaft_rad; sink = Counter; _ } ->
      let s = shaft_rad /. head_rad in
      ( thickness
      , Scad.circle head_rad
        |> Scad.extrude ~height:(thickness +. 0.05) ~scale:(v2 s s)
        |> Scad.ztrans (-0.025) )
    | Screw { head_rad; shaft_rad; sink = Pan depth; _ } ->
      ( thickness
      , Scad.ztrans (-0.05)
        @@ Scad.union
             [ Scad.cylinder ~fn:32 ~height:(depth +. 0.1) head_rad
             ; Scad.cylinder ~fn:32 ~height:(thickness +. 0.1) shaft_rad
             ] )
    | SameMagnet ->
      let Eyelet.{ inner_rad; hole; _ } = screw_config in
      let h =
        match hole with
        | Eyelet.Inset { depth; _ } -> depth
        | _ -> failwith "Case eyelet expected to be magnet inset."
      in
      magnetize inner_rad h
    | Magnet { rad; thickness } -> magnetize rad thickness
  and insets =
    let cut =
      Scad.ztrans (-0.05) @@ Scad.cylinder ~height:(bumpon_inset +. 0.1) bumpon_rad
    in
    List.filter_map
      ( locate_bump case.plate
      >> Option.map V3.of_v2
      >> Option.map (Fun.flip Scad.translate cut) )
      bump_locs
    |> Scad.union3
  and upper_left_hole_shape =
    if upper_left_hole then
      let outline_points = Connect.outline_2d case.connections in
      let min_x = List.fold_left (fun acc pt -> Float.min acc (V2.x pt)) Float.max_float outline_points in
      let max_y = List.fold_left (fun acc pt -> Float.max acc (V2.y pt)) Float.neg_infinity outline_points in
      let hole_pos = v2 (min_x +. 45.0) (max_y -. 25.0) in  (* 10mm offset from edges *)
      let hole_rad = upper_left_hole_diameter /. 2.0 in
      [ Scad.translate (V3.of_v2 hole_pos) 
        @@ Scad.ztrans (-0.05) 
        @@ Scad.cylinder ~fn:32 ~height:(thickness +. 0.1) hole_rad ]
    else
      []
  in
  let plate =
    Scad.polygon (Connect.outline_2d case.connections) |> Scad.extrude ~height:thickness
  in
  Scad.difference
    plate
    (insets :: upper_left_hole_shape @ List.map (fun l -> Scad.translate l.Eyelet.centre screw_hole) case.eyelets)