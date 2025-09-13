open OCADml
open OSCADml
open Dometyl

let body_lookups =
  (* let offset = function
    | 2 -> v3 0. 3.5 (-5.) (* middle *)
    | 3 -> v3 1. (-2.5) 0.5 (* ring *)
    | i when i >= 4 -> v3 0.5 (-16.) 8.5 (* pinky *)
    | 0 -> v3 (-2.5) 0. 5.
    | _ -> v3 0. 0. 0. *)
  let offset = function
    | 2 -> v3 1. 3.5 (-5.) (* middle *)
    | 3 -> v3 3.5 (-2.5) 0.5 (* ring *)
    | i when i >= 4 -> v3 2. (-18.) 8.5 (* pinky *)
    | 0 -> v3 (-3.5) 0. 5.
    | _ -> v3 0. 0. 0.
  and curve = function
    | i when i >= 3 ->
      (* ring and pinky: 4.2 is mt3 compatible  *)
      (* Curvature.(curve ~well:(well ~radius:37. (Float.pi /. 4.85)) ()) *)
      Curvature.(curve ~well:(well ~radius:37. (Float.pi /. 4.2)) ())
    | i when i = 0 ->
      (* outer index: 5.5 is mt3 compatible  *)
      (* Curvature.(curve ~well:(well ~tilt:(Float.pi /. 7.5) ~radius:46. (Float.pi /. 5.95)) () ) *)
      Curvature.(curve ~well:(well ~tilt:(Float.pi /. 7.5) ~radius:46. (Float.pi /. 5.5)) () )
    | _ -> 
      (* inner index and middle: 5.8 is mt3 compatible  *)
      (* Curvature.(curve ~well:(well ~radius:46.5 (Float.pi /. 6.1)) ()) *)
      Curvature.(curve ~well:(well ~radius:46.5 (Float.pi /. 5.8)) ())
  and swing = function
    | 2 -> Float.pi /. -48.
    | 3 -> Float.pi /. -19.
    | 4 -> Float.pi /. -14.
    | _ -> 0.
  and splay = function
  | i when i = 3 -> Float.pi /. -30. (* ring *)
  | i when i >= 4 -> Float.pi /. -20. (* pinky *)
  | _ -> 0.
  and rows _ = 3 in
  Plate.Lookups.body ~offset ~curve ~splay ~swing ~rows ()

let thumb_lookups =
  let curve _ =
    (* thumbs: fan ~radius:74 is mt3 compatible  *)
    (* Curvature.(
      curve
        ~fan:(fan ~radius:70. ~tilt:(Float.pi /. 48.) (Float.pi /. 9.))
        ~well:(well ~radius:47. (Float.pi /. 7.5))
        () ) *)
    Curvature.(
      curve
        ~fan:(fan ~radius:73. ~tilt:(Float.pi /. 48.) (Float.pi /. 9.))
        ~well:(well ~radius:47. (Float.pi /. 7.5))
        () )
  and rows _ = 2 in
  Plate.Lookups.thumb ~curve ~rows ()

(* let thumb_offset = v3 9. (-45.) 8. *)
let thumb_offset = v3 9. (-45.) 8.

(* Test configuration - start with 2mm *)
let plate_builder =
  Plate.make
    ~n_body_cols:5
    ~spacing:1.5
    ~tent:(Float.pi /. 10.)
    ~body_lookups
    ~thumb_lookups
    ~thumb_offset
    (* ~thumb_angle:Float.(v3 0. (pi /. -4.3) (pi /. 6.)) *)
    ~thumb_angle:Float.(v3 0. (pi /. -4.3) (pi /. 20.))

    ~caps:Caps.Matty3.row
    ~thumb_caps:Caps.MT3.thumb_1u

    (* ~caps:Caps.Cherry.row *)

    (* ~caps:Caps.OEM.row *)

(* Enhanced plate welder with comprehensive column connections *)
let plate_welder =
  fun plate ->
  (* Base skeleton bridges *)
  let skeleton_bridges = Plate.skeleton_bridges plate in
  (* Simple but effective column connections for added strength *)
  let column_connections =
    try
      let body_columns = plate.Plate.body in
      (* Connect adjacent columns with conservative parameters *)
      let safe_connections =
        [ (* Middle core connections - most stable *)
          1, 2, 0.1, 0.2, 0.3
        ; 2, 3, 0.12, 0.25, 0.35
        ; (* Index and ring connections *)
          0, 1, 0.15, 0.3, 0.4
        ; 3, 4, 0.15, 0.3, 0.4
        ]
      in
      List.filter_map
        (fun (col1, col2, in_d, out_d1, out_d2) ->
           try
             if IMap.mem col1 body_columns && IMap.mem col2 body_columns
             then
               Some
                 (Bridge.cols
                    ~columns:body_columns
                    ~in_d
                    ~out_d1
                    ~out_d2
                    ~skip:[] (* Skip middle row to avoid conflicts *)
                    col1
                    col2 )
             else None
           with
           | _ -> None )
        safe_connections
    with
    | _ -> []
  in
  (* Combine bridges *)
  match column_connections with
  | [] -> skeleton_bridges
  | bridges ->
    ( try Scad.union (skeleton_bridges :: bridges) with
      | _ -> skeleton_bridges )

let wall_builder plate =
  Walls.
    { body =
        auto_body
          ~d1:(`Abs 10.)
          ~d2:5.
          ~n_steps:(`PerZ 1.25)
          ~scale:(v2 0.8 0.9)
          ~scale_ez:(v2 0.42 1., v2 1. 1.)
          ~north_clearance:0.
          ~south_clearance:0.
          ~side_clearance:0.
          ~west_lookup:(fun row ->
            match row with
            | 0 -> true
            | 1 -> true
            | 2 -> true
            | _ -> false )
          ~east_lookup:(fun row ->
            match row with
            | 1 -> true
            | _ -> false )
          plate
      ; thumb =
        auto_thumb
        ~d1:(`Abs 10.)
        ~d2:5.
        ~n_steps:(`Flat 20)
        ~scale:(v2 0.8 0.9)
        ~scale_ez:(v2 0.42 1., v2 1. 1.)
        ~south_lookup:(fun _ -> true)
        ~east_lookup:(fun _ -> false)
        ~west_lookup:(fun _ -> true)
        ~north_lookup:(fun _ -> false)
        ~north_clearance:0.
        ~south_clearance:0.
        ~side_clearance:0.
        plate
      }
    (* ; thumb =
        auto_thumb
          ~d1:(`Abs 10.)
          ~d2:5.
          ~n_steps:(`Flat 20)
          ~scale:(v2 0.8 0.9)
          ~scale_ez:(v2 0.42 1., v2 1. 1.)
          ~south_lookup:(fun _ -> true)
          ~east_lookup:(fun _ -> false)
          ~west_lookup:(fun _ -> false)
          ~north_clearance:0.
          ~south_clearance:0.
          ~side_clearance:0.
          plate
    } *)

let base_connector =
  Connect.skeleton
    ~height:15.
    ~tight_threshold:4.3 (* HACK: lowered to avoid thin east thumb link *)
    ~index_height:15.
    ~thumb_height:13.
    ~fn:64
    ~close_thumb:false

    ~corner:(Path3.Round.chamf (`Cut 0.5))

let ports_cutter = BastardShield.(cutter ~y_off:0.5 (make ()))

(* Enhanced eyelet configuration with M3 inserts *)
let build ?right_hand ?hotswap () =
  let keyhole =
    Mx.make_hole
      ~cap_cutout_height:None
      ?hotswap
      ~clearance:2.75
      ~corner:(Path3.Round.chamf (`Cut 0.5))
      ()
  and eyelets =
    (* Create M3 config based on the Eyelet.config type *)
    let m3_config =
      Eyelet.{ outer_rad = 5.; inner_rad = 1.7; thickness = 4.0; hole = Through }
    in
    Case.eyelets
      ~config:m3_config
      ~wall_locs:Eyelet.(default_wall_locs)
      ()
  in
  Case.make
    ?right_hand
    ~eyelets
    ~plate_builder
    ~plate_welder
    ~wall_builder
    ~base_connector
    ~ports_cutter
    keyhole

let bottom case =
  let bump_locs =
    Bottom.
      [ thumb ~loc:(v2 0. 0.5) Last First
        (*
           ; thumb ~loc:(v2 0.7 0.) Last Last
      ; body ~loc:(v2 0.1 0.9) First Last
      ; body ~loc:(v2 0.5 1.) (Idx 3) Last
      ; body ~loc:(v2 0.9 0.6) Last Last
      ; body ~loc:(v2 0.8 0.) Last First *)
      ]
  and m3_fastener = Eyelet.screw_fastener ~head_rad:4. ~shaft_rad:4. () in
  Bottom.make ~bump_locs ~fastener:m3_fastener case

let bastard_skelly =
  Util.imports
  |> Printf.sprintf "%s/stls/other_dactyls/bastardkb_skeletyl_v4_103.stl"
  |> Scad.import3
  |> Scad.translate (v3 87. 0. 25.)
  |> Scad.xrot (Float.pi /. 2.)
  |> Scad.translate (v3 0. (-2.) 8.)
  |> Scad.color ~alpha:0.5 Color.DarkSlateBlue

let bk_skeletyl_w_shield () =
  let shield = BastardShield.(make ()) in
  Scad.union
    [ bastard_skelly
    ; BastardShield.to_scad ~show_screws:true shield
      |> Scad.translate (v3 (-6.71) 35.2 2.)
    ]

(* let tent case = Tent.make ~degrees:7. case *)

let tent case = Tent.make ~degrees:10. ~style:Solid case
