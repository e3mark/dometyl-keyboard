open! OCADml
open OSCADml
open! Dometyl
open! Du30

(* It is recommended to use the "<include> trick" for models produced by dometyl
    for OpenSCAD editor related performance reasons. This will generate an
    additional file prefixed with "incl_" that contains the actual model, while
    the script with the given base name simply <include>'s it. *)
let to_file name scad = Scad.to_file ~incl:true (name ^ ".scad") scad

(* NOTE: If you aren't using hotswap holders, you can simply mirror the generated case stl,
   but if you are, you will need to make a left-hand case like so. The bottom plate and
   tenting base will of course still be reversible, so you can mirror those in your slicer
   as you would a case with plain switch holes. Though you can also make bottoms/tent for
   left cases directly here as well. *)

(* let du30_mx_right = Du36_mx.build ()

let () =
  to_file "du30_mx_right" (Case.to_scad ~show_caps:false du30_mx_right);
  to_file "bottom_plate_right" (Du36_mx.bottom @@ du30_mx_right);
  to_file "tent_right" (Du36_mx.tent du30_mx_right) *)


let du30_mx_right = Du34_mx.build ()

let () =
  to_file "du30_mx_right" (Case.to_scad ~show_caps:true du30_mx_right);
  to_file "bottom_plate_right" (Du34_mx.bottom @@ du30_mx_right);
  to_file "tent_right" (Du34_mx.tent du30_mx_right)