open! OCADml
open OSCADml
open! Dometyl
open! Examples

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

let dartyl_mx_jm_right = Skeletyl.build ()

let () =
  to_file "dartyl_mx_jm_right" (Case.to_scad ~show_caps:false dartyl_mx_jm_right);
  to_file "bottom_plate_right" (Skeletyl.bottom @@ dartyl_mx_jm_right);
  to_file "tent_right" (Skeletyl.tent dartyl_mx_jm_right)