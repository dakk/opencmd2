open Bitstring;;
open Printf;;
open Loader_helper;;
open OgamlGraphics;;


module Version = struct
  type t = C1 | C2 | C3 | UNKNOWN;;

  let of_byte b = match b with
  | 0x31 -> C2
  | 0x32 -> C3
  | _ -> UNKNOWN
  ;;

  let to_string v = match v with 
  | C1 -> "Commandos1"
  | C2 -> "Commandos2"
  | C3 -> "Commandos3"
  | UNKNOWN -> "Unknown"
  ;;
end

module Point = struct
  type t = float * float * float;;

  let from_stream ic = 
    match%bitstring read_bitstring ic 12 with
    | {| 
      x : 8 * 4 : littleendian;
      y : 8 * 4 : littleendian;
      z : 8 * 4 : littleendian
    |} -> 
      (Int32.float_of_bits x, Int32.float_of_bits y, Int32.float_of_bits z)
  ;;

  let rec read_all_from_stream ic n = match n with
  | 0l -> []
  | n -> (from_stream ic) :: (read_all_from_stream ic @@ Int32.sub n Int32.one)
  ;;
end

module District = struct 
  type t = {
    attribute : int;
    texture_id: int;
    point_uvs : (int*float*float) list
  };;

  let from_stream s = 
    let rec point_uvs ic n = match n with
    | 0 -> []
    | n -> 
      match%bitstring read_bitstring ic 6 with
      | {| 
        point : 8 * 2 : littleendian;
        u : 8 * 2 : littleendian;
        v : 8 * 2 : littleendian
      |} -> 
        (point, float (u) /. 4096., float (v) /. 4096.) :: (point_uvs ic @@ n - 1)
    in
    match%bitstring read_bitstring s 2 with
    | {| 
      n : 8 * 1 : littleendian;
      texid : 8 * 1 : littleendian
    |} -> {
      attribute= 0;
      texture_id= texid;
      point_uvs= point_uvs s n
    }
  ;;

  let rec read_all_from_stream ic n = match n with
  | 0l -> []
  | n -> (from_stream ic) :: (read_all_from_stream ic @@ Int32.sub n Int32.one)
  ;;
end


module Obj = struct
  type t = {
    name                : string;
    start_district      : int32;
    next_start_district : int32;
  };;

  let from_stream s = 
    match%bitstring read_bitstring s 52 with
    | {| 
      name : 8 * 44 : string;
      start_d : 8 * 4 : littleendian;
      next_start_d : 8 * 4 : littleendian
    |} -> { name=name; start_district= start_d; next_start_district=next_start_d }
  ;;

  let rec read_all_from_stream ic n = match n with
  | 0l -> []
  | n -> (from_stream ic) :: (read_all_from_stream ic @@ Int32.sub n Int32.one)
  ;;
end


module Texture = struct
  type t = {
    width: int;
    height: int;
    palette: (int * int * int) list;
    bitmap: int list list;
  };;

  let to_image tex = Image.create @@ `Data ({x=tex.width; y=tex.height}, "");;

  let to_gif tex path = 
    let rec palette_conv l = match l with
    | [] -> ""
    | (r,g,b) :: l' -> 
      let p = string_of_bitstring [%bitstring {| r : 1 * 8; g : 1 * 8; b : 1 * 8 |}] in
      p ^ (palette_conv l')
    in
    let p = palette_conv tex.palette in
    let gif = string_of_bitstring [%bitstring {|
      "GIF87a" : 6 * 8 : string;
      tex.width : 2 * 8 : littleendian;
      tex.height : 2 * 8 : littleendian;
      1 : 1;
      25 : 3;
      1 : 1;
      3 : 3;
      0 : 8;
      0 : 8;
      p : 8 * (String.length p) : string
    |}] in
    let oc = open_out_bin path in
    output oc gif 0 @@ Bytes.length gif;
    close_out oc
  ;;
  
  let from_stream s = 
    let rec read_bitmap s w h = 
      let rec read_row s w = match w with
      | 0 -> []
      | w' -> 
        match%bitstring read_bitstring s 1 with
        | {| b : 8 * 1 : littleendian |} -> b :: read_row s (w-1)
      in
      match h with
      | 0 -> []
      | h -> read_row s w :: read_bitmap s w (h - 1)
    in
    let rec read_palette s n = match n with
    | 0 -> []
    | n -> 
      match%bitstring read_bitstring s 3 with
      | {| 
        r : 8 * 1 : littleendian;
        g : 8 * 1 : littleendian;
        b : 8 * 1 : littleendian 
        |} -> (r, g, b) :: read_palette s (n - 1)
    in
    match%bitstring read_bitstring s 12 with
    | {| 
      skip : 8 * 4 : string;
      width : 8 * 4 : littleendian;
      height : 8 * 4 : littleendian
    |} -> {
      width= Int32.to_int width;
      height= Int32.to_int height;
      palette= read_palette s 256;
      bitmap= read_bitmap s (Int32.to_int width) (Int32.to_int height);
    }
  ;;
  
  let rec read_all_from_stream ic n = match n with
  | 0l -> []
  | n -> (from_stream ic) :: (read_all_from_stream ic @@ Int32.sub n Int32.one)
  ;;
end


type t = {
  version   : Version.t;
  points    : Point.t list;
  districts : District.t list;
  objects   : Obj.t list;
  textures  : Texture.t list;
};;


let load path = 
  let ic = open_in_bin path in
  match%bitstring read_bitstring ic 12 with 
  | {| 
    version : 1 * 8 : littleendian;
    identif : 3 * 8 : string;
    npoints : 4 * 8 : littleendian;
    ndistricts : 4 * 8 : littleendian
  |} -> 
    if identif <> "IBM" then raise LoadError;
    let points = Point.read_all_from_stream ic npoints in
    let districts = District.read_all_from_stream ic ndistricts in 
    (* read objects *)
    match%bitstring read_bitstring ic 4 with 
    | {| nob : 4 * 8 : littleendian |} -> 
      let objs = Obj.read_all_from_stream ic nob in 
    (* read textures *)
    match%bitstring read_bitstring ic 4 with 
    | {| not : 4 * 8 : littleendian |} -> 
      let texts = Texture.read_all_from_stream ic not in {
        version= Version.of_byte version;
        points= points;
        districts= districts;
        objects= objs;
        textures= texts;
      }
;;


let to_obj mbi path = 
  let rec write_points oc ps = match ps with
  | [] -> ()
  | (x,y,z) :: ps' -> fprintf oc "v  %f %f %f\n" x y z; write_points oc ps'
  in
  let rec write_districts oc (ds:District.t list) = match ds with
  | [] -> ()
  | d :: ds' ->
    let rec write_district_points oc dps = match dps with
    | [] -> fprintf oc "\n"
    | (p, u, v) :: dps' -> fprintf oc "%d " @@ p + 1; write_district_points oc dps'
    in
    if List.length d.point_uvs > 2 then (
      fprintf oc "f ";
      write_district_points oc d.point_uvs;
      write_districts oc ds'
    ) else (write_districts oc ds')
  in
  let oc = open_out @@ path ^ "/scenery.obj" in
  fprintf oc "# NumPoint: %d\n" @@ List.length mbi.points;
  fprintf oc "# NumDistrict: %d\n" @@ List.length mbi.districts;
  fprintf oc "# NumObject: %d\n" @@ List.length mbi.objects;
  fprintf oc "# NumTextures: %d\n" @@ List.length mbi.textures;
  write_points oc @@ List.rev mbi.points;
  write_districts oc mbi.districts;
  close_out oc
;;


(*
            Dim MtlName As String = GetMainFileName(ObjPath)
            Dim MtlBlocks As New List(Of String)
            For n = 0 To m.Textures.Length - 1
                Dim mt As New StringBuilder
                m.ExportToGif(n, GetPath(Dir, n & ".gif"))
                mt.AppendLine(String.Format("newmtl {0}", n))
                mt.AppendLine("illum 0")
                mt.AppendLine(String.Format("map_Kd {0}", n & ".gif"))
                mt.AppendLine("Ka 0.2 0.2 0.2")
                mt.AppendLine("Kd 0.8 0.8 0.8")
                MtlBlocks.Add(mt.ToString)
            Next
            Using Mtl As New StreamEx(GetPath(Dir, MtlName & ".mtl"), FileMode.Create, FileAccess.ReadWrite)
                Mtl.Write(System.Text.Encoding.UTF8.GetBytes(String.Join(System.Environment.NewLine, MtlBlocks.ToArray)))
            End Using

            Dim Blocks As New List(Of String)
            Dim h As New StringBuilder
            h.AppendLine(String.Format("# NumPoint: {0}", m.NumPoint))
            h.AppendLine(String.Format("# NumDistrict: {0}", m.NumDistrict))
            h.AppendLine(String.Format("# NumObject: {0}", m.NumObject))
            h.AppendLine(String.Format("# NumTexture: {0}", m.NumTexture))
            Blocks.Add(h.ToString)

            Blocks.Add(String.Format("mtllib {0}", MtlName & ".mtl") & System.Environment.NewLine)

            Dim v As New StringBuilder
            For n = 0 To m.NumPoint - 1
                Dim p = m.Points(n)
                v.AppendLine(String.Format(System.Globalization.NumberFormatInfo.InvariantInfo, "v {0:r} {1:r} {2:r}", p.x, p.y, p.z))
            Next

            Blocks.Add(v.ToString)

            Dim vt As New StringBuilder
            Dim vtIndex As Integer
            Dim f As New StringBuilder
            For n = 0 To m.NumObject - 1
                Dim o = m.Objects(n)
                f.AppendLine(String.Format("g {0}", o.ObjectName))
                Dim CurrentTexture As Integer = -1
                For i = o.StartDistrictIndex To o.NextStartDistrictIndex - 1
                    Dim d = m.Districts(i)
                    If d.TextureID <> CurrentTexture Then
                        f.AppendLine(String.Format("usemtl {0}", d.TextureID))
                        CurrentTexture = d.TextureID
                    End If
                    If d.n > 0 Then
                        f.Append("f")
                        For k = d.n - 1 To 0 Step -1
                            f.Append(String.Format(" {0}/{1}", d.Point(k) + 1, vtIndex + 1))
                            vt.AppendLine(String.Format(System.Globalization.NumberFormatInfo.InvariantInfo, "vt {0:r} {1:r}", d.U(k), 1.0F - d.V(k)))
                            vtIndex += 1
                        Next
                        f.AppendLine()
                    End If
                Next
            Next

            Blocks.Add(vt.ToString)
            Blocks.Add(f.ToString)

            Using Obj As New StreamEx(ObjPath, FileMode.Create, FileAccess.ReadWrite)
                Obj.Write(System.Text.Encoding.UTF8.GetBytes(String.Join(System.Environment.NewLine, Blocks.ToArray)))
            End Using
*)