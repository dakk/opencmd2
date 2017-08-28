open Bitstring;;
open Printf;;

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
    let buff = Bytes.make 12 '0' in
    let _ = input ic buff 0 12 in
    (* TODO these are floats!*)
    match%bitstring bitstring_of_string buff with
    | {| 
      x : 8 * 4 : littleendian;
      y : 8 * 4 : littleendian;
      z : 8 * 4 : littleendian
    |} -> (Int32.float_of_bits x, Int32.float_of_bits y, Int32.float_of_bits z)
  ;;

  let rec read_all_from_stream ic n = match n with
  | 0 -> []
  | n -> (from_stream ic) :: read_all_from_stream ic (n-1)
  ;;
end

module District = struct 
  type t = {
    attribute : int;
    texture_id: int;
    point_uvs : (int*int*int) list
  };;

  let from_stream s = 
    let rec point_uvs ic n = match n with
    | 0 -> []
    | n -> 
      let buff = Bytes.make 6 '0' in
      let _ = input ic buff 0 6 in
      (* TODO these are floats!*)
      match%bitstring bitstring_of_string buff with
      | {| 
        point : 8 * 2 : littleendian;
        u : 8 * 2 : littleendian;
        v : 8 * 2 : littleendian
      |} -> 
        (point, u, v) :: point_uvs ic (n-1)
    in
    let buff = Bytes.make 2 '0' in
    let _ = input s buff 0 2 in
    (* TODO these are floats!*)
    match%bitstring bitstring_of_string buff with
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
  | 0 -> []
  | n -> (from_stream ic) :: read_all_from_stream ic (n-1)
  ;;
end


module Obj = struct
  type t = {
    name                : string;
    start_district      : int32;
    next_start_district : int32;
  };;

  let from_stream s = 
    let buff = Bytes.make 52 '0' in
    let _ = input s buff 0 52 in
    match%bitstring bitstring_of_string buff with
    | {| 
      name : 8 * 44 : string;
      start_d : 8 * 4 : littleendian;
      next_start_d : 8 * 4 : littleendian
    |} -> { name=name; start_district= start_d; next_start_district=next_start_d }
  ;;

  let rec read_all_from_stream ic n = match n with
  | 0 -> []
  | n -> (from_stream ic) :: read_all_from_stream ic (n-1)
  ;;
end


module Texture = struct
  type t = {
    width: int;
    height: int;
    palette: int32 list;
    rectangle: int list list;
  };;
  
  let from_stream s = 
    let rec read_rectangle s w h = 
      let explode ss =
        let rec exp i l = if i < 0 then l else exp (i - 1) ((Char.code ss.[i]) :: l) in
        exp (String.length ss - 1) []
      in
      match h with
      | 0 -> []
      | h ->
        let buff = Bytes.make w '0' in
        let _ = input s buff 0 w in
        match%bitstring bitstring_of_string buff with
        | {| line : 8 * w : string |} ->
          explode line :: read_rectangle s w (h-1)
    in
    let rec read_palette s n = match n with
    | 0 -> []
    | n -> 
      let buff = Bytes.make 3 '0' in
      let _ = input s buff 0 3 in
      match%bitstring bitstring_of_string buff with
      | {| pal : 8 * 3 : littleendian |} -> pal :: read_palette s (n - 1)
    in
    let buff = Bytes.make 52 '0' in
    let _ = input s buff 0 52 in
    (* TODO these are floats!*)
    match%bitstring bitstring_of_string buff with
    | {| 
      skip : 8 * 4 : string;
      width : 8 * 4 : littleendian;
      height : 8 * 4 : littleendian
    |} -> {
      width= Int32.to_int width;
      height= Int32.to_int height;
      palette= read_palette s 256;
      rectangle= read_rectangle s (Int32.to_int width) (Int32.to_int height);
    }
  ;;
  
  let rec read_all_from_stream ic n = match n with
  | 0 -> []
  | n -> (from_stream ic) :: read_all_from_stream ic (n-1)
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
  let buff = Bytes.make 13 '0' in
  let _ = input ic buff 0 13 in
  match%bitstring bitstring_of_string buff with 
  | {| 
    version : 1 * 8 : littleendian;
    identif : 3 * 8 : string;
    npoints : 4 * 8 : littleendian;
    ndistricts : 4 * 8 : littleendian
  |} -> 
    Printf.printf "%d %s %d %d\n%!" version identif (Int32.to_int npoints) (Int32.to_int ndistricts); 
    let points = Point.read_all_from_stream ic @@ Int32.to_int npoints in
    let districts = District.read_all_from_stream ic @@ Int32.to_int ndistricts in
    (* read objects *)
    let buff = Bytes.make 4 '0' in
    let _ = input ic buff 0 4 in
    match%bitstring bitstring_of_string buff with 
    | {| nob : 4 * 8 : littleendian |} -> 
      let objs = Obj.read_all_from_stream ic @@ Int32.to_int nob in
    (* read textures *)
    let buff = Bytes.make 4 '0' in
    let _ = input ic buff 0 4 in
    match%bitstring bitstring_of_string buff with 
    | {| not : 4 * 8 : littleendian |} -> 
      let texts = Texture.read_all_from_stream ic @@ Int32.to_int not in {
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
  | (x,y,z) :: ps' -> fprintf oc "v %f %f %f\n" x y z; write_points oc ps'
  in

  let oc = open_out @@ path ^ "/scenery.obj" in
  fprintf oc "# NumPoint: %d\n" @@ List.length mbi.points;
  fprintf oc "# NumDistrict: %d\n" @@ List.length mbi.districts;
  fprintf oc "# NumObject: %d\n" @@ List.length mbi.objects;
  fprintf oc "# NumTextures: %d\n" @@ List.length mbi.textures;
  write_points oc mbi.points;
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