open OpamTypes

module Opkg = OpamPackage
module Ofml = OpamFormula

let opam_url pkg =
  Printf.sprintf "http://opam.ocaml.org/packages/%s/%s/"
    (Opkg.name_to_string pkg)
    (Opkg.to_string pkg)


module V = Opkg

module E = struct
  type t = bool * Ofml.version_formula
  let compare = compare
  let default = false, Empty
  let to_string ((_, fml) : t) =
    Ofml.string_of_formula (fun (rel, v) ->
        Printf.sprintf "%s %s"
          (OpamPrinter.relop rel)
          (Opkg.Version.to_string v)) fml
end

module G = Graph.Persistent.Digraph.ConcreteLabeled(V)(E)

module Dot =
  Graph.Graphviz.Dot (struct
    let graph_attributes _ = [
      `Center true ;
      (* `Overlap false ; *)
    ]
    let default_vertex_attributes _ = []
    let get_subgraph _ = None
    let default_edge_attributes _ = []

    let common_attribs = [
      `Dir `Forward ;
    ]
    let edge_attributes (_src, fml, _dest) = match fml with
      | opt, _fml ->
        `Label (E.to_string fml) ::
        if not opt then common_attribs
        else `Color 0x999999 :: `Style `Dashed :: common_attribs

    let vertex_name v =
      "\""^Opkg.name_to_string v^"\""

    (* Make the output pretty! *)
    let vertex_attributes v = [
      `Url (opam_url v) ;
    ]

    include G
  end)

let is_not_ignored =
  let set =
    Opkg.Name.Set.of_list @@
    List.map Opkg.Name.of_string [
      "ocamlfind" ;
      (* "camlp4" ; *)
    ]
  in
  fun n -> not @@ Opkg.Name.Set.mem n.name set

let build_graph
    st
    packages
    (depends:filtered_formula package_map)
    (depots:filtered_formula package_map) =
  (* let apply_package default f name =
   *   try
   *     let pkg = Opkg.max_version packages name in
   *     if Opkg.Name.Set.mem name ignore_set then default
   *     else f pkg
   *   with Not_found -> default
   * in *)

  let find_dependencies ~opt depends pkg =
    let env = OpamPackageVar.resolve st in
    let deps =
      Opkg.Map.find pkg depends
      |> OpamPackageVar.filter_depends_formula ~env
    in
    let f (pkgname, fml) =
      G.E.create pkg (opt, fml) (OpamSwitchState.get_package st pkgname)
    in
    Ofml.fold_left (fun l x -> f x :: l) [] deps
  in

  let rec complete_dependencies g pkg =
    let edges =
      find_dependencies ~opt:false depends pkg
      @ find_dependencies ~opt:true depots pkg
      |> List.filter
        (fun v -> is_not_ignored @@ G.E.dst v && is_not_ignored @@ G.E.src v)
    in
    let new_pkgs =
      List.map G.E.dst edges
      |> List.filter (fun v -> not @@ G.mem_vertex g v)
    in
    let g = List.fold_left G.add_edge_e g edges in
    List.fold_left complete_dependencies g new_pkgs
  in

  List.fold_left complete_dependencies G.empty packages

let showdot graph =
  let f file oc x =
    Dot.output_graph oc x;
    Bos.OS.Cmd.run Bos.Cmd.(v "xdot" % p file)
  in
  Bos.OS.File.with_tmp_oc ~mode:0o644 "opam-dot%s.dot" f graph


let with_opam f =
  let root = OpamStateConfig.opamroot () in
  OpamFormatConfig.init () ;
  let _config = OpamStateConfig.load_defaults root in
  OpamStd.Config.init () ;
  OpamRepositoryConfig.init () ;
  OpamStateConfig.init ~root_dir:root () ;
  OpamGlobalState.with_ `Lock_none @@ fun gt ->
  OpamSwitchState.with_ `Lock_none gt @@ f

let dot pkgs =
  with_opam @@ fun st ->
  let pkgs = List.map (OpamSwitchState.get_package st) pkgs in
  let {u_depends ; u_depopts} =
    OpamSwitchState.universe st ~requested:Opkg.Name.Set.empty Query in
  let g = build_graph st pkgs u_depends u_depopts in
  showdot g


let _ = dot @@ [ Opkg.Name.of_string Sys.argv.(1) ]
