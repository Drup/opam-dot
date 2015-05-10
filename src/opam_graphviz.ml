open OpamTypes

module Opkg = OpamPackage
module Ofml = OpamFormula

let opam_url pkg =
  Printf.sprintf "http://opam.ocaml.org/packages/%s/%s/"
    (Opkg.name_to_string pkg)
    (Opkg.to_string pkg)


module V = Opkg

module E = struct
  type t = bool * (package_dep_flag list * Ofml.version_formula)
  let compare = compare
  let default = false, ([], Empty)
  let to_string (_, fml) =
    Ofml.string_of_formula (fun (rel, v) ->
      Printf.sprintf "%s %s"
        (Ofml.string_of_relop rel) (Opkg.Version.to_string v)) fml
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
      | _, (_, Empty) -> common_attribs
      | opt, fml ->
        (* `Label (E.to_string fml) :: *)
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

let ignore_set =
  Opkg.Name.Set.of_list @@
  List.map Opkg.Name.of_string [
    "ocamlfind" ;
    (* "camlp4" ; *)
  ]

let build_graph
    packages
    (depends:ext_formula package_map)
    (depots:ext_formula package_map) =
  let apply_package default f name =
    try
      let pkg = Opkg.max_version packages name in
      if Opkg.Name.Set.mem name ignore_set then default
      else f pkg
    with Not_found -> default
  in

  let g =
    Opkg.Set.fold
      (fun pkg g -> apply_package g (G.add_vertex g) @@ Opkg.name pkg)
      packages G.empty
  in

  let add_dependencies pkg g (dep, edge) =
    apply_package
      g (fun dep -> G.add_edge_e g (G.E.create pkg edge dep))
      dep
  in
  let add_out_edges ~opt (depends:ext_formula package_map) pkg g =
    let deps = Opkg.Map.find pkg depends in
    let dep_list =
      Ofml.fold_left (fun l (d,vc) -> (d,(opt,vc))::l) [] deps
    in
    List.fold_left
      (add_dependencies pkg)
      g
      dep_list
  in
  let g = G.fold_vertex (add_out_edges ~opt:false depends) g g in
  let g = G.fold_vertex (add_out_edges ~opt:true depots) g g in
  g

let () =
  OpamGlobals.root_dir := OpamGlobals.default_opam_dir ;
  let state = OpamState.load_state "graph" in
  let {u_installed = packages ; u_depends ; u_depopts} =
    OpamState.universe state Depends in
  let g = build_graph packages u_depends u_depopts in

  let filename = Sys.argv.(1) in
  let file = open_out filename in
  Dot.output_graph file g ;
  close_out file
