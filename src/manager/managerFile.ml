(**************************************************************************)
(*                                                                        *)
(*    Copyright 2014-2015, Gregoire Henry, OCamlPro                       *)
(*                                                                        *)
(*  All rights reserved.This file is distributed under the terms of the   *)
(*  GNU Lesser General Public License version 3.0 with linking            *)
(*  exception.                                                            *)
(*                                                                        *)
(*  OPAM is distributed in the hope that it will be useful, but WITHOUT   *)
(*  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY    *)
(*  or FITNESS FOR A PARTICULAR PURPOSE.See the GNU General Public        *)
(*  License for more details.                                             *)
(*                                                                        *)
(**************************************************************************)

open OpamStd.Op
open ManagerTypes

module Pp = OpamFormat.Pp
open Pp.Op

module ConfigSyntax = struct

  let internal = "manager/global_config"

  type t = ManagerTypes.config

  let manager_version t = t.manager_version
  let known_roots t = t.known_roots
  let default_root_name t = t.default_root_name
  let wrapper_binary t = t.wrapper_binary

  let with_manager_version manager_version t = { t with manager_version }
  let with_known_roots known_roots t = { t with known_roots }
  let with_default_root_name default_root_name t = { t with default_root_name }
  let with_wrapper_binary wrapper_binary t = { t with wrapper_binary }

  let empty = {
    manager_version = ManagerVersion.current;
    default_root_name = "**unconfigured**";
    known_roots = [];
    wrapper_binary = OpamFilename.of_string "/unconfigured";
  }

  let pp_known_root :
    (OpamTypes.value, ManagerTypes.root) Pp.t =
    Pp.V.map_pair
      Pp.V.string
      (Pp.V.string -|
       Pp.of_module "directory"
         (module OpamFilename.Dir: Pp.STR with type t = OpamFilename.Dir.t)) -|
    Pp.of_pair
      "known_root"
      ((fun (root_name, root_path) ->
         ManagerRoot.create_opam_root root_name root_path),
       (fun { root_name; root_kind = Opam_root { opam_root_path ; _ } } ->
          (root_name, opam_root_path)))
  let pp_known_roots :
    (OpamTypes.value, ManagerTypes.root list) Pp.t =
    Pp.V.map_list ~depth:2 pp_known_root

  let fields =
    [
      "opam-manager-version", Pp.ppacc
        with_manager_version manager_version
        (Pp.V.string -|
         Pp.of_module "opam-manager-version"
           (module ManagerVersion)) ;
      "default-root", Pp.ppacc
        with_default_root_name default_root_name
        Pp.V.string ;
      "known-root", Pp.ppacc
        with_known_roots known_roots
        pp_known_roots ;
      "wrapper-binary", Pp.ppacc
        with_wrapper_binary wrapper_binary
        (Pp.V.string -|
         Pp.of_module "file"
           (module OpamFilename: Pp.STR with type t = OpamFilename.t))
    ]

  let pp =
    let name = internal in
    ( Pp.I.map_file
      @@ Pp.I.check_fields ~name fields
         -| Pp.I.fields ~name ~empty fields )
    (* -| *)
    (* Pp.check ~name (fun t -> t.switch <> empty.switch) *)
      (* ~errmsg:"missing switch" *)

end

module Config = struct
  include ConfigSyntax
  include OpamFile.SyntaxFile(ConfigSyntax)
end
