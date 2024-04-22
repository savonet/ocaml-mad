module C = Configurator.V1

let pkg_config_packages = ["mad"; "libmad"]

let () =
  C.main ~name:"mad-pkg-config" (fun c ->
      let default : C.Pkg_config.package_conf =
        { libs = ["-lmad"]; cflags = [] }
      in
      let conf =
        List.fold_left
          (fun conf package ->
            match C.Pkg_config.get c with
              | None -> conf
              | Some pc -> (
                  match C.Pkg_config.query pc ~package with
                    | None -> conf
                    | Some deps -> deps))
          default pkg_config_packages
      in
      C.Flags.write_sexp "c_flags.sexp" conf.cflags;
      C.Flags.write_sexp "c_library_flags.sexp" conf.libs)
