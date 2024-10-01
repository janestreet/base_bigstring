module V = Configurator.V1

let () =
  let  conf = V.create "conf"
  in
  match
    V.ocaml_config_var conf "host"
  with
  | None -> ()
  | Some host ->
    if (String.ends_with ~suffix:"haiku" host)
    then 
      V.Flags.write_sexp "flags.sexp" ["-lroot -lgnu"]
