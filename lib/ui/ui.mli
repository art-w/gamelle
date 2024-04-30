module type S = Ui_intf.S

include S with type io := Gamelle_backend.io
