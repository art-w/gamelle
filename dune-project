(lang dune 3.15)

(generate_opam_files true)

(name gamelle)
(version 0.1)
(source (github art-w/gamelle))
(authors "Arthur Wendling" "Emile Trotignon")
(maintainers "art.wendling@gmail.com")
(license MIT)

(package
  (name gamelle)
  (synopsis "Gamelle")
  (depends
    (ocaml (>= 4.0.8))
    (js_of_ocaml (>= 5.6.0))
    (brr (= 0.0.6))
    (gg (>= 1.0.0))
    (color (>= 0.2.0))
    (ctypes (>= 0.21.1))
    (result (>= 1.5))
    (crunch (>= 3.3.1))
    (ppx_blob (>= 0.7.2))
    (uuseg (>= 15.1.0))
    (imagelib (>= 20221222)))
  (depopts
    (inotify (>= 2.1))
    (tsdl (>= 1.0.0))
    (tsdl-image (>= 0.6))
    (tsdl-mixer (>= 0.6))
    (tsdl-ttf (>= 0.6))
    conf-sdl2-gfx)
)

(package (name conf-sdl2-gfx) (allow_empty))
