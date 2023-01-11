;; -*- no-byte-compile: t; -*-
;;; modules/custom/ocaml/packages.el

(package! tuareg :pin "ad8a688b7e2aeeafc320a845f86cdd9aa7c971ce")

(package! merlin :pin "be753d9412387aedcf32aba88a1be9bcd33d97ba")
(package! merlin-eldoc :pin "db7fab1eddfe34781b7e79694f8923b285698032")
(package! merlin-company :pin "be753d9412387aedcf32aba88a1be9bcd33d97ba")

(package! ocp-indent :pin "7c4d434132cebc15a8213c8be9e7323692eb0a2b")

(package! utop :pin "bbd9a6ed45c8de8d50adcd5d4d845bdba212db63")

(package! ocamlformat
  :recipe (:host github :repo "ocaml-ppx/ocamlformat" :files ("emacs/*.el"))
  :pin "9cbd8150c28f70ba6315c347a833a4ac8c85c481")

(package! dune
  :recipe (:host github :repo "ocaml/dune" :files ("editor-integration/emacs/*.el"))
  :pin "3df932f7f91ea68c3fee789f133b4aa8f9bea807")
