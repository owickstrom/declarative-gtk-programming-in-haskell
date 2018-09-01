{ compiler ? "ghc843" }:
(import ./. { inherit compiler; }).declarative-gtk-examples-shell
