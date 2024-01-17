let
  pkgs = import <nixpkgs> {};
in pkgs.mkShell {
  name = "sml-tigr";
  buildInputs = with pkgs; [xorg.libX11 xorg.libX11.dev libglvnd.dev libGLU];
}
