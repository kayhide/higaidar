{}:

let
  nodejsOverlay = self: super: {
    nodejs = super.nodejs-13_x;
    yarn = super.yarn // {
      buildInputs = [ self.nodejs ];
    };
  };

in

import <nixpkgs> {
  overlays = [
    nodejsOverlay
  ];
}
