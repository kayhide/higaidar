{ pkgs ? import ./. {}
}:

pkgs.mkShell {
  buildInputs = with pkgs; [
    libiconv
    zlib
    ruby_2_6

    nodejs
    yarn
    purescript
    spago
    pscid
    pulp

    mysql
    libmysqlclient
  ];
}
