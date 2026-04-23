{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
  };

  outputs =
    { self, nixpkgs }:
    let
      system = "aarch64-darwin";
      pkgs = nixpkgs.legacyPackages.${system};
      zig = pkgs.stdenvNoCC.mkDerivation {
        pname = "zig";
        version = "0.16.0";
        src = pkgs.fetchurl {
          url = "https://ziglang.org/download/0.16.0/zig-aarch64-macos-0.16.0.tar.xz";
          sha256 = "b23d70deaa879b5c2d486ed3316f7eaa53e84acf6fc9cc747de152450d401489";
        };
        dontConfigure = true;
        dontBuild = true;
        dontFixup = true;
        installPhase = ''
          mkdir -p $out/bin $out/lib
          cp -r lib/* $out/lib/
          cp zig $out/bin/zig
        '';
      };
    in
    {
      devShells.${system}.default = pkgs.mkShell {
        buildInputs = [
          zig
          pkgs.nodejs
          pkgs.vsce
          # VSCode extension build: installs deps from editors/vsx/package.json
          # and compiles TypeScript via `bunx tsc`.
          pkgs.bun
        ];
      };
    };
}
