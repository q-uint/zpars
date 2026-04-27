{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
  };

  outputs =
    { self, nixpkgs }:
    let
      system = "aarch64-darwin";
      pkgs = nixpkgs.legacyPackages.${system};
      # Pinned to a 0.17.0-dev nightly so `zig build fuzz --fuzz` works
      # on aarch64-macos. 0.16.0 stable shipped with a stdlib regression
      # that breaks the test runner's fuzz rebuild path (writeStackTrace
      # vs builtin.StackTrace mismatch); upstream master fixed it but no
      # 0.16.x patch has been released. To bump, pick a build from
      # https://ziglang.org/download/index.json (the "master" entry) and
      # update both fields below. URL pattern differs between dev and
      # stable releases (`/builds/` vs `/download/{version}/`), so
      # going back to stable also means flipping the URL prefix.
      zig =
        let
          version = "0.17.0-dev.135+9df02121d";
        in
        pkgs.stdenvNoCC.mkDerivation {
          pname = "zig";
          inherit version;
          src = pkgs.fetchurl {
            url = "https://ziglang.org/builds/zig-aarch64-macos-${version}.tar.xz";
            sha256 = "b34f603f291eb25ca80c4d2233106d080ce8d39929cb0299977db614c7a0ac6e";
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
