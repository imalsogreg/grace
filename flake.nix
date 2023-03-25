{
  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/22.05;
    utils.url = github:numtide/flake-utils;
  };

  outputs = { nixpkgs, utils, ... }:
    utils.lib.eachDefaultSystem (system:
      let withCompiler = compiler:
            let overlay = pkgsNew: pkgsOld: {
                  haskell = pkgsOld.haskell // {
                    packages = pkgsOld.haskell.packages // {
                      "${compiler}" = pkgsOld.haskell.packages."${compiler}".override (old: {
                        overrides =
                          let
                            oldOverrides = old.overrides or (_: _: {});

                            manualOverrides = haskellPackagesNew: haskellPackagesOld: {
                              ghcjs-fetch =
                                pkgsNew.haskell.lib.addBuildDepend
                                  (pkgsNew.haskell.lib.doJailbreak
                                    (pkgsNew.haskell.lib.dontCheck haskellPackagesOld.ghcjs-fetch)
                                  )
                                  [ haskellPackagesNew.ghcjs-base ];

                              grace =
                                let
                                  drv =
                                    pkgsNew.haskell.lib.overrideCabal
                                      haskellPackagesOld.grace
                                      (old: {
                                        doCheck = false;

                                        src =
                                          pkgsNew.lib.cleanSourceWith
                                            { inherit (old) src;

                                              filter = path: type:
                                                    pkgsNew.lib.cleanSourceFilter path type
                                                &&  (!((pkgsNew.lib.hasPrefix "result" (baseNameOf path) && type == "symlink")
                                                    || (pkgsNew.lib.hasSuffix ".nix" (baseNameOf path) && type == "regular")
                                                    || (pkgsNew.lib.hasSuffix ".md" (baseNameOf path) && type == "regular")
                                                    || (baseNameOf path == "cabal.project.local" && type == "regular")
                                                    || (baseNameOf path == "dist" && type == "directory")
                                                    || (baseNameOf path == "dist-newstyle" && type == "directory")
                                                    || (baseNameOf path == "examples" && type == "directory")
                                                    || (baseNameOf path == "prelude" && type == "directory")
                                                    || (baseNameOf path == "website" && type == "directory")
                                                    ));
                                            };
                                      });

                                in
                                  if compiler == "ghcjs"
                                  then
                                    pkgsNew.haskell.lib.overrideCabal drv
                                      (old: {
                                        postInstall = (old.postInstall or "") +
                                        ''
                                        ${pkgsNew.closurecompiler}/bin/closure-compiler $out/bin/try-grace.jsexe/all.js --jscomp_off=checkVars --externs=$out/bin/try-grace.jsexe/all.js.externs > $out/bin/try-grace.jsexe/all.min.js
                                        '';
                                      })
                                  else
                                    drv;

                              aeson = haskellPackagesNew.aeson_1_5_6_0;

                              haskeline = haskellPackagesNew.haskeline_0_8_2;

                              insert-ordered-containers =
                                pkgsNew.haskell.lib.dontCheck
                                  haskellPackagesOld.insert-ordered-containers;

                              prettyprinter-ansi-terminal =
                                pkgsNew.haskell.lib.dontCheck
                                  haskellPackagesOld.prettyprinter-ansi-terminal;

                              text-short =
                                pkgsNew.haskell.lib.dontCheck
                                  haskellPackagesOld.text-short;

                              vector =
                                pkgsNew.haskell.lib.dontCheck haskellPackagesOld.vector;

                            };

                            sourceOverrides = pkgsNew.haskell.lib.packageSourceOverrides {
                              grace = ./.;
                            };

                          in
                            pkgsNew.lib.fold pkgsNew.lib.composeExtensions oldOverrides
                              (   [ sourceOverrides ]
                              ++  pkgsNew.lib.optional (compiler == "ghcjs") manualOverrides
                              );
                      });
                    };
                  };

                  website = pkgsNew.runCommand "try-grace" { } ''
                    mkdir -p $out/prelude
                    ${pkgsNew.rsync}/bin/rsync --archive ${./website}/ $out
                    ${pkgsNew.rsync}/bin/rsync --archive ${./prelude}/ $out/prelude
                    chmod -R u+w $out
                    cp ${pkgsNew.haskell.packages."${compiler}".grace}/bin/try-grace.jsexe/all.min.js $out/js
                  '';
                };
                config.allowBroken = true;
                pkgs = import nixpkgs { inherit config system; overlays = [ overlay ]; };

                grace = pkgs.haskell.packages."${compiler}".grace;

                graceMinimal =
                  pkgs.haskell.lib.justStaticExecutables
                    (grace.overrideAttrs (_: { doCheck = false; }));

                website = pkgs.website;
             in
            { inherit grace graceMinimal website pkgs; };

          withDefaultCompiler = withCompiler "ghc902";
          withghcjs = withCompiler "ghcjs";
       in
      rec {
        packages = {
          default = withDefaultCompiler.graceMinimal;
          website = withghcjs.website;
        };

        defaultPackage = packages.default;

        apps.default = {
          type = "app";
          program = "${withDefaultCompiler.graceMinimal}/bin/grace";
        };

        packages.server-container = withDefaultCompiler.pkgs.dockerTools.buildImage {
          name = "grace-server";
          created = "now";
          contents = withDefaultCompiler.pkgs.buildEnv {
            name = "image-root";
            paths = ["${withDefaultCompiler.graceMinimal}"];
            pathsToLink = ["/bin"];
          };
          config = {
            Cmd = ["${withDefaultCompiler.graceMinimal}/bin/grace" "serve" "--port" "8000"];
          };
        };

        defaultApp = apps.default;

        devShells = {
          default = withDefaultCompiler.grace.env;
          ghcjs = withghcjs.grace.env;
        };

        devShell = devShells.default;
      });

  nixConfig = {
    extra-substituters = [ "https://cache.garnix.io" ];

    extra-trusted-public-keys = [
      "cache.garnix.io:CTFPyKSLcx5RMJKfLo5EEPUObbA78b0YQ2DTCJXqr9g="
    ];
  };
}
