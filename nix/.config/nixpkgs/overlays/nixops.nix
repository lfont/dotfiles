self: super: {
  nixops = super.nixops.overrideDerivation (
    old: {
      patchPhase = ''
        substituteInPlace nix/eval-machine-info.nix \
            --replace 'system.nixosVersion' 'system.nixos.version'
      '';
    }
  );
}
