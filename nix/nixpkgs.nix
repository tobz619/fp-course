import (builtins.fetchTarball{
  # Descriptive name to make the store path easier to identify
  name = "nixos-unstable-2022-09-04";
  url = "https://github.com/nixos/nixpkgs/archive/728ebb50a898afaa56c286b2b06ff93932ca9ea5.tar.gz";
  # `git ls-remote https://github.com/nixos/nixpkgs-channels nixos-unstable`
  # rev = "728ebb50a898afaa56c286b2b06ff93932ca9ea5";
}) {}
