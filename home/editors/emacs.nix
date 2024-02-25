{pkgs, ...}: let
  package = pkgs.emacs-unstable-pgtk;
  configDir = pkgs.runCommand "" {} ''
    cp ${./emacs.org} emacs.org
    ${pkgs.lib.getExe package} -Q --batch \
      -l org emacs.org \
      -f org-babel-tangle

    cat <<EOF > .init.el
      ${pkgs.rice.def.elisp}
    EOF
    cat init.el >> .init.el
    mv .init.el init.el

    mkdir -p $out
    cp *.el $out
  '';
in {
  programs.emacs = {
    enable = true;
    package = pkgs.emacsWithPackagesFromUsePackage {
      inherit package;
      config = "${configDir}/init.el";

      defaultInitFile = true;
      alwaysEnsure = true;

      extraEmacsPackages = epkgs:
        with epkgs; [
          treesit-grammars.with-all-grammars
        ];

      override = final: prev: {
        smartparens-mode = prev.smartparens;
      };
    };
  };

  xdg.configFile = {
    "emacs/doom-rice-theme.el".text = ''
      ${pkgs.rice.def.elisp}
      ${builtins.readFile "${configDir}/doom-rice-theme.el"}
    '';
    "emacs/early-init.el".source = "${configDir}/early-init.el";
  };

  home.sessionVariables.EDITOR = "emacs";
}
