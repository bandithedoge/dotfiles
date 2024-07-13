{pkgs, ...}: let
  emacsPackage = pkgs.emacs-unstable-pgtk;
  configDir = pkgs.runCommand "" {} ''
    cp ${./emacs.org} emacs.org
    ${pkgs.lib.getExe emacsPackage} -Q --batch \
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
  # home = {
  #   packages = with pkgs; [
  #     (pkgs.emacsWithPackagesFromUsePackage {
  #       package = emacsPackage;
  #       config = "${configDir}/init.el";
  #
  #       defaultInitFile = true;
  #       alwaysEnsure = true;
  #
  #       extraEmacsPackages = epkgs:
  #         with epkgs; [
  #           treesit-grammars.with-all-grammars
  #         ];
  #
  #       override = final: prev: {
  #         inherit (pkgs.bandithedoge.emacsPackages)
  #           clangd-inactive-regions
  #           eglot-booster
  #           eglot-ltex
  #           eglot-yaml
  #           indent-bars
  #           once
  #           smartparens;
  #       };
  #     })
  #
  #     emacs-lsp-booster
  #   ];
  #   sessionVariables.EDITOR = "emacs";
  # };
  #
  # xdg.configFile = {
  #   "emacs/init.el".source = "${configDir}/init.el";
  #   "emacs/doom-rice-theme.el".text = ''
  #     ${pkgs.rice.def.elisp}
  #     ${builtins.readFile "${configDir}/doom-rice-theme.el"}
  #   '';
  #   "emacs/early-init.el".source = "${configDir}/early-init.el";
  # };
}
