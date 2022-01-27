rec {
  base00 = "#202020";
  base01 = "#2a2827";
  base02 = "#2e2c2b";
  base03 = "#7c6f64";
  base04 = "#c5b18d";
  base05 = "#d4be98";
  base06 = "#ddc7a1";
  base07 = "#ddc7a1";
  base08 = "#ea6962";
  base09 = "#e78a4e";
  base0A = "#d8a657";
  base0B = "#a9b665";
  base0C = "#89b482";
  base0D = "#7daea3";
  base0E = "#d3869b";
  base0F = "#d65d0e";
  base10 = "#131414";
  base11 = "#070808";
  base12 = base08;
  base13 = base0A;
  base14 = base0B;
  base15 = base0C;
  base16 = base0D;
  base17 = base0E;

  monoFont = "JetBrainsMono Nerd Font";
  uiFont = "Roboto Condensed";

  terminal = "kitty";
  wm = "river";
  menu = "rofi -show drun";

  def = {
    lua = ''
      base00 = "${base00}"
      base01 = "${base01}"
      base02 = "${base02}"
      base03 = "${base03}"
      base04 = "${base04}"
      base05 = "${base05}"
      base06 = "${base06}"
      base07 = "${base07}"
      base08 = "${base08}"
      base09 = "${base09}"
      base0A = "${base0A}"
      base0B = "${base0B}"
      base0C = "${base0C}"
      base0D = "${base0D}"
      base0E = "${base0E}"
      base0F = "${base0F}"

      base10 = "${base10}"
      base11 = "${base11}"
      base12 = "${base12}"
      base13 = "${base13}"
      base14 = "${base14}"
      base15 = "${base15}"
      base16 = "${base16}"
      base17 = "${base17}"

      monoFont = "${monoFont}"
      uiFont = "${uiFont}"
    '';
  };
}
