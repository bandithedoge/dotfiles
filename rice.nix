rec {
  base00 = "#21242b";
  base01 = "#282c34";
  base02 = "#3f444a";
  base03 = "#5B6268";
  base04 = "#9ca0a4";
  base05 = "#bbc2cf";
  base06 = "#DFDFDF";
  base07 = "#E6E6E6";
  base08 = "#ff6c6b";
  base09 = "#da8548";
  base0A = "#ECBE7B";
  base0B = "#98be65";
  base0C = "#46D9FF";
  base0D = "#51afef";
  base0E = "#c678dd";
  base0F = "#a9a1e1";
  base10 = "#1c1f24";
  base11 = "#1B2229";
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
