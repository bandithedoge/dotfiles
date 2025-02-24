{...}: {
  sops = {
    age.keyFile = "/home/bandithedoge/.age-key.txt";
    defaultSopsFile = ./secrets.yaml;
    defaultSopsFormat = "yaml";

    secrets = {
      githubToken = {};

      "u-he/ace" = {};
      "u-he/bazille" = {};
      "u-he/colourcopy" = {};
      "u-he/diva" = {};
      "u-he/filterscape" = {};
      "u-he/hive" = {};
      "u-he/mfm2" = {};
      "u-he/presswerk" = {};
      "u-he/repro" = {};
      "u-he/satin" = {};
      "u-he/twangstrom" = {};
      "u-he/uhbik" = {};
      "u-he/zebra2" = {};
    };
  };
}
