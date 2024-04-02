{...}: {
  sops = {
    age.keyFile = "/home/bandithedoge/.age-key.txt";
    defaultSopsFile = ./secrets.yaml;
    defaultSopsFormat = "yaml";

    secrets = {
      github-token = {};
    };
  };
}
