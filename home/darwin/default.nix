{ home-manager, pkgs, lib, ... }: {
  home.activation = {
    aliasApplications = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
      app_folder=$(echo ~/Applications/home-manager);
      rm -rf $app_folder;
      mkdir -p $app_folder
      for app in $(find "$genProfilePath/home-path/Applications" -type l); do
        osascript -e "tell application \"Finder\"" -e "make new alias to file (posix file \"$app\") at (posix file \"$app_folder\")" -e "set name of result to \"$(basename $app)\"" -e "end tell"
      done
    '';
  };
}
