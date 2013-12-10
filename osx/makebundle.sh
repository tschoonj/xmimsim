rm -rf .xmimsim-gui.app 
gtk-mac-bundler xmimsim.bundle || exit 1
gtk-update-icon-cache --quiet XMI-MSIM.app/Contents/Resources/share/icons/hicolor
update-mime-database XMI-MSIM.app/Contents/Resources/share/mime
