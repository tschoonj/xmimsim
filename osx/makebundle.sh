rm -rf .xmimsim-gui.app 
gtk-mac-bundler xmimsim.bundle || exit 1
update-mime-database XMI-MSIM.app/Contents/Resources/share/mime
