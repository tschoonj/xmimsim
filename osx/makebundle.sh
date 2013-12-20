rm -rf .xmimsim-gui.app 
gtk-mac-bundler xmimsim.bundle || exit 1
gtk-update-icon-cache --quiet XMI-MSIM.app/Contents/Resources/share/icons/hicolor
update-mime-database XMI-MSIM.app/Contents/Resources/share/mime
cp libintl.8.dylib libxrl.7.dylib XMI-MSIM.app/Contents/Resources/lib/
install_name_tool -change /opt/local/lib/libiconv.2.dylib @executable_path/../Resources/lib/libiconv.2.dylib XMI-MSIM.app/Contents/Resources/lib/libintl.8.dylib
