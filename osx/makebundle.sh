set -e
set -x
export PATH=$PATH:$HOME/.local/bin
rm -rf .xmimsim-gui.app 
gtk-mac-bundler xmimsim.bundle || exit 1
#find XMI-MSIM.app -name '*.svg' | xargs rm
gtk-update-icon-cache-3.0 --include-image-data --quiet XMI-MSIM.app/Contents/Resources/share/icons/hicolor
gtk-update-icon-cache-3.0 --include-image-data --quiet XMI-MSIM.app/Contents/Resources/share/icons/Adwaita
update-mime-database XMI-MSIM.app/Contents/Resources/share/mime
mkdir -p XMI-MSIM.app/Contents/Resources/etc/gtk-3.0
cp settings.ini XMI-MSIM.app/Contents/Resources/etc/gtk-3.0/
mkdir -p XMI-MSIM.app/Contents/Library/QuickLook
cp -a ../build/osx/quicklook/xmi-msim.qlgenerator XMI-MSIM.app/Contents/Library/QuickLook/
#cp libintl.8.dylib libxrl.7.dylib XMI-MSIM.app/Contents/Resources/lib/
#install_name_tool -change /opt/local/lib/libiconv.2.dylib @executable_path/../Resources/lib/libiconv.2.dylib XMI-MSIM.app/Contents/Resources/lib/libintl.8.dylib
#cp /usr/local/share/xmimsim/xmimsimdata.h5 XMI-MSIM.app/Contents/Resources/
