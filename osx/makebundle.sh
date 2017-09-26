set -e
set -x
export PATH=$PATH:$HOME/.local/bin
rm -rf .xmimsim-gui.app 
gtk-mac-bundler xmimsim.bundle || exit 1
gtk-update-icon-cache-3.0 --include-image-data --quiet XMI-MSIM.app/Contents/Resources/share/icons/hicolor
gtk-update-icon-cache-3.0 --include-image-data --quiet XMI-MSIM.app/Contents/Resources/share/icons/Adwaita
update-mime-database XMI-MSIM.app/Contents/Resources/share/mime
mkdir -p XMI-MSIM.app/Contents/Resources/etc/gtk-3.0
cp settings.ini XMI-MSIM.app/Contents/Resources/etc/gtk-3.0/
mkdir -p XMI-MSIM.app/Contents/Library/QuickLook
cp -a ../build/osx/quicklook/xmi-msim.qlgenerator XMI-MSIM.app/Contents/Library/QuickLook/
cp /usr/local/share/xmimsim/xmimsimdata.h5 XMI-MSIM.app/Contents/Resources/
cp -a ~/Downloads/Gnome\ OSX-III-1.0 XMI-MSIM.app/Contents/Resources/share/themes/Gnome-OSX-III-1.0
