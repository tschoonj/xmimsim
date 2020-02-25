set -e
set -x
unset G_MESSAGES_DEBUG
rm -rf .xmimsim-gui.app XMI-MSIM.app
gtk-mac-bundler xmimsim.bundle || exit 1
gtk3-update-icon-cache --include-image-data --quiet XMI-MSIM.app/Contents/Resources/share/icons/hicolor
gtk3-update-icon-cache --include-image-data --quiet XMI-MSIM.app/Contents/Resources/share/icons/Adwaita
mkdir -p XMI-MSIM.app/Contents/Resources/share/mime/packages/
update-mime-database XMI-MSIM.app/Contents/Resources/share/mime
mkdir -p XMI-MSIM.app/Contents/Resources/etc/gtk-3.0
cp settings.ini XMI-MSIM.app/Contents/Resources/etc/gtk-3.0/
mkdir -p XMI-MSIM.app/Contents/Library/QuickLook
cp -a ../build/osx/quicklook/xmi-msim.qlgenerator XMI-MSIM.app/Contents/Library/QuickLook/
cp /usr/local/share/xmimsim/xmimsimdata.h5 XMI-MSIM.app/Contents/Resources/
mkdir -p XMI-MSIM.app/Contents/Resources/share/themes
cp -a ~/github/Sierra-gtk-theme/build/Sierra* XMI-MSIM.app/Contents/Resources/share/themes/
