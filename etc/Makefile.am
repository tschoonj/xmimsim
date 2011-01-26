EXTRA_DIST = be.ugent.xmi.harvester.plist.in


if OS_DARWIN
install-data-local:
	$(MKDIR_P) $(DESTDIR)$(prefix)/Library/LaunchDaemons
	$(INSTALL) be.ugent.xmi.harvester.plist $(DESTDIR)$(prefix)/Library/LaunchDaemons

uninstall-local:
	rm -f $(DESTDIR)$(prefix)/Library/LaunchDaemons/be.ugent.xmi.harvester.plist

endif
