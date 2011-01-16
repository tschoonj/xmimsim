xmimsimdir = $(datadir)/xmimsim
dist_xmimsim_DATA = xmimsim-1.0.dtd 
xmimsim_DATA = xmimsim_cat.xml



xmimsim_cat.xml:
	$(XMLCATALOG) --noout --create xmimsim_cat.xml 
	$(XMLCATALOG) --noout --add rewriteURI \
	"http://www.xmi.UGent.be/xml/" \
	"file:///$(xmimsimdir)/" xmimsim_cat.xml

clean-local:
	rm -rf xmimsim_cat.xml
