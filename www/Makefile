PAGES = index.html download.html manual.html news.html readme.html features.html

TEMPLATES = defs.m4 header.html.in footer.html.in

TMPFILES = nvc.1.html README.html NEWS.html

all: $(PAGES)

manual.html: nvc.1.html

news.html: NEWS.html

readme.html: README.html

nvc.1.html: ../nvc.1
	mandoc -T html -O fragment $< > $@

%.html: ../%.md
	cmark -t html $< > $@

%.html: %.html.in $(TEMPLATES)
	m4 $< > $@

clean:
	rm -f $(PAGES) $(TMPFILES)

install: all
	cp -a $(PAGES) style.css /sites/nick/nvc/

validate: all
	vnu $(PAGES)

.PHONY: clean install validate
