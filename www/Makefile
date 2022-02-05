PAGES = index.html download.html manual.html news.html readme.html

TEMPLATES = defs.m4 header.html.in footer.html.in

TMPFILES = nvc.1.html README.html NEWS.html

all: $(PAGES)

manual.html: nvc.1.html

news.html: NEWS.html

readme.html: README.html

nvc.1.html: ../nvc.1
	mandoc -T html -O fragment $< > $@

%.html: ../%.md
	pandoc -t html -o $@ $<

%.html: %.html.in $(TEMPLATES)
	m4 $< > $@

clean:
	rm -f $(PAGES) $(TMPFILES)
