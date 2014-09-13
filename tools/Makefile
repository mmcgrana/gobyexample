all: measure.exe generate.exe css
	cd .. ; tools/build
measure.exe: measure.go
	go build -o measure.exe measure.go
generate.exe: generate.go
	go build -o generate.exe generate.go
clean:
	rm -f measure.exe generate.exe
	rm -rf ../public
	mkdir -p ../public

BSCSSURL=http://maxcdn.bootstrapcdn.com/bootstrap/3.2.0/css/bootstrap.min.css
BSCSS=../public/css/bootstrap.min.css
BSCSSTHEMEURL=http://maxcdn.bootstrapcdn.com/bootstrap/3.2.0/css/bootstrap-theme.min.css
BSCSSTHEME=../public/css/bootstrap-theme.min.css
BSJSURL=http://maxcdn.bootstrapcdn.com/bootstrap/3.2.0/js/bootstrap.min.js
BSJS=../public/js/bootstrap.min.js
JQJSURL=http://ajax.googleapis.com/ajax/libs/jquery/1.11.1/jquery.min.js
JQJS=../public/js/jquery.min.js
FONT=glyphicons-halflings-regular.woff

local:
	mkdir -p $(dir $(BSCSS))
	wget -c -O $(BSCSS) $(BSCSSURL)
	mkdir -p $(dir $(BSCSSTHEME))
	wget -c -O $(BSCSSTHEME) $(BSCSSTHEMEURL)
	mkdir -p $(dir $(BSJS))
	wget -c -O $(BSJS) $(BSJSURL)
	mkdir -p $(dir $(JQJS))
	wget -c -O $(JQJS) $(JQJSURL)
	mkdir -p ../public/fonts
	cp ../templates/$(FONT) ../public/fonts

CSSFILE=../public/site.min.css
css: $(CSSFILE)
$(CSSFILE): ../templates/site.css
	yuicompressor $< -o $@
