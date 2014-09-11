all: measure.exe generate.exe
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
BSJSURL=http://maxcdn.bootstrapcdn.com/bootstrap/3.2.0/js/bootstrap.min.js
BSJS=../public/js/bootstrap.min.js
JQJSURL=http://ajax.googleapis.com/ajax/libs/jquery/1.11.1/jquery.min.js
JQJS=../public/js/jquery.min.js
local:
	mkdir -p $(dir $(BSCSS))
	wget -c -O $(BSCSS) $(BSCSSURL)
	mkdir -p $(dir $(BSJS))
	wget -c -O $(BSJS) $(BSJSURL)
	mkdir -p $(dir $(JQJS))
	wget -c -O $(JQJS) $(JQJSURL)
CSSFILE=../public/site.min.css
css: $(CSSFILE)
$(CSSFILE): ../templates/site.css
	yuicompressor $< -o $@
