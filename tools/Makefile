all: measure.exe generate.exe
measure.exe: measure.go
	go build -o measure.exe measure.go
generate.exe: generate.go
	go build -o generate.exe generate.go
clean:
	rm -f measure.exe generate.exe
