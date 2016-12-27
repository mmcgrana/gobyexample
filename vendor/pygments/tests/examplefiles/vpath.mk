vpath  %.c src
vpath  %.h header
EXEC=hello
SRC= hello.c main.c
OBJ= $(SRC:.c=.o)

all: $(EXEC)

hello: $(OBJ)
        $(CC) -o $@ $^ $(LDFLAGS)

main.o: hello.h

%.o: %.c
        $(CC) -I header -o $@ \
        -c $< $(CFLAGS)
