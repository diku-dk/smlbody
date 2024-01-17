PROGNAME=smlbody
MLKIT=mlkit
FUTHARK_BACKEND=multicore
LIBS=
CFLAGS=-O3
UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S),Darwin)
	LDFLAGS += -arch x86_64 -framework OpenGL -framework Cocoa -L. -ltigr -lengine
else ifeq ($(UNAME_S),Linux)
	LDFLAGS += -L.
	LIBS += -libs 'm,c,dl,GLU,GL,X11,tigr,engine'
endif

.PHONY: all
all: $(PROGNAME)

engine.json engine.c: engine.fut
	futhark $(FUTHARK_BACKEND) --library engine.fut

engine.smlfut.c: engine.json
	smlfut --target=mlkit --structure-name=Engine $<

%.o: %.c
	gcc -c $< $(CFLAGS)

libengine.a: engine.o engine.smlfut.o
	ar r $@ $^

$(PROGNAME): $(PROGNAME).mlb $(PROGNAME).sml libtigr.a libengine.a
	MLCOMP=mlkit $(MLKIT) -ldexe '$(CC) $(LDFLAGS)' $(LIBS) -o $@ $<

libtigr.a: lib/github.com/diku-dk/sml-tigr/clib/tigr.o lib/github.com/diku-dk/sml-tigr/clib/tigr2.o
	ar r $@ $^

.PHONY: clean
clean:
	rm -rf $(PROGNAME) MLB libtigr.a *.o *.smlfut.c libengine.a engine.h engine.c engine.sml engine.sig engine.json
