LIBTOOL = libtool
CFLAGS+=-g -Wall
CC = $(LIBTOOL) --mode=compile gcc -O -c `freetype-config --cflags`
LIBPATH=/usr/local/lib
LINKLIB = $(LIBTOOL) --mode=link gcc -g -O -rpath $(LIBPATH) -o
LIBDEPS=-lX11 -lXrender -lGL -lGLU -ljpeg -lpng `freetype-config --libs`

OBJECTS=src/x11.o src/keysym2ucs.o src/jpeg.o src/png.o src/io.o src/freetype.o

pwin:	$(OBJECTS)
	$(LINKLIB) libpwin.la src/x11.lo src/keysym2ucs.lo src/jpeg.lo src/png.lo src/io.lo src/freetype.lo $(LIBDEPS)
	cp .libs/libpwin.so* ./

clean:
	rm -f src/*~ src/*.~*
	rm -f src/tests/*~ src/tests/*.~*
	rm -f src/*.o src/*.lo
	rm -f src/a.out
	rm -rf src/.libs
	rm -f libpwin.*
	rm -rf .libs
	rm -f *~
	rm -f *.fasl src/*.fasl src/tests/*.fasl
	rm -f pwin.o pwin.lo
