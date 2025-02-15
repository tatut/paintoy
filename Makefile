CFLAGS=$$(pkg-config --cflags --libs raylib)

paintoy: paintoy.c
	clang $(CFLAGS) -o paintoy paintoy.c

star: paintoy
	./paintoy star.pt
stars: paintoy
	./paintoy stars.pt
