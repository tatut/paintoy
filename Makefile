CFLAGS=$$(pkg-config --cflags --libs raylib)

clean:
	rm paintoy
paintoy: paintoy.c
	clang $(CFLAGS) -o paintoy paintoy.c
debug: paintoy.c
	clang -DDEBUG $(CFLAGS) -o paintoy paintoy.c

star: paintoy
	./paintoy star.pt
stars: paintoy
	./paintoy stars.pt
