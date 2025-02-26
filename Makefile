CFLAGS=$$(pkg-config --cflags --libs raylib)

clean:
	rm paintoy

paintoy: paintoy.c debug.h input_file.c
	clang $(CFLAGS) -o paintoy paintoy.c input_file.c
debug: paintoy.c debug.h input_file.c
	clang -DDEBUG $(CFLAGS) -o paintoy paintoy.c input_file.c

star: paintoy
	./paintoy star.pt
stars: paintoy
	./paintoy stars.pt

web: paintoy.c
	emcc -o paintoy.html paintoy.c -Os -Wall ./raylib-5.5_webassembly/lib/libraylib.a -I. -Iraylib-5.5_webassembly/include -L. -Lraylib-5.5_webassembly/lib/libraylib.a -s USE_GLFW=3 -s ASYNCIFY -DPLATFORM_WEB --shell-file shell.html
