CFLAGS=$$(pkg-config --cflags --libs raylib) -Iraygui/src

clean:
	rm paintoy

paintoy: paintoy.c debug.h input.h input_file.c
	clang $(CFLAGS) -o paintoy paintoy.c input_file.c
debug: paintoy.c debug.h input_file.c
	clang -fsanitize=address -DDEBUG $(CFLAGS) -o paintoy paintoy.c input_file.c

star: paintoy
	./paintoy star.pt
stars: paintoy
	./paintoy stars.pt

web: paintoy.c input.h input_fetch.c
	emcc -o paintoy.html paintoy.c input_fetch.c -Os -Wall ./raylib-5.5_webassembly/lib/libraylib.a -I. -Iraylib-5.5_webassembly/include -L. -Lraylib-5.5_webassembly/lib/libraylib.a -Iraygui/src -s USE_GLFW=3 -s FETCH -DPLATFORM_WEB --shell-file shell.html
