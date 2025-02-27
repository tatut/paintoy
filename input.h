/* Abstract native FILE* and emscripten fetch differences */
#ifndef input_h
#define input_h

#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>

#ifdef PLATFORM_WEB
typedef struct {
  char* data;
  size_t len;
  size_t pos;
} Fetched;

#define IN Fetched*

#else

#define IN FILE*

#endif


uint8_t read_uint8(IN in);
uint16_t read_uint16(IN in);
void read_bytes(IN in, void* to, size_t bytes);
size_t file_size(IN in);
size_t file_pos(IN in);
void with_file(const char* name, void (*func)(IN));

#endif
