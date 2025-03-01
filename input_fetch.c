#include "input.h"
#include <emscripten/fetch.h>
#include <string.h>
#include <stdlib.h>
#include <stdint.h>
#include <sys/types.h>
#include <stddef.h>
#include <stdio.h>

static void (*run_func)(IN);

static void fetch_success(emscripten_fetch_t *f) {
  IN in = malloc(sizeof(Fetched));
  printf("got status %d for fetch\n", f->status);
  if(f->status != 200) {
    in->len = 0;
    in->pos = 0;
    in->data = NULL;
  } else {
    in->len = f->numBytes;
    in->pos = 0;
    in->data = malloc(in->len);
    memcpy(in->data, f->data, in->len);
  }
  emscripten_fetch_close(f);
  run_func(in);
  free(in->data);
  free(in);
}

void with_file(const char* file, void (*func)(IN)) {
  run_func = func;

  emscripten_fetch_attr_t attr;
  emscripten_fetch_attr_init(&attr);
  strcpy(attr.requestMethod, "GET");
  attr.attributes = EMSCRIPTEN_FETCH_LOAD_TO_MEMORY;
  attr.onsuccess = fetch_success;
  emscripten_fetch(&attr, file);
}

uint8_t read_uint8(IN in) { return in->data[in->pos++]; }
uint16_t read_uint16(IN in) {
  uint8_t b[2];
  read_bytes(in, &b, 2);
  printf("read uint16: %d\n", (b[0]<<8) + b[1]);
  return (b[0] << 8) + b[1];
}

void read_bytes(IN in, void* to, size_t bytes) {
  memcpy(to, &in->data[in->pos], bytes);
  in->pos += bytes;
}
size_t file_size(IN in) { return in->len; }
size_t file_pos(IN in) { return in->pos; }
