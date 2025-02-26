#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "input.h"

uint16_t read_uint16(FILE* f) {
  uint8_t b[2];
  size_t read = fread(&b, 1, 2, f);
  if(read != 2) {
    //panic("Error while reading uint16 value, read %ld bytes.\n", read);
    exit(1);
  }
  return (b[0] << 8) + b[1];
}

uint8_t read_uint8(FILE* f) {
  int c = getc(f);
  if(c < 0) {
    //panic("Error while reading uint8 value.\n");
    exit(1);
  }
  return (uint8_t) c;
}

IN open_file(const char* file) {
  return fopen(file, "r");
}

void read_bytes(IN in, void *to, size_t bytes) {
  fread(to, 1, bytes, in);
}

void close_file(IN in) {
  fclose(in);
}

size_t file_size(IN in) {
  struct stat b;
  fstat(fileno(in), &b);
  return b.st_size;
}
size_t file_pos(IN in) {
  return ftell(in);
}
bool file_ok(IN in) {
  return in != NULL;
}
