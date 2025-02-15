/**
 * Paintoy bytecode VM using Raylib for graphics.
 * This loads the given bytecode file and executes it continuously
 * until user quits the program.
 *
 * See paintoy.pl for the compiler from source to bytecode.
 * See bytecode.md for description of the formats.
 */
#include <math.h>
#include <stdint.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <stdio.h>
#include <string.h>
#include "raylib.h"
#include "raymath.h"

Color palette[] = {
  (Color) {   0,   0,   0, 255 },
  (Color) {  29,  43,  83, 255 },
  (Color) { 126,  37,  83, 255 },
  (Color) {   0, 135,  81, 255 },
  (Color) { 171,  82,  54, 255 },
  (Color) {  95,  87,  79, 255 },
  (Color) { 194, 195, 199, 255 },
  (Color) { 255, 241, 232, 255 },
  (Color) { 255,   0,  77, 255 },
  (Color) { 255, 163,   0, 255 },
  (Color) { 255, 236,  39, 255 },
  (Color) {   0, 228,  54, 255 },
  (Color) {  41, 173, 255, 255 },
  (Color) { 131, 118, 156, 255 },
  (Color) { 255, 119, 168, 255 },
  (Color) { 255, 204, 170, 255 }
};

#ifdef DEBUG
#define dbg(args...)                                                           \
  printf(args);                                                                \
  printf("\n");
#endif

#ifndef DEBUG
#define dbg(args...)
#endif

#define panic(args...)                                                         \
  {                                                                            \
    printf(args);                                                              \
    printf("\n");                                                              \
    exit(1);                                                                   \
  }



/* Define possible values that can be manipulated
 * by paintoy programs.
 */
typedef enum { NUMBER, STRING } ValueType; // FIXME: list as dynarray?
typedef union {
  double number;
  char *string;
} ValueStorage;

typedef struct {
  ValueType type;
  ValueStorage value;
} Value;

typedef struct {
  size_t arg_sp; // index of first arg in stack
  uint8_t argc;
  uint16_t returnTo;
} FnEnv;

/* Define all opcodes supported */
typedef enum {
  /* basic ops for code */
  OP_CONST = 0,
  OP_CONSTL = 1,
  OP_JZ = 2,
  OP_JNZ = 3,
  OP_POP1 = 4,
  OP_POP2 = 5,
  OP_DUP = 6,
  OP_SUB = 7,
  OP_DEC = 8,
  OP_INC = 9,
  OP_STOP = 10,
  OP_MUL = 11,
  OP_DIV = 12,
  OP_ADD = 13,
  OP_CALL = 14,
  OP_RETURN = 15,
  OP_ARG = 16,
  OP_MOD = 17,
  OP_GLOBAL = 18,
  OP_GLOBAL_STORE = 19,
  /* drawing specific codes */
  OP_FD = 100,
  OP_RT = 101,

  // PEN 0-f omitted
  OP_PEN_RGB = 216,
  OP_RANDPEN = 217,
  OP_PENUP = 218,
  OP_PENDOWN = 219

} OP;

/* Define the state of code loaded from .pt file */
typedef struct {
  uint16_t num_constants;
  Value* constants;
  uint16_t code_size;
  uint8_t *code;
  uint16_t start;
  uint16_t num_globals;
  char** global_names;
} Code;

void code_free(Code* code) {
  if(code->num_constants > 0)
    free(code->constants);
  if(code->code_size > 0)
    free(code->code);
  if (code->num_globals > 0) {
    for (int i = 0; i < code->num_globals; i++) {
      free(code->global_names[i]);
    }
    free(code->global_names);
  }
}

int code_global_idx(Code *code, const char *name) {
  for (int i = 0; i < code->num_globals; i++) {
    if (strcmp(code->global_names[i], name) == 0)
      return i;
  }
  return -1;
}

uint16_t read_uint16(FILE* f) {
  uint8_t b[2];
  size_t read = fread(&b, 1, 2, f);
  if(read != 2) {
    panic("Error while reading uint16 value, read %ld bytes.\n", read);
  }
  return (b[0] << 8) + b[1];
}

uint8_t read_uint8(FILE* f) {
  int c = getc(f);
  if(c < 0) {
    panic("Error while reading uint8 value.\n");
    exit(4);
  }
  return (uint8_t) c;
}

Value number(double n) {
  Value v;
  v.type = NUMBER;
  v.value.number = n;
  return v;
}
Value string(char* str) {
  Value v;
  v.type = STRING;
  v.value.string = str;
  return v;
}

void print_value(Value v) {
  switch(v.type) {
  case STRING: printf("'%s' (string)", v.value.string); break;
  case NUMBER: printf("%f (number)", v.value.number); break;
  default:
    printf("Unknown value type: %d", v.type);
  }
}

Value value_mod(Value left, Value right) {
  return (Value) { NUMBER, fmod(left.value.number, right.value.number) };
}
Value value_mul(Value left, Value right) {
  return (Value) { NUMBER, left.value.number * right.value.number };
}
Value value_add(Value left, Value right) {
  return (Value) { NUMBER, left.value.number + right.value.number };
}
Value value_sub(Value left, Value right) {
  return (Value){NUMBER, left.value.number - right.value.number};
}

Value neg(Value n) {
  return number(-1.0 * n.value.number);
}
Value read_value(FILE* f) {
  uint8_t type = getc(f);
  switch(type) {
  case 0: return number(0);
  case 1: return number((double) read_uint8(f));
  case 2: return neg(number((double) read_uint8(f)));
  case 3: return number((double) read_uint16(f));
  case 4: return neg(number((double) read_uint16(f)));
  default:
    panic("FIXME: unsupported type %d", type);
  }
}

void code_load(Code *code, const char* file) {
  struct stat b;
  if(stat(file, &b) < 0) {
    panic("Can't read file '%s'.\n", file);
  }
  FILE* f = fopen(file, "r");
  char header[5];
  fread(&header, 5, 1, f);
  if(memcmp(&header, "PTv1\n", 5) != 0) {
    panic("File '%s' is not a paintoy bytecode file.\n", file);
  }
  // Read constant pool
  code->num_constants = read_uint16(f);
  if(code->num_constants > 0) {
    code->constants = malloc(sizeof(Value)*code->num_constants);
    for(int i=0; i<code->num_constants;i++) {
      code->constants[i] = read_value(f);
    }
  }
  // Read global names pool
  code->num_globals = read_uint16(f);
  if (code->num_globals > 0) {
    code->global_names = malloc(sizeof(char *) * code->num_globals);
    char name[128];
    for (int i = 0; i < code->num_globals; i++) {
      // read name until 0 char found
      int c = 0;
      do {
        name[c++] = getc(f);
      } while (c < 128 && name[c - 1] != 0);
      if (c == 128)
        panic("Too long identifier name!");
      code->global_names[i] = malloc(sizeof(char) * c);
      memcpy(code->global_names[i], &name[0], c);
    }
  }

  code->code_size = b.st_size - ftell(f) - 2;
  code->code = malloc(code->code_size);
  fread(code->code, 1, code->code_size, f);
  code->start = read_uint16(f);
}

// global vm stack, program counter and stack pointer
#define MAX_STACK 8192
Value stack[MAX_STACK];
size_t pc = 0; // program counter
size_t sp = 0; // stack pointer

// fn environment stack
FnEnv fn_env[MAX_STACK];
size_t envp = 0; // environment pointer

// globals storage
Value* globals; // dynamically allocated after code loading

#define check_overflow(sp)                                                       \
  {                                                                            \
    if (sp >= MAX_STACK) {                                                     \
      panic("Stack overflow!\n");                                      \
    }                                                                          \
  }
#define check_underflow(sp,n)                                                      \
  {                                                                            \
    if (sp < n) {                                                             \
      panic("Stack underflow!\n");                                     \
    }                                                                          \
  }

void pushenv(FnEnv e) {
  check_overflow(envp);
  fn_env[envp++] = e;
}
FnEnv popenv() {
  check_underflow(envp, 1);
  FnEnv e = fn_env[--envp];
  return e;
}
FnEnv peekenv() {
  check_underflow(envp, 1);
  return fn_env[envp - 1];
}

void push(Value v) {
  check_overflow(sp);
  stack[sp++] = v;
}
Value pop() {
  check_underflow(sp,1);
  Value v = stack[--sp];
  return v;
}
Value peek() {
  check_underflow(sp,1);
  return stack[sp-1];
}

/* The main loop of the bytecode interpreter */
void interpret(Code* code) {
#define r8() ((uint8_t)code->code[pc++])
#define r16() { u16 = (code->code[pc]<<8) + code->code[pc+1]; pc += 2; }
  pc = code->start;
  sp = 0;
  envp = 0;
  uint16_t u16;
  Value v;
  double x=400; double y=300; double angle=0;
  Color color = BLACK;
  bool pen = true;
  FnEnv env = (FnEnv){0, 0, 0};
  pushenv(env);
  dbg("---start---");
  for(;;) {
    dbg("pc: %ld, sp: %ld, op: %d", pc, sp, code->code[pc]);
#ifdef DEBUG
    for (size_t i = 0; i < sp; i++) {
      printf("[");
      print_value(stack[i]);
      printf("] ");
    }
    printf("\n");
#endif
    switch(r8()) {
    case OP_CONST: push(code->constants[r8()]); break;
    case OP_CONSTL: r16(); push(code->constants[u16]); break;
    case OP_JZ: {
      r16();
      if((int) peek().value.number == 0) {
        pc = u16;
      }
      break;
    }
    case OP_JNZ: {
      r16();
      if((int) peek().value.number != 0) {
        dbg("%d is not zero, jumping %d\n", (int) peek().value.number, u16);
        pc = u16;
      }
      break;
    }
    case OP_DEC: { v = pop(); v.value.number -= 1; push(v); break; }
    case OP_INC: { v = pop(); v.value.number += 1; push(v); break; }
    case OP_POP1: { check_underflow(sp,1); sp--; break; }
    case OP_POP2: { check_underflow(sp,2); sp -= 2; break; }
    case OP_STOP: return;
    case OP_MOD: {
      Value right = pop();
      Value left = pop();
      push(value_mod(left, right));
      break;
    }
    case OP_CALL: {
      uint8_t argc = r8();
      r16();
      env = (FnEnv){sp - argc, argc, pc};
      pushenv(env);
      pc = u16;
      break;
    }
    case OP_RETURN: {
      env = popenv();
      dbg("returning from, arg_sp: %zu, argc: %d, return: %d", env.arg_sp, env.argc, env.returnTo);
      sp = env.arg_sp;
      pc = env.returnTo;
      env = peekenv();
      // FIXME: if we want a separate "return with value", we need language
      // support for it (like smalltalk ^ operator?)
      break;
    }
    case OP_ARG: {
      push(stack[env.arg_sp + r8()]);
      break;
    }
    case OP_GLOBAL: {
      // FIXME: check undefined?
      r16();
      push(globals[u16]);
      break;
    }
    case OP_GLOBAL_STORE: {
      globals[r8()] = pop();
      break;
    }
    case OP_FD: {
      v = pop();
      double rad = angle*DEG2RAD;
      double x1 = x + v.value.number * cos(rad);
      double y1 = y + v.value.number * sin(rad);
      if(pen) DrawLine(x, y, x1, y1, color);
      dbg("move %f from (%f, %f) => (%f, %f)", v.value.number, x, y, x1, y1);
      x = x1;
      y = y1;
      break;
    }
    case OP_RT: {
      v = pop();
      //printf("rotate by %f\n", v.value.number);
      angle = angle + v.value.number;
      break;
    }

      // set a specific pen (ops 200 - 215 inclusive)
    case 200: case 201: case 202: case 203: case 204: case 205: case 206: case 207:
    case 208: case 209: case 210: case 211: case 212: case 213: case 214: case 215:
      color = palette[code->code[pc-1] - 200];
      break;
    case OP_PEN_RGB: {
      uint8_t r = r8();
      uint8_t g = r8();
      uint8_t b = r8();
      color = (Color){r, g, b, 255};
      break;
    }
    case OP_RANDPEN:
      color = palette[GetRandomValue(0,15)];
      break;
    case OP_PENUP:
      pen = false;
      break;
    case OP_PENDOWN:
      pen = true;
      break;

    default:
      printf("FIXME: unsupported opcode %d\n", code->code[pc-1]);
      exit(8);
    }
  }
}

void run(const char* file) {
  Code code;
  code_load(&code, file);
  printf("loaded ok with %d constants, %d names and %d code size\n",
         code.num_constants, code.num_globals, code.code_size);
  for (int i = 0; i < code.num_constants; i++) {
    printf("constant: ");
    print_value(code.constants[i]);
    printf("\n");
  }
  for (int i = 0; i < code.num_globals; i++) {
    printf("global: %s\n", code.global_names[i]);
  }
  // initialize globals
  globals = malloc(sizeof(Value) * code.num_globals);
  int time_idx = code_global_idx(&code, "time");
  int frame_idx = code_global_idx(&code, "frame");

  Value *frame = frame_idx == -1 ? NULL : &globals[frame_idx];
  if (frame != NULL) {
    frame->type = NUMBER;
    frame->value.number = 0;
  }
  Value *time = time_idx == -1 ? NULL : &globals[time_idx];
  if (time != NULL) {
    time->type = NUMBER;
  }
  char title[50];
  snprintf(&title[0], 50, "paintoy: %s", file);
  InitWindow(800, 600, title);
  SetTargetFPS(120);
  while (!WindowShouldClose()) {
    if (frame != NULL) {
      frame->value.number++;
    }
    if (time != NULL) {
      time->value.number = (double)GetTime();
    }
    BeginDrawing();
    ClearBackground(RAYWHITE);
    interpret(&code);
    DrawFPS(10,10);
    EndDrawing();

  }
  // free and exit
  free(globals);
  code_free(&code);
  exit(0);
}

int main(int argc, char **argv) {
  if(argc < 2) {
    printf("Usage: paintoy <bytecode file>\n");
    exit(1);
  }
  run(argv[1]);
}
