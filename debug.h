#ifndef debug_h
#define debug_h

typedef struct {
  const char* name;
  int op1; // how many bytes for operand 1 (0 if no operand 1)
  int op2; // how many bytes for operand 2 (0 if no operand 2)
} op_info;

op_info ops[256] = {
  [OP_CONST] = (op_info) { "CONST", 1, 0 },
  [OP_CONSTL] = (op_info) { "CONSTL", 2, 0 },
  [OP_JZ] = (op_info) {"JZ", 2, 0 },
  [OP_JNZ] = (op_info) {"JNZ", 2, 0},
  [OP_POP1] = (op_info) {"POP1", 0,0},
  [OP_POP2] = (op_info) {"POP2", 0, 0},
  [OP_DUP] = (op_info) {"DUP", 0, 0},
  [OP_SUB] = (op_info) {"SUB (-)", 0, 0},
  [OP_DEC] = (op_info) {"DEC", 0, 0},
  [OP_INC] = (op_info) {"INC", 0, 0},
  [OP_STOP] = (op_info) {"STOP", 0, 0},
  [OP_MUL] = (op_info) {"MUL (*)", 0, 0},
  [OP_DIV] = (op_info) {"DIV (/)", 0, 0},
  [OP_ADD] = (op_info) {"ADD (+)", 0, 0},
  [OP_CALL] = (op_info) {"CALL", 1, 2},
  [OP_RETURN] = (op_info) {"RETURN", 0, 0},
  [OP_ARG] = (op_info) {"ARG", 1, 0},
  [OP_MOD] = (op_info) {"MOD", 0, 0},
  [OP_GLOBAL] = (op_info) {"GLOBAL", 2, 0 },
  [OP_GLOBAL_STORE] = (op_info) {"GLOBAL_STORE", 2, 0 },
  [OP_NEG] = (op_info) {"NEG", 0, 0},
  [OP_GT] = (op_info) {"GT (>)", 0, 0},
  [OP_GTE] = (op_info) {"GTE (>=)", 0, 0},
  [OP_LT] = (op_info) {"LT (<)", 0 , 0},
  [OP_LTE] = (op_info) {"LTE (<=)", 0, 0},
  [OP_EQ] = (op_info) {"EQ (=)", 0, 0},
  [OP_AGET] = (op_info) {"AGET", 0, 0},
  [OP_STACKREF] = (op_info) {"STACKREF", 1, 0},
  [OP_JMP] = (op_info) {"JMP", 2, 0 },
  [OP_FLIP] = (op_info) {"FLIP", 0, 0},
  [OP_CALLST] = (op_info) {"CALLST", 1, 0},
  [OP_SIN] = (op_info) {"SIN", 0,0},
  [OP_COS] = (op_info) {"COS", 0,0},
  [OP_TAN] = (op_info) {"TAN", 0,0},
  [OP_FD] = (op_info) {"FD", 0, 0},
  [OP_RT] = (op_info) {"RT", 0, 0},
  [OP_ANGLE] = (op_info) {"ANGLE", 0, 0},
  [OP_SETANGLE] = (op_info) {"SETANGLE", 0, 0},
  [OP_SETXY] = (op_info) {"SETXY", 0, 0},
  [OP_RND] = (op_info) {"RND", 0, 0},
  [OP_XY] = (op_info) {"XY", 0, 0},
  [OP_LINETO] = (op_info) {"LINETO", 0, 0},
  [OP_TEXT] = (op_info) {"TEXT", 0 ,0},
  [200] = (op_info) { "PEN0", 0, 0},
  [201] = (op_info) { "PEN1", 0, 0},
  [202] = (op_info) { "PEN2", 0, 0},
  [203] = (op_info) { "PEN3", 0, 0},
  [204] = (op_info) { "PEN4", 0, 0},
  [205] = (op_info) { "PEN5", 0, 0},
  [206] = (op_info) { "PEN6", 0, 0},
  [207] = (op_info) { "PEN7", 0, 0},
  [208] = (op_info) { "PEN8", 0, 0},
  [209] = (op_info) { "PEN9", 0, 0},
  [210] = (op_info) { "PENa", 0, 0},
  [211] = (op_info) { "PENb", 0, 0},
  [212] = (op_info) { "PENc", 0, 0},
  [213] = (op_info) { "PENd", 0, 0},
  [214] = (op_info) { "PENe", 0, 0},
  [215] = (op_info) { "PENf", 0, 0},
  [OP_PEN_RGB] = (op_info) { "PEN_RGB", 0, 0},
  [OP_RANDPEN] = (op_info) { "RANDPEN", 0, 0},
  [OP_PENUP] = (op_info) { "PENUP", 0, 0},
  [OP_PENDOWN] = (op_info) { "PENDOWN", 0, 0}
};


#endif
