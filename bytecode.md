Byte code structure:

header: PTv1\n (5 bytes)
constant pool:
 - number of constants (2 bytes, max of 65536)
 - constant...
 - each constant:
 -- type: 1 byte
 -- data: variable, base on type
name pool:
 - number of names (2 bytes)
 - each name: NUL character terminated C-strings
code:
 - bytecode for all functions
entry:
 - uint16
 -- entry point address for main function
 -- or 0 if there are no functions
 types:
 - 0  the number zero (no data)
 - 1  small positive integer (1 byte of data 0 - 255)
 - 2  small negative integer (1 byte of data 0 - 255)
 - 3  medium positive integer (2 bytes of data 0 - 65536)
 - 4  medium negative integer (2 bytes)
 - 5  positive integer (4 bytes)
 - 6  negative integer (4 bytes)
 - 7  positive fixed point number (6 bytes, 4 integer part, 2 for fixed point: 0000 - 9999)
 - 8  negative fixed point number
 - 9  short string (1st byte length, N bytes content, no zero ending)
 - 10 string (2 bytes length, N bytes content)

opcodes

name    val operands  description
CONST     0        1  push constant 0 - 255 to stack from pool
CONSTL    1        2  push constant 256 - 65535 to stack from pool
JZ        2        2  if top of stack is zero, jump to code address (uint16)
JNZ       3        2  if top of stack is non-zero jump to code address (uint16)
POP1      4        0  pop 1 value off the stack
POP2      5        0  pop 2 values off the stack
DUP       6        0  duplicate the value at the top
SUB       7        0  subtract topmost value from next topmost, store value
DEC       8        0  decrement topmost value
INC       9        0  increment topmost value
STOP     10        0  execution is finished, stop interpreter
MUL      11        0  multiply 2 topmost values
DIV      12        0  divide next topmost value by topmost value
ADD      13        0  add topmost values
CALL     14        3  1 argc, 2 jump addr, prepare new local env and jump
RETURN   15        0  jump back from function (return value, if any, is at stack top)
ARG      16        1  push arg N to stack (0 is first arg)
MOD      17        0  modulo
GLOBAL   18        2  load global (uint16 index into global names pool)
GLOBAL_STORE 19    2  put topmost value into global
NEG      20        0  negate topmost value
GT       21        0  compare 2 topmost values, push 1 if 1st is larger than second (topmost) otherwise push 0
GTE      22        0  greater than or equal
LT       23        0  less than
LTE      24        0  less than or equal
EQ       25        0  equals
SIN      80        0  sine
COS      81        0  cosine
TAN      82        0  tangent
FD      100        0  move forward (takes 1 value from stack)
RT      101        0  rotate degrees (takes 1 value from stack)
ANGLE   102        0  load current angle
SETANGLE 103       0  set current angle
SETXY   104        0  set X and Y
RND     105        0  random number between 2 values from stack
XY      106        0  load current X and Y (top)
LINETO  107        0  line from current pos to x,y (in stack)
TEXT    108        0  draw text at current pos
PEN0    200        0  set pen 0
PEN1    201        0  set pen 1
PEN2    202        0  set pen 2
PEN3    203        0  set pen 3
PEN4    204        0  set pen 4
PEN5    205        0  set pen 5
PEN6    206        0  set pen 6
PEN7    207        0  set pen 7
PEN8    208        0  set pen 8
PEN9    209        0  set pen 9
PENA    210        0  set pen A
PENB    211        0  set pen B
PENC    212        0  set pen C
PEND    213        0  set pen D
PENE    214        0  set pen E
PENF    215        0  set pen F
PEN     216        3  set pen by r,g,b
RANDPEN 217        0  set a random pen
PENUP   218        0  pen up (no drawing)
PENDOWN 219        0  pen down (start drawing)


## Calling convention

Functions are called by first pushing the arguments to the stack
and then invoking `CALL` op with the address of the function.

The function can load its arguments with the `ARG` operation.

The return value is pushed to the stack and `RETURN` is invoked with the
number of parameters the function has. This will remove the return address
and the arguments from the stack, leave the return value as the topmost value
and jump back.
