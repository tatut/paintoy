:- use_module(library(dcg/basics)).
:- use_module(library(yall)).
:- set_prolog_flag(double_quotes, chars).

log(Pattern, Args) :-
    format(string(L), Pattern, Args),
    _ := log(L).

% Basic DCG state nonterminals
state(S), [S] --> [S].
state(S0, S), [S] --> [S0].

% Push and pop environment bindings
push_env, [S, Env] --> [S], { S = t(_,_,_,_,Env,_) }.
pop_env, [t(X,Y,C,Ang,EnvSaved,PenUp)] -->
    [t(X,Y,C,Ang,_,PenUp), EnvSaved].

% X,Y position accessor
pos(X,Y) --> state(t(X,Y,_,_,_,_)).
pos(X0,Y0,X,Y) --> state(t(X0,Y0,C,Ang,Env,PenUp), t(X,Y,C,Ang,Env,PenUp)).

color(C) --> state(t(_,_,C,_,_,_)).
color(OldCol, NewCol) --> state(t(X,Y,OldCol,Ang,Env,PenUp), t(X,Y,NewCol,Ang,Env,PenUp)).

%%
%% Turtle graphics DCG
%%

strip_comments([], []).
strip_comments([C],[C]).
strip_comments([(/),(/)|Rest],Out) :- skip_to("\n", Rest, Out1), strip_comments(Out1, Out).
strip_comments([(/),(*)|Rest],Out) :- skip_to("*/", Rest, Out1), strip_comments(Out1, Out).
strip_comments([C1,C2|Rest], Out) :-
    not((C1 = (/), (C2 = (*); C2 = (/)))),
    strip_comments([C2|Rest], RestOut),
    append([C1], RestOut, Out).

skip_to(End, Cs, Out) :-
    append(End, Out, Cs).
skip_to(End, [C|Cs], Out) :-
    not(append(End,Out,[C|Cs])),
    skip_to(End, Cs, Out).


ws --> [W], { char_type(W, space) }, ws.
ws --> [].

% At least one whitespace
ws1 --> [W], { char_type(W, space) }, ws.

parse(Source, Prg) :-
    strip_comments(Source, SourceStripped),
    % once(...)
    phrase(turtle(Prg), SourceStripped).

turtle([]) --> [].
turtle([P|Ps]) --> ws, turtle_command(P), ws, turtle(Ps).

turtle_command(Cmd)  --> defn(Cmd) | fncall(Cmd) |
                         fd(Cmd) | bk(Cmd) | rt(Cmd) |
                         pen(Cmd) | randpen(Cmd) |
                         repeat(Cmd) | setxy(Cmd) |  savexy(Cmd) |
                         setang(Cmd) | saveang(Cmd) |
                         for(Cmd) | when(Cmd) | say_(Cmd) |
                         pendown(Cmd) | penup(Cmd) | lineto(Cmd) | fill(Cmd) |
                         setvar(Cmd) | text(Cmd).


defn(defn(FnName, ArgNames, Body)) -->
    "def", ws1, ident(FnName), ws, "(", defn_args(ArgNames), ")", ws, "{", turtle(Body), "}".

fncall(fncall(var(FnName), ArgValues)) --> "&", ident(FnName), ws, "(", fncall_args(ArgValues), ")".
fncall(fncall(ident(FnName), ArgValues)) --> ident(FnName), ws, "(", fncall_args(ArgValues), ")".
fncall_args([]) --> [].
fncall_args([V|Vs]) --> exprt(V), more_fncall_args(Vs).
more_fncall_args([]) --> ws.
more_fncall_args(Vs) --> ws1, fncall_args(Vs).

ident_([]) --> [].
ident_([I|Is]) --> [I], { char_type(I, csymf) }, ident_(Is).
ident(I) --> ident_(Cs), { atom_chars(I, Cs) }.


defn_args([]) --> [].
defn_args([Arg|Args]) --> ws, ident(Arg), more_defn_args(Args).
more_defn_args([]) --> ws.
more_defn_args(Args) --> ws1, defn_args(Args).

repeat(repeat(Times,Program)) --> "repeat", exprt(Times), "[", turtle(Program), "]".

say_(say(Msg)) --> "say", ws, "\"", string_without("\"", Codes), "\"", { atom_codes(Msg, Codes) }.
fd(fd(N)) --> "fd", exprt(N).
bk(bk(N)) --> "bk", exprt(N).
rt(rt(N)) --> "rt", exprt(N).
pen(pen(Col)) --> "pen", ws, [Col], { char_type(Col, alnum) }, ws.
randpen(randpen) --> "randpen".
setvar(setvar(Var,Value)) --> ident(Var), ws, ":=", ws, exprt(Value).
setxy(setxy(X,Y)) --> "setxy", exprt(X), exprt(Y).
savexy(savexy(X,Y)) --> "savexy", ws, ident(X), ws1, ident(Y).
lineto(lineto(X,Y)) --> "line", exprt(X), exprt(Y).
saveang(saveang(A)) --> "saveang", ws, ident(A).
setang(setang(Deg)) --> "setang", exprt(Deg).
setang(setang(X,Y)) --> "angto", exprt(X), exprt(Y).
for(for(Var, From, To, Step, Program)) -->
    "for", ws, "[", ws, ident(Var), exprt(From), exprt(To), exprt(Step), "]", ws,
    "[", turtle(Program), "]".
for(for(Var,ListExpr,Program)) -->
    "for", ws, "[", ws, ident(Var), exprt(ListExpr), "]", ws, "[", turtle(Program), "]".
when(when(Expr, Program)) --> exprt(Expr), ws, "->", ws, "[", turtle(Program), "]".
num(N) --> "-", num_(I), { N is -I }.
num(N) --> num_(N).
num_(N) --> integer(N).
num_(F) --> digits(IP), ".", digits(FP), { append(IP, ['.'|FP], Term),  read_from_chars(Term, F) }.
arg_(num(N)) --> num(N).
arg_(var(V)) --> ":", ident(V).
arg_(rnd(Low,High)) --> "rnd", ws, exprt(Low), ws, exprt(High).
arg_(str(Str)) --> "\"", string_without("\"", Codes), "\"", { string_codes(Str, Codes) }.
arg_(list(Items)) --> "[", list_items(Items), "]".
arg_(mathfn(Fn, Arg)) --> mathfn(Fn), "(", exprt(Arg), ")".
list_items([]) --> [].
list_items([I|Items]) --> exprt(I), list_items(Items).
list_atoms_items([], []).
list_atoms_items([A|Atoms], [atom(A)|AtomsRest]) :- list_atoms_items(Atoms, AtomsRest).
penup(penup) --> "pu" | "penup".
pendown(pendown) --> "pd" | "pendown".
fill(fill(Program)) --> "fill", ws, "[", turtle(Program), "]".
text(text(Arg)) --> "text", ws, exprt(Arg).

% Parse simple math expression tree. There is no priority for multipliation and addition.
% Use parenthesis to change order.

exprt(E) --> ws, expr(E), ws. % top level, wrap with whitespace

expr(A) --> arg_(A).
expr(E) --> "(", exprt(E), ")".
expr(op(Left,Op,Right)) --> expr_left(Left), ws, op_(Op), exprt(Right).

mathfn(sin) --> "sin".
mathfn(cos) --> "cos".
mathfn(tan) --> "tan".

expr_left(E) --> "(", exprt(E), ")".
expr_left(A) --> arg_(A).

op_(*) --> "*".
op_(/) --> "/".
op_(+) --> "+".
op_(-) --> "-".
op_(>) --> ">".
op_(<) --> "<".
op_(=) --> "=".
op_('%') --> "%".


% Interpreting a turtle program.
%
% state is a compound term of:
% t(X, Y, Color, Angle, Env, PenUp)
% where Env is a dict of the current env bindings (functions and arguments)
% and PenUp is true/false atom if pen is up (should not draw when moving)

eval_all([]) --> [].
eval_all([Cmd|Cmds]) -->
    %%{ writeln(eval_cmd(Cmd)) },
    eval(Cmd),
    eval_all(Cmds).

deg_rad(Deg, Rad) :-
    Rad is Deg * pi/180.

rad_deg(Rad, Deg) :-
    Deg is Rad / pi * 180.

set_angle(A) --> state(t(X,Y,C,_,Env,PenUp), t(X,Y,C,A,Env,PenUp)).
set_pen_up --> state(t(X,Y,C,Ang,Env,_), t(X,Y,C,Ang,Env,true)).
set_pen_down --> state(t(X,Y,C,Ang,Env,_), t(X,Y,C,Ang,Env,false)).
set_pen(P) --> state(t(X,Y,C,Ang,Env,_), t(X,Y,C,Ang,Env,P)).
get_pen(P) --> state(t(_,_,_,_,_,P)).

env(Env) --> state(t(_,_,_,_,Env,_)).
env(OldEnv, NewEnv) --> state(t(X,Y,C,Ang,OldEnv,PenUp),t(X,Y,C,Ang,NewEnv,PenUp)).

%% Eval argument against current ctx, var is taken from dictionary
%% numbers are evaluated as is.
%%
%% Argument can be a math expression op as eval.

argv(var(V), Val) -->
    env(Env),
    { Val = Env.V }.

argv(num(V), V) --> [].
argv(atom(A), A) --> [].

argv(list([]), []) --> [].
argv(list([Item_|Items_]), [Item|Items]) -->
    argv(Item_, Item),
    argv(list(Items_), Items).

argv(rnd(Low_,High_), V) -->
    argv(Low_, Low),
    argv(High_, High),
    { random_between(Low,High,V) }.

argv(mathfn(sin, Arg_), V) -->
    argv(Arg_, Arg),
    { deg_rad(Arg, Rad),
      V is sin(Rad) }.

argv(mathfn(cos, Arg_), V) -->
    argv(Arg_, Arg),
    { deg_rad(Arg, Rad),
      V is cos(Rad) }.

argv(mathfn(tan, Arg_), V) -->
    argv(Arg_, Arg),
    { deg_rad(Arg, Rad),
      V is tan(Rad) }.


argv(op(Left_,Op,Right_), V) -->
    argv(Left_, Left),
    argv(Right_, Right),
    { eval_op(Left, Op, Right, V) }.

fn_name(var(Var), FnName) -->
    argv(var(Var), FnName).

fn_name(ident(FnName), FnName) --> [].

eval_op(L,+,R,V) :- V is L + R.
eval_op(L,-,R,V) :- V is L - R.
eval_op(L,*,R,V) :- V is L * R.
eval_op(L,/,R,V) :- V is L / R.
eval_op(L,>,R,0) :- L =< R.
eval_op(L,>,R,1) :- L > R.
eval_op(L,<,R,0) :- L >= R.
eval_op(L,<,R,1) :- L < R.
eval_op(L,'%',R,V) :- V is L mod R.




setval(Var, Val) -->
    env(Env0,Env1),
    { Env1 = Env0.put(Var, Val) }.

setargs([],[]) --> [].
setargs([K|Ks], []) --> { throw(error(not_enough_arguments, missing_vars([K|Ks]))) }.
setargs([], [V|Vs]) --> { throw(error(too_many_arguments, extra_values([V|Vs]))) }.
setargs([K|Ks], [V|Vs]) -->
    argv(V, Val),
    setval(K, Val), setargs(Ks,Vs).

draw_line(X,Y) -->
    state(t(X0,Y0,C,Ang,Env,PenUp), t(X,Y,C,Ang,Env,PenUp)),
    draw_line(PenUp,X0,Y0,X,Y).

draw_line(true,X0,Y0,X,Y) --> draw_line(X0,Y0,X,Y).
draw_line(fill,X0,Y0,X,Y) --> draw_line(X0,Y0,X,Y).
draw_line(false,X0,Y0,X,Y) --> begin_path, draw_line(X0,Y0,X,Y), stroke.

begin_path -->  { _ := 'CTX'.beginPath() }.
stroke -->  { _ := 'CTX'.stroke() }.

draw_line(X0,Y0,X,Y) -->
    { % Call JS interop on the JS global CTX
      _ := 'CTX'.moveTo(X0,Y0),
      _ := 'CTX'.lineTo(X,Y) }.

% Moving with pen up or down
move_forward(true, X, Y) --> pos(_,_,X,Y).
move_forward(false, X, Y) --> pos(X0,Y0,X,Y), begin_path, draw_line(X0,Y0,X,Y), stroke.
move_forward(fill, X, Y) --> pos(_,_,X,Y), { _ := 'CTX'.lineTo(X,Y) }.

% Convert variable value to string for JS (now just works for lists of atoms)
to_text([], Out) :- atom_string('', Out).
to_text([C|Cs], Out) :-
    to_text(Cs, CsText),
    atom_string(C, CText),
    string_concat(CText, CsText, Out).


eval(rt(DegArg)) -->
    argv(DegArg, Deg),
    state(t(X,Y,C,Ang0,Env,PenUp), t(X,Y,C,Ang1,Env,PenUp)),
    { Ang1 is round(Ang0 + Deg) mod 360 }.

eval(fd(LenArg)) -->
    argv(LenArg, Len),
    state(t(X,Y,_,Ang,_,PenUp)),
    { deg_rad(Ang, Rad),
      X1 is X + Len * cos(Rad),
      Y1 is Y + Len * sin(Rad) },
    move_forward(PenUp, X1, Y1).

eval(bk(LenArg)) -->
    argv(LenArg, Len),
    { MinusLen is -Len },
    eval(fd(num(MinusLen))).

eval(pen(C)) -->
    color(_,C),
    { color_rgb(C, RGB),
      atom_string(RGB, Str),
      'CTX'.strokeStyle := Str
      % _ := setcolor(Str)
    }.

eval(randpen) -->
    { C is random(16), format(atom(Col), '~16r', [C]) },
    eval(pen(Col)).

eval(repeat(num(0), _)) --> [].
eval(repeat(NArg, Cmds)) -->
    argv(NArg, N),
    { N > 0,
      N1 is N - 1 },
    eval_all(Cmds),
    eval(repeat(num(N1), Cmds)).

eval(setxy(XArg,YArg)) -->
    argv(XArg, X), argv(YArg, Y),
    state(t(_,_,C,Ang,Env,PenUp), t(X,Y,C,Ang,Env,PenUp)).

eval(savexy(XVar,YVar)) -->
    pos(X, Y),
    setval(XVar, X),
    setval(YVar, Y).

eval(saveang(AVar)) -->
    state(t(_,_,_,Ang,_,_)),
    setval(AVar, Ang).

eval(setang(AngArg)) -->
    argv(AngArg, Ang),
    set_angle(Ang).

% Set angle towards a target point
eval(setang(TargetX_,TargetY_)) -->
    pos(PosX,PosY),
    argv(TargetX_, TargetX),
    argv(TargetY_, TargetY),
    set_angle(Deg),
    { Ang is atan2(TargetY-PosY, TargetX-PosX),
      rad_deg(Ang, Deg0),
      Deg is round(Deg0) %writeln(angle(to(TargetX,TargetY),from(PosX,PosY),rad_deg(Ang,Deg)))
    }.

eval(penup) --> set_pen_up.
eval(pendown) --> set_pen_down.

% Set value in context
eval(setvar(Ident, Expr)) -->
    argv(Expr, Val),
    setval(Ident, Val).

%% Loop from lower to upper number
eval(for_(_, From, To, Step, _)) -->
    { (Step > 0, From > To); (Step < 0, From < To) }, [].

eval(for_(Var, From, To, Step, Program)) -->
    setval(Var, From),
    eval_all(Program),
    { From1 is From + Step },
    eval(for_(Var, From1, To, Step, Program)).

eval(for(Var, From_, To_, Step_, Program)) -->
    argv(From_, From),
    argv(To_, To),
    argv(Step_, Step),
    eval(for_(Var, From, To, Step, Program)).

%% Loop through a list

eval(for_(_, [], _)) --> [].
eval(for_(Var, [Item|Items], Program)) -->
    setval(Var, Item),
    eval_all(Program),
    eval(for_(Var, Items, Program)).

eval(for(Var, List_, Program)) -->
    argv(List_, List),
    eval(for_(Var, List, Program)).


eval(say(Msg)) -->
    say(Msg).

eval(defn(FnName, ArgNames, Body)) -->
    setval(FnName, fn(ArgNames,Body)).

eval(fncall(FnName_, ArgValues)) -->
    fn_name(FnName_, FnName),
    push_env,
    env(Env),
    { fn(ArgNames,Body) = Env.FnName },
    setargs(ArgNames, ArgValues),
    eval_all(Body),
    pop_env.

eval(lineto(X_, Y_)) -->
    argv(X_, X),
    argv(Y_, Y),
    draw_line(X,Y).

eval(fill(Program)) -->
    color(C),
    { color_rgb(C, RGB), atom_string(RGB,Str),
      'CTX'.fillStyle := Str,
      _ := 'CTX'.beginPath() },
    get_pen(PenNow),
    set_pen(fill),
    eval_all(Program),
    %{ writeln(after_eval(Program)) },
    { _ := 'CTX'.fill() },
    set_pen(PenNow).

eval(text(T)) -->
    pos(X,Y),
    argv(T, Text),
    { to_text(Text, Text1),
      _ := 'CTX'.fillText(Text1, X, Y) }.

eval(when(Expr, Program)) -->
    argv(Expr, V),
    eval_when(V, Program).

eval_when(0, _) --> [].
eval_when(N, Program) --> { N > 0 }, eval_all(Program).

color_rgb('0', 'rgb(0,0,0)').
color_rgb('1', 'rgb(29,43,83)').
color_rgb('2', 'rgb(126,37,83)').
color_rgb('3', 'rgb(0,135,81)').
color_rgb('4', 'rgb(171,82,54)').
color_rgb('5', 'rgb(95,87,79)').
color_rgb('6', 'rgb(194,195,199)').
color_rgb('7', 'rgb(255,241,232)').
color_rgb('8', 'rgb(255,0,77)').
color_rgb('9', 'rgb(255,163,0)').
color_rgb('a', 'rgb(255,236,39)').
color_rgb('b', 'rgb(0,228,54)').
color_rgb('c', 'rgb(41,173,255)').
color_rgb('d', 'rgb(131,118,156)').
color_rgb('e', 'rgb(255,119,168)').
color_rgb('f', 'rgb(255,204,170)').
color_rgb([R,G,B], RGB) :-
    format(atom(RGB), 'rgb(~w,~w,~w)', [R,G,B]).

exec(Program, S0) :-
    catch((log('Executing program',[]),
           call_time(phrase(eval_all(Program), [S0], [_S1]), Time),
           log('DONE in ~3f seconds', [Time.cpu])),
          error(Error,ErrCtx),
          (log('ERROR: ~w (~w)', [Error, ErrCtx]))).

run(InputStr, X0, Y0, C0, Frame) :-
    get_time(T), Time is round(T * 1000),
    string_chars(InputStr, Cs),
    ( once(parse(Cs, Program)),
      %log('Program: ~w, Frame: ~w',[Program, Frame]),
      exec(Program, t(X0, Y0, C0, 0, ctx{'time': Time, 'frame': Frame}, false)) )
    ; log('Parse error', []).


sample(star, "repeat 100 [ randpen rt 36 fd 50 repeat 5 [ fd 25 rt 144 ] ]").
sample(stars, "def star(size) { repeat 5 [ fd :size rt 144 ] }

rt :frame % 360

repeat 10 [ randpen star(50) rt 60 pu fd 100 pd ]").


parse_sample(Sample, Prg) :- sample(Sample, Str), string_chars(Str, Cs),
                             parse(Cs, Prg), !.


%%%%%%%%%%%%%%
% Compiler into paintoy bytecode (.ptc) format
%
% Compiler walks the program with a DCG and converts it into
% bytecode operations, a constant pool and a globals names pool.
%
% The compiler state is kept in semicontext.

% The initial compiler context:
% c{pos: 0,
%   globals: [], num_globals: 0,
%   constants: [], num_constants: 0,
%   code: []).
% where:
% pos           the bytecode position,
% globals       list of pairs Idx-Name (index and symbol name)
% num_globals   how many globals there are currently
% constants     list of pairs Idx-Value (index and value)
% num_constants how many constants there are currently
% code          list of bytes emitted so far for the bytecode in reverse order
% args          Idx-Name list of arguments when compiling function
% functions     accumulating Name-Pos list of defined functions start positions
compiler_init(c{pos: 0,
                globals: [], num_globals: 0,
                constants: [], num_constants: 0,
                code: [], args: [], functions: []}).

curpos(P) --> state(S), { P = S.pos }.

dbg(X) --> { writeln(user_error, X) }, [].

% Basic access to parts of state: set new value, get current value and swap value with goal
set(K,V) --> state(S0, S1), { put_dict(K, S0, V, S1) }.
get(K,V) --> state(S0), { get_dict(K, S0, V) }.
swap(K, Goal) --> state(S0, S1), { get_dict(K, S0, Val0),
                                   call(Goal, Val0, Val1),
                                   put_dict(K, S0, Val1, S1) }.
swap([]) --> [].
swap([K-Goal|KGs]) --> swap(K,Goal), swap(KGs).
add_to_list(Key, Val) --> get(Key, List0),
                          set(Key, [Val|List0]).

incpos --> swap(pos, plus(1)).

simple_opcode(pop1, 4).
simple_opcode(pop2, 5).
simple_opcode(dup, 6).
simple_opcode(sub, 7).
simple_opcode(dec, 8).
simple_opcode(inc, 9).
simple_opcode(stop, 10).
simple_opcode(mul, 11).
simple_opcode(div, 12).
simple_opcode(add, 13).
simple_opcode(return, 15).
simple_opcode(mod, 17).
simple_opcode(fd, 100).
simple_opcode(rt, 101).
simple_opcode(angle, 102).
simple_opcode(setang, 103).
simple_opcode(setxy, 104).
simple_opcode(rnd, 105).
simple_opcode(xy, 106).
simple_opcode(neg, 20).
simple_opcode(lineto, 107).
simple_opcode(gt, 21).
simple_opcode(gte, 22).
simple_opcode(lt, 23).
simple_opcode(lte, 24).
simple_opcode(eq, 25).
simple_opcode(aget, 26).
simple_opcode(flip, 29).
simple_opcode(text, 108).
simple_opcode(sin, 80).
simple_opcode(cos, 81).
simple_opcode(tan, 82).

emit_byte(B) --> incpos, add_to_list(code, B).

emit([]) --> [].
emit([X|Xs]) --> emit(X), emit(Xs).
emit(Op) --> { simple_opcode(Op, Byte), ! }, emit_byte(Byte).
emit(X) --> { integer(X) }, emit_byte(X).


% emit instructions with operands
emit(const, Val) --> constant(Idx, Val),
                     emit_const_load(Idx).
% emit constant that is always 16bits (to have predictable instr size)
emit(constl, Val) --> constant(Idx, Val), emit(1), emit_uint16(Idx).

emit(jz, Pos) --> emit(2), emit_uint16(Pos).
emit(jnz, Pos) --> emit(3), emit_uint16(Pos).
emit(jmp, Pos) --> emit(28), emit_uint16(Pos).

emit(call, ArgC-JumpPos) --> emit(14), emit(ArgC), emit_uint16(JumpPos).
emit(callst, ArgC) --> emit(30), emit(ArgC).

emit(arg, Arg) --> emit(16), emit(Arg).
emit(global, Idx) --> emit(18), emit_uint16(Idx).
emit(global_store, Idx) --> emit(19), emit_uint16(Idx).
emit(stackref, N) --> emit(27), emit(N).

emit_const_load(Idx) --> { Idx < 256 }, !, emit(0), emit(Idx).
emit_const_load(Idx) --> emit(1), emit_uint16(Idx).

% Get or add new constant
constant(Idx,Value) --> get(constants, C), { member(Idx-Value, C) }, !.
constant(Idx,Value) --> get(constants, C), get(num_constants, Idx),
                        set(constants, [Idx-Value|C]),
                        swap(num_constants, plus(1)).

% Get or add new global
global(Idx,Name) --> get(globals, G), { member(Idx-Name, G) }, !.
global(Idx,Name) --> get(globals, G), get(num_globals, Idx),
                     set(globals, [Idx-Name|G]),
                     swap(num_globals, plus(1)).


uint16_bytes(V, Hi, Lo) :-  Hi is (V>>8) /\ 255, Lo is V /\ 255.
emit_uint16(V) --> { uint16_bytes(V, Hi,Lo) }, emit_byte(Hi), emit_byte(Lo).
write_uint16(V) :- uint16_bytes(V, Hi, Lo),  put_byte(Hi), put_byte(Lo).

write_constant(0) :- put_byte(0), !.
write_constant(C) :- integer(C), between(1, 255, C), put_byte(1), put_byte(C), !.
write_constant(C) :- integer(C), between(-255, -1, C), put_byte(2), V is abs(C), put_byte(V), !.
write_constant(C) :- integer(C), between(256, 65535, C), put_byte(3),
                    write_uint16(C), !.
write_constant(C) :- integer(C), between(-65535, -256, C), put_byte(4),
                    V is abs(C), write_uint16(V), !.
write_constant(S) :- string_length(S,L), L =< 255,
                     put_byte(9), put_byte(L), string_codes(S, Cs),
                     maplist(put_byte, Cs).
write_constant(S) :- string_length(S,L), L > 255,
                     put_byte(10), write_uint16(L), string_codes(S,Cs),
                     maplist(put_byte, Cs).

write_constant_pool(Compiler) :-
    c{constants: Constants, num_constants: NC} :< Compiler,
    write_uint16(NC),
    pairs_values(Constants, Values0),
    reverse(Values0, Values),
    maplist(write_constant,Values).

write_global(Name) :- atom_codes(Name, Codes), maplist(put_byte, Codes), put_byte(0).

write_global_pool(Compiler) :-
    c{globals: Globals, num_globals: C} :< Compiler,
    write_uint16(C),
    pairs_values(Globals, Names0),
    reverse(Names0, Names),
    maplist(write_global, Names).

arg_positions([],_) --> [].
arg_positions([Name|Names], N) --> add_to_list(args, Name-N),
                                   { N1 is N + 1 },
                                   arg_positions(Names, N1).

% Do a sub compilation without emitting to current
% compilation or modifying current position.
sub(Compilation, CompiledBytes, CompiledLen) -->
    get(pos, SavedPos),
    get(code, SavedCode),
    set(code, []),
    call_dcg(Compilation),
    get(code, CompiledBytes0),
    { reverse(CompiledBytes0, CompiledBytes),
      length(CompiledBytes, CompiledLen) },
    set(pos, SavedPos),
    set(code, SavedCode).

compile([]) --> [].
compile([X|Xs]) --> compile(X), compile(Xs).

compile(repeat(Times, Prg)) -->
    compile(Times),
    % take start position for jump
    curpos(StartPos),
    compile(Prg),
    % decrement to loop counter
    emit(dec),
    % duplicate value, as jump consumes it
    emit(dup),
    % jump to start if non-zero
    emit(jnz, StartPos),
    % pop counter from stack
    emit(pop1).


compile(fd(Dist)) -->
    compile(Dist),
    emit(fd).
compile(bk(Dist)) -->
    compile(Dist),
    emit(neg),
    emit(fd).
compile(rt(Angle)) -->
    compile(Angle),
    emit(rt).
compile(lineto(X,Y)) -->
    compile(X),
    compile(Y),
    emit(lineto).

compile(num(N)) --> emit(const, N).
compile(str(S)) --> emit(const, S).

compile(pen(A)) --> {atom_number(A,N), P is 200 + N }, emit(P).
compile(pen(a)) --> emit(210).
compile(pen(b)) --> emit(211).
compile(pen(c)) --> emit(212).
compile(pen(d)) --> emit(213).
compile(pen(e)) --> emit(214).
compile(pen(f)) --> emit(215).
compile(randpen) --> emit(217).

compile(penup) --> emit(218).
compile(pendown) --> emit(219).

compile(mathfn(Fn, Of)) --> compile(Of), emit(Fn).

compile(op(Left, Op, Right)) -->
    compile(Left),
    compile(Right),
    compile(Op).
compile('%') --> emit(mod).
compile('/') --> emit(div).
compile('*') --> emit(mul).
compile('+') --> emit(add).
compile('-') --> emit(sub).
compile('>') --> emit(gt).
compile('<') --> emit(lt).
compile('=') --> emit(eq).


% var might be an argument or a global
compile(var(Name)) --> get(args, Args), { memberchk(Name-Idx, Args), !} , emit(arg, Idx).
compile(var(Name)) --> global(Idx,Name), emit(global, Idx).


compile(saveang(Name)) -->
    emit(angle),
    global(Idx, Name),
    emit(global_store, Idx).

compile(setang(To)) -->
    compile(To),
    emit(setang).
compile(setxy(X,Y)) -->
    compile(X),
    compile(Y),
    emit(setxy).

compile(rnd(Lo,Hi)) -->
    compile(Lo),
    compile(Hi),
    emit(rnd).

compile(savexy(NameX,NameY)) -->
    emit(xy),
    global(YIdx, NameY),
    emit(global_store, YIdx),
    global(XIdx, NameX),
    emit(global_store, XIdx).

compile(defn(FnName, ArgNames, Body)) -->
    % Record our start position so we can jump to it later
    curpos(FnStart),
    add_to_list(functions, FnName-FnStart),
    % record stack positions for our arguments
    get(args, OldArgs),
    arg_positions(ArgNames, 0),
    compile(Body),
    compile(fn_end(FnName)),
    set(args,OldArgs).

compile(fn_end('__main__')) --> emit(stop), !.
compile(fn_end(_)) --> emit(return).

compile(fncall(ident(FnName), ArgValues)) -->
    get(functions, Fns),
    { memberchk(FnName-JumpPos, Fns) },
    compile(ArgValues),
    { length(ArgValues, ArgC) },
    emit(call, ArgC-JumpPos).

compile(fncall(var(FnNameVar), ArgValues)) -->
    % ugly hack to fn by name when we don't have the
    % names even stored... emit comparison and jump for
    % each function we know of
    dbg(fncall_from_var(FnNameVar)),
    emit(const,0), % default "not found" jump addr
    compile(var(FnNameVar)), % push value to compare
    % compile check+call for each fn
    get(functions, Fns0),
    get(pos, StartPos),
    { exclude(=('__main__'-_), Fns0, Fns),
      length(ArgValues, ArgC),
      length(Fns, FnsC),
      Size = 17, % bytecode of each fncall_switch item
      EndPos is StartPos + FnsC*Size
    },
    dbg(before_fncall_switch(Fns)),
    sub(compile(fncall_switch(EndPos, Fns)), SwitchBytes, _Len),
    % ^ after that, we have jump address in stack (or zero if not found)
    dbg(after_fncall_switch),
    dbg(switch_code(SwitchBytes)),
    emit(SwitchBytes),

    % after all checks, see if we need a call
    emit(pop1), % discard comparison value
    emit(dup), % duplicate jump address
    dbg(before_sub),
    sub((compile(ArgValues),
         { JmpPos is ArgC + 1 },
         emit(stackref, JmpPos),
         emit(callst, ArgC)), CallBytes, CallLen),
    dbg(after_sub),
    get(pos, Pos),
    { NoCallPos is Pos + CallLen + 3 },
    emit(jz, NoCallPos),
    emit(CallBytes),
    emit(pop1), % remove last call address
    dbg(fncall_by_var_finish).


compile(fncall_switch(_, [])) --> dbg(last_switch_compiled).
compile(fncall_switch(FoundPos, ['__main__'-_|Fns])) -->
    !,
    compile(fncall_switch(FoundPos, Fns)).
compile(fncall_switch(FoundPos, [FnName-JumpPos|Fns])) -->
    dbg(fncall_switch_0(foundpos(FoundPos),fn(FnName),jumppos(JumpPos),fns(Fns))),
    % [addr] [comparison value]
    % this consumes the comparison value if a match is found
    % and leaves the jump addr to the stack
    %
    % if there is no match, the stack is left unchanged
    emit(dup), % duplicate the var value we use for the eq check
    emit(constl, FnName),
    emit(eq),
    dbg(fncall_switch_1),

    get(pos, Before), { After is Before + 12 },
    emit(jz, After), % jump after call

    % push jump address to stack
    dbg(fncall_switch_1(emitting_call)),
    emit(flip), emit(pop1), % leaves [comparison value] on stack
    emit(constl, JumpPos), % push jump pos to stack
    emit(flip), % leaves [addr] [comparison value] on stack
    emit(jmp, FoundPos),

    compile(fncall_switch(FoundPos,Fns)).

% compile_file('programs/letters.pt', 'test.ptc').

compile(for(Var, From, To, Step, Program)) -->
    global(Idx, Var), % should implement locals!
    compile(From),
    emit(global_store, Idx), % store initial value for loop
    curpos(LoopStart),
    compile(Program),
    compile(Step),
    emit(global, Idx),
    emit(add), % add step to loop counter
    emit(dup), % duplicate loop counter value
    emit(global_store, Idx), % update counter value
    compile(To),
    emit(gt), % check if loop counter > to
    emit(jz, LoopStart).

compile(for(Var, ListExpr, Program)) -->
    % compile list expr and store it and 0 (index) to stack
    % each loop advances index and gets the current value from list
    % if the value is zero (C string termination), exits the loop
    compile(ListExpr),
    emit(const, 0), % push zero to stack
    % load current value in list to global Var
    global(LoopVarIdx, Var),
    get(pos, LoopStart),
     % duplicate top 2 items in the stack (listexpr, and loop pos)
    emit(stackref, 2),
    emit(stackref, 2),
    % call AGET to access Nth item from list
    emit(aget),
    emit(dup),
    %emit(dup),
    emit(global_store, LoopVarIdx),
    % compile program to get its size
    get(pos, BeforeProgram),
    { ProgramPos is BeforeProgram + 3 }, % reserve space for jz + addr
    get(code, SavedCode),
    set(pos, ProgramPos),
    set(code, []),
    compile(Program),
    get(code, ProgramCode),
    set(code, SavedCode),
    set(pos, BeforeProgram),
    % emit loop end check
    { length(ProgramCode, ProgramLength),
      EndAddr is BeforeProgram + 3 + ProgramLength + 4,
      reverse(ProgramCode, Bytes) },
    emit(jz, EndAddr), % if aget returned 0
    emit(Bytes),
    % increment idx
    emit(inc),
    emit(jmp, LoopStart),
    emit(pop2).

compile(when(Expr, Program)) -->
    compile(Expr),
    % need to compile program to temp buffer
    % to calculate its instruction size.
    % the compiled program might also need pos, so adjust it
    % to leave space for the jump instruction
    get(pos, BeforeJump),
    { ProgramPos is BeforeJump + 3 }, % reserve space for jz + addr
    set(pos, ProgramPos),
    get(code, SavedCode), % save current code
    set(code, []), % replace with empty code
    { writeln(user_error,compiling(pos(ProgramPos), prg(Program))) },
    compile(Program),
    get(code, WhenCode),
    set(code, SavedCode),
    set(pos, BeforeJump),
    % emit jump to after the when code
    { length(Bytes, ProgramLen) },
    { JmpAddr is ProgramPos + ProgramLen },
    emit(jz, JmpAddr),
    % output the program here
    { reverse(WhenCode, Bytes) },
    emit(Bytes).

compile(text(Expr)) -->
    compile(Expr),
    emit(text).

extract_functions([], [], []).

extract_functions([defn(N,A,B)|Code], Functions, Main) :-
    extract_functions(Code, Functions0, Main),
    append([defn(N,A,B)], Functions0, Functions), !.
extract_functions([Prg|Code], Functions, Main) :-
    extract_functions(Code, Functions, Main0),
    append([Prg], Main0, Main).

% Compile whole program
compile_program(P) :-
    compiler_init(C0), % empty compiler state

    % split fn definitions and any main code
    extract_functions(P, Functions, Main),
    phrase((compile(Functions),
            compile(defn('__main__',[],Main))),
           [C0], [C1]),

    % Write magic header to identify paintoy program
    maplist(put_byte, `PTv1\n`),
    % Output constant & global pools
    write_constant_pool(C1),
    write_global_pool(C1),

    reverse(C1.code, Bytes),
    maplist(put_byte, Bytes),
    memberchk('__main__'-StartPos, C1.functions),
    write_uint16(StartPos).

compile(Program, OutputFile) :-
    setup_call_cleanup(
        open(OutputFile, write, Out, [encoding(octet)]),
        with_output_to(Out, compile_program(Program)),
        close(Out)).

% convenience to compile star
cstar :- parse_sample(star, Prg), compile(Prg, 'star.ptc').
cstars :- parse_sample(stars, Prg), compile(Prg, 'stars.ptc').

parse_file(File, Prg) :-
    read_file_to_string(File, Str, []),
    string_chars(Str, Chars),
    parse(Chars, Prg), !.

compile_file(File, CompiledFile) :-
    parse_file(File, Prg),
    (compile(Prg, CompiledFile)
    -> writeln(ok)
    ; writeln('Compilation failed')).
