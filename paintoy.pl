:- use_module(library(dcg/basics)).
:- use_module(library(yall)).
:- set_prolog_flag(double_quotes, chars).
:- dynamic constant/2. % idx and value of constants
:- dynamic pos/1. % current position in bytecode emission
:- dynamic fn_pos/2. % fn jump position
:- dynamic arg_stack/2. % arg position in stack
:- dynamic global/2. % idx and name for global identifiers

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
arg_(list(Items)) --> "\"", string_without("\"", Atoms), "\"", { list_atoms_items(Atoms, Items) }.
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

curpos(P) :- pos(P).
incpos :- pos(P), P1 is P + 1, retract(pos(P)), asserta(pos(P1)).
clearpos :- retractall(pos(_)), asserta(pos(0)).

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


emit([]) :- !.
emit([X|Xs]) :- emit(X), !, emit(Xs).
emit(X) :- integer(X), put_byte(X), incpos, !.
emit(Op) :- simple_opcode(Op, Byte), emit(Byte).

% emit instructions with operands
emit(const, Val) :- constant(Idx, Val),
                    (Idx < 256
                    -> (emit(0), emit(Idx))
                    ; (emit(1), emit_uint16(Idx))).

emit(jz, Pos) :- emit(2), emit_uint16(Pos).
emit(jnz, Pos) :- emit(3), emit_uint16(Pos).
emit(call, ArgC-JumpPos) :- emit(14), emit(ArgC), emit_uint16(JumpPos).
emit(arg, Arg) :- emit(16), emit(Arg).
emit(global, Idx) :- emit(18), emit_uint16(Idx).

constant_count(Idx) :- (aggregate_all(max(Idx0), constant(Idx0,_), Idx1)
                       -> Idx is Idx1 + 1
                       ; Idx = 0).
add_constant(Value) :- constant(_, Value), !.
add_constant(Value) :- \+ constant(_, Value),
                       constant_count(Idx),
                       asserta(constant(Idx, Value)).

global_count(Idx) :- (aggregate_all(max(Idx0), global(Idx0,_), Idx1)
                     -> Idx is Idx1 + 1
                     ; Idx = 0).

add_global(Name) :- global(_, Name), !.
add_global(Name) :- \+ global(_, Name),
                    global_count(Idx),
                    asserta(global(Idx,Name)).

% walk the program tree and record all constants
constants(num(N)) :- add_constant(N), !.
constants(X) :- compound(X), compound_name_arguments(X, _Name, Args),
                maplist(constants, Args).
constants(X) :- atom(X).
constants([]).
constants([X|Xs]) :- maplist(constants, [X|Xs]).

% walk the program tree and record all global names
globals(X) :- atom(X), !.
globals(X) :- number(X), !.
globals([]) :- !.
globals([X|Xs]) :- !, maplist(globals, [X|Xs]).
globals(var(Name)) :- add_global(Name), !.
globals(X) :- compound(X), compound_name_arguments(X, _Name, Args),
              maplist(globals, Args).


emit_uint16(V) :- Hi is (V>>8) /\ 255, Lo is V /\ 255,
                  emit(Hi), emit(Lo).
emit_constant(0) :- emit(0), !.
emit_constant(C) :- integer(C), between(1, 255, C), emit(1), emit(C), !.
emit_constant(C) :- integer(C), between(-255, -1, C), emit(2), V is abs(C), emit(V), !.
emit_constant(C) :- integer(C), between(256, 65535, C), emit(3),
                    emit_uint16(C), !.
emit_constant(C) :- integer(C), between(-65535, -256, C), emit(4),
                    V is abs(C), emit_uint16(V), !.
% FIXME: support the rest of the types

emit_constant_pool :-
    constant_count(C),
    emit_uint16(C),
    findall(Idx-V, constant(Idx,V), Constants0),
    sort(Constants0, Constants),
    pairs_values(Constants, Values),
    maplist(emit_constant, Values).

emit_global(Name) :- atom_codes(Name, Codes), emit(Codes), emit(0).

emit_global_pool :-
    global_count(C),
    emit_uint16(C),
    findall(Idx-Name, global(Idx,Name), Globals0),
    sort(Globals0, Globals),
    pairs_values(Globals, Names),
    maplist(emit_global, Names).

arg_positions([],_).
arg_positions([Name|Names], N) :- asserta(arg_stack(Name, N)),
                                  N1 is N + 1,
                                  arg_positions(Names, N1).
compile([]).
compile([X|Xs]) :- compile(X), compile(Xs).
compile(repeat(Times, Prg)) :-
    compile(Times),
    % take start position for jump
    curpos(StartPos),
    compile(Prg),
    % decrement to loop counter
    emit(dec),
    % jump to start if non-zero
    emit(jnz, StartPos),
    % pop counter from stack
    emit(pop1).

compile(fd(Dist)) :-
    compile(Dist),
    emit(fd).

compile(rt(Angle)) :-
    compile(Angle),
    emit(rt).

compile(num(N)) :- emit(const, N).

compile(pen(N)) :- integer(N), P is 200 - N, emit(P).
compile(pen(a)) :- emit(210).
compile(pen(b)) :- emit(211).
compile(pen(c)) :- emit(212).
compile(pen(d)) :- emit(213).
compile(pen(e)) :- emit(214).
compile(pen(f)) :- emit(215).
compile(randpen) :- emit(217).

compile(penup) :- emit(218).
compile(pendown) :- emit(219).

compile(op(Left, Op, Right)) :-
    compile(Left),
    compile(Right),
    compile(Op).
compile('%') :- emit(mod).

% var might be an argument or a global
compile(var(Name)) :-
    arg_stack(Name, Idx), !,
    emit(arg, Idx).
compile(var(Name)) :-
    global(Idx, Name),
    emit(global, Idx).



compile(defn(FnName, ArgNames, Body)) :-
    % Record our start position so we can jump to it later
    curpos(FnStart),
    asserta(fn_pos(FnName, FnStart)),
    % record stack positions for our arguments
    retractall(arg_stack(_,_)),
    arg_positions(ArgNames, 0),
    compile(Body),
    (FnName='__main__'
    -> emit(stop)
    ; emit(return)).

compile(fncall(ident(FnName), ArgValues)) :-
    fn_pos(FnName, JumpPos),
    maplist(compile, ArgValues),
    length(ArgValues, ArgC),
    emit(call, ArgC-JumpPos).

extract_functions([], [], []).

extract_functions([defn(N,A,B)|Code], Functions, Main) :-
    extract_functions(Code, Functions0, Main),
    append([defn(N,A,B)], Functions0, Functions), !.
extract_functions([Prg|Code], Functions, Main) :-
    extract_functions(Code, Functions, Main0),
    append([Prg], Main0, Main).

% Compile whole program
compile_program(P) :-
    clearpos,
    % Emit magic header to identify paintoy program
    emit(`PTv1\n`),
    % Gather all constants and globals
    retractall(constant(_,_)), % clear constant pool
    retractall(global(_,_)),
    constants(P), !,
    globals(P), !,
    % Output constant & global pools
    emit_constant_pool,
    emit_global_pool,
    % Compile the AST
    clearpos, !,
    extract_functions(P, Functions, Main),
    compile(Functions),
    compile(defn('__main__',[],Main)),
    fn_pos('__main__', StartPos),
    emit_uint16(StartPos).

compile(Program, OutputFile) :-
    setup_call_cleanup(
        open(OutputFile, write, Out, [encoding(octet)]),
        with_output_to(Out, compile_program(Program)),
        close(Out)).

% convenience to compile star
cstar :- parse_sample(star, Prg), compile(Prg, 'star.ptc').
cstars :- parse_sample(stars, Prg), compile(Prg, 'stars.ptc').

compile_file(File, CompiledFile) :-
    read_file_to_string(File, Str, []),
    string_chars(Str, Chars),
    parse(Chars, Prg), !,
    (compile(Prg, CompiledFile)
    -> writeln(ok)
    ; writeln('Compilation failed')).
