:- module(graph,
	[
		parser/1,
		parser_subs/2,
		graf/4
	]).

%:- use_rendering(graphviz).
:- dynamic data/1, t/3, t_/3, tt/3, g/6, color/6,node/2,colorY/2, arc/2, color_known/6,tg/4,as/3,s_/1,ss_/1.
	
	%Общие свойства отношений, например транзитивность.
tz(часть,транзит).
tz(причина,транзит).
tz(функция,транзит).
	
%Расцветка дуг графа
%arc(all,['color=yellow','fontsize=9']).
arc(индивид,['color=blue','style=dashed']).
arc(подкласс,['color=red','fontcolor=red', 'fontsize=9']).
arc(надкласс,['color=red','fontcolor=red', 'fontsize=9']).
arc(_,[]).
	
%Рассцветка вершин графа
%node(all,['shape=diamond']).
node(X,['color=blue','fontcolor=blue']):-t_(X,индивид,_).
%node(X,['shape=box','color=red',/*'style=filled','fillcolor=red',*/'fontsize=22']):-t_(_,подкласс,X).
node(_,[]).

% Обратные отношения
%t(X,надкласс,Y):-t(Y,подкласс,X).

%Общая часть для всех онтологий-------------------------------------------------------------------
%Типовые отношения и их свойства, передаваемые через наследование
tt(X,транзит,R/Y):-not(R=транзит),t(X,R,Y),tz(R,транзит).
tt(X,R,R/Y):-t(X,R,Y).
%tt(X,индивид,индивид/Y):-t(X,индивид,Z),tt(Z,подкласс,_/Y).  %Транзитивность по подклассу
%tt(X,индивид,индивид/Y):-t(X,индивид,Z),tt(Z,экв,_/Y).      %Транзитивность по эквивалентности
tt(X,R,Y):-t(X,индивид,Z),tt(Z,R,Y),not(R=подкласс;R=экв).  %Транзитивность по индивидуму
tt(X,R,Y):-t(X,экв,Z),tt(Z,R,Y).                        %Наследование свойств от эквивалентного класса
tt(X,R,Y):-t(X,подкласс,Z),tt(Z,R,Y).                    %Наследование свойств от родительского класса
tt(X,R,Y):-tz(R,транзит),t(X,R,Z),tt(Z,R,Y).             %Выполняется свойство транзитивности 

%Утилиты-----------------------------------------------------------
%Упорядоченные списоки объектов и отношений----------------------------------------------------------
%Упорядоченный список  из всех отношений
r(Lr):-setof(R,S^O^t(S,R,O),Lr). 
%Упорядоченный список  из всех субъектов
s(Ls):-setof(S,R^O^t(S,R,O),Ls). 
%Упорядоченный список  из всех объектов
o(Lo):-setof(O,S^R^t(S,R,O),Lo). 
%Упорядоченный список  из всех субъектов и объектов
so(Lso):-s(X),o(Y),append(X,Y,Z),sort(0,@<,Z,Lso).
%Упорядоченный список всех отношений для cубъекта S. S=>.
sr(S, Lr):-setof(R,Y^t(S,R,Y),Lr). 
%Упорядоченный список из всех отношений для oбъекта O. =>O.
or(O, Lr):-setof(R,X^t(X,R,O),Lr). 
%Упорядоченный список из всех субъектов для отношения R. Выходящая стрелка из Ls=>R.
rs(R, Ls):-setof(S,O^t(S,R,O),Ls). 
%Упорядоченный список из всех объектов для отношения R. Входящая стрелка в R=>Lo. 
ro(R, Lo):-setof(O,S^t(S,R,O),Lo). 
%Упорядоченный список из всех объектов для субъекта. S=>R/O (Фрейм)
sro(S,Lpr):-setof(O,R^tro(S,R,O),Lpr).
%s_o(S,Lpr):-(var(S),so(Lpr));(nonvar(S),setof(O,R^t(S,R,O),Lpr)). 
s_o(S,Lpr):-setof(O,R^tro(S,R,O),Lpr).
%%Упорядоченный список из всех субъектов для объекта. О/R=>S 
os(O,Lpr):-setof(S,R^tsr(S,R,O),Lpr).
o_s(O,Lpr):-(var(O),o(Lpr));(nonvar(O),setof(S,R^t(S,R,O),Lpr)). 
%%%Упорядоченный список из всех индивидов для класса. 
ind(C, Li):-setof(X,t(X,индивид,C),Li). 
%Упорядоченный список из всех подклассов для класса C.
sub(C,Lsub):-setof(X,t(X,подкласс,C),Lsub). 
%Упорядоченный список из всех надклассов для класса C.
super(C,Lsub):-setof(X,t(C,подкласс,X),Lsub). 

%Вспомогательные предикаты-------------------------------------------------------------------------
tr(X/RR/Y):-member(R,RR),t(X,R,Y).
trr(RR,X/R/Y):-member(R,RR),t(X,R,Y).
t_ro(X,R,R/Y):-t_(X,R,Y).
tro(X,R,R/Y):-t(X,R,Y).
t_sr(R/X,R,Y):-t_(X,R,Y).
tsr(X/R,R,Y):-t(X,R,Y).

%Предикаты общего назначения----------------------------------------------------------------------
%Длина списка
ln([],0).
ln([_|L],X):-ln(L,X1),X is X1+1.

%Сумма элементов списка
sum([],0).
sum([X|XX],S):-sum(XX,S_), S is X+S_.
	
%Максимальный и минимальный элементы списка
max(X,Max):-sort(0,@>,X,[Max|_]).
min(X,Min):-sort(0,@<,X,[Min|_]).

%Печать списка
writeL_([]):-nl.
writeL_([_/X|L]):-format("~w ",[X]),nl, writeL_(L).
	
writeL([]):-nl.
writeL([X|L]):-format("~w ",[X]),nl, writeL(L).

%Построение расцвеченного семантического графа для заданного множества отношений-------------------------

%%Триплет XRY принадлежит пути Pt для R из RR
tp(RR,X,R,Y,Pt):-
	member(X/Rxy/Y,Pt),
	member(R,Rxy),
	member(R,RR).

%%неповторяющиеся триплеты
tu(X,R,Y):-setof(R_,t(X,R_,Y),RR),member(R,RR). %Только факты и правила
%tu(X,R,Y):-setof(R_,tt(X,R_,_/Y),RR),member(R,RR). %Все транзитивные замыкания 

%триплеты, для запроса в граф
tg(X,RR,Y,X,R,Y):-
	(var(X);var(Y)),
	var_(RR,RR_),!,
	member(R,RR_),
	tu(X,R,Y).
tg(X,_,Y,X_,R,Y_):-
	nonvar(X),nonvar(Y),
	ppr_min(X,Y,Pt,0),
	member(X_/Rxy/Y_,Pt),
	member(R,Rxy).

%%Развертывание структуры в строку
reduce_XRY(X,R,Y, X_,R_,Y_):-
	X=..XX,atomic_list_concat(XX,'_',X_),
	R=..RR,atomic_list_concat(RR,'_',R_),
	Y=..YY,atomic_list_concat(YY,'_',Y_).

var_(R,R_):-var(R),r(R_),!.
var_(R,R):-nonvar(R),!.

%Еще один нефункциональный вариант графа (все остальные а Онтология6)
graf(X,RR,Y,G):-
    drawing(X,RR,Y,D_str),
	coloring(X,RR,Y,C_str),
	atomic_list_concat(['digraph G {rankdir=LR;',D_str,C_str,'}'],' ', S),
	G=dot(S),!.
	%G=circo(S),!.
	%G=neato(S),!.
	
drawing(X,RR,Y,D_str):-
	retractall(s_(_)),
	retractall(ss_(_)),
	start_arc(StrA),start_node(StrN),string_concat(StrA,StrN,Str),
	asserta(ss_(Str)),
	forall(tg(X,RR,Y,X_,R_,Y_),rr_str(X_,R_,Y_)),
	forall(tg(X,RR,Y,X_,R_,Y_),xx_str(X_,R_,Y_)),
	forall(tg(X,RR,Y,X_,R_,Y_),yy_str(X_,R_,Y_)),
	forall(s_(S),ss_str(S)),
	retract(ss_(D_str)),!.

start_arc(Str):-
	arc(all,L_str),
	atomic_list_concat(L_str,', ',Str_),
	not(Str_=''),
	atomic_list_concat(['edge [',Str_,']; '],' ',Str),!.
start_arc('').
	
start_node(Str):-
	node(all,L_str),
	atomic_list_concat(L_str,', ',Str_),
	not(Str_=''),
	atomic_list_concat(['node [',Str_,']; '],' ',Str),!.
start_node('').

coloring(X,_,Y,C_str):-%начальная и конечная точки пути окрашиваются красным
	nonvar(X),nonvar(Y),
	atomic_list_concat([X, '[color=', red, '];',Y,'[color=', red, '];'],' ',C_str),!.
coloring(_,_,_,''):-!.    

	
%расскраска дуг и вершин
rr_str(X,R,Y):-
	reduce_XRY(X,R,Y, X_,R_,Y_),
	r_str(R,StrR),
	atomic_list_concat([X_, ->, Y_, '[label=', R_,' ', StrR , '];'],' ',S),
	asserta(s_(S)).
	
xx_str(X,_,_):-
	x_str(X,StrX),
	assertS(s_(StrX)).
	
yy_str(_,_,Y):-
	x_str(Y,StrY),
	assertS(s_(StrY)).

ss_str(S):- 
	retract(ss_(SS)),
	string_concat(SS,S,SSS),
	asserta(ss_(SSS)).
	
assertS(s_(X)):-retract(s_(X)),asserta(s_(X)),!.
assertS(s_(X)):-asserta(s_(X)).
	
r_str(R,Str):-
	arc(R,LAtr),not(LAtr=[]),
	atomic_list_concat([' '|LAtr],', ',Str),!.     
r_str(R,''):-arc(R,[]),!.
	
x_str(X,Str):-
	node(X,LAtr),not(LAtr=[]),
	atomic_list_concat(LAtr,', ',Str_),
	atomic_list_concat([X,'[',Str_,']; '],' ',Str),!.
x_str(_,''):-!.
	
%Поск ациклического пути на графе
pp(X,Y,Pt):-pp_(X,[Y],Pt),not([Y]=Pt).
pp_(X,[X|P],[X|P]).
pp_(X,[Z|Pz],Pt):-
	t(V,_,Z),
	not(member(V,Pz)),
	pp_(X,[V,Z|Pz],Pt).

%Исключение повторяющихся ациклических путей
ppl(X,Y,PPt):-setof(Pt,pp(X,Y,Pt),PPt).
ppu(X,Y,Pt):-ppl(X,Y,Ptt),member(Pt,Ptt).
	
%Оснащение списка ациклического пути связями
ppr(X,Y,Ptr):-ppu(X,Y,Pt),in_r(Pt,Ptr).
	
%%Оснащение списка  длиной пути
ppr_(X,Y,[Ln|Pt]):-ppr(X,Y,Pt),length(Pt,Ln).
	
in_r([_],[]).
in_r([X,Y|XX],[X/RR/Y|XXr]):-
	setof(R,t(X,R,Y),RR),
	in_r([Y|XX],XXr).
	
%Упорядоченные по длине пути из X в Y
pprg(X,Y,PPtrg):-setof(Pt,ppr_(X,Y,Pt),PPtrg).
	
%Путь с минимальной длинной(количеством дуг) при L=0. L - превышение относительно минимума
ppr_min(X,Y,Pt,L):-
	setof(Pt,ppr_(X,Y,Pt),PPtrg),
	PPtrg=[[L1|_]|_],!,
	member([L_|Pt],PPtrg),L_=<L1+L.
	
%Ранжирование узлов-доноров по емкости
s_rk(Lrk):-s(L),rks(L,Lr),sort(0,@>,Lr,Lrk),!.
rks([],[]).
rks([S|SS],[Rk/S|SSr]):-
	%so(S,Out),
	(setof(O,R^t(S,R,O),Out); so(_,[])),
	length(Out,Rk),
	rks(SS,SSr).

%%Ранжирование узлов-приемников по емкости 
o_rk(Lrk):-o(L),rko(L,Lr),sort(0,@>=,Lr,Lrk),!.
rko([],[]).
rko([O|SS],[Rk/O|SSr]):-
	%os(O,In),
	(setof(S,R^t(S,R,O),In); os(_,[])),
	length(In,Rk),
	rko(SS,SSr).
	
%%Формализация знания. 
%Знать объект. ZZ - список соответствия истина/знание. 
%Ass - оценка "кол_правильных:кол_всех"
know_o(X,ZZ,Ass):-
	s_o(X,YY),
	%write('Устнановите правильный объект из следующего списка:'),nl,
	format("Для субъекта '~w' устнановите правильный объект из следующего списка:",[X]),nl, 
	writeL_(YY),%Надо бы рандомизировать YY  или как-то его разбавить!
	ask_o(X,YY,ZZ),
	ass(ZZ,Ass).

%Недостаток данного варианта - порядок вопросов и порядок предлагаемых ответов соответствуют.
%Тредуется рандомизация списка с целью сокращения количества вопросов.
ask_o(_,[],[]):-!.
ask_o(X,[R/Y|YY],[Y/A|ZZ]):-
	format("Объект'~w' находится в отношении '~w' с объектом ...",[X,R]),nl, 
	prompt(_,'введите объект: (можно его выделить в предлагаемом списке и перетащить)'),
	read(A),
	ask_o(X,YY,ZZ).

%Знать предикат для объекта. ZZ - список соответствия истина/знание
%Ass - оценка "кол_правильных:кол_всех"
know_r(X,ZZ,Ass):-
	s_o(X,YY),
	sr(X,RR),sort(0,@<,[индивид,подкласс,подкласс,экв,причина,следствие|RR],SRR),
	write('Устнановите правильное отношение из следующего списка:'),nl,
	writeL(SRR),%Надо бы рандомизировать SRR  или как-то его разбавить!
	ask_r(X,YY,ZZ),
	ass(ZZ,Ass).
	
ask_r(_,[],[]):-!.
ask_r(X,[R/Y|YY],[R/A|ZZ]):-
	format("Объект'~w' находится в отношении ... с объектом '~w'",[X,Y]),nl, 
	prompt(_,'введите отношение: (можно его выделить в предлагаемом списке и перетащить)'),
	read(A),
	ask_r(X,YY,ZZ).
	
%Количество совпадающих пар в списке пар???(не работает для функторов, например, a+1=a+1)
eq_par([],0).
eq_par([A/B|L1],X):-A=B,eq_par(L1,X1),X is X1+1,!.
eq_par([_|L1],X1):-eq_par(L1,X1),!.
	
%Оценка знаний (отношение количества правильных ответов к общему количеству вопросов)
ass(L,A):-eq_par(L,X),ln(L,Y),A=X:Y.
	
%Карта знаний G об объекте X (что мы знаем об X?). 
%Отображение цветом на онтографе оценок AS-вершинам и AR-по связям 
know_map(X,AS,AR,G):-
	know_o(X,ZY,AS),    
	know_r(X,ZR,AR),    
	color_k(X,ZR,ZY),
	r(RR),graf(_,RR,_,G),!,
	retract(node(_,_)),retract(arc(_,_)),!.
	
color_k(X,ZR,ZY):-
	member(R/R_,ZR),
	member(Y/Y_,ZY),
	t(X,R,Y), 
	color_ans(R/R_,Cr),
	color_ans(Y/Y_,Cy),
	%assert_k(color_known(X,R,Y,blue,Cr,Cy)),
	asserta(node(X,blue)), 
	asserta(arc(R,Cr)), 
	asserta(node(Y,Cy)), 
	fail.
color_k(_,_,_):-!.
	
%Исключение повторов в базе данных
assert_k(color_known(X,R,Y,_,_,_)):-retract(color_known(X,R,Y,_,_,_)).
assert_k(color_known(X,R,Y,Cx,Cr,Cy)):-assertz(color_known(X,R,Y,Cx,Cr,Cy)).
	
%Ответ-цвет
color_ans(X/X,green):-!.
color_ans(_,red):-!.
	 
	
	
%**************************************************************
%Синтаксический анализатор фраз на КЕЯ
sentence_sub([Cl])-->clause_sub(Cl),dot.
sentence_sub([Cl|Ss])-->clause_sub(Cl),dot,sentence_sub(Ss).
	
clause_sub(S)-->sub(S),
	pred_obj(_).

sentence([Cl])-->clause(Cl),dot.
sentence([Cl|Ss])-->clause(Cl),dot,sentence(Ss).
	
clause(cl(S,P_Os))-->sub(S),
	pred_obj(P_Os).

pred_obj([P/Os])-->pred(P),obj(Os).  
pred_obj([P/Os|POs])-->pred(P),obj(Os),semicolon,pred_obj(POs).
	
obj([O])-->tok(O).
obj([O|Os])-->tok(O),coma,obj(Os).
	
sub(S)-->[S].
pred(P)-->[P].
	
tok(A)-->[A].
	
dot-->['.'].
coma-->[','].
coma-->['и'].
semicolon-->[;].
	
t(X,R,Y):-
	data(SS),!,
	member(cl(X,RR),SS),
	member(R/YY,RR),
	member(Y,YY).
	
parser(Str):-
	retractall(data(_)),
	string_lower(Str,Str_L),
	tokenize_atom_(Str_L , TokenList ),     
	sentence(S, TokenList,[]),
	assert(data(S)),!.

parser_subs(Str, S):-
	string_lower(Str,Str_L),
	tokenize_atom_(Str_L , TokenList ),     
	sentence_sub(S, TokenList,[]),
	!.
%parser:-writeln("Что-то здесь не так! Возможно некорректно имя.").

%Стандарный предикат: tokenize_atom("a_b_c d", [a,'_',b,'_',c, d])
%Модифицированный предикат: tokenize_atom_("a_b_c d", [a_b_c, d])
tokenize_atom_(Str, TokenList_1):-
    tokenize_atom(Str, TokenList),
    t_1(TokenList_,TokenList,[]),
    t_(TokenList_1,TokenList_,[]).

%Элегантное решение для распознования цепочек типа a_b_c через DCG
%?-t_([a_b_c,d],[a,'_',b,'_',c, d],[])
t_([A])-->a(A).
t_([A|Ts])-->a(A),t_(Ts).
t_([A_T|Ts])-->a(A),['_'],t_([T|Ts]),{atomic_list_concat([A,'_',T],A_T)}.
a(A)-->[A],{ not(A='_')}.

t_1([T])-->a1(T).
t_1([A|Ts])-->a1(A),t_1(Ts).
a1(A)-->[A],['('],t_(_),[')'],!.   
a1(A)-->[A].