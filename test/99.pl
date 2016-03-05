% 99 Prolog Problems
% from http://www.ic.unicamp.br/~meidanis/courses/mc336/2009s2/prolog/problemas/
%
% Solutions to at least a few of these, for testing purposes.

% P01
my_last(X, [X]).
my_last(X, [_ | T]) :-
    my_last(X, T).

% P02
my_lastbutone(X, [X, _]).
my_lastbutone(X, [_ | T]) :-
    my_lastbutone(X, T).

% P03
my_nth(X, [X | _], 1).
my_nth(X, [_ | T], N) :-
    M is N - 1,
    my_nth(X, T, M).

% P04
my_len([], 0).
my_len([_ | T], Length) :-
    my_len(T, M),
    Length is M + 1.

my_len_acc([], A, A).
my_len_acc([_ | T], A, Length) :-
    B is A + 1,
    my_len_acc(T, B, Length).

my_len2(L, Length) :-
    my_len_acc(L, 0, Length).

% P05
my_reverse_acc([], Acc, Acc).
my_reverse_acc([X | T], Acc, Reversed) :-
    my_reverse_acc(T, [X | Acc], Reversed).

my_reverse(L, R) :-
    my_reverse_acc(L, [], R).

% P06
my_palindrome(L) :-
    my_reverse(L, L).

% P07
my_flatten([], []).

my_flatten([Atom | Tail], [Atom | FlatTail]) :-
    \+ is_list(Atom),
    my_flatten(Tail, FlatTail).

my_flatten([HeadList | Tail], Flattened) :-
    is_list(HeadList),
    my_flatten(HeadList, FlatHeadList),
    my_flatten(Tail, FlatTail),
    append(FlatHeadList, FlatTail, Flattened).
