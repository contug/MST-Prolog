%%%% Constantin Brinza 857047 - Giacomo Contu 856672

%%%% -*- Mode: Prolog -*-

:- dynamic graph/1.
:- dynamic vertex/2.
:- dynamic arc/4.

new_graph(G) :- graph(G), !.
new_graph(G) :- assert(graph(G)), !.

delete_graph(G) :-
    retractall(arc(G, _, _, _)),
    retractall(vertex(G, _)),
    retract(graph(G)).

new_vertex(G, V) :- vertex(G, V), !.
new_vertex(G, V) :-
    graph(G),
    assert(vertex(G, V)), !.

graph_vertices(G, Vs) :-
    graph(G),
    findall(V, vertex(G, V), Vs).

list_vertices(G) :-
    graph(G),
    listing(vertex(G, _)).

new_arc(G, U, V) :- new_arc(G, U, V, 1), !.

new_arc(G, U, V, Weight) :-
    arc(G, U, V, Weight),
    !.

new_arc(G, U, V, Weight) :-
    arc(G, V, U, Weight),
    !.

new_arc(G, U, V, Weight) :-
    arc(G, U, V, W1),
    Weight \= W1,
    retract(arc(G, U, V, W1)),
    assert(arc(G, U, V, Weight)), !.

new_arc(G, U, V, Weight) :-
    arc(G, V, U, W1),
    Weight \= W1,
    retract(arc(G, V, U, W1)),
    assert(arc(G, U, V, Weight)), !.

new_arc(G, U, V, Weight) :-
    graph(G),
    vertex(G, U),
    vertex(G, V),
    assert(arc(G, U, V, Weight)), !.


graph_arcs(G, Es) :-
    graph(G),
    findall(arc(G, U, V, Weight), arc(G, U, V, Weight), Es).

vertex_neighbors(G, V, Ns) :-
    vertex(G, V),
    findall(arc(G, V, N, Weight),
            arc(G, V, N, Weight), Ns1),
    findall(arc(G, N, V, Weight),
            arc(G, N, V, Weight), Ns2),
    append(Ns1, Ns2, Ns).

adjs(G, V, Vs) :-
    vertex(G, V),
    findall(N, arc(G, V, N, _), Vs1),
    findall(N, arc(G, N, V, _), Vs2),
    append(Vs1, Vs2, Vs).

list_arcs(G) :-
    graph(G),
    listing(arc(G, _, _, _)).

list_graph(G) :-
    list_vertices(G),
    list_arcs(G).

read_graph(G, FileName) :- graph(G),
    delete_graph(G),
    csv_read_file(FileName, Output, [separator(0'\t)]),
    save(G, Output), !.

read_graph(G, FileName):- new_graph(G),
    csv_read_file(FileName, Output, [separator(0'\t)]),
    save(G, Output), !.

save(_, []) :- !.
save(G, [T | H]) :- arg(1, T, X),
    arg(2, T, Y),
    arg(3, T, Z),
    new_vertex(G, X),
    new_vertex(G, Y),
    new_arc(G, X, Y, Z),
    save(G, H).

write_graph(G, FileName) :-
    write_graph(G, FileName, graph).

write_graph(G, FileName, graph) :-
    graph(G),
    findall(row(A, B, C), arc(G, A, B, C), Input),
    csv_write_file(FileName, Input, [separator(0'\t)]), !.

write_graph(G, FileName, edge) :-
    format_arc(G, X),
    csv_write_file(FileName, X, [separator(0'\t)]).

format_arc([], []) :- !.
format_arc([arc(_, A, B, C) | Xs], [arc(A, B, C) | Ys]) :-
    format_arc(Xs, Ys).





:- dynamic vertex_key/3.
:- dynamic vertex_previous/3.

restore(_G) :-
    heap(q, _),
    delete_heap(q),
    retractall(vertex_key(_, _, _)),
    retractall(vertex_previous(_, _, _)), !.

restore(_) :-
    not(heap(q, _)), !.

mst_prim(G, Source) :-
    vertex(G, Source),
    restore(G),
    graph_vertices(G, Ls),
    key_set(G, Ls),
    retract(vertex_key(G, Source, inf)),
    asserta(vertex_key(G, Source, 0)),
    list_vertex_key(G, List_vertex_key),
    new_heap(q),
    fill_queue(q, List_vertex_key),
    heap_has_size(q, S),
    prim(q, G, S), !.

prim(_, _, 0) :- !.

prim(q, G, S) :-
    heap_not_empty(q),
    heap_extract(q, _, U),
    adjs(G, U, Ns),
    compare(Ns, q, G, U),
    S1 is S - 1,
    prim(q, G, S1), !.


compare([], _, _, _) :- !.

compare([V | Vs], q, G, U) :-
    not(heap_entry(q, _, _, V)),
    compare(Vs, q, G, U), !.


compare([V | Vs], q, G, U) :-
    vertex_key(G, V, K1),
    heap_entry(q, _, K1, V),
    arc_weight(G, U, V, W),
    not(W < K1),
    compare(Vs, q, G, U), !.


compare([V | Vs], q, G, U) :-
    vertex_key(G, V, K1),
    heap_entry(q, _, K1, V),
    arc_weight(G, U, V, W),
    W < K1,
    retract(vertex_previous(G, V, _)),
    assert(vertex_previous(G, V, U)),
    retract(vertex_key(G, V, _)),
    assert(vertex_key(G, V, W)),
    modify_key(q, W, K1, V),
    compare(Vs, q, G, U), !.


key_set(_, []) :- !.

key_set(G, [X | Xs]) :-
    assert(vertex_key(G, X, inf)),
    assert(vertex_previous(G, X, nil)),
    key_set(G, Xs), !.

list_vertex_key(G, Ls) :-
    findall(vertex_key(G, V, K), vertex_key(G, V, K), Ls).

fill_queue(_, []) :- !.

fill_queue(q, [vertex_key(_, V, K) | Xs]) :-
    heap_insert(q, K, V),
    fill_queue(q, Xs), !.


arc_weight(G, U, V, Weight) :-
    arc(G, V, U, Weight), !.

arc_weight(G, U, V, Weight) :-
    arc(G, U, V, Weight), !.


list_order2([], X, Ordered) :-
    append(X, [], Ordered), !.
list_order2([vertex_key(G, V, _) | Ks], Xs, Ordered) :-
    vertex_previous(G, V, P),
    append(Xs, [vertex_previous(G, V, P)], Ls),
    list_order2(Ks, Ls, Ordered), !.

list_order1([], X, Ordered) :-
    sort(3, =<, X, Ordered), !.
list_order1([vertex_previous(G, V, _)| Ps], Xs, Ordered) :-
    vertex_key(G, V, K),
    append(Xs, [vertex_key(G, V, K)], Ls),
    list_order1(Ps, Ls, Ordered), !.


mst_tree(_, [], _) :- !.
mst_tree(Source, Vs, Q) :-
    list_order1(Vs, [], Xs),
    list_order2(Xs, [], Ordered),
    Ordered = [vertex_previous(G, V, Source) | Ps],
    vertex_key(G, V, K),
    add_queue(Q, arc(G, Source, V, K)),
    findall(vertex_previous(G, U, V),
            vertex_previous(G, U, V), Child),
    mst_tree(V, Child, Q),
    mst_tree(Source, Ps, Q), !.

mst_get(G, Source, PreorderTree) :-
    assert(queue(q, zero, 0)),
    retractall(queue(_, _, _)),
    findall(vertex_previous(G, V, Source),
            vertex_previous(G, V, Source), Child),
    mst_tree(Source, Child, q),
    findall(arc(G, V, U, K),
            queue(_, arc(G, V, U, K), _),
            PreorderTree), !.


:- dynamic queue/3.

add_queue(Q, Elemento) :-
    findall(queue(_, _, _), queue(_, _, _), Qs),
    length(Qs, X),
    Y is X + 1,
    assert(queue(Q, Elemento, Y)), !.





:- dynamic heap/2.
:- dynamic heap_entry/4.

new_heap(H) :-
    heap(H, _S), !.

new_heap(H) :-
    assert(heap(H, 0)), !.

new_heap(H, S) :- heap(H, S), !.
new_heap(H, S) :- assert(heap(H, S)), !.

delete_heap(H) :-
    retractall(heap_entry(H, _, _, _)),
    retract(heap(H, _)).

heap_has_size(H, S) :-
    heap(H, S).

heap_empty(H) :-
    heap(H, 0).

heap_not_empty(H) :-
    heap(H, S),
    S >= 1.

heap_head(H, K, V) :-
    heap(H, _S),
    heap_entry(H, 1, K, V).

list_entry(H, L) :-
    heap(H, _),
    findall(heap_entry(H, P, K, V), heap_entry(H, P, K, V), Ls),
    sort(2, <, Ls, L),
    !.

heap_insert(H, K, V) :-
    heap_entry(H, _, K, V),
    !,
    fail.

heap_insert(H, K, V) :-
    heap(H, S),
    heap_empty(H),
    S1 is S + 1,
    retract(heap(H, S)),
    new_heap(H, S1),
    assert(heap_entry(H, 1, K, V)), !.

heap_insert(H, K, V) :-
    retract(heap(H, S)),
    S1 is S + 1,
    new_heap(H, S1),
    assert(heap_entry(H, S1, K, V)),
    ordina(H, S1, K, V),
    !.


nesimo(1, [X | _], X) :- !.
nesimo(N, [_ | Ls], X) :-
    N1 is N - 1,
    nesimo(N1, Ls, X), !.


ordina(_H, Element_index, _, _) :- Element_index = 1, !.

ordina(H, Element_index, K, _) :-
    Parent_index is floor(Element_index / 2),
    heap_entry(H, Parent_index, Parent_key, _),
    K >= Parent_key, !.

ordina(H, Element_index, K, V) :-
    Parent_index is floor(Element_index / 2),
    heap_entry(H, Parent_index, Parent_key, Parent_value),
    K < Parent_key,
    retract(heap_entry(H, Parent_index, Parent_key, Parent_value)),
    retract(heap_entry(H, Element_index, K, V)),
    assert(heap_entry(H, Parent_index, K, V)),
    assert(heap_entry(H, Element_index, Parent_key, Parent_value)),
    ordina(H, Parent_index, K, V), !.


heap_extract(H, K, V) :-
    heap_has_size(H, S),
    S > 1,
    retract(heap_entry(H, 1, K, V)),
    retract(heap_entry(H, S, K1, V1)),
    retract(heap(H, S)),
    S1 is S - 1,
    assert(heap(q, S1)),
    assert(heap_entry(H, 1, K1, V1)),
    heapify(H, 1, S1), !.

heap_extract(H, K, V) :-
    heap_has_size(H, S),
    S = 1,
    retract(heap_entry(H, 1, K, V)),
    retract(heap(H, S)),
    assert(heap(H, 0)), !.

heap_extract(H, _, _) :-
    heap_empty(H), !.


heapify(H, I, Length) :-
    Length >= (2 * I),
    Length >= ((2 * I) + 1),
    Left is 2 * I,
    Right is (2 * I) + 1,
    heap_entry(H, I, K, V),
    heap_entry(H, Left, Kl, Vl),
    heap_entry(H, Right, Kr, _),
    Kl < K,
    Kl =< Kr,
    retract(heap_entry(H, I, K, V)),
    retract(heap_entry(H, Left, Kl, Vl)),
    assert(heap_entry(H, Left, K, V)),
    assert(heap_entry(H, I, Kl, Vl)),
    heapify(H, (2 * I), Length),
    !.

heapify(H, I, Length) :-
    Length >= (2 * I),
    Length >= ((2 * I) + 1),
    Left is 2 * I,
    Right is (2 * I) + 1,
    heap_entry(H, I, K, V),
    heap_entry(H, Left, Kl, _),
    heap_entry(H, Right, Kr, Vr),
    Kr < K,
    Kr < Kl,
    retract(heap_entry(H, I, K, V)),
    retract(heap_entry(H, Right, Kr, Vr)),
    assert(heap_entry(H, Right, K, V)),
    assert(heap_entry(H, I, Kr, Vr)),
    heapify(H, ((2 * I) + 1), Length),
    !.

heapify(H, I, Length) :-
    Length >= (2 * I),
    Length >= ((2 * I) + 1),
    Left is 2 * I,
    Right is (2 * I) + 1,
    heap_entry(H, I, K, _),
    heap_entry(H, Left, Kl, _),
    heap_entry(H, Right, Kr, _),
    K =< Kr,
    K =< Kl,
    !.

heapify(H, I, Length) :-
    Length >= (2 * I),
    Length < ((2 * I) + 1),
    Left is 2 * I,
    heap_entry(H, I, K, V),
    heap_entry(H, Left, Kl, Vl),
    Kl < K,
    retract(heap_entry(H, I, K, V)),
    retract(heap_entry(H, Left, Kl, Vl)),
    assert(heap_entry(H, Left, K, V)),
    assert(heap_entry(H, I, Kl, Vl)),
    heapify(H, (2 * I), Length),
    !.

heapify(H, I, Length) :-
    Length >= (2 * I),
    Length < ((2 * I) + 1),
    Left is 2 * I,
    heap_entry(H, I, K, _),
    heap_entry(H, Left, Kl, _),
    K =< Kl,
    !.

heapify(_, I, Length) :-
    Length < (2 * I),
    !.

modify_key(H, NewKey, OldKey, V) :-
    not(heap_entry(H, _, NewKey, V)),
    retract(heap_entry(H, P, OldKey, V)),
    assert(heap_entry(H, P, NewKey, V)),
    ordina(H, P, NewKey, V), !.

list_heap(H) :-
    list_entry(H, Ls),
    length(Ls, N),
    ordina_dati(Ls, N),
    listing(heap_entry(H, _, _, _)), !.


ordina_dati([], _) :- !.

ordina_dati(_, 0) :- !.

ordina_dati(Ls, N) :-
    nesimo(N, Ls, X),
    retract(X),
    asserta(X),
    N1 is N - 1,
    ordina_dati(Ls, N1), !.



%%%% end of file -- mst.pl --
