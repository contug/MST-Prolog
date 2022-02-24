Constantin Brinza 857047 - Giacomo Contu 856672


MST Prim - Prolog


Interfaccia grafi


P: save/2
Sotto Predicato di “read_graph”.
Dopo aver letto il file .csv, salva i vertici e archi dentro la base di dati Prolog.


P: format_arc/2
Sotto Predicato di “write_graph”.
Indica il formato degli archi da scrivere sul file .csv.






MST Prim


P: restore/1
Sotto Predicato di MST-Prim.
Ripristina lo heap per essere pronto in vista di un nuovo lancio dell’algoritmo MST-Prim.


P: prim/3
Sotto Predicato di MST-Prim.
Chiama “heap-extract/3” per essere successivamente confrontato con i vertici “vicini”.


P: compare/4
Sotto Predicato di MST-Prim.
Esegue il confronto tra il peso dell’arco che collega il vertice preso in considerazione
e la chiave di uno dei suoi “vertex-adjs”, fino ad esaurire la lista di tutti i vertici vicini.


P: key_set/2
Per ogni vertice di G, inserisce i predicati vertex_key e vertex_previous 
con valori inf e nil nella base dati.


P: list_vertex_key/2
Restituisce una lista di ogni vertex_key(G, V, K) presente nella base dati.


P: fill_queue/2
Sotto Predicato di MST-Prim.
Inizializza la “priority-queue” Q dopo aver settato tutte le “keys” dei vertici 
a “inf” e i “previous” a nil.


P: arc_weight/4
Dati due vettori U e V, restituisce il peso dell’arco che gli unisce.


P: list_order1/3 e list_order2/3
Data una lista di vertex_previous(G, V, P) non ordinata,
i due predicati (da usare concatenati) restituiscono una lista di 
vertex_previous(G, V, P) ordinati secondo le rispettive chiavi.


P: mst_tree/3
Dato un vertice Source, utilizza il predicato add_queue per aggiungere gli 
archi appartenenti al MST ad una coda, secondo una lettura preorder


P: add_queue/3
Aggiunge queue(Q, arc(G, V, U, Weight), Indice) alla base dati, 
per ottenere la lista PreorderTree nel predicato mst_get.




Min-Heap


P: list_entry/2
Ritorna la lista di tutte le “heap_entry” di uno heap.


P: ordina/4
Simile alla “swim operation” in un min-heap. 
Confronta il parent dell’elemento (all’indice “index”) e li scambia se parent.key 
risulta maggiore di child.key. Il processo è ripetuto finchè l’elemento si trova 
nella posizione “giusta” che rispetti la “Heap priority”.


P: heapify/3
Implementazione dell’algoritmo Heapify su Lista e heap H, con indice I e lunghezza Length.


P: ordina_dati/2
Riordina la base dati in base alla posizione delle “heap_entry” per l'utilizzo del predicato list.