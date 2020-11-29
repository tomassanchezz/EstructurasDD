#include "queue.h"

struct NodeQ {
  ElemQ elem;
  NodeQ* next;
};

struct QueueRep {
    NodeQ* first;
    NodeQ* last;
    NodeQ* current;
    int len;
};

/**
Inv. Rep:
  + si first es NULL, last también
  + len es la cantidad de nodos desde first hasta last
**/

// emptyQ :: Queue a
// O(1)
Queue emptyQ() {
    QueueRep* qRep = new QueueRep;
    qRep->len   = 0;
    qRep->first = NULL;
    qRep->last  = NULL;
    qRep->current = NULL;
    return qRep;
}

// isEmptyQ :: Queue a -> Bool
// O(1)
bool isEmptyQ(Queue q) {
    return q->len == 0;
}

/// subtarea
// Prec.: nodoActual no es NULL
// O(n), caro
//void agregarAlFinal(NodeQ* nodoActual, NodeQ* nuevoNodo) {
//    while(nodoActual->next != NULL) {
//        nodoActual = nodoActual->next;
//    }
//    nodoActual->next = nuevoNodo;
//}

// enqueue :: a -> Queue a -> Queue a
// O(1)
void enqueue(ElemQ x, Queue q) {
    NodeQ* node = new NodeQ;
    node->elem = x;
    node->next = NULL;
    if (q->first == NULL) {
        q->first = node;
        q->last  = node;
    } else {
        q->last->next = node;
        q->last = node;
    }
    q->len++;
}

// dequeue :: Queue a -> Queue a
void dequeue(Queue q) {
    NodeQ* tmp = q->first;
    q->first = q->first->next;
    delete tmp;
    if(q->first == NULL) {
        q->last = NULL;
    }
    q->len--;
}

// firstQ   :: Queue a -> a
ElemQ firstQ(Queue q) {
    return q->first->elem;
}

// lenQ    :: Queue a -> Int
int lenQ(Queue q) {
   return q->len;
}

// O(n)
Queue copiarQ(Queue q) {
    Queue newQ = emptyQ();
    NodeQ* nodoActual = q->first;
    while(nodoActual != NULL) {
        enqueue(nodoActual->elem, newQ);
        nodoActual = nodoActual->next;
    }
    return newQ;
}

void printQ(Queue q) {
    cout << "Queue:" << endl;
    cout << "  first: "  << q->first << endl;
    cout << "  last:  "  << q->last  << endl;
    cout << "  len: "    << q->len   << endl;
    cout << "  nodos: "  << endl;
    NodeQ* nodoActual = q->first;
    while(nodoActual != NULL) {
        cout << "    dir: " << nodoActual       << ", ";
        cout << "elem: "    << nodoActual->elem << ", ";
        cout << "next: "    << nodoActual->next;
        cout << endl;
        nodoActual = nodoActual->next;
    }
}

void destroyQ(Queue q) {
    for(int i = 0; i < q->len; i++) {
        dequeue(q);
    }
    delete q;
}

/**
  ITERADOR
**/

void inicializarRecorrido(Queue q) {
    q->current = q->first;
}

void pasarAlSiguienteElemento(Queue q) {
    q->current = q->current->next;
}

ElemQ obtenerElementoActual(Queue q) {
    return q->current->elem;
}

bool terminoElRecorrido(Queue q) {
    return q->current == NULL;
}
