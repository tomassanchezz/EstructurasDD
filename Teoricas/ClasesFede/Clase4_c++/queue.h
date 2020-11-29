#include <iostream>
#include "tree.h"

using namespace std;

typedef Tree ElemQ;

struct NodeQ;

struct QueueRep;

typedef QueueRep* Queue;

// emptyQ :: Queue a
Queue emptyQ();

// isEmptyQ :: Queue a -> Bool
bool isEmptyQ(Queue q);

// enqueue :: a -> Queue a -> Queue a
void enqueue(ElemQ x, Queue q);

// dequeue :: Queue a -> Queue a
void dequeue(Queue q);

// firstQ   :: Queue a -> a
ElemQ firstQ(Queue q);

// lenQ    :: Queue a -> Int
int lenQ(Queue q);

Queue copiarQ(Queue q);

void printQ(Queue q);

void destroyQ(Queue q);

/**
  ITERADOR
**/

void inicializarRecorrido(Queue q);
void pasarAlSiguienteElemento(Queue q);
ElemQ obtenerElementoActual(Queue q);
bool terminoElRecorrido(Queue q);
