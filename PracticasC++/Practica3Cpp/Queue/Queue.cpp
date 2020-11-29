#include <iostream>
#include "Queue.h"

using namespace std;

struct NodoQ {
    int elem; // valor del nodo
    NodoQ* siguiente; // puntero al siguiente nodo
};

struct QueueSt {
    int cantidad; // cantidad de elementos
    NodoQ* primero; // puntero al primer nodo
    NodoQ* ultimo; // puntero al ultimo nodo
};

typedef QueueSt* Queue;

//Crea una lista vacía.
//Costo: O(1).
Queue emptyQ() {
    Queue q = new QueueSt;

    q -> cantidad = 0;
    q -> primero = NULL;
    q -> ultimo = NULL;

    return q;
}

//Indica si la lista está vacía.
//Costo: O(1).
bool isEmptyQ(Queue q) {
    return q -> primero == NULL;
}

//Devuelve el primer elemento.
//Costo: O(1).
int firstQ(Queue q) {
    return q -> primero -> elem;
}

//Agrega un elemento al final de la cola.
//Costo: O(1).
void enqueue(int x, Queue q) {
    NodoQ* nuevoNodo = new NodoQ;

    nuevoNodo -> elem = x;
    nuevoNodo -> siguiente = NULL;

    if (!isEmptyQ(q)) {
        q -> ultimo -> siguiente = nuevoNodo;
    } else {
        q -> primero = nuevoNodo;
    }

    q -> ultimo = nuevoNodo;
    q -> cantidad++;
}

//Quita el primer elemento de la cola.
//Costo: O(1).
void dequeue(Queue q) {
    NodoQ* exPrimero = q -> primero;

    q -> primero = q -> primero -> siguiente;
    q -> cantidad--;

    delete exPrimero;
}

//Devuelve la cantidad de elementos de la cola.
//Costo: O(1).
int lengthQ(Queue q) {
    return q -> cantidad;
}

//Anexa q2 al final de q1, liberando la memoria inservible de q2 en el proceso.
//Nota: Si bien se libera memoria de q2, no necesariamente la de sus nodos.
//Costo: O(n).
void mergeQ(Queue q1, Queue q2) {
    while (!isEmptyQ(q2)) {
        enqueue(firstQ(q2), q1);
        dequeue(q2);
    }
    delete q2;
}

//Libera la memoria ocupada por la lista.
//Costo: O(n).
void destroyQ(Queue q) {
    while (!isEmptyQ(q)) {
        dequeue(q);
    }
    delete q;
}

void printQ (Queue q) {
    NodoQ* nodoActual = q -> primero;

    cout << "[";
    while (nodoActual != NULL) {
        cout << " " << nodoActual->elem;
        nodoActual = nodoActual->siguiente;
    }
    cout << "]";
}


