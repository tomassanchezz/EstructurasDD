#include <iostream>
#include "LinkedList.h"

using namespace std;

struct NodoL {
    int elem; // valor del nodo
    NodoL* siguiente; // puntero al siguiente nodo
};

struct LinkedListSt {
    int cantidad; // cantidad de elementos
    NodoL* primero; // puntero al primer nodo
    NodoL* actual; // puntero al nodo actual (para recorridos)
    NodoL* ultimo; // puntero al ultimo nodo (para que snoc sea O(1))
};

typedef LinkedListSt* LinkedList;

//Crea una lista vac�a.
LinkedList nil() {
    LinkedList xs = new LinkedListSt;

    xs -> cantidad = 0;
    xs -> primero = NULL;
    xs -> actual = NULL;
    xs -> ultimo = NULL;

    return xs;
}

//Indica si la lista est� vac�a.
bool isEmpty(LinkedList xs) {
    return xs->cantidad == 0;
}

//Devuelve el primer elemento.
int head(LinkedList xs) {
    return xs->primero->elem;
}

//Agrega un elemento al principio de la lista.
void cons(int x, LinkedList xs) {
    NodoL* node = new NodoL;

    node -> elem = x;
    node -> siguiente = xs -> primero;

    xs -> primero = node;
    xs -> cantidad++;
}

//Quita el primer elemento.
void tail(LinkedList xs) {
    NodoL* exPrimero = xs -> primero; //Se almacena el primero para luego poder eliminarlol.
    xs -> primero = xs -> primero -> siguiente;
    delete exPrimero;
}

//Devuelve la cantidad de elementos.
int length(LinkedList xs) {
    return xs -> cantidad;
}

//Agrega un elemento al final de la lista.
void snoc(int x, LinkedList xs) {
    if (isEmpty(xs)) {
        cons(x, xs);
    } else {
        initialize (xs);
        while (!finished(xs)){
            next(xs);
        }
        cons(x, xs);
    }

//    NodoL* exUltimo = xs -> ultimo;
//    NodoL* nuevoUltimo = new NodoL;
//
//    nuevoUltimo -> elem = x;
//    nuevoUltimo -> siguiente = NULL;
//
//    exUltimo -> siguiente = nuevoUltimo;
//    xs -> ultimo = nuevoUltimo;
}

//Apunta el recorrido al primer elemento
void initialize(LinkedList xs) {
    xs -> actual = xs -> primero;
}

//Devuelve el elemento actual en el recorrido.
int current(LinkedList xs) {
    return xs -> actual -> elem;
}

//Reemplaza el elemento actual por otro elemento.
void setCurrent(int x, LinkedList xs) {
    xs -> actual -> elem = x;
}

//Pasa al siguiente elemento.
void next(LinkedList xs) {
    xs -> actual = xs -> actual -> siguiente;
}

//Indica si el recorrido ha terminado.
bool finished(LinkedList xs) {
    return xs -> actual == NULL;
}


//Libera la memoria ocupada por la lista
void destroyL(LinkedList xs) {
    while (!isEmpty(xs)) {
        tail (xs);
    }
    delete xs;
}

void printLinkedList (LinkedList xs) {
    NodoL* nodoActual = xs -> primero;

    cout << "[";
    while (nodoActual != NULL) {
        cout << " " << nodoActual->elem;
        nodoActual = nodoActual->siguiente;
    }
    cout << "]";
}






