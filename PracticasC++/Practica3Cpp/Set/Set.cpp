#include <iostream>
#include "Set.h"
#include "LinkedList.h"

using namespace std;

struct NodoS {
    int elem; // valor del nodo
    NodoS* siguiente; // puntero al siguiente nodo
};

struct SetSt {
    int cantidad; // cantidad de elementos diferentes
    NodoS* primero; // puntero al primer nodo
};

typedef SetSt* Set;

Set emptyS() {
    Set s = new SetSt;

    s -> cantidad = 0;
    s -> primero = NULL;

    return s;
}

bool isEmptyS(Set s) {
    return s -> cantidad == 0;
}

void belongsS(int x, Set s) {
    int actual = s -> primero;

    while (actual != NULL && actual -> elem != x) {
        actual = actual -> siguiente;
    }

    return actual -> elem == x;
}

void addS(int x, Set s) {
    NodoS* nodo = new NodoS;

    nodo -> elem = x;
    nodo -> siguiente = s -> primero;

    if (belongsS (x, s)) {
        delete (nodo);
    } else {
        s -> primero = nodo;
        s -> cantidad++;
    }
}

void removeS(int x, Set s) {
    NodoS* actual = s -> primero;
    NodoS* anterior = NULL;

    if (belongsS(x, s)) {
        while(actual -> elem != x) {
            anterior = actual;
            actual = actual -> siguiente;
        }
        if (anterior == NULL;){
            s -> primero = actual -> siguiente;
            delete actual;
        } else {
            anterior -> siguiente = actual -> siguiente;
            delete actual;
        }
    }
}

int sizeS(Set s) {
    return s -> cantidad;
}

LinkedList setToList(Set s) {
    int actual = s -> primero;
    LinkedList* ll = new LinkedListSt;


    while (actual != NULL) {
        cons((actual->elem), ll);
        actual = actual -> siguiente;
    }

    return ll;
}

//void destroyS(Set s) {
//
//}


