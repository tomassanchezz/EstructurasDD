#include <iostream>
#include "LinkedList.h"

using namespace std;

LinkedList repetir (int n, int x) {
    LinkedList xs = nil ();

    while (n > 0) {
        cons (x, xs);
        n--;
    }

    return xs;
}

//Devuelve la suma de todos los elementos.
///ESQUEMA DE RECORRIDOS.
int sumatoria (LinkedList xs) {
    int res = 0;
    initialize(xs);
    while (!finished(xs)) {
        res = res + current(xs);
        next(xs);
    }
    return res;
}

//Incrementa en uno todos los elementos.
void sucesores(LinkedList xs) {
    initialize(xs);
    while (!finished(xs)) {
        setCurrent((current(xs) + 1), xs);
        next(xs);
    }
}

//Indica si el elemento pertenece a la lista.
bool pertenece(int x, LinkedList xs) {
    initialize(xs);
    while (!finished(xs) && current(xs) != x) {
        next(xs);
    }
    return current(xs) == x;
}

//Indica la cantidad de elementos iguales a x.
int apariciones(int x, LinkedList xs) {
    int contador = 0;
    initialize(xs);
    while (!finished(xs)) {
        if (current(xs) == x) {
            contador++;
            next(xs);
        } else {
            next(xs);
        }
    }
    return contador;
}

//Devuelve el elemento más chico de la lista.
int minimo(LinkedList xs) {
    initialize(xs);
    int minimo = current (xs);

    while (!finished(xs)) {
        if (current(xs) < minimo) {
            minimo = current (xs);
            next(xs);
        } else {
            next(xs);
        }
    }

    return minimo;
}

//LinkedList copy(LinkedList xs)
//
//void append(LinkedList xs, LinkedList ys)

int main() {
    LinkedList xs = repetir (10, 75);
//    printLinkedList(xs);
//    cout << pertenece(75, xs) << endl;
//    cout << (NULL == 8) << endl;
//    cout << "" << endl;
//    cout << sumatoria(xs) << endl;
//    sucesores(xs);
    printLinkedList(xs);
    cout << "" << endl;
    cout << apariciones(75, xs) << endl;
    snoc (74, xs);
    cout << minimo(xs) << endl;
    destroyL(xs);
    return 0;
}

