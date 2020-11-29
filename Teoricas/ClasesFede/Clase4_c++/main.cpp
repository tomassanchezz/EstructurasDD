#include <iostream>
#include "queue.h"
#include "stack.h"
#include "tree.h"

using namespace std;

int sumTR(Tree t) {
    if(isEmptyT(t)) {
        return 0;
    } else {
        return rootT(t) + sumTR(leftT(t)) + sumTR(rightT(t));
    }
}

int sumBFS(Tree t) {
    if (isEmptyT(t)) {
        return 0;
    }
    int resultado = 0;
    Queue pendientes = emptyQ();
    Tree actual = t;
    enqueue(actual, pendientes);
    while(!isEmptyQ(pendientes)) {
        // obtengo el arbol actual
        actual = firstQ(pendientes);

        // proceso la raiz actual
        resultado = resultado + rootT(actual);

        // lo quito porque ya está procesado
        dequeue(pendientes);

        // pasar al siguiente elemento

        // encolo el left si no es emptyT
        if (!isEmptyT(leftT(actual))) {
            enqueue(leftT(actual), pendientes);
        }

        // encolo el right si no es emptyT
        if (!isEmptyT(rightT(actual))) {
            enqueue(rightT(actual), pendientes);
        }
    }
    return resultado;
}

int sumDFS(Tree t) {
    if (isEmptyT(t)) {
        return 0;
    }
    int resultado = 0;
    Stack pendientes = emptyS();
    Tree actual = t;
    push(actual, pendientes);
    while(!isEmptyS(pendientes)) {
        // obtengo el arbol actual
        actual = top(pendientes);

        // proceso la raiz actual
        resultado = resultado + rootT(actual);

        // lo quito porque ya está procesado
        pop(pendientes);

        // pasar al siguiente elemento

        // apilo el right si no es emptyT
        if (!isEmptyT(rightT(actual))) {
            push(rightT(actual), pendientes);
        }

        // apilo el left si no es emptyT
        if (!isEmptyT(leftT(actual))) {
            push(leftT(actual), pendientes);
        }
    }
    return resultado;
}

int main()
{
    Tree t1 = nodeT(
      1,
        nodeT(2, leafT(4), leafT(5)),
        nodeT(3, leafT(6), leafT(7))
    );

    int sr1 = sumTR(t1);
    int sbfs1 = sumBFS(t1);
    int sdfs1 = sumDFS(t1);
    cout << "sum sr1: " << sr1 << endl;
    cout << "sum sbfs1: " << sbfs1 << endl;
    cout << "sum sdfs1: " << sdfs1 << endl;
    return 0;
}
