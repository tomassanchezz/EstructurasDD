//Integrador C++

struct MSNode {
    int elem;
    int cantidad;
    MSNode* next;
};

struct MSSt {
    MSNode* first;
};

typedef MSSt* MultiSet;

// Propósito: indica la cantidad de veces que aparece el elemento dado.
int ocurrencias(int elem, MultiSet ms) {
    MSNode elementoActual = ms -> first;

    while (elementoActual -> next != NULL && elementoActual -> elem != elem) {
        elementoActual = elementoActual -> next;
    };

    if (elementoActual -> elem == elem) {
        return elementoActual -> cantidad;
    } else {
        return 0;
    };
}

// Propósito: aumenta en una la cantidad de apariciones del elemento dado.
void agregarOcurrencia(int elem, MultiSet ms) {
    MSNode elementoActual = ms -> first;

    while (elementoActual -> next != NULL && elementoActual -> elem != elem) {
        elementoActual = elementoActual -> next;
    };

    if (elementoActual -> elem == elem) {
        elementoActual -> cantidad = elementoActual -> cantidad + 1;
    };
}


