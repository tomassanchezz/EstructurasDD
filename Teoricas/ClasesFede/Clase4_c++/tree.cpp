#include <iostream>
#include "tree.h"

using namespace std;

Tree emptyT() {
    return NULL;
}

bool isEmptyT(Tree t) {
    return t == NULL;
}

Elem rootT(Tree t) {
    return t->root;
}

Tree leftT(Tree t) {
    return t->left;
}

Tree rightT(Tree t) {
    return t->right;
}

// Por comodidad para la creación de árboles
Tree leafT(Elem x) {
    return nodeT(x, emptyT(), emptyT());
}

Tree nodeT(Elem x, Tree left, Tree right) {
    NodeT* newNode = new NodeT;
    newNode->root = x;
    newNode->left = left;
    newNode->right = right;
    return newNode;
}

// Prec.: la memoria de todos los nodos del arbol
// no fue previamente liberada
void destroyT(Tree t) {
    // TAREA: COMPLETAR
}
