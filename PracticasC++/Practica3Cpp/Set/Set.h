#include <iostream>

using namespace std;

struct NodoS;

struct SetSt;

typedef SetSt* Set;

//Crea un conjunto vac�o.
Set emptyS();

//Indica si el conjunto est� vac�o.
bool isEmptyS(Set s);

//Indica si el elemento pertenece al conjunto.
void belongsS(int x, Set s);

//Agrega un elemento al conjunto.
void addS(int x, Set s);

//Quita un elemento dado.
void removeS(int x, Set s);

//Devuelve la cantidad de elementos.
int sizeS(Set s);

//Devuelve una lista con los lementos del conjunto.
LinkedList setToList(Set s);

//Libera la memoria ocupada por el conjunto.
void destroyS(Set s);
