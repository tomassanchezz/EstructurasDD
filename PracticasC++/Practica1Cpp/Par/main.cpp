#include <iostream>

using namespace std;

//Ej 3
struct Par {
    int x;
    int y;
};

// Prop�sito: construye un par
Par consPar(int x, int y) {
    Par p;
    p.x = x;
    p.y = y;
    return p;
}

// Prop�sito: devuelve la primera componente
int fst(Par p) {
    return p.x;
}

// Prop�sito: devuelve la segunda componente
int snd(Par p) {
    return p.y;
}

// Prop�sito: devuelve la mayor componente
int maxDelPar(Par p) {
    return max (p.x, p.y);
}

void mostrarPar (Par p) {
    cout << "Par: (" << p.x  <<  ", "<<  p.y  << ")" << endl;
}

// Prop�sito: devuelve un par con las componentes intercambiadas
Par swapp(Par p) {
    return consPar(p.y, p.x);
}

// Prop�sito: devuelve un par donde la primer componente
// es la divisi�n y la segunda el resto entre ambos n�meros.
Par divisionYResto(int n, int m) {
    Par newP = consPar (n, m);

    newP.x = newP.x / newP.y;
    newP.y = newP.x % newP.y;

    return newP;
}

int main(){
    Par p = consPar(8, 9);
    Par p1 = divisionYResto(4, 2);
    cout << fst (p) << endl;
    cout << snd (p) << endl;
    cout << maxDelPar(p) << endl;

    mostrarPar(p);
    mostrarPar(swapp(p));
    mostrarPar(p1);

    return 0;
}
