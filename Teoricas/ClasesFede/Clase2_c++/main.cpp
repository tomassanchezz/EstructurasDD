#include <iostream>
#include "persona.h"

using namespace std;

// new :: int -> int*

//int succ(int x) {
//    return x + 1;
//}

//void succP(int* px) {
//    (*px) = (*px) + 1;
//}

void inicializar(int* xs, int tam) {
    for(int i = 0; i < tam; i++) {
        xs[i] = 0;
    }
}

void agregarGatosA(Persona p, string* nombres, int cantidad) {
    Mascota* mascotas = new Mascota[cantidad];
    for(int i = 0; i < cantidad; i++) {
        mascotas[i] = consMascota("gato", nombres[i]);
    }
    agregarMascotas(p, mascotas, cantidad);
}

// Clase pasada:
// Pasaje de parámetros por copia

// Clase actual:
// Pasaje de parámetros por referencia
int main()
{
//    int x = 5;
//    cout << "x: " << x << endl;
//    x = succ(x);
//    cout << "x: " << x << endl;

//    int* px = new int;
//    (*px) = 5;
//    cout << "px: " << (*px) << endl;
//    succP(px);
//    cout << "px: " << (*px) << endl;

//    Persona p = consPersona("jorge", 32);
//    imprimirP(p);
//
//    crecer(p);
//    imprimirP(p);
//
//    cambiarNombre(p, "julian");
//    imprimirP(p);
//
//    Persona m = consPersona("maria", 44);
//    cambiarNombre(m, "maria la del barrio");
//    imprimirP(m);

//    int* xs = new int[6];
//
//    inicializar(xs, 6);
//
//    cout << xs << endl;
//    cout << xs[0] << endl;
//    cout << xs[1] << endl;
//    cout << xs[2] << endl;
//    cout << xs[3] << endl;
//    cout << xs[4] << endl;
//    cout << xs[5] << endl;

    Persona pers1 = consPersona("Jose de San Martin", 242);
    Persona pers2 = consPersona("Tomy", 1);
    Persona pers3 = consPersona("Mi mama", 58);

    Mascota* mascotasDeJose = new Mascota[3];
    mascotasDeJose[0] = consMascota("Pura Sangre", "Blanquito");
    mascotasDeJose[1] = consMascota("Sangre Sucia", "Negrito");
    mascotasDeJose[2] = consMascota("Mula", "Griselda");

    agregarMascotas(pers1, mascotasDeJose, 3);

    imprimirP(pers1);

    Mascota* mascotasDeTomy = new Mascota[1];
    mascotasDeTomy[0] = consMascota("Sabueso", "firulais");

    agregarMascotas(pers2, mascotasDeTomy, 1);

    imprimirP(pers2);

    // array literal
    string nombres[] = {
        "Lucy",
        "Tomy",
        "La gata gris",
        "La gata blanca grande",
        "La gata peluda"
    };

    agregarGatosA(pers3, nombres, 5);

    imprimirP(pers3);

    return 0;
}

