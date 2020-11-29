#include <iostream>

using namespace std;

struct MascotaSt;

typedef MascotaSt* Mascota;

Mascota consMascota(string raza, string nombre);
string nombreM(Mascota m);
void cambiarNombreM(string nombre, Mascota m);
void imprimirM(Mascota m);
