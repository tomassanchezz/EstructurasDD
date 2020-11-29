#include <iostream>
#include "mascota.h"

using namespace std;

struct PersonaSt;

typedef PersonaSt* Persona;

// vamos a hacer algo similar
//type DNI = Int
//type Edad = Int
//data Persona = P DNI Edad String

// en Haskell sería
// type Person
Persona consPersona(string nombre, int edad);
void crecer(Persona p);
void cambiarNombre(Persona p, string nombre);
void agregarMascotas(Persona p, Mascota* mascotas, int cantidad);
void imprimirP(Persona p);

