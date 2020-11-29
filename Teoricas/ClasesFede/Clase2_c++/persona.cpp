#include "persona.h"

struct PersonaSt {
    string nombre;
    int edad;
    Mascota* mascotas;
    int cantidadMascotas;
};

/**
  Invariante de representaci�n:
  * mascotas no es basura s�lo si cantidadDeMascotas > 0;
**/

// Recordatorio: (*loquesea) representa
// lo que hay en esa direccion de memoria

Persona consPersona(string nombre, int edad) {
    PersonaSt* p = new PersonaSt;
    // (*p).edad = edad;
    p->edad = edad;
    // (*p).nombre = nombre;
    p->nombre = nombre;
    p->cantidadMascotas = 0;
    return p;
}

void crecer(Persona p) {
    // (*p).edad++;
    p->edad++;
}

void cambiarNombre(Persona p, string nombre) {
    // (*p).nombre = nombre;
    p->nombre = nombre;
}

void agregarMascotas(Persona p, Mascota* mascotas, int cantidad) {
    p->mascotas = mascotas;
    p->cantidadMascotas = cantidad;
}

/// Precondici�n: la persona tiene mascotas;
void imprimirMascotas(Persona p) {
    for(int i = 0; i < p->cantidadMascotas; i++) {
        cout << nombreM(p->mascotas[i]);
        cout << ", ";
    }
}

void imprimirP(Persona p) {
    cout << "Persona " << p << " {" << endl;
    cout << "  nombre: " << p->nombre << endl;
    cout << "  edad: " << p->edad << endl;
    if (p->cantidadMascotas > 0) {
        cout << "  mascotas: ";
        imprimirMascotas(p);
        cout << endl;
    }
    cout << "}" << endl;
}
