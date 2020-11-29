#include "mascota.h"

struct MascotaSt {
    string raza;
    string nombre;
};

Mascota consMascota(string raza, string nombre) {
    MascotaSt* m = new MascotaSt;
    m->raza = raza;
    m->nombre = nombre;
    return m;
}

string nombreM(Mascota m) {
    return m->nombre;
}

void cambiarNombreM(string nombre, Mascota m) {
    m->nombre = nombre;
}

void imprimirM(Mascota m) {
    cout << "Mascota " << m << " {" << endl;
    cout << "  raza: " << m->raza << endl;
    cout << "  nombre: " << m->nombre << endl;
    cout << "}" << endl;
}
