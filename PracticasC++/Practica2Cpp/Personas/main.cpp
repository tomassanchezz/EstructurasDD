#include <iostream>
#include "persona.h"

using namespace std;

int main()
{
    Persona p1 = consPersona ("Tomas", 21);
    Persona p2 = consPersona ("Matias", 34);
    Persona p3 = consPersona ("Claudia", 55);

    cout << edad(p1) << endl;
    crecer(p1);
    cout << edad(p1) << endl;
    cout << "" << endl;
    cout << nombre(p2) << endl;
    cambioDeNombre("Claudio", p2);
    cout << nombre(p2) << endl;
    cout << "" << endl;
    cout << esMayorQueLaOtra (p1, p2) << endl;
    cout << esMayorQueLaOtra (p2, p1) << endl;
    cout << "" << endl;
    imprimirP (laQueEsMayor(p1, p2));
    return 0;
}
