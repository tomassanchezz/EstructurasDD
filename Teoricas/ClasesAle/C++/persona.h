#include <string>

struct PersonaSt;

typedef PersonaSt* Persona;

Persona mkPersona(std::string nombre);

std::string nombre(Persona persona);

void destroyPersona(Persona persona);