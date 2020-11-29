#include <string>
#include "persona.h"

struct PersonaSt{
  std::string nombre;
};

Persona mkPersona(std::string nombre){
  Persona result = new PersonaSt;
  result->nombre = nombre;
  return result; 
}

std::string nombre(Persona persona){
  return persona->nombre;
}

void destroyPersona(Persona persona){
  delete persona;
}



