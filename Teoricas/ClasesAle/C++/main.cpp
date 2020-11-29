#include <iostream>
#include <string>
#include "persona.h"

int main()
{
    Persona german = mkPersona("german");
    std::string n = nombre(german);
    destroyPersona(german);
    std::cout << "hola " << n << std::endl;
    return 0;
}