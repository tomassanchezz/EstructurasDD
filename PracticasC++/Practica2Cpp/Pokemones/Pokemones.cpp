#include <iostream>
#include "Pokemones.h"

using namespace std;

struct PokemonSt {
    TipoDePokemon tipo;
    int vida;
};

Pokemon consPokemon(TipoDePokemon tipo){
    PokemonSt* p = new PokemonSt;

    p-> tipo = tipo;
    p-> vida = 100;

    return p;
}

 void imprimirP(Pokemon p) {
    cout << "Pokemon " << p << " {" << endl;
    cout << "  Tipo: " << p->tipo << endl;
    cout << "  Edad: " << p->vida << endl;
    cout << "}" << endl;
}

TipoDePokemon tipoDePokemon(Pokemon p) {
    return p->tipo;
}

int energia(Pokemon p) {
    return p->vida;
}

void perderEnergia(int energia, Pokemon p) {
    p->vida -= energia;
}

bool esAgua (Pokemon p) {
    return tipoDePokemon (p) == "Agua";
}

bool esFuego (Pokemon p) {
    return tipoDePokemon (p) == "Fuego";
}

bool esPlanta (Pokemon p) {
    return tipoDePokemon (p) == "Planta";
}

bool superaA(Pokemon p1, Pokemon p2) {
    return (esAgua(p1) && esFuego(p2)) ||
               (esFuego(p1) && esPlanta(p2)) ||
               (esPlanta(p1) && esAgua(p2));
}

