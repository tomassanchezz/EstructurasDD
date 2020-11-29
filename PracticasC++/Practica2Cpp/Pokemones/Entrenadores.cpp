#include <iostream>
#include "Pokemones.h"
#include "Entrenadores.h"
using namespace std;

struct EntrenadorSt {
    string nombre;
    Pokemon* pokemon;
    int cantPokemon;
};

Entrenador consEntrenador(string nombre, int cantidad, Pokemon* pokemones){
    EntrenadorSt* e = new EntrenadorSt;

    e->nombre = nombre;
    e->pokemon = pokemones;
    e-> cantPokemon = cantidad;
}

string nombreDeEntrenador(Entrenador e) {
    return e->nombre;
}

int cantidadDePokemon(Entrenador e) {
    return e->cantPokemon;
}

int cantidadDePokemonDe(TipoDePokemon tipo, Entrenador e) {
    int deTipo = 0;

    for (int i = 0; i < cantidadDePokemon(e) ; i++) {
        if (tipoDePokemon(e->pokemon[i]) == tipo) {
            deTipo++;
        }
    }

    return deTipo;
}

Pokemon pokemonNro(int i, Entrenador e) {
    return e->pokemon[i-1];
}


//bool leGanaATodos(Entrenador e1, Entrenador e2) {
//
//}

