#include <iostream>
#include "Pokemones.h"

using namespace std;

//---------------------------------------------------------------------------------------------
struct EntrenadorSt;

typedef EntrenadorSt* Entrenador;
//---------------------------------------------------------------------------------------------

//Dado un nombre, una cantidad de pokemones, y un array de pokemones de ese tamaño,
//devuelve un entrenador.
Entrenador consEntrenador(string nombre, int cantidad, Pokemon* pokemones);

//Devuelve el nombre del entrenador.
string nombreDeEntrenador(Entrenador e);

//Devuelve la cantidad de pokémon que posee el entrenador.
int cantidadDePokemon(Entrenador e);

//Devuelve la cantidad de pokémon de determinado tipo que posee el entrenador.
int cantidadDePokemonDe(TipoDePokemon tipo, Entrenador e);

//Devuelve el pokémon número i de los pokemones del entrenador.
//Precondición: existen al menos i − 1 pokemones.
Pokemon pokemonNro(int i, Entrenador e);

//Dados dos entrenadores, indica si, para cada pokemon del segundo entrenador, el primero
//posee al menos un pókemon que le gane.
//bool leGanaATodos(Entrenador e1, Entrenador e2);
