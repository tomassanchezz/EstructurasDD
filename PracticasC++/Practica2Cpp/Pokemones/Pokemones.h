#include <iostream>

using namespace std;

//---------------------------------------------------------------------------------------------
typedef string TipoDePokemon;

struct PokemonSt;

typedef PokemonSt* Pokemon;
//---------------------------------------------------------------------------------------------

//Dado un tipo devuelve un pokémon con 100 % de energía.
Pokemon consPokemon(TipoDePokemon tipo);

//Imprime por consola el pokemon pasado por parametro.
void imprimirP(Pokemon p);

bool esAgua (Pokemon p);
bool esFuego (Pokemon p);
bool esPlanta (Pokemon p);

//Devuelve el tipo de un pokemon.
TipoDePokemon tipoDePokemon(Pokemon p);

//Devuelve el porcentaje de energía.
int energia(Pokemon p);

//Le resta energía al pokémon.
void perderEnergia(int energia, Pokemon p);

//Dados dos pokémon indica si el primero, en base al tipo, es superior al segundo. Agua supera
//a fuego, fuego a planta y planta a agua. Y cualquier otro caso es falso.
bool superaA(Pokemon p1, Pokemon p2);
