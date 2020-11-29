#include <iostream>
#include "Entrenadores.h"

using namespace std;

int main()
{
    ///Testeos Pokemones.
    Pokemon p1 = consPokemon ("Fuego");
    Pokemon p2 = consPokemon ("Agua");
    Pokemon p3 = consPokemon ("Planta");
//
//    imprimirP(p1);
//    imprimirP(p2);
//
//    cout << "" << endl;
//
//    cout <<  tipoDePokemon(p1) << endl;
//    cout <<  tipoDePokemon(p2) << endl;
//
//    cout << "" << endl;
//
//    cout << energia(p3) << endl;
//    perderEnergia(50 ,p3);
//    cout << energia(p3) << endl;
//
//    cout << "" << endl;
//
//    cout << esAgua(p1) << endl;
//    cout << esAgua(p2) << endl;
//
//    cout << "" << endl;
//
//    cout << superaA(p1, p2) << endl;
//    cout << superaA(p2, p1) << endl;
    ///Testeos Entrenadores.

    Pokemon* pokemonesE1= new Pokemon[2];
    pokemonesE1[0] = p1;
    pokemonesE1[1] = p2;

    Entrenador e1 = consEntrenador("Ash", 2, pokemonesE1);

    cout << nombreDeEntrenador(e1) << endl;
    cout << cantidadDePokemon(e1) << endl;
    cout << cantidadDePokemonDe("Planta", e1) << endl;
    imprimirP (pokemonNro(2, e1));


    return 0;
}
