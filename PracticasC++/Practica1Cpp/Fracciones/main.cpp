#include <iostream>

using namespace std;

struct Fraccion {
    float numerador;
    float denominador;
};

// Suponer que el denominador no es cero
// Propósito: construye una fraccion

Fraccion consFraccion(float numerador, float denominador) {
    Fraccion f;
    f.numerador = numerador;
    f.denominador = denominador;
    return f;
}

void mostrarFraccion (Fraccion f) {
    cout << f.numerador << endl;
    cout << "--" << endl;
    cout << f.denominador << endl;
}

// Propósito: devuelve el numerador

float numerador(Fraccion f) {
    return f.numerador;
}

// Propósito: devuelve el denominador

float denominador(Fraccion f) {
    return f.denominador;
}

// Propósito: devuelve el resultado de hacer la división

float division(Fraccion f){
    return numerador(f) / denominador(f);
}

// Propósito: devuelve una fracción que resulta de multiplicar las fracciones
// (sin simplificar)

Fraccion multF(Fraccion f1, Fraccion f2) {
    Fraccion newF;
    newF.numerador = numerador(f1) * numerador (f2);
    newF.denominador = denominador(f1) * denominador (f2);
    return newF;
}

// Propósito: devuelve una fracción que resulta
// de simplificar la dada por parámetro

Fraccion simplificada(Fraccion p) {
    if (denominador(p) == 1) {
        return p;
    } else {
        float b=2;
            while(b<=numerador(p)){
                if(denominador(p)%b==0.0 && numerador(p)%b==0.0){
                    p.denominador=p.denominador/b;
                    p.numerador=p.numerador/b;
                }else{
                    b++;
                }
    }
}

// Propósito: devuelve la primera componente

Fraccion sumF(Fraccion f1, Fraccion f2) {

}

int main()
{
    Fraccion f1 = consFraccion(1, 2);
    Fraccion f2 = consFraccion (8, 4);
    mostrarFraccion(f2);
    mostrarFraccion(simplificada (f2));
    return 0;
}
