#include <iostream>

using namespace std;

struct Fraccion {
    float numerador;
    float denominador;
};

// Suponer que el denominador no es cero
// Prop�sito: construye una fraccion

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

// Prop�sito: devuelve el numerador

float numerador(Fraccion f) {
    return f.numerador;
}

// Prop�sito: devuelve el denominador

float denominador(Fraccion f) {
    return f.denominador;
}

// Prop�sito: devuelve el resultado de hacer la divisi�n

float division(Fraccion f){
    return numerador(f) / denominador(f);
}

// Prop�sito: devuelve una fracci�n que resulta de multiplicar las fracciones
// (sin simplificar)

Fraccion multF(Fraccion f1, Fraccion f2) {
    Fraccion newF;
    newF.numerador = numerador(f1) * numerador (f2);
    newF.denominador = denominador(f1) * denominador (f2);
    return newF;
}

// Prop�sito: devuelve una fracci�n que resulta
// de simplificar la dada por par�metro

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

// Prop�sito: devuelve la primera componente

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
