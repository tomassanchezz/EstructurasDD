#include <iostream>

// Enumerativos
enum COLOR { ROJO, AZUL, VERDE, NEGRO };

using namespace std;

/// Propósito: imprime las letras de
/// la 'a' a la 'z'
/// (separados por saltos de línea)
void letrasMinusculasConWhile() {
    char c = 'a'; // inicialización
    while(c <= 'z') { // condicion para seguir
        cout << c << endl;
        c++; // paso al que sigue
    }
}

/// Propósito: imprime las letras de
/// la 'a' a la 'z'
/// (separados por saltos de línea)
void letrasMinuculasConFor() {
    for(char c = 'a'; c <= 'z'; c++) {
        cout << c << endl;
    }
}

int sucesor(int x) {
    return x + 1;
}

/// Costo operacional: lineal, O(n)
/// Costo en espacio: constante, O(1)
int factorialIterativo(int x) {
    int r = 1;
    while(x > 0) {
        r = r * x;
        x--;
    }
    return r;
}

/// factorial 0 = 1
/// factorial n = n * factorial (n-1)

/// Costo operacional: lineal, O(n)
/// Costo en espacio: lineal, O(n)
int factorialRec(int x) {
    if (x == 0) {
        return 1;
    }

    return x * factorialRec(x - 1);
}

void mostrarColor(COLOR c) {

}

void mostrarLetras(string s) {
//    for(int i = 0; i < s.size(); i++) {
//        cout << s[i] << endl;
//    }
    // foreach
    for(char c : s) {
        cout << c << endl;
    }
}

struct Persona {
    string nombre;
    int edad;
};

/// Muestra una persona
void mostrarPersona(Persona p) {
    cout << "Persona {" << endl;
    cout << "  nombre: " << p.nombre << endl;
    cout << "  edad: " << p.edad << endl;
    cout << "}" << endl;
}

/// Incrementa la edad de una persona
Persona incrementarEdad(Persona p) {
    p.edad++;
    return p;
}

/// Constructor para obligar a poner todos
/// los parámetros
Persona consPersona(string nombre, int edad) {
    Persona p;
    p.nombre = nombre;
    p.edad = edad;
    return p;
}

int main()
{
    Persona p = consPersona("Carlos", 32);
    mostrarPersona(p);
    p = incrementarEdad(p);
    mostrarPersona(p);
    return 0;
}
