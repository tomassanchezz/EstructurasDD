//Parcial C++ 
#include <iostream>
using namespace std;

//------------------------------------------------------
struct PersonaSt {
    string nombre;
    int evidencia;
};

struct InvestigacionSt {
    ArrayList sospechosos; // lista de Persona
};

typedef PersonaSt* Persona;
typedef InvestigacionSt* Investigacion;
//------------------------------------------------------

// Invariantes de Representacion:
//  *Si un sospechoso no tiene evidencia en su contra
//   evidencia es igual a 0.
//  *Los sospechosos del ArrayList corresponden a personas,
//   tengan o no aun evidencia en su contra.
//  *No existen dos personas con el mismo nombre.

// Propósito: crea una investigación sin personas.
// Eficiencia: O(1)
Investigacion comenzarInvestigacion() {
    Investigacion nuevaInv = new InvestigacionSt;

    nuevaInv -> sospechosos = emptyAL();

    return nuevaInv;
}

Persona consPersona(string n){
    Persona nuevaP = new PersonaSt;

    nuevaP -> nombre = n;
    nuevaP -> evidencia = 0;

    return nuevaP;
}

// Propósito: devuelve los nombres de personas ingresadas.
// Nota: suponer que el ArrayList es de nombres.
// Eficiencia: O(N)
ArrayList nombresIngresados(Investigacion investigacion) {
    ArrayList res = emptyAL();
    ArrayList sosp = investigacion -> sospechosos;

    for (int i = 0; isValidIndex(i, sosp); i++){
        string elem = nombrePersona(get(i, sosp));
        add(elem, res);             
    }

    return res;
}

// Proposito: retorna el nombre de una persona.
// Eficiencia: O(1)
string nombrePersona (Persona persona) {
    return persona -> nombre;
}

ArrayList sospechosos (Investigacion investigacion) {
    return investigacion -> sospechosos;
}

// Propósito: devuelve la sumatoria de evidencia de las personas.
// Eficiencia: O(N)
int cantEvidenciaDePersonas(Investigacion investigacion) {
    int res = 0;
    ArrayList sosp = investigacion -> sospechosos;

    for (int i = 0; isValidIndex(i, sosp); i++){
        int cantE = cantEvidencia(get(i, sosp));
        res = res + cantE;              
    }

    return res;
}

// Proposito: retorna la cantidad de evidencia de una persona.
// Eficiencia: O(1)
int cantEvidencia (Persona persona){
    return persona -> evidencia;
}

// Propósito: indica si la investigación posee al menos una persona con al 
// menos 5 evidencias en su contra.
// Eficiencia: O(N)
bool casoCerrado(Investigacion investigacion){
    ArrayList sosp = investigacion -> sospechosos;
    int contador = 0;
    int mayorEvidencia = 0;

    while (isValidIndex(contador, sosp) && mayorEvidencia <= 5) {
        int cantE = cantEvidencia(get(i, sosp));
        if(cantE > mayorEvidencia) {                    
            mayorEvidencia = cantE;
        }
    }

    return mayorEvidencia >= 5; 
}

// Propósito: indica si esa persona tiene al menos una evidencia en su contra.
// Nota: la persona puede no existir.
// Eficiencia: O(N)
bool esSospechosa(string nombre, Investigacion investigacion) {
    ArrayList sosp = investigacion -> sospechosos;
    bool res = false;

    for (int i = 0; isValidIndex(i, sosp); i++){
        Persona personaActual = get(i, sosp);

        if (nombrePersona(personaActual) == nombre) {
            res = cantEvidencia(personaActual) > 0;
        }
    }

    return res;
}


// Propósito: devuelve a las personas con cero evidencia en su contra.
// Nota: suponer que el ArrayList es de nombres.
// Eficiencia: O(N)
ArrayList posiblesInocentes(Investigacion investigacion) {
    ArrayList sosp = investigacion -> sospechosos;
    ArrayList res  = emptyAl();

    for (int i = 0; isValidIndex(i, sosp); i++) {
        if (cantEvidencias(get(i, sosp)) == 0) {
            add(nombrePersona(i), res);
        }
    }

    return res;
}

// Propósito: ingresa a personas nuevas a la investigación (mediante sus nombres), 
// sin evidencia en su contra.
// Nota: suponer que el ArrayList es de nombres.
// Precondición: las personas no existen en la investigación y no hay nombres repetidos.
// Eficiencia: O(N)
void ingresarPersonas(ArrayList nombres, Investigacion investigacion) {
    for (int i = 0; isValidIndex(i, nombres); i++) {
        string nombre = get(i, nombres);
        add(consPersona(nombre), sospechosos(investigacion));
    }
}

// Propósito: incrementa en uno la evidencia a una lista de personas.
// Precondición: la evidencia aún no está asociada a esa persona.
// Nota 1: la persona y la evidencia existen, pero NO están asociadas.
// Nota 2: suponer que el ArrayList es de nombres.
// Eficiencia: O(N)
void ingresarUnaEvidencia(ArrayList nombres, Investigacion investigacion) {
    for (int i = 0; isValidIndex(i, nombres); i++) {
        string nombreActual = get(i, nombres);
        Persona nuevaPersona = new PersonaSt;
        nuevaPersona -> nombre = nombreActual;
        nuevaPersona -> evidencia = cantEvidencia(get(posicionPersona(nombreActual, sospechosos(investigacion)))) + 1

        set(posicionPersona(nombreActual, sospechosos(investigacion)), nuevaPersona, sospechosos(investigacion))
    }
}


// Proposito: Indica la posicion a traves de un nombre.
// Precondicion: La persona existe.
// Eficiencia: O(N)
int posicionPersona (string nombre, ArrayList sospechosos) {
    int contador = 0;

    while(nombrePersona(get(contador, sospechoso)) != nombre) {
        contador++;
    }

    return contador;
}


//------------------------------------------------------
// Usuario.

// Propósito: Indica la cantidad de inocentes.
int cantidadDeInocentes(Investigacion investigacion) {
    int res = 0;
    ArrayList inocentes = posiblesInocentes (investigacion);

    for(int i = 0; isValidIndex(i, inocentes); i++) {
        res++;
    }

    return res;
}

// Propósito: Indica si la evidencia dada por parámetro 
// es suficiente para cerrar el caso.
bool seCierraSiSeAñadenEstasEvidencias(ArrayList nombres, int evidencias, Investigacion investigacion) {

    for (int i = 0; i <= evidencias; i++){
        ingresarUnaEvidencia(nombres, investigacion);
    }

    return casoCerrado(investigacion);

}

// Propósito: Indica si las personas en la investigación son todas inocentes.
bool todasInocentes(Investigacion investigacion) {
    return cantEvidenciaDePersonas(investigacion) == 0;
}


