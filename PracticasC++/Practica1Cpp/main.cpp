#include <iostream>

using namespace std;

//Ej 4

//1
//Impletentacion iterativa.

void printN(int n, string s) {
    for (int x = 0; x < n; x++){
        cout << s << endl;
    }
}

//Implementacion recursiva.
//void printN (int n, string s) {
//    if (n == 0) {
//        cout << "" << endl;
//    } else {
//        cout << s << endl;
//        printN (n-1,  s);
//    }
//}

//2
//Impletentacion iterativa.

void cuentaRegresiva(int n) {
   for (int x = n; x >= 0; x--) {
       cout << x << endl;
    }
}

//Implementacion recursiva.
//void cuentaRegresiva (int n) {
//    if (n == 0) {
//        cout << 0 << endl;
//    } else {
//        cout << n << endl;
//        cuentaRegresiva (n-1);
//    }
//}


//3
//Impletentacion iterativa.
void desdeCeroHastaN(int n) {
    for (int i = 0; i <= n ; i++) {
        cout << i << endl;
    };
}

//Implementacion recursiva.
//void desdeCeroHastaN (int n) {
//    int i = 0;
//    if (i < n) {
//        cout << i << endl;
//    } else {
//        cout << n << endl;
//    }
//    desdeCeroHastaN(i = i + 1);
//}

//4
int mult(int n, int m) {
    for (int i = m; i <= m; i++) {
        return n + n;
    }
}

//5
void primerosN(int n, string s){
    for (int i = 0; i < n; i++) {
        cout << s.at(i) << endl;
    }
}

//6
int pertenece(char c, string s) {
    bool pert = false;

    for (int i = 0; i < s.length(); i++) {
        pert = pert || (s.at(i) == c);
    }

    return pert;
};

//7
int apariciones(char c, string s) {
    int cont = 0;

    for (int i = 0; i < s.length(); i++) {
        if (c == s.at(i)){
            cont++;
        }
    }

    return cont;
}

int main() {
    cout << apariciones ('h', "casa") << endl;

    return 0;
}
