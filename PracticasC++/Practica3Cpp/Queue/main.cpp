#include <iostream>
#include "Queue.h"

using namespace std;

int main()
{
    Queue q1 = emptyQ();
    Queue q2 = emptyQ();


    enqueue(1, q1);
    enqueue(2, q1);
    enqueue(3, q1);
    enqueue(4, q1);
    enqueue(5, q1);

    enqueue(6, q2);
    enqueue(7, q2);
    enqueue(8, q2);
    enqueue(9, q2);
    enqueue(10, q2);

    printQ (q1);
    cout << "" << endl;
    cout << firstQ (q1) << endl;
    dequeue(q1);
    printQ (q1);
    cout << "" << endl;
    cout << lengthQ (q1) << endl;
    mergeQ (q1, q2);
    printQ (q1);
//    destroyQ (q1);
//    printQ (q1);

    return 0;
}
