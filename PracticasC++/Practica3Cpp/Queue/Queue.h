#include <iostream>

using namespace std;

struct NodoQ;

struct QueueSt;

typedef QueueSt* Queue;

Queue emptyQ();

bool isEmptyQ(Queue q);

int firstQ(Queue q);

void enqueue(int x, Queue q);

void dequeue(Queue q);

int lengthQ(Queue q);

void mergeQ(Queue q1, Queue q2);

void destroyQ(Queue q);

void printQ (Queue q);
