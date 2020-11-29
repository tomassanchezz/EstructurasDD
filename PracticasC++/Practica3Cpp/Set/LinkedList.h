#include <iostream>

using namespace std;
//---------------------------------------------------------------------------------------------
struct NodoL;

struct LinkedListSt;

typedef LinkedListSt* LinkedList;
//---------------------------------------------------------------------------------------------

LinkedList nil();

bool isEmpty(LinkedList xs);

int head(LinkedList xs);

void cons(int x, LinkedList xs);

void tail(LinkedList xs);

int length(LinkedList xs);

void snoc(int x, LinkedList xs);

void initialize(LinkedList xs);

int current(LinkedList xs);

void setCurrent(int x, LinkedList xs);

void next(LinkedList xs);

bool finished(LinkedList xs);

void destroyL(LinkedList xs);

void printLinkedList(LinkedList xs);
