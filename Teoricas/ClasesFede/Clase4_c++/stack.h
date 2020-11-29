#include <iostream>
#include "tree.h"

using namespace std;

typedef Tree ElemS;

struct NodeS;

struct StackRep;

typedef StackRep* Stack;

Stack emptyS();

bool isEmptyS(Stack q);

void push(ElemS x, Stack q);

void pop(Stack q);

ElemS top(Stack q);
