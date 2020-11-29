#include "stack.h"

struct NodeS {
  ElemS elem;
  NodeS* next;
};

struct StackRep {
    NodeS* first;
};

Stack emptyS() {
    StackRep* sRep = new StackRep;
    sRep->first = NULL;
    return sRep;
}

bool isEmptyS(Stack s) {
    return s->first == NULL;
}

void push(ElemS x, Stack s) {
    NodeS* node = new NodeS;
    node->elem = x;
    node->next = s->first;
    s->first = node;
}

void pop(Stack s) {
    NodeS* tmp = s->first;
    s->first = s->first->next;
    delete tmp;
}

ElemS top(Stack s) {
    return s->first->elem;
}
