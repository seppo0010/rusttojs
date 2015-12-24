// Copyright 2015 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>

extern int yylex();
extern int rsparse();

#define PUSHBACK_LEN 4
#define debug(...)

static char pushback[PUSHBACK_LEN];

void print(const char* format, ...) {
  va_list args;
  va_start(args, format);
  vprintf(format, args);
  va_end(args);
}

// If there is a non-null char at the head of the pushback queue,
// dequeue it and shift the rest of the queue forwards. Otherwise,
// return the token from calling yylex.
int rslex() {
  if (pushback[0] == '\0') {
    return yylex();
  } else {
    char c = pushback[0];
    memmove(pushback, pushback + 1, PUSHBACK_LEN - 1);
    pushback[PUSHBACK_LEN - 1] = '\0';
    return c;
  }
}

// Note: this does nothing if the pushback queue is full. As long as
// there aren't more than PUSHBACK_LEN consecutive calls to push_back
// in an action, this shouldn't be a problem.
void push_back(char c) {
  for (int i = 0; i < PUSHBACK_LEN; ++i) {
    if (pushback[i] == '\0') {
      pushback[i] = c;
      break;
    }
  }
}

extern int rsdebug;
int rsdebug = 0;

struct node {
  struct node *next;
  struct node *prev;
  int own_string;
  char const *name;
  int n_elems;
  struct node *elems[];
};

struct node *nodes = NULL;
int n_nodes;

struct node *mk_node(char const *name, int n, ...) {
  va_list ap;
  int i = 0;
  unsigned sz = sizeof(struct node) + (n * sizeof(struct node *));
  struct node *nn, *nd = (struct node *)malloc(sz);

  debug("# New %d-ary node: %s = %p\n", n, name, nd);

  nd->own_string = 0;
  nd->prev = NULL;
  nd->next = nodes;
  if (nodes) {
    nodes->prev = nd;
  }
  nodes = nd;

  nd->name = name;
  nd->n_elems = n;

  va_start(ap, n);
  while (i < n) {
    nn = va_arg(ap, struct node *);
    debug("#   arg[%d]: %p\n", i, nn);
    debug("#            (%s ...)\n", nn->name);
    nd->elems[i++] = nn;
  }
  va_end(ap);
  n_nodes++;
  return nd;
}

struct node *mk_atom(char *name) {
  struct node *nd = mk_node((char const *)strdup(name), 0);
  nd->own_string = 1;
  return nd;
}
struct node *mk_none() {
  return NULL;
}

struct node *ext_node(struct node *nd, int n, ...) {
  va_list ap;
  int i = 0, c = nd->n_elems + n;
  unsigned sz = sizeof(struct node) + (c * sizeof(struct node *));
  struct node *nn;

  debug("# Extending %d-ary node by %d nodes: %s = %p",
        nd->n_elems, c, nd->name, nd);

  if (nd->next) {
    nd->next->prev = nd->prev;
  }
  if (nd->prev) {
    nd->prev->next = nd->next;
  }
  nd = realloc(nd, sz);
  nd->prev = NULL;
  nd->next = nodes;
  nodes->prev = nd;
  nodes = nd;

  debug(" ==> %p\n", nd);

  va_start(ap, n);
  while (i < n) {
    nn = va_arg(ap, struct node *);
    debug("#   arg[%d]: %p\n", i, nn);
    debug("#            (%s ...)\n", nn->name);
    nd->elems[nd->n_elems++] = nn;
    ++i;
  }
  va_end(ap);
  return nd;
}

int const indent_step = 4;

static void print_json_str(const char *str) {
  print("\"");
  for (;*str!='\0';++str) {
      switch (*str) {
        case '\"': print("\\\""); break;
        case '\\': print("\\\\"); break;
        case '/':  print("\\/"); break;
        case '\b': print("\\b"); break;
        case '\f': print("\\f"); break;
        case '\n': print("\\n"); break;
        case '\r': print("\\r"); break;
        case '\t': print("\\t"); break;
        default:
          if (*str <= 0x1f) {
              print("\\u00");
              if (*str & 0x10) {
                print("1");
              } else {
                print("0");
              }
              switch (*str & 0xF) {
                case 0x0: print("0"); break;
                case 0x1: print("1"); break;
                case 0x2: print("2"); break;
                case 0x3: print("3"); break;
                case 0x4: print("4"); break;
                case 0x5: print("5"); break;
                case 0x6: print("6"); break;
                case 0x7: print("7"); break;
                case 0x8: print("8"); break;
                case 0x9: print("9"); break;
                case 0xA: print("A"); break;
                case 0xB: print("B"); break;
                case 0xC: print("C"); break;
                case 0xD: print("D"); break;
                case 0xE: print("E"); break;
                case 0xF: print("F"); break;
              }
          }
          print("%c", *str);
          break;
      }
  }
  print("\"");
}

void print_node(struct node *n, int depth) {
  int i = 0;
  if (n == NULL) {
    print("null");
  } else if (n->n_elems == 0) {
    print_json_str(n->name);
  } else {
    print("{\"name\":");
    print_json_str(n->name);
    print(",\"nodes\":[");
    for (i = 0; i < n->n_elems; ++i) {
      print_node(n->elems[i], depth + indent_step);
      if (i != n->n_elems - 1) {
        print(",");
      }
    }
    print("]}");
  }
}

int main(int argc, char **argv) {
  int ret = 0;
  struct node *tmp;
  memset(pushback, '\0', PUSHBACK_LEN);
  ret = rsparse();
  if (nodes) {
    print_node(nodes, 0);
  }
  while (nodes) {
    tmp = nodes;
    nodes = tmp->next;
    if (tmp->own_string) {
      free((void*)tmp->name);
    }
    free(tmp);
  }
  return ret;
}

void rserror(char const *s) {
  fprintf(stderr, "%s\n", s);
}

int yywrap() { return 1; }
