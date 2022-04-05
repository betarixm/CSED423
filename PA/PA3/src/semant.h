#ifndef SEMANT_H_
#define SEMANT_H_

#include <assert.h>

#include <iostream>
#include <map>
#include <vector>
#include <set>
#include <algorithm>

#include "cool-tree.h"
#include "list.h"
#include "stringtab.h"
#include "symtab.h"
#include "list.h"

#define TRUE 1
#define FALSE 0

class ClassTable;

class method_class;

class attr_class;

typedef ClassTable *ClassTableP;

// This is a structure that may be used to contain the semantic
// information such as the inheritance graph.  You may use it or not as
// you like: it is only here to provide a container for the supplied
// methods.

class ClassTable {
private:
    int semant_errors;
    std::map<Symbol, Class_> _symbol_class_map;
    std::map<Symbol, std::map<Symbol, method_class *> *> _method_map;
    std::map<Symbol, std::map<Symbol, attr_class *> *> _attr_map;
    std::map<Symbol, Symbol> parent_map;
    cool::SymbolTable<Symbol, Symbol> _object_symtab;
    ostream &error_stream;

    void install_basic_classes();

    void add_class(Symbol name, Class_ c);

public:
    ClassTable(Classes);

    int errors() { return semant_errors; }

    bool is_child(Symbol child, Symbol parent, Class_ current_class);

    Symbol lca(Symbol c1, Symbol c2, Class_ current_class);

    ostream &semant_error();

    ostream &semant_error(Class_ c);

    ostream &semant_error(Symbol filename, tree_node *t);

    std::map<Symbol, Symbol> *get_parent_map();

    std::map<Symbol, Class_> *symbol_class_map();

    std::map<Symbol, std::map<Symbol, method_class *> *> * method_map();

    std::map<Symbol, std::map<Symbol, attr_class *> *> * attr_map();

    cool::SymbolTable<Symbol, Symbol> *object_symtab();

    method_class *get_method(Symbol class_name, Symbol method_name, Class_ current_class);
};

#endif
