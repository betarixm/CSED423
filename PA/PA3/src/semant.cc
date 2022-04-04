

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include "semant.h"
#include "utilities.h"
#include <set>
#include <queue>
#include <map>
#include "symtab.h"

#define RED 31
#define GREEN 32
#define YELLOW 33
#define BLUE 34
#define MAGENTA 35
#define CYAN 36
#define WHITE 37
#define RESET 0

extern int semant_debug;
extern char *curr_filename;

#define VERBOSE

void print(std::string str);

std::string coloring(std::string str, int color);

std::map<Symbol, Class_> ClassTable::m;
int ClassTable::semant_errors_;

extern int semant_debug;
extern char *curr_filename;

//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
Symbol
        arg,
        arg2,
        Bool,
        concat,
        cool_abort,
        copy,
        Int,
        in_int,
        in_string,
        IO,
        length,
        Main,
        main_meth,
        No_class,
        No_type,
        Object,
        out_int,
        out_string,
        prim_slot,
        self,
        SELF_TYPE,
        Str,
        str_field,
        substr,
        type_name,
        val;

void print(std::string str) {
#ifdef VERBOSE
    std::cout << coloring("[*] OUT: ", YELLOW) << str << std::endl;
#endif
}

std::string coloring(std::string str, int color) {
    return "\x1b[" + std::to_string(color) + "m" + str + "\x1b[" + std::to_string(RESET) + "m";
}

//
// Initializing the predefined symbols.
//
static void initialize_constants(void) {
    arg = idtable.add_string("arg");
    arg2 = idtable.add_string("arg2");
    Bool = idtable.add_string("Bool");
    concat = idtable.add_string("concat");
    cool_abort = idtable.add_string("abort");
    copy = idtable.add_string("copy");
    Int = idtable.add_string("Int");
    in_int = idtable.add_string("in_int");
    in_string = idtable.add_string("in_string");
    IO = idtable.add_string("IO");
    length = idtable.add_string("length");
    Main = idtable.add_string("Main");
    main_meth = idtable.add_string("main");
    //   _no_class is a symbol that can't be the name of any
    //   user-defined class.
    No_class = idtable.add_string("_no_class");
    No_type = idtable.add_string("_no_type");
    Object = idtable.add_string("Object");
    out_int = idtable.add_string("out_int");
    out_string = idtable.add_string("out_string");
    prim_slot = idtable.add_string("_prim_slot");
    self = idtable.add_string("self");
    SELF_TYPE = idtable.add_string("SELF_TYPE");
    Str = idtable.add_string("String");
    str_field = idtable.add_string("_str_field");
    substr = idtable.add_string("substr");
    type_name = idtable.add_string("type_name");
    val = idtable.add_string("_val");
}

// Define basic classes as global variable
static Class_
        Object_class,
        IO_class,
        Int_class,
        Bool_class,
        Str_class;

ClassTable::ClassTable(Classes classes) : semant_errors(0), error_stream(cerr) {
    install_basic_classes();

    // Construct a mpa: Symbol -> Class_
    m[SELF_TYPE] = NULL;
    m[Object] = Object_class;
    m[Int] = Int_class;
    m[IO] = IO_class;
    m[Str] = Str_class;
    m[Bool] = Bool_class;
    for (int i = classes->first(); classes->more(i); i = classes->next(i)) {
        Class_ c = classes->nth(i);
        m[c->get_name()] = c;
    }

    for (int i = classes->first(); classes->more(i); i = classes->next(i)) {
        Class_ c = classes->nth(i);

        // TODO: Detect undefined parent class
        Symbol parent = c->get_parent();
        while (parent != No_class) {
            if (c->get_name() == parent) {
                ostream &err_stream = semant_error(c);
                err_stream << "Class " << c << " inherits from itself.\n";
                break;
            } else if (m.count(parent) == 0) {
                ostream &err_stream = semant_error(c);
                err_stream << "Class " << c << " inherits from undefined class " << parent << ".\n";
                break;
            } else {
                parent = m[parent]->get_parent();
            }
        }

        // TODO: Cannot inherit from SELF_TYPE
        if (c->get_parent() == SELF_TYPE) {
            ostream &err_stream = semant_error();
            err_stream << "Class " << c->get_name() << " cannot inherit class " << c->get_parent() << ".\n";
        }
    }

    bool is_main_exists = false;

    for (int i = classes->first(); classes->more(i); i = classes->next(i)) {
        Class_ c = classes->nth(i);

        // TODO: check if Main class exists
        if (c->get_name() == Main) {
            is_main_exists = true;
        }

        // TODO: detect class redefinition
        if (m[c->get_name()] != c) {
            ostream& err_stream = semant_error(c);
            err_stream << "Class " << c->get_name() << " has already been defined.\n";
        }
    }

    if (!is_main_exists) {
        ostream &err_stream = semant_error();
        err_stream << "Class Main is not defined.\n";
    }

    // Save scope for each class
    for (int i = classes->first(); classes->more(i); i = classes->next(i)) {
        Class_ c = classes->nth(i);
        Features fl = c->get_features();

        c->get_attr_symtab()->addid(self, SELF_TYPE);

        for (int j = fl->first(); fl->more(j); j = fl->next(j)) {
            Feature f = fl->nth(j);

            // TODO
            cool::SymbolTable<Symbol, Entry> *symtab = f->is_attr() ? c->get_attr_symtab() : c->get_method_symtab();
            symtab->addid(f->get_name(), f->get_type());
        }
    }

    // Check scope for each class (Interpreter Pattern applied)
    for (int i = classes->first(); classes->more(i); i = classes->next(i)) {
        Class_ c = classes->nth(i);
        Features fl = c->get_features();

        for (int j = fl->first(); fl->more(j); j = fl->next(j)) {
            Feature f = fl->nth(j);

            cool::SymbolTable<Symbol, Entry> *object_symtab = new cool::SymbolTable<Symbol, Entry>();
            object_symtab->enterscope();

            semant_errors += f->check_scope(c, object_symtab);
        }
    }

    // Check type for each class (Interpreter Pattern applied)
    for (int i = classes->first(); classes->more(i); i = classes->next(i)) {
        Class_ c = classes->nth(i);
        Features fl = c->get_features();

        for (int j = fl->first(); fl->more(j); j = fl->next(j)) {
            Feature f = fl->nth(j);

            cool::SymbolTable<Symbol, Entry> *object_symtab = new cool::SymbolTable<Symbol, Entry>();
            object_symtab->enterscope();

            semant_errors += f->check_type(c, object_symtab);
        }
    }
}

void ClassTable::install_basic_classes() {

    // The tree package uses these globals to annotate the classes built below.
    curr_lineno = 0;
    Symbol filename = stringtable.add_string("<basic class>");

    // The following demonstrates how to create dummy parse trees to
    // refer to basic Cool classes.  There's no need for method
    // bodies -- these are already built into the runtime system.

    // IMPORTANT: The results of the following expressions are
    // stored in local variables.  You will want to do something
    // with those variables at the end of this method to make this
    // code meaningful.

    //
    // The Object class has no parent class. Its methods are
    //        abort() : Object    aborts the program
    //        type_name() : Str   returns a string representation of class name
    //        copy() : SELF_TYPE  returns a copy of the object
    //
    // There is no need for method bodies in the basic classes---these
    // are already built in to the runtime system.

    Object_class =
            class_(Object,
                   No_class,
                   append_Features(
                           append_Features(
                                   single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
                                   single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
                           single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
                   filename);
    Object_class->get_method_symtab()->addid(cool_abort, Object);
    Object_class->get_method_symtab()->addid(type_name, Str);
    Object_class->get_method_symtab()->addid(copy, SELF_TYPE);
    Object_class->get_attr_symtab()->addid(self, SELF_TYPE);

    //
    // The IO class inherits from Object. Its methods are
    //        out_string(Str) : SELF_TYPE       writes a string to the output
    //        out_int(Int) : SELF_TYPE            "    an int    "  "     "
    //        in_string() : Str                 reads a string from the input
    //        in_int() : Int                      "   an int     "  "     "
    //
    IO_class =
            class_(IO,
                   Object,
                   append_Features(
                           append_Features(
                                   append_Features(
                                           single_Features(method(out_string, single_Formals(formal(arg, Str)),
                                                                  SELF_TYPE, no_expr())),
                                           single_Features(method(out_int, single_Formals(formal(arg, Int)),
                                                                  SELF_TYPE, no_expr()))),
                                   single_Features(method(in_string, nil_Formals(), Str, no_expr()))),
                           single_Features(method(in_int, nil_Formals(), Int, no_expr()))),
                   filename);
    IO_class->get_method_symtab()->addid(out_string, SELF_TYPE);
    IO_class->get_method_symtab()->addid(out_int, SELF_TYPE);
    IO_class->get_method_symtab()->addid(in_string, Str);
    IO_class->get_method_symtab()->addid(in_int, Int);
    IO_class->get_attr_symtab()->addid(self, SELF_TYPE);

    //
    // The Int class has no methods and only a single attribute, the
    // "val" for the integer.
    //
    Int_class =
            class_(Int,
                   Object,
                   single_Features(attr(val, prim_slot, no_expr())),
                   filename);
    Int_class->get_attr_symtab()->addid(val, prim_slot);
    Int_class->get_attr_symtab()->addid(self, SELF_TYPE);

    //
    // Bool also has only the "val" slot.
    //
    Bool_class =
            class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())), filename);
    Bool_class->get_attr_symtab()->addid(val, prim_slot);
    Bool_class->get_attr_symtab()->addid(self, SELF_TYPE);

    //
    // The class Str has a number of slots and operations:
    //       val                                  the length of the string
    //       str_field                            the string itself
    //       length() : Int                       returns length of the string
    //       concat(arg: Str) : Str               performs string concatenation
    //       substr(arg: Int, arg2: Int): Str     substring selection
    //
    Str_class =
            class_(Str,
                   Object,
                   append_Features(
                           append_Features(
                                   append_Features(
                                           append_Features(
                                                   single_Features(attr(val, Int, no_expr())),
                                                   single_Features(attr(str_field, prim_slot, no_expr()))),
                                           single_Features(method(length, nil_Formals(), Int, no_expr()))),
                                   single_Features(method(concat,
                                                          single_Formals(formal(arg, Str)),
                                                          Str,
                                                          no_expr()))),
                           single_Features(method(substr,
                                                  append_Formals(single_Formals(formal(arg, Int)),
                                                                 single_Formals(formal(arg2, Int))),
                                                  Str,
                                                  no_expr()))),
                   filename);
    Str_class->get_attr_symtab()->addid(val, Int);
    Str_class->get_attr_symtab()->addid(str_field, prim_slot);
    Str_class->get_method_symtab()->addid(length, Int);
    Str_class->get_method_symtab()->addid(concat, Str);
    Str_class->get_method_symtab()->addid(substr, Str);
    Str_class->get_attr_symtab()->addid(self, SELF_TYPE);
}

////////////////////////////////////////////////////////////////////
//
// semant_error is an overloaded function for reporting errors
// during semantic analysis.  There are three versions:
//
//    ostream& ClassTable::semant_error()
//
//    ostream& ClassTable::semant_error(Class_ c)
//       print line number and filename for `c'
//
//    ostream& ClassTable::semant_error(Symbol filename, tree_node *t)
//       print a line number and filename
//
///////////////////////////////////////////////////////////////////

ostream &ClassTable::semant_error(Class_ c) {
    return semant_error(c->get_filename(), c);
}

ostream &ClassTable::semant_error(Symbol filename, tree_node *t) {
    error_stream << filename << ":" << t->get_line_number() << ": ";
    return semant_error();
}

ostream &ClassTable::semant_error() {
    semant_errors++;
    return error_stream;
}

void ClassTable::semant_error(Class_ c, tree_node *t, char *err_msg) {
    cerr << c->get_filename() << ":" << t->get_line_number() << ": ";
    cerr << err_msg << endl;
    semant_errors_++;
}

bool ClassTable::partial_ordered(Symbol derived, Symbol base) {
    // TODO
    return false;
}

Symbol ClassTable::lub(Symbol s1, Symbol s2, Symbol c) {
    // TODO
    return Object;
}

/*   This is the entry point to the semantic checker.

     Your checker should do the following two things:

     1) Check that the program is semantically correct
     2) Decorate the abstract syntax tree with type information
        by setting the `type' field in each Expression node.
        (see `tree.h')

     You are free to first do 1), make sure you catch all semantic
     errors. Part 2) can be done in a second stage, when you want
     to build mycoolc.
 */
void program_class::semant() {
    initialize_constants();

    /* ClassTable constructor may do some semantic analysis */
    ClassTable *classtable = new ClassTable(classes);

    /* some semantic analysis code may go here */

    if (classtable->errors()) {
        cerr << "Compilation halted due to static semantic errors." << endl;
        exit(1);
    }
}


