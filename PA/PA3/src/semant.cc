

#include "semant.h"

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>

#include "utilities.h"

#define RED 31
#define GREEN 32
#define YELLOW 33
#define BLUE 34
#define MAGENTA 35
#define CYAN 36
#define WHITE 37
#define RESET 0

ClassTable *classtable;

extern int semant_debug;
extern char *curr_filename;

// #define VERBOSE

void print(std::string str);

std::string coloring(std::string str, int color);

//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
static Symbol arg, arg2, Bool, concat, cool_abort, copy, Int, in_int, in_string,
        IO, length, Main, main_meth, No_class, No_type, Object, out_int, out_string,
        prim_slot, self, SELF_TYPE, Str, str_field, substr, type_name, val;

void print(std::string str) {
#ifdef VERBOSE
    std::cout << coloring("[*] OUT: ", YELLOW) << str << std::endl;
#endif
}

std::string coloring(std::string str, int color) {
    return "\x1b[" + std::to_string(color) + "m" + str + "\x1b[" +
           std::to_string(RESET) + "m";
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

bool is_primitive(Symbol class_name) {
    return class_name == Object || class_name == IO || class_name == Bool || class_name == Str || class_name == Int;
}

ClassTable::ClassTable(Classes classes) : semant_errors(0), error_stream(cerr) {
    /* Fill this in */
    print("(ClassTable) Start");

    bool is_main_exists = false;

    install_basic_classes();

    for (int i = classes->first(); classes->more(i); i = classes->next(i)) {
        Class_ c = classes->nth(i);
        Symbol c_name = c->get_name();

        if (this->symbol_class_map()->count(c_name) > 0) {
            this->semant_error(c) << "Class " << c->get_name() << " was previously defined." << std::endl;
        } else if (is_primitive(c_name)) {
            this->semant_error(c) << "Redefinition of basic class " << c->get_name() << "." << std::endl;
        } else {
            this->add_class(c_name, c);
        }
    }

    for (auto i: *this->symbol_class_map()) {
        Class_ c = i.second;
        Symbol c_name = i.first;

        this->parent_map[c_name] = c->get_parent();

        if (c_name == Main) {
            is_main_exists = true;
        }

        print("(ClassTable) parent_map[" + std::string(c_name->get_string()) + "] = " +
              std::string(c->get_parent()->get_string()));
    }

    if (!is_main_exists) {
        semant_error() << "Class Main is not defined." << std::endl;
    }

    for (auto i: this->parent_map) {
        Symbol child_name = i.first;
        Symbol parent_name = i.second;

        while (parent_name != No_class) {
            if (parent_name == SELF_TYPE || parent_name == Int || parent_name == Bool || parent_name == Str) {
                this->semant_error((*this->symbol_class_map())[child_name]) << "Class " << child_name
                                                                            << " cannot inherit class " << parent_name
                                                                            << ".\n";
                break;
            } else if (this->symbol_class_map()->count(parent_name) == 0) {
                this->semant_error((*this->symbol_class_map())[child_name]) << "Class " << child_name
                                                                            << " cannot inherit from an undefined class "
                                                                            << parent_name
                                                                            << ".\n";
                break;
            } else {
                parent_name = this->parent_map[parent_name];
            }
            print("(ClassTable) Check inheritance for child " + std::string(child_name->get_string()));
        }
    }
    print("(ClassTable) Done");
}

std::map<Symbol, Symbol> *ClassTable::get_parent_map() {
    return &this->parent_map;
}

std::map<Symbol, Class_> *ClassTable::symbol_class_map() {
    return &this->_symbol_class_map;
}

cool::SymbolTable<Symbol, Symbol> *ClassTable::object_symtab() {
    return &this->_object_symtab;
}

void ClassTable::add_class(Symbol name, Class_ c) {
    this->_symbol_class_map[name] = c;
    this->_method_map[name] = new std::map<Symbol, method_class *>();
    this->_attr_map[name] = new std::map<Symbol, attr_class *>();

    Features features = c->get_features();

    for (int j = features->first(); features->more(j); j = features->next(j)) {
        Feature f = features->nth(j);
        if (f->is_attr()) {
            auto *attr = dynamic_cast<attr_class *> (f);
            (*this->_attr_map[name])[attr->get_name()] = attr;
        } else {
            auto *method = dynamic_cast<method_class *> (f);
            (*this->_method_map[name])[method->get_name()] = method;
        }
    }

    print("(add_class) " + std::string(name->get_string()));
}

bool ClassTable::is_child(Symbol child, Symbol parent, Class_ current_class) {
    if (child == No_type || (parent == SELF_TYPE && parent == child)) {
        return true;
    }

    Symbol cur_parent = child == SELF_TYPE ? current_class->get_name() : child;

    while (cur_parent != NULL) {
        if (cur_parent == parent) {
            return true;
        } else {
            cur_parent = this->parent_map[cur_parent];
        }
    }

    return false;
}

Symbol ClassTable::lca(Symbol c1, Symbol c2, Class_ current_class) {
    c1 = c1 == SELF_TYPE ? current_class->get_name() : c1;
    c2 = c2 == SELF_TYPE ? current_class->get_name() : c2;

    Symbol cur_c1 = c1, cur_c2 = c2;
    std::vector<Symbol> parents;

    while (cur_c1 != Object) {
        parents.push_back(cur_c1);
        cur_c1 = this->parent_map[cur_c1];
    }

    while (cur_c2 != Object) {
        if (std::find(parents.begin(), parents.end(), cur_c2) != parents.end()) {
            return cur_c2;
        } else {
            cur_c2 = this->parent_map[cur_c2];
        }
    }

    return Object;
}

std::map<Symbol, std::map<Symbol, method_class *> *> *ClassTable::method_map() {
    return &this->_method_map;
}


std::map<Symbol, std::map<Symbol, attr_class *> *> *ClassTable::attr_map() {
    return &this->_attr_map;
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

    Class_ Object_class = class_(
            Object, No_class,
            append_Features(
                    append_Features(single_Features(method(cool_abort, nil_Formals(),
                                                           Object, no_expr())),
                                    single_Features(method(type_name, nil_Formals(),
                                                           Str, no_expr()))),
                    single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
            filename);

    //
    // The IO class inherits from Object. Its methods are
    //        out_string(Str) : SELF_TYPE       writes a string to the output
    //        out_int(Int) : SELF_TYPE            "    an int    "  "     "
    //        in_string() : Str                 reads a string from the input
    //        in_int() : Int                      "   an int     "  "     "
    //
    Class_ IO_class = class_(
            IO, Object,
            append_Features(
                    append_Features(
                            append_Features(
                                    single_Features(method(out_string,
                                                           single_Formals(formal(arg, Str)),
                                                           SELF_TYPE, no_expr())),
                                    single_Features(method(out_int,
                                                           single_Formals(formal(arg, Int)),
                                                           SELF_TYPE, no_expr()))),
                            single_Features(
                                    method(in_string, nil_Formals(), Str, no_expr()))),
                    single_Features(method(in_int, nil_Formals(), Int, no_expr()))),
            filename);

    //
    // The Int class has no methods and only a single attribute, the
    // "val" for the integer.
    //
    Class_ Int_class =
            class_(Int, Object, single_Features(attr(val, prim_slot, no_expr())),
                   filename);

    //
    // Bool also has only the "val" slot.
    //
    Class_ Bool_class =
            class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),
                   filename);

    //
    // The class Str has a number of slots and operations:
    //       val                                  the length of the string
    //       str_field                            the string itself
    //       length() : Int                       returns length of the string
    //       concat(arg: Str) : Str               performs string concatenation
    //       substr(arg: Int, arg2: Int): Str     substring selection
    //
    Class_ Str_class = class_(
            Str, Object,
            append_Features(
                    append_Features(
                            append_Features(
                                    append_Features(
                                            single_Features(attr(val, Int, no_expr())),
                                            single_Features(attr(str_field, prim_slot, no_expr()))),
                                    single_Features(
                                            method(length, nil_Formals(), Int, no_expr()))),
                            single_Features(method(concat, single_Formals(formal(arg, Str)),
                                                   Str, no_expr()))),
                    single_Features(
                            method(substr,
                                   append_Formals(single_Formals(formal(arg, Int)),
                                                  single_Formals(formal(arg2, Int))),
                                   Str, no_expr()))),
            filename);

    this->add_class(Object, Object_class);
    this->add_class(IO, IO_class);
    this->add_class(Int, Int_class);
    this->add_class(Bool, Bool_class);
    this->add_class(Str, Str_class);
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
    classtable = new ClassTable(classes);

    /* some semantic analysis code may go here */
    print("(semant) Start traversal for type checking");

    for (auto i: *classtable->symbol_class_map()) {
        Class_ c = i.second;
        Symbol c_name = i.first;
        Features features = c->get_features();

        print("(semant) " + std::string(c->get_name()->get_string()));

        classtable->object_symtab()->enterscope();

        Symbol cursor = c_name;

        int depth = 0;

        bool is_inherit_attr_error = false;

        while (cursor != NULL) {
            if (classtable->attr_map()->count(cursor) == 0) {
                break;
            }

            classtable->object_symtab()->enterscope();
            ++depth;
            for (const auto a: *(*classtable->attr_map())[cursor]) {
                classtable->object_symtab()->addid(a.second->get_name(), new Symbol(a.second->get_type()));
            }
            cursor = (*classtable->get_parent_map())[cursor];
        }

        classtable->object_symtab()->enterscope();

        classtable->object_symtab()->addid(self, new Symbol(c_name));

        for (const auto &l: *(*classtable->attr_map())[c_name]) {
            Symbol cur_parent = (*classtable->get_parent_map())[c->get_name()];

            if (is_primitive(cur_parent)) {
                continue;
            }

            while (cur_parent != No_class) {
                if ((*classtable->attr_map())[cur_parent]->count(l.first) > 0) {
                    is_inherit_attr_error = true;
                    classtable->semant_error(c)
                            << "Attribute "
                            << l.first
                            << " is an attribute of an inherited class." << std::endl;
                    break;
                }

                cur_parent = (*classtable->get_parent_map())[cur_parent];
            }

            if (is_inherit_attr_error) { break; }
        }

        for (const auto &l: *(*classtable->method_map())[c_name]) {
            method_class *_method = l.second;
            Symbol _method_name = l.first;
            Symbol _parent = (*classtable->get_parent_map())[c_name];

            method_class *_parent_method;

            while (_parent != No_class) {

                if (_parent == No_class || (*classtable->method_map())[_parent]->count(_method_name) == 0) { break; }
                _parent_method = (*(*classtable->method_map())[_parent])[_method_name];

                Formals _parent_method_argv = _parent_method->get_formals();
                Formals _tester_method_argv = _method->get_formals();

                int _parent_method_argc = 0, _tester_method_argc = 0;

                int error_count = 0;

                for (int _ = _parent_method_argv->first();
                     _parent_method_argv->more(_); _ = _parent_method_argv->next(_), _parent_method_argc++);

                for (int _ = _tester_method_argv->first();
                     _tester_method_argv->more(_); _ = _tester_method_argv->next(_), _tester_method_argc++);

                if (_parent_method_argc != _tester_method_argc) {
                    classtable->semant_error(c) << "Incompatible number of formal parameters in redefined method "
                                                << _method_name << "." << std::endl;
                }

                for (int _tester_idx = _tester_method_argv->first(), _parent_idx = _parent_method_argv->first();
                     _tester_method_argv->more(_tester_idx) && _parent_method_argv->more(_tester_idx);
                     _tester_idx = _tester_method_argv->next(_tester_idx),
                             _parent_idx = _parent_method_argv->next(_parent_idx)) {

                    Symbol _tester_arg_type = _tester_method_argv->nth(_tester_idx)->get_type();
                    Symbol _parent_arg_type = _parent_method_argv->nth(_parent_idx)->get_type();

                    if (_tester_arg_type != _parent_arg_type) {
                        classtable->semant_error(c)
                                << "In redefined method " << _method_name
                                << ", parameter type " << _tester_arg_type << " is different from original type "
                                << _parent_arg_type
                                << std::endl;

                    }
                }

                if (classtable->get_parent_map()->count(_parent) == 0) { break; }
                _parent = (*classtable->get_parent_map())[_parent];
            }
        }

        if (!is_inherit_attr_error) {
            for (const auto &l: *(*classtable->attr_map())[c_name]) {
                print("(semant) " + std::string(c->get_name()->get_string()) + " " +
                      std::string(l.first->get_string()));
                l.second->check_type(classtable->object_symtab(), c, classtable);
            }

            for (const auto &l: *(*classtable->method_map())[c_name]) {
                print("(semant) " + std::string(c->get_name()->get_string()) + " " +
                      std::string(l.first->get_string()));
                l.second->check_type(classtable->object_symtab(), c, classtable);
            }
        }

        classtable->object_symtab()->exitscope();

        for (int d = 0; d < depth; d++) {
            classtable->object_symtab()->exitscope();
        }

        classtable->object_symtab()->exitscope();

        if (is_inherit_attr_error) { break; }
    }

    print("(semant) Done traversal for type checking");

    if (classtable->errors()) {
        cerr << "Compilation halted due to static semantic errors." << endl;
        exit(1);
    }
}

Symbol object_class::check_type(cool::SymbolTable<Symbol, Symbol> *o, Class_ current_class, ClassTable *ct) {
    if (this->name == self) {
        this->set_type(SELF_TYPE);
    } else if (o->lookup(this->name) != nullptr) {
        this->set_type(*o->lookup(this->name));
    } else {
        this->set_type(Object);
        ct->semant_error(current_class->get_filename(), this) << "Undeclared identifier "
                                                              << name << "." << std::endl;
    }

    return this->get_type();
}

Symbol no_expr_class::check_type(cool::SymbolTable<Symbol, Symbol> *o, Class_ current_class, ClassTable *ct) {
    this->set_type(No_type);
    return this->get_type();
}

Symbol isvoid_class::check_type(cool::SymbolTable<Symbol, Symbol> *o, Class_ current_class, ClassTable *ct) {
    this->e1->check_type(o, current_class, ct);

    this->set_type(Bool);
    return this->get_type();
}

Symbol new__class::check_type(cool::SymbolTable<Symbol, Symbol> *o, Class_ current_class, ClassTable *ct) {
    if (this->type_name == SELF_TYPE || ct->symbol_class_map()->count(this->type_name) > 0) {
        this->set_type(this->type_name);
    } else {
        this->set_type(Object);
        ct->semant_error(current_class->get_filename(), this) << "'new' used with undefined class "
                                                              << type_name << "."
                                                              << std::endl;
    }
    return this->get_type();
}

Symbol comp_class::check_type(cool::SymbolTable<Symbol, Symbol> *o, Class_ current_class, ClassTable *ct) {
    Symbol e1_type = this->e1->check_type(o, current_class, ct);

    if (e1_type == Bool) {
        this->set_type(e1_type);
    } else {
        this->set_type(Object);
        ct->semant_error(current_class->get_filename(), this) << "Argument of 'not' has type " << e1_type
                                                              << " instead of Bool." << std::endl;
    }

    return this->get_type();
}

Symbol leq_class::check_type(cool::SymbolTable<Symbol, Symbol> *o, Class_ current_class, ClassTable *ct) {
    Symbol e1_type = this->e1->check_type(o, current_class, ct);
    Symbol e2_type = this->e2->check_type(o, current_class, ct);

    if (e1_type == Int && e2_type == Int) {
        this->set_type(Bool);
    } else {
        this->set_type(Object);
        ct->semant_error(current_class->get_filename(), this)
                << "non-Int arguments: "
                << e1_type << " <= "
                << e2_type << std::endl;
    }
    return this->get_type();
}

Symbol eq_class::check_type(cool::SymbolTable<Symbol, Symbol> *o, Class_ current_class, ClassTable *ct) {
    Symbol e1_type = this->e1->check_type(o, current_class, ct);
    Symbol e2_type = this->e2->check_type(o, current_class, ct);

    this->set_type(Bool);

    if ((e1_type == Int || e2_type == Int || e1_type == Str || e2_type == Str || e1_type == Bool || e2_type == Bool) &&
        e1_type != e2_type) {
        ct->semant_error(current_class->get_filename(), this) << "Illegal comparison with a basic type." << std::endl;
    }

    return this->get_type();
}

Symbol lt_class::check_type(cool::SymbolTable<Symbol, Symbol> *o, Class_ current_class, ClassTable *ct) {
    Symbol e1_type = this->e1->check_type(o, current_class, ct);
    Symbol e2_type = this->e2->check_type(o, current_class, ct);

    if (e1_type == Int && e2_type == Int) {
        this->set_type(Bool);
    } else {
        this->set_type(Object);
        ct->semant_error(current_class->get_filename(), this)
                << "non-Int arguments: "
                << e1_type << " < "
                << e2_type << std::endl;
    }
    return this->get_type();
}

Symbol neg_class::check_type(cool::SymbolTable<Symbol, Symbol> *o, Class_ current_class, ClassTable *ct) {
    Symbol e1_type = this->e1->check_type(o, current_class, ct);

    if (e1_type == Int) {
        this->set_type(Int);
    } else {
        this->set_type(Object);
        ct->semant_error(current_class->get_filename(), this) << "Argument of '~' has type "
                                                              << e1_type << " instead of Int." << std::endl;
    }

    return this->get_type();
}

Symbol divide_class::check_type(cool::SymbolTable<Symbol, Symbol> *o, Class_ current_class, ClassTable *ct) {
    Symbol e1_type = this->e1->check_type(o, current_class, ct);
    Symbol e2_type = this->e2->check_type(o, current_class, ct);

    if (e1_type == Int && e2_type == Int) {
        this->set_type(Int);
    } else {
        this->set_type(Object);

        ct->semant_error(current_class->get_filename(), this)
                << "non-Int arguments: "
                << e1_type << " / "
                << e2_type << std::endl;
    }

    return this->get_type();
}

Symbol mul_class::check_type(cool::SymbolTable<Symbol, Symbol> *o, Class_ current_class, ClassTable *ct) {
    Symbol e1_type = this->e1->check_type(o, current_class, ct);
    Symbol e2_type = this->e2->check_type(o, current_class, ct);

    if (e1_type == Int && e2_type == Int) {
        this->set_type(Int);
    } else {
        this->set_type(Object);

        ct->semant_error(current_class->get_filename(), this)
                << "non-Int arguments: "
                << e1_type << " * "
                << e2_type << std::endl;
    }

    return this->get_type();
}

Symbol sub_class::check_type(cool::SymbolTable<Symbol, Symbol> *o, Class_ current_class, ClassTable *ct) {
    Symbol e1_type = this->e1->check_type(o, current_class, ct);
    Symbol e2_type = this->e2->check_type(o, current_class, ct);

    if (e1_type == Int && e2_type == Int) {
        this->set_type(Int);
    } else {
        this->set_type(Object);

        ct->semant_error(current_class->get_filename(), this)
                << "non-Int arguments: "
                << e1_type << " - "
                << e2_type << std::endl;
    }

    return this->get_type();
}

Symbol plus_class::check_type(cool::SymbolTable<Symbol, Symbol> *o, Class_ current_class, ClassTable *ct) {
    Symbol e1_type = this->e1->check_type(o, current_class, ct);
    Symbol e2_type = this->e2->check_type(o, current_class, ct);

    if (e1_type == Int && e2_type == Int) {
        this->set_type(Int);
    } else {
        this->set_type(Object);

        ct->semant_error(current_class->get_filename(), this)
                << "non-Int arguments: "
                << e1_type << " + "
                << e2_type << std::endl;
    }

    return this->get_type();
}

Symbol let_class::check_type(cool::SymbolTable<Symbol, Symbol> *o, Class_ current_class, ClassTable *ct) {
    Symbol init_type = this->init->check_type(o, current_class, ct);


    if (this->type_decl != SELF_TYPE && ct->symbol_class_map()->count(this->type_decl) == 0) {
        ct->semant_error(current_class->get_filename(), this) << "Class " << this->type_decl
                                                              << " of let-bound identifier "
                                                              << this->identifier
                                                              << " is undefined." << std::endl;
    }

    if (init_type != No_type && !ct->is_child(init_type, this->type_decl, current_class)) {
        ct->semant_error(current_class->get_filename(), this) << "Inferred type " << init_type
                                                              << " of initialization of " << this->identifier
                                                              << " does not conform to identifier's declared type "
                                                              << this->type_decl << "."
                                                              << std::endl;
    }

    if (identifier == self) {
        ct->semant_error(current_class->get_filename(), this) << "'self' cannot be bound in a 'let' expression."
                                                              << std::endl;
    } else {
        o->enterscope();
        {
            o->addid(this->identifier, new Symbol(this->type_decl));
            this->set_type(this->body->check_type(o, current_class, ct));
        }
        o->exitscope();
    }

    return this->get_type();
}

Symbol block_class::check_type(cool::SymbolTable<Symbol, Symbol> *o, Class_ current_class, ClassTable *ct) {
    this->set_type(Object);
    for (int i = this->body->first(); body->more(i); i = body->next(i)) {
        this->set_type(body->nth(i)->check_type(o, current_class, ct));
    }
    return this->get_type();
}

Symbol branch_class::check_type(cool::SymbolTable<Symbol, Symbol> *o, Class_ current_class, ClassTable *ct) {
    if (this->name != self) {
        o->enterscope();
        {
            o->addid(this->name, new Symbol(this->type_decl));
            this->set_type(this->expr->check_type(o, current_class, ct));
        }
        o->exitscope();
    } else {
        this->set_type(Object);
        ct->semant_error(current_class->get_filename(), this) << "'self' cannot be bound in a 'branch' expression.";
    }

    return this->get_type();
}

Symbol typcase_class::check_type(cool::SymbolTable<Symbol, Symbol> *o, Class_ current_class, ClassTable *ct) {
    Symbol branch_type;
    std::vector<Symbol> branch_types;
    std::set<Symbol> branch_decl_types;

    this->expr->check_type(o, current_class, ct);

    for (int i = this->cases->first(); this->cases->more(i); i = cases->next(i)) {
        auto branch = dynamic_cast<branch_class *>(cases->nth(i));

        if (i != this->cases->first() && branch_decl_types.find(branch->get_type_decl()) != branch_decl_types.end()) {
            ct->semant_error(current_class) << "Duplicate branch " << branch->get_type_decl()
                                            << " in case statement." << std::endl;
        } else {
            branch_decl_types.insert(branch->get_type_decl());

            o->enterscope();

            if (branch->get_name() == self) {
                ct->semant_error(current_class) << "'self' bound in 'case'." << std::endl;
            }

            if (branch->get_type_decl() == SELF_TYPE) {
                ct->semant_error(current_class) << "Identifier " << branch->get_name()
                                                << " declared with type SELF_TYPE in case branch." << std::endl;
            }

            o->addid(branch->get_name(), new Symbol(branch->get_type_decl()));
            branch_types.push_back(branch->check_type(o, current_class, ct));

            o->exitscope();
        }
    }
    if (!branch_types.empty()) {
        branch_type = branch_types[0];
        for (const auto &b: branch_types) {
            branch_type = ct->lca(branch_type, b, current_class);
        }
        this->set_type(branch_type);
    }

    return this->get_type();
}

Symbol loop_class::check_type(cool::SymbolTable<Symbol, Symbol> *o, Class_ current_class, ClassTable *ct) {
    Symbol pred_type = this->pred->check_type(o, current_class, ct);

    this->body->check_type(o, current_class, ct);
    this->set_type(Object);

    if (pred_type == Bool) {

    } else {
        ct->semant_error(current_class->get_filename(), this) << "Loop condition does not have type Bool." << std::endl;
    }

    return this->get_type();
}

Symbol cond_class::check_type(cool::SymbolTable<Symbol, Symbol> *o, Class_ current_class, ClassTable *ct) {
    Symbol pred_type = this->pred->check_type(o, current_class, ct);
    Symbol then_exp_type = this->then_exp->check_type(o, current_class, ct);
    Symbol else_exp_type = this->else_exp->check_type(o, current_class, ct);

    if (pred_type == Bool) {

    } else {
        ct->semant_error(current_class->get_filename(), this) << "Predicate of 'if' does not have type Bool."
                                                              << std::endl;
    }

    this->set_type(ct->lca(then_exp_type, else_exp_type, current_class));

    return this->get_type();
}

Symbol int_const_class::check_type(cool::SymbolTable<Symbol, Symbol> *o, Class_ current_class, ClassTable *ct) {
    this->set_type(Int);
    return this->get_type();
}

Symbol bool_const_class::check_type(cool::SymbolTable<Symbol, Symbol> *o, Class_ current_class, ClassTable *ct) {
    this->set_type(Bool);
    return this->get_type();
}

Symbol string_const_class::check_type(cool::SymbolTable<Symbol, Symbol> *o, Class_ current_class, ClassTable *ct) {
    this->set_type(Str);
    return this->get_type();
}

method_class *ClassTable::get_method(Symbol class_name, Symbol method_name, Class_ current_class) {
    Symbol cursor = class_name == SELF_TYPE ? current_class->get_name() : class_name;

    while (cursor != No_class) {
        if ((*this->method_map())[cursor]->count(method_name) > 0) {
            return (*(*this->method_map())[cursor])[method_name];
        }
        cursor = this->parent_map.count(cursor) > 0 ? this->parent_map[cursor] : No_type;
    }

    return nullptr;
}

Symbol dispatch_class::check_type(cool::SymbolTable<Symbol, Symbol> *o, Class_ current_class, ClassTable *ct) {
    Symbol expr_checked_type = this->expr->check_type(o, current_class, ct);
    Symbol expr_type = expr_checked_type == SELF_TYPE ? current_class->get_name() : expr_checked_type;

    method_class *method = ct->get_method(expr_type, this->name, current_class);

    if (expr_checked_type != SELF_TYPE && ct->symbol_class_map()->count(expr_checked_type) == 0) {
        this->set_type(Object);
        ct->semant_error(current_class->get_filename(), this) << "Dispatch on undefined class " << expr_checked_type
                                                              << "."
                                                              << std::endl;
    } else if (method == nullptr) {
        this->set_type(Object);
        ct->semant_error(current_class->get_filename(), this) << "Dispatch to undefined method " << this->name << "."
                                                              << std::endl;
    } else {
        Formals decl_method_argv = method->get_formals();
        Expressions call_method_argv = this->actual;

        int decl_method_argc = 0;

        int error_count = 0;

        for (int i = decl_method_argv->first();
             decl_method_argv->more(i); i = decl_method_argv->next(i), ++decl_method_argc);

        if (call_method_argv->len() != decl_method_argc) {
            this->set_type(Object);
            ct->semant_error(current_class->get_filename(), this)
                    << "Method " << method->get_name() << " called with wrong number of arguments." << std::endl;
        } else {
            for (int decl_idx = decl_method_argv->first(), call_idx = call_method_argv->first();
                 decl_method_argv->more(decl_idx) && call_method_argv->more(call_idx);
                 decl_idx = decl_method_argv->next(decl_idx), call_idx = call_method_argv->next(call_idx)) {
                Formal decl_arg = decl_method_argv->nth(decl_idx);
                Expression call_arg = call_method_argv->nth(call_idx);

                Symbol call_arg_type = call_arg->check_type(o, current_class, ct);

                if (!ct->is_child(call_arg_type, decl_arg->get_type(), current_class)) {
                    error_count += 1;
                    ct->semant_error(current_class->get_filename(), this) << "In call of the method "
                                                                          << method->get_name()
                                                                          << ", type " << call_arg_type
                                                                          << " of parameter "
                                                                          << decl_arg->get_name()
                                                                          << " does not conform to declared type "
                                                                          << decl_arg->get_type() << "." << std::endl;
                }
            }
        }

        this->set_type(method->get_return_type() == SELF_TYPE ? expr_checked_type : method->get_return_type());
    }

    return this->get_type();
}

Symbol static_dispatch_class::check_type(cool::SymbolTable<Symbol, Symbol> *o, Class_ current_class, ClassTable *ct) {
    Symbol expr_type = this->expr->check_type(o, current_class, ct);

    method_class *method = ct->get_method(expr_type, this->name, current_class);

    if (this->type_name == SELF_TYPE) {
        this->set_type(Object);
        ct->semant_error(current_class->get_filename(), this) << "Static dispatch to SELF_TYPE." << std::endl;
    } else if (ct->symbol_class_map()->count(this->type_name) == 0) {
        this->set_type(Object);
        ct->semant_error(current_class->get_filename(), this) << "Static dispatch on undefined class "
                                                              << this->type_name << "." << std::endl;
    } else if (method == nullptr) {
        this->set_type(Object);
        ct->semant_error(current_class->get_filename(), this) << "Static dispatch to undefined method " << this->name
                                                              << "." << std::endl;
    } else {
        if (!ct->is_child(expr_type, this->type_name, current_class)) {
            ct->semant_error(current_class->get_filename(), this) << "Expression type " << expr_type
                                                                  << " does not conform to declared static dispatch type "
                                                                  << this->type_name << "." << std::endl;
        }

        Formals decl_method_argv = method->get_formals();
        Expressions call_method_argv = this->actual;

        int decl_method_argc = 0, call_method_argc = 0;

        int error_count = 0;

        for (int i = decl_method_argv->first();
             decl_method_argv->more(i); i = decl_method_argv->next(i), ++decl_method_argc);

        for (int i = call_method_argv->first();
             call_method_argv->more(i); i = call_method_argv->next(i), ++call_method_argc);

        if (call_method_argc != decl_method_argc) {
            this->set_type(Object);
            ct->semant_error(current_class->get_filename(), this)
                    << "Method " << method->get_name() << " invoked with wrong number of arguments." << std::endl;
        }

        for (int decl_idx = decl_method_argv->first(), call_idx = call_method_argv->first();
             decl_method_argv->more(decl_idx) && call_method_argv->more(call_idx);
             decl_idx = decl_method_argv->next(decl_idx), call_idx = call_method_argv->next(call_idx)) {
            Formal decl_arg = decl_method_argv->nth(decl_idx);
            Expression call_arg = call_method_argv->nth(call_idx);

            Symbol call_arg_type = call_arg->check_type(o, current_class, ct);

            if (!ct->is_child(call_arg_type, decl_arg->get_type(), current_class)) {
                this->set_type(Object);
                error_count += 1;
                ct->semant_error(current_class->get_filename(), this) << "In the call of the method "
                                                                      << method->get_name()
                                                                      << ", type " << call_arg_type
                                                                      << " of parameter "
                                                                      << decl_arg->get_name()
                                                                      << " does not conform to declared type "
                                                                      << decl_arg->get_type() << "." << std::endl;
            }
        }

        if (error_count == 0) {
            this->set_type(method->get_return_type() == SELF_TYPE ? expr_type : method->get_return_type());
        }
    }

    return this->get_type();
}

Symbol assign_class::check_type(cool::SymbolTable<Symbol, Symbol> *o, Class_ current_class, ClassTable *ct) {
    Symbol *name_type = o->lookup(this->name);

    if (this->name == self) {
        ct->semant_error(current_class->get_filename(), this) << "Cannot assign to 'self'." << std::endl;
    }

    if (!name_type) {
        ct->semant_error(current_class->get_filename(), this) << "Assignment to undeclared variable " << this->name
                                                              << "." << std::endl;
    }

    Symbol expr_type = this->expr->check_type(o, current_class, ct);
    this->set_type(expr_type);
    if (ct->is_child(expr_type, *name_type, current_class)) {

    } else {
        ct->semant_error(current_class->get_filename(), this) << "Type "
                                                              << expr_type
                                                              << " of assigned expression does not conform to declared type "
                                                              << *name_type
                                                              << " of identifier "
                                                              << this->name << "." << std::endl;
    }

    return this->get_type();
}

Symbol method_class::check_type(cool::SymbolTable<Symbol, Symbol> *o, Class_ current_class, ClassTable *ct) {
    o->enterscope();
    {
        std::set<Symbol> argv;

        if ((!(is_primitive(this->return_type) || this->return_type == SELF_TYPE)) &&
            ct->symbol_class_map()->count(this->return_type) == 0) {
            ct->semant_error(current_class->get_filename(), this) << "Undefined return type "
                                                                  << this->return_type
                                                                  << " in method "
                                                                  << this->name << "." << endl;
        }

        for (int i = this->formals->first(); this->formals->more(i); i = formals->next(i)) {
            Formal _arg = formals->nth(i);

            if (_arg->get_name() == self) {
                // ct->semant_error(current_class->get_filename(), _arg)
                //        << "'self' cannot be the name of a method argument." << std::endl;
            } else if (argv.count(_arg->get_name()) > 0) {
                // ct->semant_error(current_class->get_filename(), _arg) << "The argument " << _arg->get_name()
                //                                                       << " in the signature of method "
                //                                                       << this->get_name()
                //                                                       << " has already been defined." << std::endl;
            } else {
                // argv.insert(_arg->get_name());

                if (ct->symbol_class_map()->count(_arg->get_type()) == 0) {
                    //     ct->semant_error(current_class->get_filename(), _arg) << "The argument " << _arg->get_name()
                    //                                                           << " in the signature of method "
                    //                                                           << get_name()
                    //                                                           << " has undefined type " << _arg->get_type()
                    //                                                           << "."
                    //                                                           << std::endl;
                } else {
                    o->addid(_arg->get_name(), new Symbol(_arg->get_type()));
                }
            }
        }

        Symbol expr_type = this->expr->check_type(o, current_class, ct);

        if (!ct->is_child(expr_type, this->get_return_type(), current_class)) {
            ct->semant_error(current_class->get_filename(), this) << "Inferred return type " << expr_type
                                                                  << " of method "
                                                                  << this->get_name()
                                                                  << " does not conform to declared return type "
                                                                  << this->get_return_type() << "." << std::endl;
        }
    }
    o->exitscope();

    return this->get_return_type();
}

Symbol attr_class::check_type(cool::SymbolTable<Symbol, Symbol> *o, Class_ current_class, ClassTable *ct) {
    Symbol init_expr_type = this->get_init_expr()->check_type(o, current_class, ct);
    init_expr_type = init_expr_type == SELF_TYPE ? current_class->get_name() : init_expr_type;

    if (this->get_init_expr()->get_type() == No_type) {
        return this->get_type();
    } else if (this->get_name() == self) {
        ct->semant_error(current_class->get_filename(), this) << "'self' cannot be the name of an attribute."
                                                              << std::endl;
    } else if (ct->symbol_class_map()->count(this->type_decl) == 0) {
        ct->semant_error(current_class->get_filename(), this->get_init_expr()) << "Class " << this->type_decl
                                                                               << " of attribute " << this->name
                                                                               << " is undefined." << endl;
    } else if (!ct->is_child(init_expr_type, this->get_type(), current_class)) {
        ct->semant_error(current_class->get_filename(), this->get_init_expr()) << "Inferred type "
                                                                               << this->init->get_type()
                                                                               << " of initialization of attribute "
                                                                               << this->name
                                                                               << " does not conform to declared type "
                                                                               << this->type_decl << "." << endl;
    }
    return this->get_type();
}