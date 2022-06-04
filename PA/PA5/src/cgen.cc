//**************************************************************
//
// Code generator SKELETON
//
// Read the comments carefully and add code to build an LLVM program
//**************************************************************

#define EXTERN
#include "cgen.h"
#include <string>
#include <sstream>
#include <algorithm>

//
#define VERBOSE

#define RED 31
#define GREEN 32
#define YELLOW 33
#define BLUE 34
#define MAGENTA 35
#define CYAN 36
#define WHITE 37
#define RESET 0

extern int cgen_debug;

void print(std::string str);

std::string coloring(std::string str, int color);

void print(std::string str)
{
#ifdef VERBOSE
    std::cout << coloring("[*] OUT: ", YELLOW) << str << std::endl;
#endif
}

std::string coloring(std::string str, int color)
{
    return "\x1b[" + std::to_string(color) + "m" + str + "\x1b[" +
           std::to_string(RESET) + "m";
}

//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.  Feel free to add your
// own definitions as you see fit.
//
//////////////////////////////////////////////////////////////////////
EXTERN Symbol
    // required classes
    Object,
    IO,
    String,
    Int,
    Bool,
    Main,

    // class methods
    cool_abort,
    type_name,
    cool_copy,
    out_string,
    out_int,
    in_string,
    in_int,
    length,
    concat,
    substr,

    // class members
    val,

    // special symbols
    No_class,  // symbol that can't be the name of any user-defined class
    No_type,   // If e : No_type, then no code is generated for e.
    SELF_TYPE, // Special code is generated for new SELF_TYPE.
    self,      // self generates code differently than other references

    // extras
    arg,
    arg2,
    prim_string,
    prim_int,
    prim_bool;

//********************************************************
//
// PREDEFINED FUNCTIONS:
//
// The following functions are already coded, you should
// not need to modify them, although you may if necessary.
//
//********************************************************

//
// Initializing the predefined symbols.
//
static void initialize_constants(void)
{
    Object = idtable.add_string("Object");
    IO = idtable.add_string("IO");
    String = idtable.add_string("String");
    Int = idtable.add_string("Int");
    Bool = idtable.add_string("Bool");
    Main = idtable.add_string("Main");

    cool_abort = idtable.add_string("abort");
    type_name = idtable.add_string("type_name");
    cool_copy = idtable.add_string("copy");
    out_string = idtable.add_string("out_string");
    out_int = idtable.add_string("out_int");
    in_string = idtable.add_string("in_string");
    in_int = idtable.add_string("in_int");
    length = idtable.add_string("length");
    concat = idtable.add_string("concat");
    substr = idtable.add_string("substr");

    val = idtable.add_string("val");

    No_class = idtable.add_string("_no_class");
    No_type = idtable.add_string("_no_type");
    SELF_TYPE = idtable.add_string("SELF_TYPE");
    self = idtable.add_string("self");

    arg = idtable.add_string("arg");
    arg2 = idtable.add_string("arg2");
    prim_string = idtable.add_string("sbyte*");
    prim_int = idtable.add_string("int");
    prim_bool = idtable.add_string("bool");
}

//*********************************************************
//
// Define method for code generation
//
// This is the method called by the compiler driver
// `cgtest.cc'. cgen takes an `ostream' to which the assembly will be
// emitted, and it passes this and the class list of the
// code generator tree to the constructor for `CgenClassTable'.
// That constructor performs all of the work of the code
// generator.
//
//*********************************************************
void program_class::cgen(ostream &os)
{
    initialize_constants();
    class_table = new CgenClassTable(classes, os);
}

// Create definitions for all String constants
void StrTable::code_string_table(ostream &s, CgenClassTable *ct)
{
    for (List<StringEntry> *l = tbl; l; l = l->tl())
    {
        l->hd()->code_def(s, ct);
    }
}

// Create definitions for all Int constants
void IntTable::code_string_table(ostream &s, CgenClassTable *ct)
{
    for (List<IntEntry> *l = tbl; l; l = l->tl())
    {
        l->hd()->code_def(s, ct);
    }
}

//
// Sets up declarations for extra functions needed for code generation
// You should not need to modify this code for PA5
//
void CgenClassTable::setup_external_functions()
{
    ValuePrinter vp;
    // setup function: external int strcmp(sbyte*, sbyte*)
    op_type i32_type(INT32), i8ptr_type(INT8_PTR), vararg_type(VAR_ARG);
    vector<op_type> strcmp_args;
    strcmp_args.push_back(i8ptr_type);
    strcmp_args.push_back(i8ptr_type);
    vp.declare(*ct_stream, i32_type, "strcmp", strcmp_args);

    // setup function: external int printf(sbyte*, ...)
    vector<op_type> printf_args;
    printf_args.push_back(i8ptr_type);
    printf_args.push_back(vararg_type);
    vp.declare(*ct_stream, i32_type, "printf", printf_args);

    // setup function: external void abort(void)
    op_type void_type(VOID);
    vector<op_type> abort_args;
    vp.declare(*ct_stream, void_type, "abort", abort_args);

    // setup function: external i8* malloc(i32)
    vector<op_type> malloc_args;
    malloc_args.push_back(i32_type);
    vp.declare(*ct_stream, i8ptr_type, "malloc", malloc_args);

#ifdef PA5
    // Setup external functions for built in object class functions
    op_type objectptr_type{"Object*"}, stringptr_type{"String*"}, ioptr_type{"IO*"}, int32_t_type{INT32}, int32_tptr_type{INT32_PTR}, int_type{"Int"}, intptr_type{"Int*"}, boolptr_type{"Bool*"}, i1_type{INT1};

    // setup function: external Object* Object_new (void);
    vector<op_type> object_new_args{void_type};
    vp.declare(*ct_stream, objectptr_type, "Object_new", object_new_args);

    // setup function: external Object* Object_abort (Object*);
    vector<op_type> object_abort_args{objectptr_type};
    vp.declare(*ct_stream, objectptr_type, "Object_abort", object_abort_args);

    // setup function: external const String* Object_type_name (Object*);
    vector<op_type> object_type_name_args{objectptr_type};
    vp.declare(*ct_stream, stringptr_type, "Object_type_name", object_type_name_args);

    // setup function: external Object* Object_copy (Object*);
    vector<op_type> object_copy_args{objectptr_type};
    vp.declare(*ct_stream, objectptr_type, "Object_copy", object_copy_args);

    // setup function: external IO* IO_new (void);
    vector<op_type> io_new_args{void_type};
    vp.declare(*ct_stream, ioptr_type, "IO_new", io_new_args);

    // setup function: external IO* IO_out_string (IO*, String*);
    vector<op_type> io_out_string_args{ioptr_type, stringptr_type};
    vp.declare(*ct_stream, ioptr_type, "IO_out_string", io_out_string_args);

    // setup function: external IO* IO_out_int (IO*, int32_t*);
    vector<op_type> io_out_int_args{ioptr_type, int32_tptr_type};
    vp.declare(*ct_stream, ioptr_type, "IO_out_int", io_out_int_args);

    // setup function: external String* IO_in_string (IO*);
    vector<op_type> io_in_string_args{ioptr_type};
    vp.declare(*ct_stream, stringptr_type, "IO_in_string", io_in_string_args);

    // setup function: external int32_t* IO_in_int (IO*);
    vector<op_type> io_in_int_args{ioptr_type};
    vp.declare(*ct_stream, int32_tptr_type, "IO_in_int", io_in_int_args);

    // setup function: external String* String_new (void);
    vector<op_type> string_new_args{void_type};
    vp.declare(*ct_stream, stringptr_type, "String_new", string_new_args);

    // setup function: external int32_t  String_length (String*);
    vector<op_type> string_length_args{stringptr_type};
    vp.declare(*ct_stream, int32_t_type, "String_length", string_length_args);

    // setup function: external String* String_concat (String*, String*);
    vector<op_type> string_concat_args{stringptr_type, stringptr_type};
    vp.declare(*ct_stream, stringptr_type, "String_concat", string_concat_args);

    // setup function: external String* String_substr (String*, int32_t, int32_t);
    vector<op_type> string_substr_args{stringptr_type, int32_t_type, int32_t_type};
    vp.declare(*ct_stream, stringptr_type, "String_substr", string_substr_args);

    // setup function: external Int* Int_new (void);
    vector<op_type> int_new_args{void_type};
    vp.declare(*ct_stream, intptr_type, "Int_new", int_new_args);

    // setup function :external void Int_init (Int*, int32_t);
    vector<op_type> int_init_args{intptr_type, int32_t_type};
    vp.declare(*ct_stream, void_type, "Int_init", int_init_args);

    // setup function: external Bool* Bool_new (void);
    vector<op_type> bool_new_args{void_type};
    vp.declare(*ct_stream, boolptr_type, "Bool_new", bool_new_args);

    // setup function: external void Bool_init (Bool*, i1);
    vector<op_type> bool_init_args{boolptr_type, i1_type};
    vp.declare(*ct_stream, void_type, "Bool_init", bool_init_args);
#endif
}

// Creates AST nodes for the basic classes and installs them in the class list
void CgenClassTable::install_basic_classes()
{
    // The tree package uses these globals to annotate the classes built below.
    curr_lineno = 0;
    Symbol filename = stringtable.add_string("<basic class>");

    //
    // A few special class names are installed in the lookup table but not
    // the class list. Thus, these classes exist, but are not part of the
    // inheritance hierarchy.

    // No_class serves as the parent of Object and the other special classes.
    Class_ noclasscls = class_(No_class, No_class, nil_Features(), filename);
    install_special_class(new CgenNode(noclasscls, CgenNode::Basic, this));
    delete noclasscls;

#ifdef PA5
    // SELF_TYPE is the self class; it cannot be redefined or inherited.
    Class_ selftypecls = class_(SELF_TYPE, No_class, nil_Features(), filename);
    install_special_class(new CgenNode(selftypecls, CgenNode::Basic, this));
    delete selftypecls;
    //
    // Primitive types masquerading as classes. This is done so we can
    // get the necessary Symbols for the innards of String, Int, and Bool
    //
    Class_ primstringcls = class_(prim_string, No_class, nil_Features(), filename);
    install_special_class(new CgenNode(primstringcls, CgenNode::Basic, this));
    delete primstringcls;
#endif
    Class_ primintcls = class_(prim_int, No_class, nil_Features(), filename);
    install_special_class(new CgenNode(primintcls, CgenNode::Basic, this));
    delete primintcls;
    Class_ primboolcls = class_(prim_bool, No_class, nil_Features(), filename);
    install_special_class(new CgenNode(primboolcls, CgenNode::Basic, this));
    delete primboolcls;
    //
    // The Object class has no parent class. Its methods are
    //        cool_abort() : Object   aborts the program
    //        type_name() : Str       returns a string representation of class name
    //        copy() : SELF_TYPE      returns a copy of the object
    //
    // There is no need for method bodies in the basic classes---these
    // are already built in to the runtime system.
    //
    Class_ objcls =
        class_(Object,
               No_class,
               append_Features(
                   append_Features(
                       single_Features(method(cool_abort, nil_Formals(),
                                              Object, no_expr())),
                       single_Features(method(type_name, nil_Formals(),
                                              String, no_expr()))),
                   single_Features(method(cool_copy, nil_Formals(),
                                          SELF_TYPE, no_expr()))),
               filename);
    install_class(new CgenNode(objcls, CgenNode::Basic, this));
    delete objcls;

    //
    // The Int class has no methods and only a single attribute, the
    // "val" for the integer.
    //
    Class_ intcls =
        class_(Int,
               Object,
               single_Features(attr(val, prim_int, no_expr())),
               filename);
    install_class(new CgenNode(intcls, CgenNode::Basic, this));
    delete intcls;

    //
    // Bool also has only the "val" slot.
    //
    Class_ boolcls =
        class_(Bool,
               Object,
               single_Features(attr(val, prim_bool, no_expr())),
               filename);
    install_class(new CgenNode(boolcls, CgenNode::Basic, this));
    delete boolcls;

#ifdef PA5
    //
    // The class String has a number of slots and operations:
    //       val                                  the string itself
    //       length() : Int                       length of the string
    //       concat(arg: Str) : Str               string concatenation
    //       substr(arg: Int, arg2: Int): Str     substring
    //
    Class_ stringcls =
        class_(String,
               Object,
               append_Features(
                   append_Features(
                       append_Features(
                           single_Features(attr(val, prim_string, no_expr())),
                           single_Features(method(length, nil_Formals(),
                                                  Int, no_expr()))),
                       single_Features(method(concat,
                                              single_Formals(formal(arg, String)),
                                              String,
                                              no_expr()))),
                   single_Features(method(substr,
                                          append_Formals(
                                              single_Formals(formal(arg, Int)),
                                              single_Formals(formal(arg2, Int))),
                                          String,
                                          no_expr()))),
               filename);
    install_class(new CgenNode(stringcls, CgenNode::Basic, this));
    delete stringcls;
#endif

#ifdef PA5
    //
    // The IO class inherits from Object. Its methods are
    //        out_string(Str) : SELF_TYPE          writes a string to the output
    //        out_int(Int) : SELF_TYPE               "    an int    "  "     "
    //        in_string() : Str                    reads a string from the input
    //        in_int() : Int                         "   an int     "  "     "
    //
    Class_ iocls =
        class_(IO,
               Object,
               append_Features(
                   append_Features(
                       append_Features(
                           single_Features(method(out_string,
                                                  single_Formals(formal(arg, String)),
                                                  SELF_TYPE, no_expr())),
                           single_Features(method(out_int, single_Formals(formal(arg, Int)),
                                                  SELF_TYPE, no_expr()))),
                       single_Features(method(in_string, nil_Formals(), String,
                                              no_expr()))),
                   single_Features(method(in_int, nil_Formals(), Int, no_expr()))),
               filename);
    install_class(new CgenNode(iocls, CgenNode::Basic, this));
    delete iocls;
#endif
}

//
// install_classes enters a list of classes in the symbol table.
//
void CgenClassTable::install_classes(Classes cs)
{
    for (int i = cs->first(); cs->more(i); i = cs->next(i))
    {
        install_class(new CgenNode(cs->nth(i), CgenNode::NotBasic, this));
    }
}

//
// Add this CgenNode to the class list and the lookup table
//
void CgenClassTable::install_class(CgenNode *nd)
{
    Symbol name = nd->get_name();

    if (probe(name))
        return;

    // The class name is legal, so add it to the list of classes
    // and the symbol table.
    nds = new List<CgenNode>(nd, nds);
    addid(name, nd);
}

//
// Add this CgenNode to the special class list and the lookup table
//
void CgenClassTable::install_special_class(CgenNode *nd)
{
    Symbol name = nd->get_name();

    if (probe(name))
        return;

    // The class name is legal, so add it to the list of special classes
    // and the symbol table.
    special_nds = new List<CgenNode>(nd, special_nds);
    addid(name, nd);
}

//
// CgenClassTable::build_inheritance_tree
//
void CgenClassTable::build_inheritance_tree()
{
    for (List<CgenNode> *l = nds; l; l = l->tl())
        set_relations(l->hd());
}

//
// CgenClassTable::set_relations
//
// Takes a CgenNode and locates its, and its parent's, inheritance nodes
// via the class table.  Parent and child pointers are added as appropriate.
//
void CgenClassTable::set_relations(CgenNode *nd)
{
    CgenNode *parent_node = probe(nd->get_parent());
    nd->set_parentnd(parent_node);
    parent_node->add_child(nd);
}

// Get the root of the class tree.
CgenNode *CgenClassTable::root()
{
    return probe(Object);
}

//////////////////////////////////////////////////////////////////////
//
// Special-case functions used for the method Int Main::main() for
// PA5 only.
//
//////////////////////////////////////////////////////////////////////

#ifndef PA5

CgenNode *CgenClassTable::getMainmain(CgenNode *c)
{
    if (c && !c->basic())
        return c; // Found it!

    List<CgenNode> *children = c->get_children();
    for (List<CgenNode> *child = children; child; child = child->tl())
    {
        if (CgenNode *foundMain = this->getMainmain(child->hd()))
            return foundMain; // Propagate it up the recursive calls
    }

    return 0; // Make the recursion continue
}

#endif

//-------------------------------------------------------------------
//
// END OF PREDEFINED FUNCTIONS
//
//-------------------------------------------------------------------

///////////////////////////////////////////////////////////////////////////////
//
// coding string, int, and boolean constants
//
// Cool has three kinds of constants: strings, ints, and booleans.
// This section defines code generation for each type.
//
// All string constants are listed in the global "stringtable" and have
// type stringEntry.  stringEntry methods are defined both for string
// constant definitions and references.
//
// All integer constants are listed in the global "inttable" and have
// type IntEntry.  IntEntry methods are defined for Int
// constant definitions and references.
//
// Since there are only two Bool values, there is no need for a table.
// The two booleans are represented by instances of the class BoolConst,
// which defines the definition and reference methods for Bools.
//
///////////////////////////////////////////////////////////////////////////////

//
// Create global definitions for constant Cool objects
//
void CgenClassTable::code_constants()
{
#ifdef PA5
    stringtable.code_string_table(*this->ct_stream, this);
#endif
}

// generate code to define a global string constant
void StringEntry::code_def(ostream &s, CgenClassTable *ct)
{
#ifdef PA5
    ValuePrinter vp{s};

    string name = this->get_llvm_constant_name(false);
    string internal_name = this->get_llvm_constant_name(true);
    string value = string(this->get_string());

    op_type vtableptr_type = op_type{"String_vtable", 1};
    op_arr_type internal_name_type = op_arr_type{INT8, (int)value.length() + 1};

    vector<op_type> field_types{
        vtableptr_type,    // vtable pointer
        op_type{INT8_PTR}, // val
    };

    vector<const_value> init_values{
        const_value{vtableptr_type, "@String_vtable_prototype", 1},
        const_value{internal_name_type, "@" + internal_name, true},
    };

    vp.init_constant(this->get_llvm_constant_name(true), const_value{op_arr_type{INT8, (int)string(this->get_string()).length() + 1}, this->get_string(), true});
    vp.init_struct_constant(global_value{op_type{"String"}, name}, field_types, init_values);
#endif
}

string StringEntry::get_llvm_constant_name(bool is_internal)
{
    return is_internal ? "str." + itos(this->index) : "String." + itos(this->index);
}

// generate code to define a global int constant
void IntEntry::code_def(ostream &s, CgenClassTable *ct)
{
    // Leave this method blank, since we are not going to use global
    // declarations for int constants.
}

//////////////////////////////////////////////////////////////////////////////
//
//  CgenClassTable methods
//
//////////////////////////////////////////////////////////////////////////////

//
// CgenClassTable constructor orchestrates all code generation
//
CgenClassTable::CgenClassTable(Classes classes, ostream &s)
    : nds(0)
{
    if (cgen_debug)
        std::cerr << "Building CgenClassTable" << endl;
    ct_stream = &s;
    // Make sure we have a scope, both for classes and for constants
    enterscope();

    // Create an inheritance tree with one CgenNode per class.
    install_basic_classes();
    install_classes(classes);
    build_inheritance_tree();

    // First pass
    setup();

    // Second pass
    code_module();
    // Done with code generation: exit scopes
    exitscope();
}

CgenClassTable::~CgenClassTable()
{
}

// The code generation first pass.  Define these two functions to traverse
// the tree and setup each CgenNode
void CgenClassTable::setup()
{
    setup_external_functions();
    setup_classes(root(), 0);
}

void CgenClassTable::setup_classes(CgenNode *c, int depth)
{
    // MAY ADD CODE HERE
    // if you want to give classes more setup information

    c->setup(current_tag++, depth);
    List<CgenNode> *children = c->get_children();
    for (List<CgenNode> *child = children; child; child = child->tl())
        setup_classes(child->hd(), depth + 1);

    c->set_max_child(current_tag - 1);

    /*
    if (cgen_debug)
        std::cerr << "Class " << c->get_name() << " assigned tag "
            << c->get_tag() << ", max child " << c->get_max_child()
            << ", depth " << c->get_depth() << endl;
    */
}

// The code generation second pass. Add code here to traverse the tree and
// emit code for each CgenNode
void CgenClassTable::code_module()
{
    code_constants();

#ifndef PA5
    // This must be after code_module() since that emits constants
    // needed by the code() method for expressions
    CgenNode *mainNode = getMainmain(root());
    mainNode->codeGenMainmain();
#endif
    code_main();

#ifdef PA5
    code_classes(root());
#else
#endif
}

#ifdef PA5
void CgenClassTable::code_classes(CgenNode *c)
{
    c->code_class();

    List<CgenNode> *children = c->get_children();

    for (List<CgenNode> *child = children; child != NULL; child = child->tl())
    {
        this->code_classes(child->hd());
    }
}
#endif

//
// Create LLVM entry point. This function will initiate our Cool program
// by generating the code to execute (new Main).main()
//
void CgenClassTable::code_main()
{
    ValuePrinter vp{*ct_stream};

    /*
    define i32 @main() {
    entry:
        %main.obj = call %Main*() @Main_new( )
        %main.retval = call i32(%Main*) @Main_main( %Main* %main.obj )
        ret i32 0
    }
    */

    // Define a function main that has no parameters and returns an i32
    vector<op_type> main_args_t;
    vector<operand> main_args_v;
    op_type main_retn_type{INT32};

    vp.define(main_retn_type, "main", main_args_v);
    {
        // Define an entry basic block
        vp.begin_block("entry");

        // Call Main_main(). This returns int* for phase 1, Object for phase 2

#ifndef PA5
        operand main_retn_val = vp.call(main_args_t, main_retn_type, "Main_main", true, main_args_v);

        string main_out_0_str = "Main_main() returned %d\n";
        op_type main_out_0_typ(INT8_PTR);
        op_arr_type main_out_0_arr{INT8, (int)main_out_0_str.length() + 1};
        const_value main_out_0_cst{main_out_0_arr, main_out_0_str, false};

        vp.init_constant(".str", main_out_0_cst);

        // Get the address of the string "Main_main() returned %d\n" using
        // getelementptr

        global_value main_out_0_ptr{op_arr_type{INT8_PTR, (int)main_out_0_str.length() + 1}, ".str", main_out_0_cst};

        operand main_out_0 = vp.getelementptr(main_out_0_arr, main_out_0_ptr, int_value(0), int_value(0), main_out_0_typ);

        // Call printf with the string address of "Main_main() returned %d\n"
        // and the return value of Main_main() as its arguments

        vector<op_type> printf_args_t{main_out_0_typ, op_type{VAR_ARG}};
        vector<operand> printf_args_v{main_out_0, main_retn_val};
        op_type printf_retn_t{INT32};

        vp.call(printf_args_t, printf_retn_t, "printf", true, printf_args_v);

        // Insert return 0
        vp.ret(int_value(0));

#else
        // Phase 2
        vector<op_type> main_new_args_t;
        vector<operand> main_new_args_v;
        op_type main_new_retn_type{op_type("Main*()")};
        operand main_new_result_op{main_new_retn_type, "main.obj"};

        vp.call(*this->ct_stream, main_new_args_t, "Main_new", true, main_new_args_v, main_new_result_op);

        vector<op_type> main_main_args_t;
        vector<operand> main_main_args_v;
        op_type main_main_retn_type{this->lookup(Main)->get_method_info_by_name("Main_main").llvm_ret_type.get_name().substr(1) + "(%Main*)"};
        operand main_main_result_op{main_main_retn_type, "main.retval"};

        vp.call(*this->ct_stream, main_main_args_t, "Main_main", true, main_main_args_v, main_main_result_op);

        vp.ret(int_value(0));
#endif
    }
    vp.end_define();
}

///////////////////////////////////////////////////////////////////////
//
// CgenNode methods
//
///////////////////////////////////////////////////////////////////////

op_type symbol_to_op_type(Symbol type_decl, CgenNode *cls)
{
    op_type result;

    if (type_decl == No_class)
    {
        result = op_type{VOID};
    }
    else if (type_decl == No_type)
    {
        result = op_type{EMPTY};
    }
    else if (type_decl == SELF_TYPE)
    {
        result = op_type{cls->get_type_name(), 1};
    }
    else if (type_decl == Int || type_decl == prim_int)
    {
        result = op_type{INT32};
    }
    else if (type_decl == Bool || type_decl == prim_bool)
    {
        result = op_type{INT1};
    }
    else if (type_decl == prim_string)
    {
        result = op_type{INT8_PTR};
    }
    else
    {
        result = op_type{type_decl->get_string(), 1};
    }

    return result;
}

CgenNode::CgenNode(Class_ nd, Basicness bstatus, CgenClassTable *ct)
    : class__class((const class__class &)*nd),
      parentnd(0), children(0), basic_status(bstatus), class_table(ct), tag(-1), methods_offset(0), attributes_offset(0)
{
}

void CgenNode::add_child(CgenNode *n)
{
    children = new List<CgenNode>(n, children);
}

void CgenNode::set_parentnd(CgenNode *p)
{
    assert(parentnd == NULL);
    assert(p != NULL);
    parentnd = p;
}

//
// Class setup.  You may need to add parameters to this function so that
// the classtable can provide setup information (such as the class tag
// that should be used by this class).
//
// Things that setup should do:
//  - layout the features of the class
//  - create the types for the class and its vtable
//  - create global definitions used by the class such as the class vtable
//
void CgenNode::setup(int tag, int depth)
{
    this->tag = tag;
#ifdef PA5
    layout_features();

    ValuePrinter vp{*(this->get_classtable()->ct_stream)};

    vector<method_info> methods;
    vector<attribute_info> attributes;

    op_type i32_type = op_type{INT32};
    op_type i8ptr_type = op_type{INT8_PTR};

    op_arr_type i8_arr_type = op_arr_type{INT8, (int)this->get_type_name().length() + 1};

    op_func_type constructor_type = op_func_type{op_type{this->get_type_name(), 1}, vector<op_type>{}};

    string node_name = "str." + this->get_type_name();

    vector<op_type> vtable_fields{
        i32_type,         // id
        i32_type,         // size
        i8ptr_type,       // name
        constructor_type, // constructor
    };

    vector<const_value> vtable_init_values{
        int_value{this->tag},
        const_value{i32_type, "ptrtoint (%" + this->get_type_name() + "* getelementptr (%" + this->get_type_name() + ", %" + this->get_type_name() + "* null, i32 1) to i32)", true},
        const_value{i8_arr_type, "@" + node_name, true},
        const_value{constructor_type, "@" + this->get_constructor_name(), true},
    };

    std::transform(methods_layout.begin(), methods_layout.end(), std::back_inserter(methods), [](std::pair<Entry *, CgenNode::method_info> i)
                   { return i.second; });

    std::transform(attributes_layout.begin(), attributes_layout.end(), std::back_inserter(attributes), [](std::pair<Entry *, CgenNode::attribute_info> i)
                   { return i.second; });

    std::sort(methods.begin(), methods.end(), [](method_info a, method_info b)
              { return a.offset < b.offset; });

    std::sort(attributes.begin(), attributes.end(), [](attribute_info a, attribute_info b)
              { return a.offset < b.offset; });

    for (method_info &method : methods)
    {
        op_func_type ret_type = op_func_type{method.llvm_ret_type, method.llvm_args_types};

        vtable_fields.push_back(ret_type);

        if (method.def_class_name == this->get_type_name())
        {
            vtable_init_values.push_back(const_value{ret_type, "@" + method.llvm_mangled_name, true});
        }
        else
        {
            op_type evaluated_self_type = (method.is_ret_self_type) ? op_type{method.def_class_name, 1} : op_type{method.llvm_ret_type};

            method.llvm_args_types.at(0) = evaluated_self_type;

            vtable_init_values.push_back(casted_value{ret_type, "@" + method.llvm_mangled_name, op_func_type{evaluated_self_type, method.llvm_args_types}});
        }
    }

    for (attribute_info attr : attributes)
    {
    }

    vp.init_constant(node_name, const_value{i8ptr_type, this->get_type_name(), true});

    vp.type_define(this->get_type_name() + "_vtable", vtable_fields);

    vp.init_struct_constant(global_value{op_type{this->get_type_name() + "_vtable"}, this->get_type_name() + "_vtable_prototype"}, vtable_fields, vtable_init_values);

#endif
}

#ifdef PA5
//
// Class codegen. This should performed after every class has been setup.
// Generate code for each method of the class.
//
void CgenNode::code_class()
{
    // No code generation for basic classes. The runtime will handle that.
    if (basic())
        return;

    ostream &o = *this->get_classtable()->ct_stream;
    CgenEnvironment *env = new CgenEnvironment{o, this};
    ValuePrinter vp{o};

    for (int i = this->features->first(); this->features->more(i); i = this->features->next(i))
    {
        Feature f = this->features->nth(i);
        f->code(env);
    }

    env->enterscope();
    {
        // Define constructor
        vp.define(op_type{this->get_type_name(), 1}, this->get_constructor_name(), vector<operand>{});
        {
            vp.begin_block("entry");

            // Get size of class
            global_value vtable_global{op_type{this->get_vtable_type_name(), 1}, this->get_vtable_name()};

            operand v_size_ptr{op_type{INT32_PTR}, env->new_name()};
            operand v_size{op_type{INT32}, env->new_name()};

            vp.getelementptr(o, op_type{this->get_vtable_type_name()}, vtable_global, int_value{0}, int_value{1}, v_size_ptr);
            vp.load(o, op_type{INT32}, v_size_ptr, v_size);

            // Malloc
            operand v_malloced{op_type{INT8_PTR}, env->new_name()};

            vector<op_type> malloc_arg_t{
                op_type{INT32}, // size
            };

            vector<operand> malloc_arg_v{
                v_size,
            };

            vp.call(o, malloc_arg_t, "malloc", true, malloc_arg_v, v_malloced);

            // Cast to Main*
            op_type mainptr_type{this->get_type_name(), 1};
            operand v_main_ptr{mainptr_type, env->new_name()};

            vp.bitcast(o, v_malloced, mainptr_type, v_main_ptr);

            //
            op_type main_type{this->get_type_name()};
            operand v_main{op_type{this->get_vtable_type_name(), 2}, env->new_name()};

            vp.getelementptr(o, main_type, v_main_ptr, int_value{0}, int_value{0}, v_main);

            //
            vp.store(vtable_global, v_main);

            //
            operand v_main_stack{op_type{this->get_type_name(), 2}, env->new_name()};
            vp.alloca_mem(o, mainptr_type, v_main_stack);

            //
            vp.store(v_main_ptr, v_main_stack);

            // env->addid();

            for (attribute_info attr : this->get_sorted_attribute_layout())
            {
                attr.attr_node->code(env);
            }

            //
            vp.ret(v_main_ptr);

            vp.begin_block("abort");
            vp.call(vector<op_type>{}, VOID, "abort", true, vector<operand>{});
            vp.unreachable();
        }
        vp.end_define();
    }

    env->exitscope();

    delete env;
}

// Laying out the features involves creating a Function for each method
// and assigning each attribute a slot in the class structure.
void CgenNode::layout_features()
{
    if (this->parentnd)
    {
        this->attributes_offset = this->parentnd->attributes_offset;
        this->attributes_layout = this->parentnd->attributes_layout;

        this->methods_offset = this->parentnd->methods_offset;
        this->methods_layout = this->parentnd->methods_layout;
    }

    for (int i = this->features->first(); this->features->more(i); i = this->features->next(i))
    {
        this->features->nth(i)->layout_feature(this);
    }
}

void CgenNode::add_method(Entry *method_entry, const op_type llvm_ret_type, const std::vector<op_type> llvm_args_types, string llvm_mangled_name, string def_class_name, bool is_ret_self_type)
{
    if (this->methods_layout.count(method_entry) > 0)
    {
        this->methods_layout[method_entry].llvm_mangled_name = llvm_mangled_name;
        this->methods_layout[method_entry].def_class_name = def_class_name;
    }
    else
    {
        this->methods_layout[method_entry] = method_info{this->methods_offset++, llvm_ret_type, llvm_args_types, llvm_mangled_name, def_class_name, is_ret_self_type};
    }
}

void CgenNode::add_attribute(Symbol name, op_type llvm_type, bool is_self_type, Feature attr_node)
{
    this->attributes_layout[name] = attribute_info{this->attributes_offset++, llvm_type, is_self_type, attr_node};
}

vector<CgenNode::attribute_info> CgenNode::get_sorted_attribute_layout()
{
    vector<attribute_info> attributes;

    std::transform(attributes_layout.begin(), attributes_layout.end(), std::back_inserter(attributes), [](std::pair<Entry *, CgenNode::attribute_info> i)
                   { return i.second; });
    std::sort(attributes.begin(), attributes.end(), [](attribute_info a, attribute_info b)
              { return a.offset < b.offset; });

    return attributes;
}

#else

//
// code-gen function main() in class Main
//
void CgenNode::codeGenMainmain()
{
    ValuePrinter vp{*(this->class_table->ct_stream)};
    // In Phase 1, this can only be class Main. Get method_class for main().
    assert(std::string(this->name->get_string()) == std::string("Main"));
    method_class *mainMethod = (method_class *)features->nth(features->first());

    // ADD CODE HERE TO GENERATE THE FUNCTION int Mainmain().
    // Generally what you need to do are:
    // -- setup or create the environment, env, for translating this method
    // -- invoke mainMethod->code(env) to translate the method

    auto env = new CgenEnvironment(*(this->class_table->ct_stream), this);

    vector<operand> main_args_v;
    op_type main_retn_type{INT32};

    vp.define(main_retn_type, "Main_main", main_args_v);
    {
        vp.begin_block("entry");
        mainMethod->code(env);
    }
    vp.end_define();
}

#endif

//
// CgenEnvironment functions
//

//
// Class CgenEnvironment should be constructed by a class prior to code
// generation for each method.  You may need to add parameters to this
// constructor.
//
CgenEnvironment::CgenEnvironment(std::ostream &o, CgenNode *c)
{
    cur_class = c;
    cur_stream = &o;
    var_table.enterscope();
    tmp_count = block_count = ok_count = 0;
    // ADD CODE HERE
}

// Look up a CgenNode given a symbol
CgenNode *CgenEnvironment::type_to_class(Symbol t)
{
    return t == SELF_TYPE ? get_class()
                          : get_class()->get_classtable()->lookup(t);
}

// Provided CgenEnvironment methods
// Generate unique string names
std::string CgenEnvironment::new_name()
{
    std::stringstream s;
    s << tmp_count++;
    return "tmp." + s.str();
}

std::string CgenEnvironment::new_ok_label()
{
    std::stringstream s;
    s << ok_count++;
    return "ok." + s.str();
}
const std::string CgenEnvironment::new_label(const std::string &prefix,
                                             bool increment)
{
    std::string suffix = itos(block_count);
    block_count += increment;
    return prefix + suffix;
}

void CgenEnvironment::add_local(Symbol name, operand &vb)
{
    var_table.enterscope();
    var_table.addid(name, &vb);
}

void CgenEnvironment::kill_local()
{
    var_table.exitscope();
}

////////////////////////////////////////////////////////////////////////////
//
// APS class methods
//
////////////////////////////////////////////////////////////////////////////

//******************************************************************
//
//   Fill in the following methods to produce code for the
//   appropriate expression.  You may add or remove parameters
//   as you wish, but if you do, remember to change the parameters
//   of the declarations in `cool-tree.handcode.h'.
//
//*****************************************************************

#ifdef PA5
// conform and get_class_tag are only needed for PA5

// conform - If necessary, emit a bitcast or boxing/unboxing operations
// to convert an object to a new type. This can assume the object
// is known to be (dynamically) compatible with the target type.
// It should only be called when this condition holds.
// (It's needed by the supplied code for typecase)
operand conform(operand src, op_type type, CgenEnvironment *env)
{
    // ADD CODE HERE (PA5 ONLY)
    return operand();
}

// Retrieve the class tag from an object record.
// src is the object we need the tag from.
// src_class is the CgenNode for the *static* class of the expression.
// You need to look up and return the class tag for it's dynamic value
operand get_class_tag(operand src, CgenNode *src_cls, CgenEnvironment *env)
{
    // ADD CODE HERE (PA5 ONLY)
    return operand();
}
#endif

//
// Create a method body
//
void method_class::code(CgenEnvironment *env)
{
    if (cgen_debug)
        std::cerr << "method" << endl;

    // ADD CODE HERE
    ValuePrinter vp{*(env->cur_stream)};

    vector<operand> method_body_args_v;
    vector<op_type> method_body_args_t;

    vp.ret(expr->code(env));

    vp.begin_block("abort");
    vp.call(method_body_args_t, VOID, "abort", true, method_body_args_v);
    vp.unreachable();
}

//
// Codegen for expressions.  Note that each expression has a value.
//

operand assign_class::code(CgenEnvironment *env)
{
    if (cgen_debug)
        std::cerr << "assign" << endl;

    ValuePrinter vp{*(env->cur_stream)};

    operand value = this->expr->code(env);
    vp.store(value, *(env->lookup(name)));

    return value;
}

operand cond_class::code(CgenEnvironment *env)
{
    if (cgen_debug)
        std::cerr << "cond" << endl;
    ValuePrinter vp{*(env->cur_stream)};

    string branch_then_label = env->new_label("cond.then.", 1);
    string branch_else_label = env->new_label("cond.else.", 1);
    string branch_done_label = env->new_label("cond.done.", 1);

    op_type result_type;
    operand result_ptr, op_then, op_else;

    ostream *cur_stream = env->cur_stream;
    env->cur_stream = new std::stringstream();
    result_type = this->then_exp->code(env).get_type();
    env->cur_stream = cur_stream;

    result_ptr = vp.alloca_mem(result_type);

    vp.branch_cond(this->pred->code(env), branch_then_label, branch_else_label);

    // then
    vp.begin_block(branch_then_label);
    op_then = this->then_exp->code(env);
    vp.store(op_then, result_ptr);
    vp.branch_uncond(branch_done_label);

    // else
    vp.begin_block(branch_else_label);
    op_else = this->else_exp->code(env);
    vp.store(op_else, result_ptr);
    vp.branch_uncond(branch_done_label);

    // done
    vp.begin_block(branch_done_label);

    return vp.load(result_type, result_ptr);
}

operand loop_class::code(CgenEnvironment *env)
{
    if (cgen_debug)
        std::cerr << "loop" << endl;
    ValuePrinter vp{*(env->cur_stream)};

    string branch_cond_label = env->new_label("loop.cond.", 1);
    string branch_body_label = env->new_label("loop.body.", 1);
    string branch_done_label = env->new_label("loop.done.", 1);

    vp.branch_uncond(branch_cond_label);

    vp.begin_block(branch_cond_label);

    vp.branch_cond(
        this->pred->code(env),
        branch_body_label,
        branch_done_label);

    vp.begin_block(branch_body_label);

    operand op_body = this->body->code(env);

    vp.branch_uncond(branch_cond_label);

    vp.begin_block(branch_done_label);

    return op_body;
}

operand block_class::code(CgenEnvironment *env)
{
    if (cgen_debug)
        std::cerr << "block" << endl;
    operand block;

    for (int i = this->body->first(); this->body->more(i); i = body->next(i))
    {
        block = this->body->nth(i)->code(env);
    }
    return block;
}

operand let_class::code(CgenEnvironment *env)
{
    if (cgen_debug)
        std::cerr << "let" << endl;
    ValuePrinter vp{*(env->cur_stream)};

    op_type type = (strcmp(this->type_decl->get_string(), "Bool") == 0) ? INT1 : INT32;

    operand let_init_value = this->init->code(env);
    operand let_name = vp.alloca_mem(type);

    env->add_local(this->identifier, let_name);

    if (let_init_value.get_type().get_id() == EMPTY)
        vp.store(const_value(type, (type.get_id() == INT1) ? "false" : "0", true), let_name);
    else
        vp.store(let_init_value, let_name);

    operand result = this->body->code(env);

    env->kill_local();

    return result;
}

operand plus_class::code(CgenEnvironment *env)
{
    if (cgen_debug)
        std::cerr << "plus" << endl;
    ValuePrinter vp{*(env->cur_stream)};
    return vp.add(this->e1->code(env), this->e2->code(env));
}

operand sub_class::code(CgenEnvironment *env)
{
    if (cgen_debug)
        std::cerr << "sub" << endl;
    ValuePrinter vp{*(env->cur_stream)};
    return vp.sub(this->e1->code(env), this->e2->code(env));
}

operand mul_class::code(CgenEnvironment *env)
{
    if (cgen_debug)
        std::cerr << "mul" << endl;
    ValuePrinter vp{*(env->cur_stream)};
    return vp.mul(this->e1->code(env), this->e2->code(env));
}

operand divide_class::code(CgenEnvironment *env)
{
    if (cgen_debug)
        std::cerr << "div" << endl;
    ValuePrinter vp{*(env->cur_stream)};

    string branch_ok_label = env->new_ok_label();
    operand e1_code = this->e1->code(env);
    operand e2_code = this->e2->code(env);

    vp.branch_cond(
        vp.icmp(EQ, e2_code, int_value(0)),
        "abort",
        branch_ok_label);

    vp.begin_block(branch_ok_label);
    return vp.div(e1_code, e2_code);
}

operand neg_class::code(CgenEnvironment *env)
{
    if (cgen_debug)
        std::cerr << "neg" << endl;
    ValuePrinter vp{*(env->cur_stream)};
    return vp.xor_in(int_value(0xFFFFFFFF), this->e1->code(env));
}

operand lt_class::code(CgenEnvironment *env)
{
    if (cgen_debug)
        std::cerr << "lt" << endl;
    ValuePrinter vp{*(env->cur_stream)};
    return vp.icmp(LT, this->e1->code(env), this->e2->code(env));
}

operand eq_class::code(CgenEnvironment *env)
{
    if (cgen_debug)
        std::cerr << "eq" << endl;
    ValuePrinter vp{*(env->cur_stream)};
    return vp.icmp(EQ, this->e1->code(env), this->e2->code(env));
}

operand leq_class::code(CgenEnvironment *env)
{
    if (cgen_debug)
        std::cerr << "leq" << endl;
    ValuePrinter vp{*(env->cur_stream)};
    return vp.icmp(LE, this->e1->code(env), this->e2->code(env));
}

operand comp_class::code(CgenEnvironment *env)
{
    if (cgen_debug)
        std::cerr << "complement" << endl;
    ValuePrinter vp{*(env->cur_stream)};

    operand e1_code = e1->code(env);

    return vp.xor_in(e1_code, bool_value(true, true));
}

operand int_const_class::code(CgenEnvironment *env)
{
    if (cgen_debug)
        std::cerr << "Integer Constant" << endl;
    return int_value(atoi(this->token->get_string()));
}

operand bool_const_class::code(CgenEnvironment *env)
{
    if (cgen_debug)
        std::cerr << "Boolean Constant" << endl;
    return bool_value(this->val, this->val);
}

operand object_class::code(CgenEnvironment *env)
{
    if (cgen_debug)
        std::cerr << "Object" << endl;
    ValuePrinter vp{*(env->cur_stream)};

    operand ret = *(env->lookup(name));
    return vp.load(ret.get_type().get_deref_type(), ret);
}

operand no_expr_class::code(CgenEnvironment *env)
{
    if (cgen_debug)
        std::cerr << "No_expr" << endl;
    return operand{};
}

//*****************************************************************
// The next few functions are for node types not supported in Phase 1
// but these functions must be defined because they are declared as
// methods via the Expression_SHARED_EXTRAS hack.
//*****************************************************************

operand static_dispatch_class::code(CgenEnvironment *env)
{
    if (cgen_debug)
        std::cerr << "static dispatch" << endl;
#ifndef PA5
    assert(0 && "Unsupported case for phase 1");
#else
        // ADD CODE HERE AND REPLACE "return operand()" WITH SOMETHING
        // MORE MEANINGFUL
#endif
    return operand();
}

operand string_const_class::code(CgenEnvironment *env)
{
    if (cgen_debug)
        std::cerr << "string_const" << endl;
#ifndef PA5
    assert(0 && "Unsupported case for phase 1");
#else
        // ADD CODE HERE AND REPLACE "return operand()" WITH SOMETHING
        // MORE MEANINGFUL
#endif
    return operand();
}

operand dispatch_class::code(CgenEnvironment *env)
{
    if (cgen_debug)
        std::cerr << "dispatch" << endl;
#ifndef PA5
    assert(0 && "Unsupported case for phase 1");
#else
        // ADD CODE HERE AND REPLACE "return operand()" WITH SOMETHING
        // MORE MEANINGFUL
#endif
    return operand();
}

operand typcase_class::code(CgenEnvironment *env)
{
    if (cgen_debug)
        std::cerr << "typecase::code()" << endl;
#ifndef PA5
    assert(0 && "Unsupported case for phase 1");
#else
        // ADD CODE HERE AND REPLACE "return operand()" WITH SOMETHING
        // MORE MEANINGFUL
#endif
    return operand();
}

operand new__class::code(CgenEnvironment *env)
{
    if (cgen_debug)
        std::cerr << "newClass" << endl;
#ifndef PA5
    assert(0 && "Unsupported case for phase 1");
#else
        // ADD CODE HERE AND REPLACE "return operand()" WITH SOMETHING
        // MORE MEANINGFUL
#endif
    return operand();
}

operand isvoid_class::code(CgenEnvironment *env)
{
    if (cgen_debug)
        std::cerr << "isvoid" << endl;
#ifndef PA5
    assert(0 && "Unsupported case for phase 1");
#else
        // ADD CODE HERE AND REPLACE "return operand()" WITH SOMETHING
        // MORE MEANINGFUL
#endif
    return operand();
}

// Create the LLVM Function corresponding to this method.
void method_class::layout_feature(CgenNode *cls)
{
#ifndef PA5
    assert(0 && "Unsupported case for phase 1");
#else
    std::vector<op_type> args{symbol_to_op_type(SELF_TYPE, cls)};

    for (int i = this->formals->first(); this->formals->more(i); i = this->formals->next(i))
    {
        Formal f = this->formals->nth(i);
        args.push_back(symbol_to_op_type(f->get_type_decl(), cls));
    }

    cls->add_method(this->name, symbol_to_op_type(this->return_type, cls), args, cls->get_type_name() + "_" + this->name->get_string(), cls->get_type_name(), this->return_type == SELF_TYPE);
#endif
}

// If the source tag is >= the branch tag and <= (max child of the branch class) tag,
// then the branch is a superclass of the source
operand branch_class::code(operand expr_val, operand tag,
                           op_type join_type, CgenEnvironment *env)
{
#ifndef PA5
    assert(0 && "Unsupported case for phase 1");
#else
    // ADD CODE HERE AND REPLACE "return operand()" WITH SOMETHING
    // MORE MEANINGFUL
#endif
    return operand();
}

// Assign this attribute a slot in the class structure
void attr_class::layout_feature(CgenNode *cls)
{
#ifndef PA5
    assert(0 && "Unsupported case for phase 1");
#else
    cls->add_attribute(this->name, symbol_to_op_type(this->type_decl, cls), this->type_decl == SELF_TYPE, (Feature)this);
#endif
}

void attr_class::code(CgenEnvironment *env)
{
#ifndef PA5
    assert(0 && "Unsupported case for phase 1");
#else
    // ADD CODE HERE
#endif
}
