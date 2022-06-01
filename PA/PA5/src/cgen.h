//
// This is the PA4  skeleton cgen header.  As given, it contains only basic
// functionality.  You will need to add members to each of the classes
// to get them to perform their desired functions.  Document your important
// design decisions below.  We should be able to read your documentation and
// get a general overview of how your compiler generates code.  For instance,
// how does your compiler generate structures for classes, how is inheritance
// modeled, how do you handle dynamic binding, etc.
//

// ------------------ INSERT DESIGN DOCUMENTATION HERE --------------------- //


// ----------------------------- END DESIGN DOCS --------------------------- //

#include "cool-tree.h"
#include "symtab.h"
#include "value_printer.h"

//
// CgenClassTable represents the top level of a Cool program, which is
// basically a list of classes.  The class table is used to look up classes
// (CgenNodes) by name, and it also handles global code generation tasks.
// The CgenClassTable constructor is where you'll find the entry point for
// code generation for an entire Cool program.
// 
class CgenClassTable : public cool::SymbolTable<Symbol,CgenNode> 
{
private:
	// Class list
	List<CgenNode> *nds;
	List<CgenNode> *special_nds;
	int current_tag;
    
#ifndef PA5
	CgenNode* getMainmain(CgenNode* c);
#endif

public:
	// The ostream where we are emitting code
	ostream *ct_stream;
	// CgenClassTable constructor begins and ends the code generation process
	CgenClassTable(Classes, ostream& str);
	~CgenClassTable();

	// Get the root of the class Tree, i.e. Object
	CgenNode *root();
	int get_num_classes() const	{ return current_tag; }

private:
	// COMPLETE FUNCTIONS
    
	// Create declarations for C runtime functions we need to generate code
	void setup_external_functions();
	void setup_classes(CgenNode *c, int depth);

#ifdef PA5
	void code_classes(CgenNode *c);
#endif

	// The following creates an inheritance graph from a list of classes.  
	// The graph is implemented as a tree of `CgenNode', and class names 
	// are placed in the base class symbol table.
	void install_basic_classes();
	void install_class(CgenNode *nd);
	void install_special_class(CgenNode *nd);
	void install_classes(Classes cs);
	void build_inheritance_tree();
	void set_relations(CgenNode *nd);
    
	// INCOMPLETE FUNCTIONS
    
	// Setup each class in the table and prepare for code generation phase
	void setup();
    
	// Code generation functions. You need to write these functions.
	void code_module();
	void code_constants();
	void code_main();

	// ADD CODE HERE

};

    
//
// Each CgenNode corresponds to a Cool class.  As such, it is responsible for
// performing code generation on the class level.  This includes laying out
// the class attributes, creating the necessary Types for the class and 
// generating code for each of its methods.
//
class CgenNode : public class__class 
{
public:
	enum Basicness
	{ Basic, NotBasic };

#ifndef PA5
	void codeGenMainmain();
#endif

private: 
	CgenNode *parentnd;                        // Parent of class
	List<CgenNode> *children;                  // Children of class
	Basicness basic_status;                    // `Basic' or 'NotBasic'
	CgenClassTable *class_table;
    
	// Class tag.  Should be unique for each class in the tree
	int tag;
	int max_child;


	// ADD CODE HERE


public:
	// COMPLETE FUNCTIONS

	// Relationships with other nodes in the tree
 	CgenNode *get_parentnd() { return parentnd; }
	void add_child(CgenNode *child);
	void set_parentnd(CgenNode *p);
	int basic() { return (basic_status == Basic); }
	List<CgenNode> *get_children() { return children; }
    
	// Accessors for other provided fields
	int get_tag() const 	{ return tag; }
	CgenClassTable *get_classtable() { return class_table; }

	void set_max_child(int mc) 	{ max_child = mc; }
	int get_max_child() const 	{ return max_child; }

	// INCOMPLETE FUNCTIONS
    
	// Constructs a CgenNode from a Class
	CgenNode(Class_ c, Basicness bstatus, CgenClassTable *class_table);
	virtual ~CgenNode() { }

	// Class setup. You need to write the body of this function.
	void setup(int tag, int depth);

	// Class codegen. You need to write the body of this function.
	void code_class();

	// ADD CODE HERE
	string get_type_name() { return string(name->get_string()); }


private:
	// Layout the methods and attributes for code generation
	// You need to write the body of this function.
	void layout_features();

	// ADD CODE HERE

};

//
// CgenEnvironment provides the environment for code generation of a method.
// Its main task is to provide a mapping from Cool names to LLVM Values.
// This mapping needs to be maintained as scopes are entered and exited, new
// variables are declared, and so on. CgenEnvironment is also a good place
// to put non-local information you will need during code generation.  Two
// examples are the current CgenNode and the current Function.
//
class CgenEnvironment
{
private:
	// mapping from variable names to memory locations
	cool::SymbolTable<Symbol,operand> var_table;

	// Keep counters for unique name generation in the current method
	int block_count;
	int tmp_count;
	int ok_count;

	// ADD CODE HERE
	CgenNode *cur_class;


public:
	std::ostream *cur_stream;

	// fresh name generation functions
	string new_name();
	string new_ok_label();
	const string new_label(const std::string& prefix, bool increment);

	// Used in provided code for the (case..of) construct
	string next_label;
	operand branch_operand;    
	void add_local(Symbol name, operand &vb);
	void kill_local();
	// end of helpers for provided code

	CgenEnvironment(ostream &strea, CgenNode *cur_class);


	operand *lookup(Symbol name)	{ return var_table.lookup(name); }
    
	CgenNode *get_class() { return cur_class; }
	void set_class(CgenNode *c) { cur_class = c; }
    
	// INCOMPLETE FUNCTIONS

	// Must return the CgenNode for a class given the symbol of its name
	CgenNode *type_to_class(Symbol t);
	// ADD CODE HERE
	
};

// Utitlity function
// Generate any code necessary to convert from given operand to
// dest_type, assuing it has already been checked to be compatible
operand conform(operand src, op_type dest_type, CgenEnvironment *env);
// Retrieve the class tag from operand src. Argument is the cgen node for
// the static class of src.
operand get_class_tag(operand src, CgenNode *src_cls, CgenEnvironment *env);
