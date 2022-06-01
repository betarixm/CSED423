#include "value_printer.h"
#include "cool-io.h"     // for cerr, <<, manipulators
#include <sstream>

static int value_printer_counter = 0;
static void embed_getelementptr (ostream &o, op_type type, operand op1, operand op2, operand op3);

operand make_fresh_operand(op_type type) {
	stringstream out;
	out << "vtpm." << (value_printer_counter++);
	string name = out.str();
 	return operand(type, name);
}

void my_print_escaped_string(ostream& str, const char *s)
{
	while (*s) {
		switch (*s) {
		case '\\' : str << "\\5C"; break;
		case '\"' : str << "\\22"; break;
		case '\n' : str << "\\0A"; break;
		case '\t' : str << "\\09"; break;
		case '\b' : str << "\\08"; break;
		case '\f' : str << "\\0C"; break;

		default:
			if (isprint(*s))
				str << *s;
			else 
				// 
				// Unprintable characters are printed using octal equivalents.
				// To get the sign of the octal number correct, the character
				// must be cast to an unsigned char before coverting it to an
				// integer.
				//
				str << '\\' << oct << setfill('0') << setw(3)
				    << (int) ((unsigned char) (*s))
				    << dec << setfill(' ');
			break;
		}
		s++;
	}
}

/* Constant initialization 
 * Format: @name = [internal] constant type value
 */
void ValuePrinter::init_constant(ostream &o, string name, const_value op) {
	o << "@" + name + " = " + (op.is_internal() ? "internal " : "") 
	  + "constant " + op.get_typename() + " ";
	if (op.get_type().get_id() == INT8) {
		o << "c\"";
		my_print_escaped_string(o, op.get_value().c_str());
		o << "\\00\"";
  	}
	else
		o << op.get_value();
	o << "\n";
}

void ValuePrinter::init_constant(string name, const_value op) {
	check_ostream();
	init_constant(*stream, name, op);
}

void ValuePrinter::init_ext_constant(ostream &o, string name, op_type type) {
	o << "@" + name + " = " + "external constant " 
	  + type.get_name() + "\n";
}

void ValuePrinter::init_ext_constant(string name, op_type type) {
	check_ostream();
	init_ext_constant(*stream, name, type);
}

/* Function definition
 * Format: define return_type function_name(args) {
 * Note: Must terminate the function definition with a "}" or by using end_define() after
 * printing all the instructions in a function body.
 */
void ValuePrinter::define(ostream &o, op_type ret_type, string name, vector<operand> args) {
	check_ostream(o);
	o << "define " + ret_type.get_name() + " @" + name + "(";
	for (unsigned i = 0; i < args.size(); ++i)
		o << args[i].get_typename() + " " + args[i].get_name()  +  (i + 1 < args.size() ? ", " : "");
	o << ") {\n";
}
void ValuePrinter::define(op_type ret_type, string name, vector<operand> args) {
	define(*stream, ret_type, name, args);
}

/* Function declaration
 * Format: declare return_type function_name(arg_types)
 */
void ValuePrinter::declare(ostream &o, op_type ret_type, string name, vector<op_type> args) {
	check_ostream(o);
	o << "declare " + ret_type.get_name() + " @" + name + "(";
	for (unsigned i = 0; i < args.size(); ++i)
		o << args[i].get_name()  +  (i + 1 < args.size() ? ", " : "");
	o << ")\n";
}
void ValuePrinter::declare(op_type ret_type, string name, vector<op_type> args) {
	declare(*stream, ret_type, name, args);
}

/* Type definition
 * Format class_name = type {
 * 	attributes,
 * 	...
 * 	}
 */
void ValuePrinter::type_define(ostream &o, string class_name, vector<op_type> attributes) {
	check_ostream(o);
	o << "%" << class_name << " = type {\n\t";
	for(unsigned i = 0; i < attributes.size(); ++i)
		o <<  attributes[i].get_name() + (i + 1 < attributes.size() ? ",\n\t" : "\n}\n\n");
}


void ValuePrinter::type_define(string class_name, vector<op_type> attributes) {
	type_define(*stream, class_name, attributes);
}

void ValuePrinter::type_alias_define(ostream &o, string alias_name, op_type type){
	check_ostream(o);
	o << "%" << alias_name << " = type " << type.get_name() << "\n";
}

void ValuePrinter::type_alias_define(string alias_name, op_type type) {
	type_alias_define(*stream, alias_name, type);
}


/* Structure constant definition
 * Format constant_name = {
 * 	attribute init_value,
 * 	...
 * 	}
 */
void ValuePrinter::init_struct_constant(ostream &o, operand constant,
                vector<op_type> field_types, vector<const_value> init_values) {
	check_ostream(o);
	o << constant.get_name() << " = constant " + constant.get_typename() + " {\n\t";
	for(unsigned i = 0; i < init_values.size(); ++i) {
		o << field_types[i].get_name() + " ";
		if (init_values[i].get_type().get_id() == INT8 && field_types[i].get_id() == INT8_PTR)
			embed_getelementptr(o, init_values[i].get_type(), 
			    init_values[i], int_value(0), int_value(0));
		else
			o << init_values[i].get_value();
		o << (i + 1 < init_values.size() ? ",\n\t" : "\n}\n\n");
	}
}


void ValuePrinter::init_struct_constant(operand constant,
      vector<op_type> field_types, vector<const_value> init_values) {
	init_struct_constant(*stream, constant, field_types, init_values);
}



void ValuePrinter::begin_block(string label)
{
	check_ostream();
	*stream << "\n" + label + ":\n";
}

/* Binary instruction
 * Format: [result = ] inst_name type op1_name, op2_name
 */
void ValuePrinter::bin_inst(ostream &o, string inst_name, operand op1, operand op2, operand result) {
	check_ostream(o);
	o  << "\t";	
	if (!result.is_empty())
		o <<  result.get_name() + " = ";
	o << inst_name + " " + op1.get_typename() + " "  + op1.get_name() + ", " + op2.get_name() + "\n";
}
operand ValuePrinter::bin_inst(string inst_name, operand op1, operand op2) {
	operand ret = make_fresh_operand(op1.get_type());
	bin_inst(*stream, inst_name, op1, op2, ret);
	return ret;
}

void ValuePrinter::add(ostream &o, operand op1, operand op2, operand result) {
	bin_inst(o, "add", op1, op2, result);
}
operand ValuePrinter::add(operand op1, operand op2) {
	return bin_inst("add", op1, op2);
}

void ValuePrinter::sub(ostream &o, operand op1, operand op2, operand result) {
	bin_inst(o, "sub", op1, op2, result);
}
operand ValuePrinter::sub(operand op1, operand op2) {
	return bin_inst("sub", op1, op2);
}

void ValuePrinter::div(ostream &o, operand op1, operand op2, operand result) {
	bin_inst(o, "sdiv", op1, op2, result);
}
operand ValuePrinter::div(operand op1, operand op2) {
	return bin_inst("sdiv", op1, op2);
}

void ValuePrinter::mul(ostream &o, operand op1, operand op2, operand result) {
	bin_inst(o, "mul", op1, op2, result);
}
operand ValuePrinter::mul(operand op1, operand op2) {
	return bin_inst("mul", op1, op2);
}

void ValuePrinter::xor_in(ostream &o, operand op1, operand op2, operand result) {
	bin_inst(o, "xor", op1, op2, result);
}
operand ValuePrinter::xor_in(operand op1, operand op2) {
	return bin_inst("xor", op1, op2);
}

/* malloc instruction
 * Format: result_name = call i8* @malloc(i32 size)
 * size could be an integer or an operand
 */
void ValuePrinter::malloc_mem(ostream &o, int size, operand result) {
	check_ostream(o);
	o << "\t" + result.get_name() + " = call i8*  @malloc(i32 " << size << ")\n";
}

operand ValuePrinter::malloc_mem(int size)
{
	operand result = make_fresh_operand(INT8_PTR);
	malloc_mem(*stream, size, result);
	return result;
}

void ValuePrinter::malloc_mem(ostream &o, operand size, operand result)
{
	check_ostream(o);
	o << "\t" + result.get_name() + " = call i8* @malloc(i32 " + size.get_name() + ")\n";
}

operand ValuePrinter::malloc_mem(operand size)
{
	operand result = make_fresh_operand(INT8_PTR);
	malloc_mem(*stream, size, result);
	return result;
}

/* alloca instruction
 * Format: result_name = alloca type
 */
void ValuePrinter::alloca_mem(ostream &o, op_type type, operand result) {
	check_ostream(o);
	o << "\t" + result.get_name() + " = alloca " + type.get_name() + "\n";
}
operand ValuePrinter::alloca_mem(op_type type) {
	operand result = make_fresh_operand(type.get_ptr_type());
	alloca_mem(*stream, type, result);
	return result;
}

/* load instruction
 * Format: [result =] load type, op_type op_name
 */
void ValuePrinter::load(ostream &o, op_type type, operand op, operand result) {
	check_ostream(o);
	o << "\t";
	o << result.get_name() + " = "; 
	o << "load " + type.get_name() + ", " + op.get_typename() + " " + op.get_name() + "\n";
}
operand ValuePrinter::load(op_type type, operand op) {
	operand result = make_fresh_operand(op.get_type().get_deref_type());
	load(*stream, type, op, result);
	return result;
}

/* store instruction
 * store op1_type op1_name, result_type result_name
 */
void ValuePrinter::store(ostream &o, operand op, operand result) {
	check_ostream(o);
	o << "\tstore " + op.get_typename() + " " + op.get_name() 
	  + ", " + result.get_typename() + " " + result.get_name() + "\n";
}
void ValuePrinter::store(operand op, operand result) {
	store(*stream, op, result);
}

/* getelementptr instruction
 * Format: getelementptr type, op1_type op1_name, op2_type op2_name, op3_type op3_name
 * Note: This instruction can take a different number of operands but you'll only need this one
 * in PA4.
 */
void ValuePrinter::getelementptr(ostream &o, op_type type, operand op1, operand op2, operand op3, operand result) {
	check_ostream(o);
	o << "\t";
	if (result.get_type().get_id() != VOID)
		o << result.get_name() << " = ";
	o << "getelementptr " + type.get_name() + ", " 
	  + op1.get_typename() + " " + op1.get_name() + ", "
	  + op2.get_typename() + " " + op2.get_name() + ", " 
	  + op3.get_typename() + " " + op3.get_name() + "\n";
}
operand ValuePrinter::getelementptr(op_type type, operand op1, operand op2, operand op3, op_type result_type) {
	operand result = make_fresh_operand(result_type);
	getelementptr(*stream, type, op1, op2, op3, result);
	return result;
}

/* getelementptr instruction
 * Format: getelementptr type, op1_type op1_name, op2_type op2_name, op3_type op3_name
 */
void ValuePrinter::getelementptr(ostream &o, op_type type, operand op1, operand op2, operand result) {
	check_ostream(o);
	o << "\t";
	if (result.get_type().get_id() != VOID)
		o << result.get_name() << " = ";
	o << "getelementptr " + type.get_name() + ", " 
	  + op1.get_typename() + " " + op1.get_name() + ", "
	  + op2.get_typename() + " " + op2.get_name() + "\n";
}
operand ValuePrinter::getelementptr(op_type type, operand op1, operand op2, op_type result_type) {
	operand result = make_fresh_operand(result_type);
	getelementptr(*stream, type, op1, op2, result);
	return result;
}

/* getelementptr instruction
 * Format: getelementptr type, op1_type op1_name, op2_type op2_name, op3_type op3_name, op4_type op4_name
 * Note: This instruction can take a different number of operands but you'll only need this one
 * in PA4.
 */
void ValuePrinter::getelementptr(ostream &o, op_type type, operand op1, operand op2, operand op3, operand op4, operand result) {
	check_ostream(o);
	o << "\t";
	if (result.get_type().get_id() != VOID)
		o << result.get_name() << " = ";
	o << "getelementptr " + type.get_name() + ", " 
	  + op1.get_typename() + " " + op1.get_name() + ", "
	  + op2.get_typename() + " " + op2.get_name() + ", " 
	  + op3.get_typename() + " " + op3.get_name() + ", " 
	  + op4.get_typename() + " " + op4.get_name() + "\n";
}
operand ValuePrinter::getelementptr(op_type type, operand op1, operand op2, operand op3, operand op4, op_type result_type) {
	operand result = make_fresh_operand(result_type);
	getelementptr(*stream, type, op1, op2, op3, op4, result);
	return result;
}

void ValuePrinter::getelementptr(ostream &o, op_type type, operand op1, operand op2, operand op3, operand op4, operand op5, operand result) {
	check_ostream(o);
	o << "\t";
	if (result.get_type().get_id() != VOID)
		o << result.get_name() << " = ";
	o << "getelementptr " + type.get_name() + ", " 
	  + op1.get_typename() + " " + op1.get_name() + ", "
	  + op2.get_typename() + " " + op2.get_name() + ", " 
	  + op3.get_typename() + " " + op3.get_name() + ", " 
	  + op4.get_typename() + " " + op4.get_name() + ", " 
	  + op5.get_typename() + " " + op5.get_name() + "\n";
}
operand ValuePrinter::getelementptr(op_type type, operand op1, operand op2, operand op3, operand op4, operand op5, op_type result_type) {
	operand result = make_fresh_operand(result_type);
	getelementptr(*stream, type, op1, op2, op3, op4, op5, result);
	return result;
}

/* getelementptr that takes a variable number of operands */
void ValuePrinter::getelementptr(ostream &o, op_type type, vector<operand> op, operand result) {
	check_ostream(o);
	o << "\t";
	if (result.get_type().get_id() != VOID)
		o << result.get_name() << " = ";
	o << "getelementptr " + type.get_name() + ", ";
	assert (op.size() > 0 && "no operands given to getelementptr");
	for (unsigned i = 0; i < op.size(); ++i) {
		o << op[i].get_typename() + " " + op[i].get_name();
		if (i + 1 < op.size())
			o << ", ";
	}
	o << "\n";
}
operand ValuePrinter::getelementptr(op_type type, vector<operand> op, op_type result_type) {
	operand result = make_fresh_operand(result_type);
	getelementptr(*stream, type, op, result);
	return result;
}

/* simple version of getelemenptr suitable for embedding */
static void embed_getelementptr (ostream &o, op_type type, operand op1, operand op2, operand op3) {
	o << "getelementptr (" + type.get_name() + ", "
	  + op1.get_typename() + "* " + op1.get_name() + ", "
	  + op2.get_typename() + " " + op2.get_name() + ", "
	  + op3.get_typename() + " " + op3.get_name() + ")";
}

/* select instruction
 * Format: result = select op1_type op1_name, op2_type op2_name, op3_type op3_name
 */
void ValuePrinter::select(ostream &o, operand op1, operand op2, operand op3, operand result) {
	check_ostream(o);
	o << "\t" + result.get_name() + " = select "
	  + op1.get_typename() + " " + op1.get_name() + ", " 
	  + op2.get_typename() + " " + op2.get_name() + ", "
	  + op3.get_typename() + " " + op3.get_name() + "\n";
}
operand ValuePrinter::select(operand op1, operand op2, operand op3) {
	operand result = make_fresh_operand(op2.get_type());
	select(*stream, op1, op2, op3, result);
	return result;
}

/* Conditional branch instruction
 * Format: br op_type op_value, label %true_label, label %false_label
 */
void ValuePrinter::branch_cond(ostream &o, operand op, label label_true, label label_false) {
	check_ostream(o);
	o << "\tbr " + op.get_typename() + " " + op.get_name() + ", label %" + label_true 
	  + ", label %" + label_false + "\n";
}
void ValuePrinter::branch_cond(operand op, label label_true, label label_false) {
	branch_cond(*stream, op, label_true, label_false);
}

/* Unconditional branch instruction
 * Format: br label %label_name
 */
void ValuePrinter::branch_uncond(ostream &o, label l) {
	check_ostream(o);
	o << "\tbr label %" + l + "\n";
}
void ValuePrinter::branch_uncond(label l) {
	branch_uncond(*stream, l);
}

/* icmp instruction
 * Format: result_name = icmp icmp_val type op1_name, op2_name
 */
void ValuePrinter::icmp(ostream &o, icmp_val v, operand op1, operand op2, operand result) {
	check_ostream(o);
	o << "\t" + result.get_name() + " = icmp ";
	switch(v) {
		case EQ:
			o << "eq";
			break;
		case LT:
			o << "slt";
			break;
		case LE:
			o << "sle";
			break;
		case GT:
			o << "sgt";
			break;
		case GE:
			o << "sge";
			break;
		case NE:
			o << "ne";
			break;	
		default:
			assert(0 && "Bad icmp opcode");
	}
	o << " " + op1.get_typename() + " " + op1.get_name() + ", " + op2.get_name() + "\n";
}
operand ValuePrinter::icmp(icmp_val v, operand op1, operand op2) {
	operand result = make_fresh_operand(op_type(INT1));
	icmp(*stream, v, op1, op2, result);
	return result;
}

/* Function call instruction
 * Format: call result_return_type arg_types @function_name(arg1_type arg1_name, ...)
 */
void ValuePrinter::call(ostream &o, vector<op_type> arg_types, string fn_name, 
			bool is_global, vector<operand> args, operand result_op) {
	check_ostream(o);
	o << "\t";	
	if (result_op.get_type().get_id() != VOID)
		o << result_op.get_name() << " = ";
	o << "call " + result_op.get_typename();
	if (arg_types.size() > 0) {
		o << "(";
		for (unsigned i = 0; i < arg_types.size(); ++i)
			o << arg_types[i].get_name() << (i + 1 < arg_types.size() ? ", " : "");
		o << " )";
	}
	o << (is_global?" @":" %") + fn_name + "( ";
	for (unsigned i = 0; i < args.size(); ++i)
		o << args[i].get_typename() + " " + args[i].get_name() + (i + 1 < args.size() ? ", " : "");
	o << " )\n";
}
operand ValuePrinter::call(vector<op_type> arg_types, op_type result_type,
      string fn_name, bool is_global, vector<operand> args) {
	operand result = make_fresh_operand(result_type);
	call(*stream, arg_types, fn_name, is_global, args, result);
	return result;
}

/* Function return instruction
 * Format: ret return_type return_val
 */
void ValuePrinter::ret(ostream &o, operand op) {
	check_ostream(o);
	o << "\tret ";
	if (op.get_type().get_id() != VOID)
		o << op.get_typename() + " " + op.get_name() + "\n";
	else 
		o << "void\n";
}
void ValuePrinter::ret(operand op) {
	ret(*stream, op);
}

/* Bitcast instruction
 * Format: result = bitcast op_type op_name to new_type
 */
void ValuePrinter::bitcast(ostream &o, operand op, op_type new_type, operand result) {
	check_ostream(o);
	o << "\t" << result.get_name() << " = bitcast " << op.get_typename() << " " << op.get_name()
	  << " to " << new_type.get_name() << "\n";
}
operand ValuePrinter::bitcast(operand op, op_type new_type) {
	operand result = make_fresh_operand(new_type);
	bitcast(*stream, op, new_type, result);
	return result;
}

/* Ptrtoint instruction
 * Format: result = ptrtoint op_type op_name to new_type
 */
void ValuePrinter::ptrtoint(ostream &o, operand op, op_type new_type, operand result) {
	check_ostream(o);
	o << "\t" << result.get_name() << " = ptrtoint " << op.get_typename() << " " << op.get_name()
	  << " to " << new_type.get_name() << "\n";
}
operand ValuePrinter::ptrtoint(operand op, op_type new_type) {
	operand result = make_fresh_operand(new_type);
	ptrtoint(*stream, op, new_type, result);
	return result;
}
