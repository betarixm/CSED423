/* ValuePrinter
 * Contains methods to print the instructions in the format that LLVM supports. You are only 
 * given the instructions that you'll need to use in creating the code generator in MP2.1.
 * For a full list of LLVM instructions, visit http://llvm.org/docs/LangRef.html
 */

#ifndef __VALUE_PRINTER_H
#define __VALUE_PRINTER_H

#include "operand.h"
#include <assert.h>
#include <vector>

typedef string label;
/* Values acceptable by the icmp instruction */
typedef enum {EQ, NE, LT, LE, GT, GE} icmp_val;

class ValuePrinter {
	private:
		void bin_inst(ostream &o, string inst_name, operand op1, operand op2, operand result);
		operand bin_inst(string inst_name, operand op1, operand op2);
		// if an ostream is explicitly supplied, check it is compatible with constructor arguments.
		void check_ostream(ostream& supplied) { assert (!stream || (stream == &supplied)); }
		// if called without an explicitly supplied ostream,
		// check that one was provided in the constructor.
		void check_ostream() { assert(stream); }
		ostream* stream;
	public:
		/* default constructor. */
		ValuePrinter() : stream(NULL) {}
		/* constructor taking ostream.
		   If any methods taking and explicit ostream are called, the supplied ostream must match. */
		ValuePrinter(ostream& o) : stream(&o) {}

		/* Global constant initialization */
		void init_constant(ostream &o, string name, const_value op);
		void init_constant(string name, const_value op);
		/* External constant declaration */
		void init_ext_constant(ostream &o, string name, op_type type);
		void init_ext_constant(string name, op_type type);
		
		/* Function definitions and declarations */
		void declare(ostream &o, op_type ret_type, string name, vector<op_type> args);
		void declare(op_type ret_type, string name, vector<op_type> args);
		void define(ostream &o, op_type ret_type, string name, vector<operand> args);
		void define(op_type ret_type, string name, vector<operand> args);
		void end_define(ostream &o) { check_ostream(o); o << "}\n\n"; }
		void end_define() { *stream << "}\n\n"; }

		/* Type definition */
		void type_define(ostream &o, string class_name, vector<op_type> attributes);
		void type_define(string class_name, vector<op_type> attributes);

		void type_alias_define(ostream &o, string alias_name, op_type type);
		void type_alias_define(string alias_name, op_type type);

		/* Structure constant definition */
		void init_struct_constant(ostream &o, operand constant,
			vector<op_type> field_types, vector<const_value> init_values);
		void init_struct_constant(operand constant,
			vector<op_type> field_types, vector<const_value> init_values);

	/* Print a label */
	void begin_block(string label);

		/* Instruction Output methods are duplicated, once with the old signature taking
		   an ostream and the result, and a new signature which does not
		   take an ostream, and usually produces the result operand itself. */
		/* Binary operations */
		void add(ostream &o, operand op1, operand op2, operand result);	
		void sub(ostream &o, operand op1, operand op2, operand result);	
		void mul(ostream &o, operand op1, operand op2, operand result);	
		void div(ostream &o, operand op1, operand op2, operand result);
		void xor_in(ostream &o, operand op1, operand op2, operand result);

		operand add(operand op1, operand op2);	
		operand sub(operand op1, operand op2);	
		operand mul(operand op1, operand op2);	
		operand div(operand op1, operand op2);
		operand xor_in(operand op1, operand op2);

		/* Memory access instructions */
		void malloc_mem(ostream &o, int size, operand result);
		void malloc_mem(ostream &o, operand size, operand result);
		void alloca_mem(ostream &o, op_type type, operand op2);
		void load(ostream &o, op_type type, operand op, operand op2);
		void store(ostream &o, operand op, operand op2);
		void getelementptr(ostream &o, op_type type, operand op1, operand op2, operand result);
		void getelementptr(ostream &o, op_type type, operand op1, operand op2, operand op3, operand result);
		void getelementptr(ostream &o, op_type type, operand op1, operand op2, operand op3, operand op4, operand result);
		void getelementptr(ostream &o, op_type type, operand op1, operand op2, operand op3, operand op4, operand op5, operand result);
		void getelementptr(ostream &o, op_type type, vector<operand> op, operand result);

		operand malloc_mem(int size);
		operand malloc_mem(operand size);
		operand alloca_mem(op_type type);
		operand load(op_type type, operand op);

		/* store does not produce a result */
		void store(operand op, operand op2);

		/* getelementptr continues to requre an argument for the result type,
		   becuase it is difficult to compute. */
		operand getelementptr(op_type type, operand op1, operand op2, op_type result_type);
		operand getelementptr(op_type type, operand op1, operand op2, operand op3, op_type result_type);
		operand getelementptr(op_type type, operand op1, operand op2, operand op3, operand op4, op_type result_type);
		operand getelementptr(op_type type, operand op1, operand op2, operand op3, operand op4, operand op5, op_type result_type);
		operand getelementptr(op_type type, vector<operand> op, op_type result_type);

		/* Terminator instructions */
		void branch_cond(ostream &o, operand op, label label_true, label label_false);
		void branch_uncond(ostream &o, string label);
		void ret(ostream &o, operand op);
		void unreachable(ostream &o) { check_ostream(o); o << "\tunreachable\n"; }

		void branch_cond(operand op, label label_true, label label_false);
		void branch_uncond(string label);
		void ret(operand op);
		void unreachable() { unreachable(*stream); }

		/* Other operations */		
		void select(ostream &o, operand op1, operand op2, operand op3, operand result);
		void icmp(ostream &o, icmp_val v, operand op1, operand op2, operand result);
		void call(ostream &o, vector<op_type> arg_types, string fn_name,
			bool is_global, vector<operand> args, operand result);
		void bitcast(ostream &o, operand op, op_type new_type, operand result);
		void ptrtoint(ostream &o, operand op, op_type new_type, operand result);

		operand select(operand op1, operand op2, operand op3);
		operand icmp(icmp_val v, operand op1, operand op2);
		operand call(vector<op_type> arg_types, op_type result_type,
			string fn_name, bool is_global, vector<operand> args);
		operand bitcast(operand op, op_type new_type);
		operand ptrtoint(operand op, op_type new_type);
};

#endif
