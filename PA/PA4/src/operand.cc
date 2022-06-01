#include "operand.h"

op_type::op_type(op_type_id i) : id(i){
	switch (id) {
		case EMPTY:
			name = "";
			break;
		case VOID:
			name = "void";
			break;
		case INT1:
			name = "i1";
			break;
		case INT8:
			name = "i8";
			break;
		case INT32:
			name = "i32";
			break;
		case INT1_PTR:
			name = "i1*";
			break;
		case INT8_PTR:
			name = "i8*";
			break;
		case INT32_PTR:
			name = "i32*";
			break;
		case INT1_PPTR:
			name = "i1**";
			break;
		case INT8_PPTR:
			name = "i8**";
			break;
		case INT32_PPTR:
			name = "i32**";
			break;
		case VAR_ARG:
			name = "...";
			break;
		case OBJ:
		case OBJ_PTR:
		case OBJ_PPTR:
			break;
		default:
			assert(0 && "Variable type not implemented");
			
	}
}

op_type::op_type(string n, int ptr_level) {
	name = "%" + n;
	id = OBJ;
	// Pointer to an object
	if (ptr_level == 1) {
		name += "*";
		id = OBJ_PTR;
	}
	// Pointer to a pointer to an object;
	if (ptr_level == 2) {
		id = OBJ_PPTR;
		name += "**";
	}

	if (ptr_level > 2 || ptr_level < 0)
		assert(0 && "Invalid pointer level");
}	

/* Get a pointer type of the current type
 */
op_type op_type::get_ptr_type() {
	op_type_id ptr_id;
	switch (id) {
		case INT1:
			ptr_id = INT1_PTR;
			break;
		case INT8:
			ptr_id = INT8_PTR;
			break;
		case INT32:
			ptr_id = INT32_PTR;
			break;
		case INT1_PTR:
			ptr_id = INT1_PPTR;
			break;
		case INT8_PTR:
			ptr_id = INT8_PPTR;
			break;
		case INT32_PTR:
			ptr_id = INT32_PPTR;
			break;
		case OBJ:
			ptr_id = OBJ_PTR;
			break;
		case OBJ_PTR:
			ptr_id = OBJ_PPTR;
			break;
		default:
			assert(0 && "get_ptr_type(): Type unsupported");
	}
	op_type ptr_type;
	if (ptr_id == OBJ_PTR || ptr_id == OBJ_PPTR) {
		op_type new_type(string(name.c_str() + 1), 1);
		new_type.set_id(ptr_id);
		ptr_type = new_type;
	}
	else {
		op_type new_type(ptr_id);
		ptr_type = new_type;
	}
	return ptr_type;
}

/* The inverse of get_ptr_type()
 */
op_type op_type::get_deref_type() {
	op_type_id deref_id;
	switch(id) {
		case INT1_PTR:
			deref_id = INT1;
			break;
		case INT8_PTR:
			deref_id = INT8;
			break;
		case INT32_PTR:
			deref_id = INT32;
			break;
		case INT1_PPTR:
			deref_id = INT1_PTR;
			break;
		case INT8_PPTR:
			deref_id = INT8_PTR;
			break;
		case INT32_PPTR:
			deref_id = INT32_PTR;
			break;
		case OBJ_PTR:
			deref_id = OBJ;
			break;
		case OBJ_PPTR:
			deref_id = OBJ_PTR;
			break;
		default:
			assert(0 && "get_deref_type(): Cannot get type after dereferencing");
	}
	op_type deref_type;
	if (deref_id == OBJ || deref_id == OBJ_PTR) {
		//char *new_name = name.c_str() + 1;
		//new_name[strlen(new_name) - 1] = '\0';
		string new_name = name.substr(1, name.length() - 2);
		op_type new_type(new_name);
		new_type.set_id(deref_id);
		deref_type = new_type;
	}
	else {
		op_type new_type(deref_id);
		deref_type = new_type;
	}
	return deref_type;
}

/* Array type
 * Format: [size x type]
 */
op_arr_type::op_arr_type(op_type_id i, int s) : op_type(i){
	int num_width = 1;
	string type_name = name, size_str = " ";
	size = s;
	for (int j = s; j > 9; j %= 10)
		size_str += " ";
	if (is_ptr())
		type_name.erase(type_name.length() - 1);
	name = "[" + itoa(s, size_str) + " x " + type_name + "]";
	if (is_ptr())
		name += "*";
};

/* Function and Function pointer types */
op_func_type::op_func_type(op_type res_type, vector<op_type> arg_types)
  : op_type(EMPTY), res(res_type), args(arg_types)
{
	name = string(res_type.get_name() + " (");
	if (arg_types.size() == 0)
		name.append(") *");
	else {
		unsigned i;
		for (i = 0; i < arg_types.size()-1; i++)
			name.append(arg_types[i].get_name() + ",");
		name.append(arg_types[i].get_name() + ") *");
	}
}

op_type op_func_type::get_ptr_type() { return op_func_ptr_type(res, args); }

op_type op_func_type::get_deref_type() {
	assert(0 && "get_deref_type(): Cannot get type after dereferencing");
}

op_func_ptr_type::op_func_ptr_type(op_type res_type, vector<op_type> arg_types)
  : op_type(EMPTY), res(res_type), args(arg_types)
{
	op_func_type func_type(res_type, arg_types);
	name = string(func_type.get_name() + "*");
}

op_type op_func_ptr_type::get_ptr_type() {
	assert(0 && "get_ptr_type(): Type unsupported");
}

op_type op_func_ptr_type::get_deref_type() { return op_func_type(res, args); }

