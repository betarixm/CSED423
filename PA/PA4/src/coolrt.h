/* 
 * This file provides the runtime library for cool. It implements
 * the cool classes in C.  Feel free to change it to match the structure
 * of your code generator.
*/

#include <stdbool.h>

typedef struct Object Object;
typedef struct String String;
typedef struct IO IO;
typedef struct Int Int;
typedef struct Bool Bool;

typedef struct Object_vtable Object_vtable;
typedef struct String_vtable String_vtable;
typedef struct IO_vtable IO_vtable;
typedef struct Int_vtable Int_vtable;
typedef struct Bool_vtable Bool_vtable;

/* class type definitions */
struct Object {
	/* ADD CODE HERE */
};

struct Int {
	/* ADD CODE HERE */
};

struct Bool {
	/* ADD CODE HERE */
};

struct String {
	/* ADD CODE HERE */
};

struct IO {
	/* ADD CODE HERE */
};


/* vtable type definitions */
struct Object_vtable {
	/* ADD CODE HERE */
};

struct IO_vtable {
	/* ADD CODE HERE */
};

struct Int_vtable {
	/* ADD CODE HERE */
};

struct Bool_vtable {
	/* ADD CODE HERE */
};
   
struct String_vtable {
	/* ADD CODE HERE */
};

/* methods in class Object */
Object* Object_abort(Object *self);
const String* Object_type_name(Object *self);
	/* ADD CODE HERE */

/* methods in class IO */
IO* IO_new(void);
void IO_init(IO *self);
IO* IO_out_string(IO *self, String *x);
IO* IO_out_int(IO *self, Int *x);
String* IO_in_string(IO *self);
Int* IO_in_int(IO *self);

/* methods in class Int */
	/* ADD CODE HERE */


/* methods in class Bool */
	/* ADD CODE HERE */


/* methods in class String */
	/* ADD CODE HERE */
