/*
 * This file provides the runtime library for cool. It implements
 * the cool classes in C.  Feel free to change it to match the structure
 * of your code generator.
 */

#include <stdbool.h>
#include <stdint.h>

#define VTABLE_META  \
	int32_t id;      \
	int32_t address; \
	int8_t *name;

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
struct Object
{
	/*
	%Object = type {
		%Object_vtable*
	}
	*/

	Object_vtable *vtblptr;
};

struct Int
{
	/*
	@str.Int = internal constant [4 x i8] c"Int\00"
	%Int = type {
		%Int_vtable*,
		i32
	}
	*/

	Int_vtable *vtblptr;
	int32_t val;
};

struct Bool
{
	/*
	@str.Bool = internal constant [5 x i8] c"Bool\00"
	%Bool = type {
		%Bool_vtable*,
		i1
	}
	*/

	Bool_vtable *vtblptr;
	bool val;
};

struct String
{
	/*
	@str.String = internal constant [7 x i8] c"String\00"
	%String = type {
		%String_vtable*,
		i8*
	}
	*/

	String_vtable *vtblptr;
	int8_t *val;
};

struct IO
{
	/*
	@str.IO = internal constant [3 x i8] c"IO\00"
	%IO = type {
		%IO_vtable*
	}
	*/

	IO_vtable *vtblptr;
};

/* vtable type definitions */
struct Object_vtable
{
	/*
	%Object_vtable = type {
		i32,
		i32,
		i8*,
		%Object* () *,
		%Object* (%Object*) *,
		%String* (%Object*) *,
		%Object* (%Object*) *
	}

	@Object_vtable_prototype = constant %Object_vtable {
		i32 0,
		i32 ptrtoint (%Object* getelementptr (%Object, %Object* null, i32 1) to i32),
		i8* getelementptr ([7 x i8], [7 x i8]* @str.Object, i32 0, i32 0),
		%Object* () * @Object_new,
		%Object* (%Object*) * @Object_abort,
		%String* (%Object*) * @Object_type_name,
		%Object* (%Object*) * @Object_copy
	}
	*/

	VTABLE_META;
	Object *(*Object_new)(void);
	Object *(*Object_abort)(Object *);
	String *(*Object_type_name)(Object *);
	Object *(*Object_copy)(Object *);
};

struct IO_vtable
{
	/*
	%IO_vtable = type {
		i32,
		i32,
		i8*,
		%IO* () *,
		%Object* (%IO*) *,
		%String* (%IO*) *,
		%IO* (%IO*) *,
		%IO* (%IO*,%String*) *,
		%IO* (%IO*,i32) *,
		%String* (%IO*) *,
		i32 (%IO*) *
	}

	@IO_vtable_prototype = constant %IO_vtable {
		i32 4,
		i32 ptrtoint (%IO* getelementptr (%IO, %IO* null, i32 1) to i32),
		i8* getelementptr ([3 x i8], [3 x i8]* @str.IO, i32 0, i32 0),
		%IO* () * @IO_new,
		%Object* (%IO*) * bitcast (%Object* (%Object*) * @Object_abort to %Object* (%IO*) *),
		%String* (%IO*) * bitcast (%String* (%Object*) * @Object_type_name to %String* (%IO*) *),
		%IO* (%IO*) * bitcast (%Object* (%Object*) * @Object_copy to %IO* (%IO*) *),
		%IO* (%IO*,%String*) * @IO_out_string,
		%IO* (%IO*,i32) * @IO_out_int,
		%String* (%IO*) * @IO_in_string,
		i32 (%IO*) * @IO_in_int
	}
	*/

	VTABLE_META;
	IO *(*IO_new)(void);
	Object *(*Object_abort)(IO *);
	String *(*Object_type_name)(IO *);
	IO *(*Object_copy)(IO *);
	IO *(*IO_out_string)(IO *, String *);
	IO *(*IO_out_int)(IO *, int32_t *);
	String *(*IO_in_string)(IO *);
	int32_t *(*IO_in_int)(IO *);
};

struct Int_vtable
{
	/*
	%Int_vtable = type {
		i32,
		i32,
		i8*,
		%Int* () *,
		%Object* (%Int*) *,
		%String* (%Int*) *,
		%Int* (%Int*) *
	}
	@Int_vtable_prototype = constant %Int_vtable {
		i32 1,
		i32 ptrtoint (%Int* getelementptr (%Int, %Int* null, i32 1) to i32),
		i8* getelementptr ([4 x i8], [4 x i8]* @str.Int, i32 0, i32 0),
		%Int* () * @Int_new,
		%Object* (%Int*) * bitcast (%Object* (%Object*) * @Object_abort to %Object* (%Int*) *),
		%String* (%Int*) * bitcast (%String* (%Object*) * @Object_type_name to %String* (%Int*) *),
		%Int* (%Int*) * bitcast (%Object* (%Object*) * @Object_copy to %Int* (%Int*) *)
	}
	*/
	VTABLE_META;
	Int *(*Int_new)(void);
	Object *(*Object_abort)(Int *);
	String *(*Object_type_name)(Int *);
	Int *(*Object_copy)(Int *);
};

struct Bool_vtable
{
	/*
	%Bool_vtable = type {
		i32,
		i32,
		i8*,
		%Bool* () *,
		%Object* (%Bool*) *,
		%String* (%Bool*) *,
		%Bool* (%Bool*) *
	}
	@Bool_vtable_prototype = constant %Bool_vtable {
		i32 2,
		i32 ptrtoint (%Bool* getelementptr (%Bool, %Bool* null, i32 1) to i32),
		i8* getelementptr ([5 x i8], [5 x i8]* @str.Bool, i32 0, i32 0),
		%Bool* () * @Bool_new,
		%Object* (%Bool*) * bitcast (%Object* (%Object*) * @Object_abort to %Object* (%Bool*) *),
		%String* (%Bool*) * bitcast (%String* (%Object*) * @Object_type_name to %String* (%Bool*) *),
		%Bool* (%Bool*) * bitcast (%Object* (%Object*) * @Object_copy to %Bool* (%Bool*) *)
	}
	*/
	VTABLE_META;
	Bool *(*Bool_new)(void);
	Object *(*Object_abort)(Bool *);
	String *(*Object_type_name)(Bool *);
	Bool *(*Object_copy)(Bool *);
};

struct String_vtable
{
	/*
	%String_vtable = type {
		i32,
		i32,
		i8*,
		%String* () *,
		%Object* (%String*) *,
		%String* (%String*) *,
		%String* (%String*) *,
		i32 (%String*) *,
		%String* (%String*,%String*) *,
		%String* (%String*,i32,i32) *
	}
	@String_vtable_prototype = constant %String_vtable {
		i32 3,
		i32 ptrtoint (%String* getelementptr (%String, %String* null, i32 1) to i32),
		i8* getelementptr ([7 x i8], [7 x i8]* @str.String, i32 0, i32 0),
		%String* () * @String_new,
		%Object* (%String*) * bitcast (%Object* (%Object*) * @Object_abort to %Object* (%String*) *),
		%String* (%String*) * bitcast (%String* (%Object*) * @Object_type_name to %String* (%String*) *),
		%String* (%String*) * bitcast (%Object* (%Object*) * @Object_copy to %String* (%String*) *),
		i32 (%String*) * @String_length,
		%String* (%String*,%String*) * @String_concat,
		%String* (%String*,i32,i32) * @String_substr
	}
	*/
	VTABLE_META;
	String *(*String_new)(void);
	Object *(*Object_abort)(String *);
	String *(*Object_type_name)(String *);
	String *(*Object_copy)(String *);
	int32_t (*String_length)(String *);
	String *(*String_concat)(String *, String *);
	String *(*String_substr)(String *, int32_t, int32_t);
};

/* methods in class Object */
Object *Object_abort(Object *self);
const String *Object_type_name(Object *self);
/* ADD CODE HERE */

/* methods in class IO */
IO *IO_new(void);
void IO_init(IO *self);
IO *IO_out_string(IO *self, String *x);
IO *IO_out_int(IO *self, Int *x);
String *IO_in_string(IO *self);
Int *IO_in_int(IO *self);

/* methods in class Int */
Int *Int_new(void);

/* methods in class Bool */
Bool *Bool_new(void);

/* methods in class String */
String *String_new(void);
int32_t String_length(String *self);
String *String_concat(String *self, String *s);
String *String_substr(String *self, int32_t i, int32_t l);
