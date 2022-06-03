#include "coolrt.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <sys/types.h>

/* This file provides the runtime library for cool. It implements
   the functions of the cool classes in C
   */

/* Class name strings */
const char Object_string[] = "Object";
const char String_string[] = "String";
const char Int_string[] = "Int";
const char Bool_string[] = "Bool";
const char IO_string[] = "IO";

const char default_string[] = "";

/* Class vtable prototypes */
const Object_vtable Object_vtable_prototype = {
    /* ADD CODE HERE */
};

/* ADD CODE HERE FOR MORE VTABLE PROTOTYPES */

/*
// Methods in class object (only some are provided to you)
*/
Object *Object_abort(Object *self)
{
    printf("Abort called from class %s\n",
           !self ? "Unknown" : self->vtblptr->name);
    exit(1);
    return self;
}

const String *Object_type_name(Object *self)
{
    if (self == 0)
    {
        fprintf(stderr, "At __FILE__(line __LINE__): self is NULL\n");
        abort();
    }
    String *s = String_new();
    s->val = self->vtblptr->name;
    return s;
}

Object *Object_new(void)
{
    return malloc(sizeof(Object));
}

Object *Object_copy(Object *self)
{
    Object *object = Object_new();

    memcpy(object->vtblptr, self->vtblptr, sizeof(Object_vtable));

    return object;
}

/*
// Methods in class IO (only some are provided to you)
*/

IO *IO_out_string(IO *self, String *x)
{
    if (self == 0 || x == 0)
    {
        fprintf(stderr, "At __FILE__(line __LINE__): NULL object\n");
        abort();
    }
    printf("%s", x->val);
    return self;
}

IO *IO_out_int(IO *self, Int *x)
{
    if (self == 0 || x == 0)
    {
        fprintf(stderr, "At __FILE__(line __LINE__): NULL object\n");
        abort();
    }
    printf("%d", x->val);
    return self;
}

/*
 * Get one line from stream using get_line(), then discard newline character.
 * Allocate string *in_string_p and store result there.
 * Return number of chars read.
 */
static int get_one_line(char **in_string_p, FILE *stream)
{
    /* Get one line worth of input */
    size_t len = 0;
    ssize_t num_chars_read;
    num_chars_read = getline(in_string_p, &len, stdin);
    if (*in_string_p == 0)
    {
        fprintf(stderr, "At __FILE__(line __LINE__):\n   ");
        fprintf(stderr, "    allocation failed in IO::in_string()\n");
        exit(1);
    }

    /* Discard the newline char, if any.  It may not exist if EOF reached. */
    if (num_chars_read > 0 && (*in_string_p)[num_chars_read - 1] == '\n')
    {
        (*in_string_p)[num_chars_read - 1] = '\0';
        --len;
    }

    return len;
}

/*
 * The method IO::in_string(): String reads a string from
 * the standard input, up to but not including a newline character.
 */
String *IO_in_string(IO *self)
{
    if (self == 0)
    {
        fprintf(stderr, "At __FILE__(line __LINE__): self is NULL\n");
        abort();
    }

    /* Get one line worth of input with the newline, if any, discarded */
    char *in_string = 0;
    ssize_t len = get_one_line(&in_string, stdin);
    assert(in_string);

    /* We can take advantage of knowing the internal layout of String objects */
    String *str = String_new();
    str->val = in_string;
    return str;
}

/*
 * The method IO::in_int(): Int reads a single integer, which may be preceded
 * by whitespace.
 * Any characters following the integer, up to and including the next newline,
 * are discarded by in_int.
 */
Int *IO_in_int(IO *self)
{
    if (self == 0)
    {
        fprintf(stderr, "At __FILE__(line __LINE__): self is NULL\n");
        abort();
    }

    /* Get one line worth of input with the newline, if any, discarded */
    char *in_string = 0;
    ssize_t len = get_one_line(&in_string, stdin);
    assert(in_string);

    /* Now extract initial int and ignore the rest of the line */
    Int *x = Int_new();
    int num_ints = 0;
    if (len)
        num_ints = sscanf(in_string, " %d", &(x->val)); /* Discards initial spaces*/

    /* If no text found, abort. */
    if (num_ints == 0)
    {
        fprintf(stderr, "At __FILE__(line __LINE__):\n   ");
        fprintf(stderr, "    Invalid integer on input in IO::in_int()");
        Object_abort((Object *)self);
    }
    return x;
}

IO *IO_new(void)
{
    return malloc(sizeof(IO));
}

/* methods in class Int */

Int *Int_new(void)
{
    return malloc(sizeof(Int));
}

void Int_init(Int *self, int32_t val)
{
    self->val = val;
}

/* methods in class Bool */

Bool *Bool_new(void)
{
    return malloc(sizeof(Bool));
}

void Bool_init(Bool *self, bool val)
{
    self->val = val;
}

/* methods in class String */

String *String_new(void)
{
    return malloc(sizeof(String));
}

int32_t String_length(String *self)
{
    return strlen(self->val);
}

String *String_concat(String *self, String *s)
{
    String *result = String_new();
    char *val = malloc((String_length(self) + String_length(s) + 1) * sizeof(int8_t));

    val = "";

    strcat(val, self->val);
    strcat(val, s->val);

    result->val = val;
    result->vtblptr = self->vtblptr;

    return result;
}

String *String_substr(String *self, int32_t i, int32_t l)
{
    if (String_length(self) < (i + l))
    {
        error("Index out of range");
    }

    String *result = String_new();
    char *val = malloc((l + 1) * sizeof(int8_t));

    strncpy(val, &self->val[i], l);
    val[l] = '\0';

    result->val = val;
    result->vtblptr = self->vtblptr;

    return result;
}
