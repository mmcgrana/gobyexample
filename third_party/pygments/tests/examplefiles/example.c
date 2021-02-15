#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include "codegen.h"
#include "symboltable.h"
#include "stringbuffer.h"

extern void yyerror(char* msg);

static stringBuffer* staticVariableBuffer;
static stringBuffer* classInitBuffer;
static stringBuffer* currentMethodBuffer;
static stringBuffer* finishedMethodsBuffer;
static stringBuffer* mainBuffer;

static int currentMethodBufferIndex;
static int currentMethodStackSize;
static int currentMethodStackSizeMax;
static int currentMethodNumberOfLocals;

static int classInitBufferIndex;
static int classInitStackSize;
static int classInitStackSizeMax;

static int labelCounter = 0;
static int global       = 1;

char tempString[MAX_LENGTH_OF_COMMAND];

extern char* className;        /* from minako-syntax.y */

/* forward declarations */
static void increaseStackby(int stackdiff);
char convertType(int type);

void codegenInit() {
	staticVariableBuffer  = newStringBuffer();
	classInitBuffer       = newStringBuffer();
	currentMethodBuffer   = 0;
	finishedMethodsBuffer = newStringBuffer();
	mainBuffer            = newStringBuffer();

	stringBufferAppend(mainBuffer, "; ------- Header --------------------------------------------"); 
	sprintf(tempString, ".class  public synchronized %s", className);
	stringBufferAppend(mainBuffer, tempString);
	stringBufferAppend(mainBuffer, ".super  java/lang/Object");
	stringBufferAppend(mainBuffer, "; -----------------------------------------------------------");
	stringBufferAppend(mainBuffer, "");
	
	stringBufferAppend(finishedMethodsBuffer, "; ------- Constructor ---------------------------------------");
	stringBufferAppend(finishedMethodsBuffer, ".method public <init>()V");
	stringBufferAppend(finishedMethodsBuffer, "\t.limit stack 1");
	stringBufferAppend(finishedMethodsBuffer, "\t.limit locals 1");
	stringBufferAppend(finishedMethodsBuffer, "\taload_0");
	stringBufferAppend(finishedMethodsBuffer, "\tinvokenonvirtual java/lang/Object/<init>()V");
	stringBufferAppend(finishedMethodsBuffer, "\treturn");
	stringBufferAppend(finishedMethodsBuffer, ".end method");
	stringBufferAppend(finishedMethodsBuffer, "; -----------------------------------------------------------");
	stringBufferAppend(finishedMethodsBuffer, "");

	stringBufferAppend(staticVariableBuffer, "; ------- Class Variables -----------------------------------");

	stringBufferAppend(classInitBuffer, "; ------- Class Initializer ---------------------------------");
	stringBufferAppend(classInitBuffer, ".method static <clinit>()V");
	classInitBufferIndex = classInitBuffer->numberOfNextElement;
	stringBufferAppend(classInitBuffer, "\t.limit locals 0");

}

void codegenAppendCommand(char* cmd, int stackdiff) {
	char tempString[MAX_LENGTH_OF_COMMAND];
	sprintf(tempString, "\t%s", cmd);
	if (global) stringBufferAppend(classInitBuffer, tempString);
	else stringBufferAppend(currentMethodBuffer, tempString);
	increaseStackby(stackdiff);
}

void codegenInsertCommand(int address, char* cmd, int stackdiff) {
	char tempString[MAX_LENGTH_OF_COMMAND];
	sprintf(tempString, "\t%s", cmd);
	if (global) stringBufferInsert(classInitBuffer, address, tempString);
	else stringBufferInsert(currentMethodBuffer, address, tempString);
	increaseStackby(stackdiff);
}

void codegenAppendLabel(int label) {
	char tempString[MAX_LENGTH_OF_COMMAND];
	sprintf(tempString, "Label%d:", label);
	if (global) stringBufferAppend(classInitBuffer, tempString);
	else stringBufferAppend(currentMethodBuffer, tempString);
}

void codegenAddVariable(char* name, int type) {
	/*fprintf(stderr, "add variable %s(%d) global=%d ", name, convertType(type), global);*/
	if (global) {
		if (type == TYPE_INT) sprintf(tempString, ".field static %s %c", name, 'I');
		else if (type == TYPE_FLOAT) sprintf(tempString, ".field static %s %c", name, 'F');
		else if (type == TYPE_BOOLEAN) sprintf(tempString, ".field static %s %c", name, 'Z');
		else yyerror("compiler-intern error in codegenAddGlobalVariable().\n");
		stringBufferAppend(staticVariableBuffer, tempString);
	}
	else {
		currentMethodNumberOfLocals++;
	}
}

int codegenGetNextLabel() {
	return labelCounter++;
}

int codegenGetCurrentAddress() {
	if (global) return classInitBuffer->numberOfNextElement;
	else return currentMethodBuffer->numberOfNextElement;
}

void codegenEnterFunction(symtabEntry* entry) {
	currentMethodBuffer = newStringBuffer();
	currentMethodStackSize = 0;
	currentMethodStackSizeMax = 0;
	labelCounter = 1;
	global = 0;
	
	if (strcmp(entry->name, "main") == 0) {
		if (entry->idtype != TYPE_VOID) yyerror("main has to be void.\n");
		currentMethodNumberOfLocals = 1;
		symtabInsert(strdup("#main-param#"), TYPE_VOID, CLASS_FUNC);
		stringBufferAppend(currentMethodBuffer, "; ------- Methode ---- void main() --------------------------");
		stringBufferAppend(currentMethodBuffer, ".method public static main([Ljava/lang/String;)V");
	}
	else {
		int i;
		currentMethodNumberOfLocals = entry->paramIndex;
		stringBufferAppend(currentMethodBuffer, "; ------- Methode -------------------------------------------");
		sprintf(tempString, ".method public static %s(", entry->name);
		for (i=entry->paramIndex-1; i>=0; i--) {
			int type = entry->params[i]->idtype;
			tempString[strlen(tempString)+1] = 0;
			tempString[strlen(tempString)] = convertType(type);
		}
		tempString[strlen(tempString)+2] = 0;
		tempString[strlen(tempString)+1] = convertType(entry->idtype);
		tempString[strlen(tempString)]   = ')';
		stringBufferAppend(currentMethodBuffer, tempString);
	}
	currentMethodBufferIndex = currentMethodBuffer->numberOfNextElement;
}

void codegenLeaveFunction() {
	global = 1;
	sprintf(tempString, "\t.limit locals %d", currentMethodNumberOfLocals);
	stringBufferInsert(currentMethodBuffer, currentMethodBufferIndex, tempString);
	sprintf(tempString, "\t.limit stack %d", currentMethodStackSizeMax);
	stringBufferInsert(currentMethodBuffer, currentMethodBufferIndex, tempString);
	stringBufferAppend(currentMethodBuffer, "\treturn");
	stringBufferAppend(currentMethodBuffer, ".end method");
	stringBufferAppend(currentMethodBuffer, "; -----------------------------------------------------------");
	stringBufferAppend(currentMethodBuffer, "");

	stringBufferConcatenate(finishedMethodsBuffer, currentMethodBuffer);
}



void codegenFinishCode() {
	stringBufferAppend(staticVariableBuffer, "; -----------------------------------------------------------");
	stringBufferAppend(staticVariableBuffer, "");

	sprintf(tempString, "\t.limit stack %d", classInitStackSizeMax);
	stringBufferInsert(classInitBuffer, classInitBufferIndex, tempString);
	stringBufferAppend(classInitBuffer, "\treturn");
	stringBufferAppend(classInitBuffer, ".end method");
	stringBufferAppend(classInitBuffer, "; -----------------------------------------------------------");
	
	stringBufferConcatenate(mainBuffer, staticVariableBuffer);
	stringBufferConcatenate(mainBuffer, finishedMethodsBuffer);
	stringBufferConcatenate(mainBuffer, classInitBuffer);

	stringBufferPrint(mainBuffer);
}

static void increaseStackby(int stackdiff) {
	if (global) {
		classInitStackSize += stackdiff;
		if (classInitStackSize > classInitStackSizeMax) classInitStackSizeMax = classInitStackSize;
	}
	else {
		currentMethodStackSize += stackdiff;
		if (currentMethodStackSize > currentMethodStackSizeMax) currentMethodStackSizeMax = currentMethodStackSize;
	}
}

char convertType(int type) {
	switch(type) {
		case TYPE_VOID:    return 'V';
		case TYPE_INT:     return 'I';
		case TYPE_FLOAT:   return 'F';
		case TYPE_BOOLEAN: return 'Z';
		default : yyerror("compiler-intern error in convertType().\n");
	}
	return 0; /* to avoid compiler-warning */
}


//#include <stdlib.h>
//#include <stdio.h>

int main() {
	int a = 12, b = 44;
	while (a != b) {
		if (a > b)
			a -= b;
		else
			b -= a;
	}
	printf("%d\n%d", a, 0X0);\
}


/**********************************************************************

  array.c -

  $Author: murphy $
  $Date: 2005-11-05 04:33:55 +0100 (Sa, 05 Nov 2005) $
  created at: Fri Aug  6 09:46:12 JST 1993

  Copyright (C) 1993-2003 Yukihiro Matsumoto
  Copyright (C) 2000  Network Applied Communication Laboratory, Inc.
  Copyright (C) 2000  Information-technology Promotion Agency, Japan

**********************************************************************/

#include "ruby.h"
#include "util.h"
#include "st.h"
#include "node.h"

VALUE rb_cArray, rb_cValues;

static ID id_cmp;

#define ARY_DEFAULT_SIZE 16


void
rb_mem_clear(mem, size)
    register VALUE *mem;
    register long size;
{
    while (size--) {
	*mem++ = Qnil;
    }
}

static inline void
memfill(mem, size, val)
    register VALUE *mem;
    register long size;
    register VALUE val;
{
    while (size--) {
	*mem++ = val;
    }
}

#define ARY_TMPLOCK  FL_USER1

static inline void
rb_ary_modify_check(ary)
    VALUE ary;
{
    if (OBJ_FROZEN(ary)) rb_error_frozen("array");
    if (FL_TEST(ary, ARY_TMPLOCK))
	rb_raise(rb_eRuntimeError, "can't modify array during iteration");
    if (!OBJ_TAINTED(ary) && rb_safe_level() >= 4)
	rb_raise(rb_eSecurityError, "Insecure: can't modify array");
}

static void
rb_ary_modify(ary)
    VALUE ary;
{
    VALUE *ptr;

    rb_ary_modify_check(ary);
    if (FL_TEST(ary, ELTS_SHARED)) {
	ptr = ALLOC_N(VALUE, RARRAY(ary)->len);
	FL_UNSET(ary, ELTS_SHARED);
	RARRAY(ary)->aux.capa = RARRAY(ary)->len;
	MEMCPY(ptr, RARRAY(ary)->ptr, VALUE, RARRAY(ary)->len);
	RARRAY(ary)->ptr = ptr;
    }
}

VALUE
rb_ary_freeze(ary)
    VALUE ary;
{
    return rb_obj_freeze(ary);
}

/*
 *  call-seq:
 *     array.frozen?  -> true or false
 *
 *  Return <code>true</code> if this array is frozen (or temporarily frozen
 *  while being sorted).
 */

static VALUE
rb_ary_frozen_p(ary)
    VALUE ary;
{
    if (OBJ_FROZEN(ary)) return Qtrue;
    if (FL_TEST(ary, ARY_TMPLOCK)) return Qtrue;
    return Qfalse;
}

static VALUE ary_alloc(VALUE);
static VALUE
ary_alloc(klass)
    VALUE klass;
{
    NEWOBJ(ary, struct RArray);
    OBJSETUP(ary, klass, T_ARRAY);

    ary->len = 0;
    ary->ptr = 0;
    ary->aux.capa = 0;

    return (VALUE)ary;
}

static VALUE
ary_new(klass, len)
    VALUE klass;
    long len;
{
    VALUE ary;

    if (len < 0) {
	rb_raise(rb_eArgError, "negative array size (or size too big)");
    }
    if (len > 0 && len * sizeof(VALUE) <= len) {
	rb_raise(rb_eArgError, "array size too big");
    }
    if (len == 0) len++;
    
    ary = ary_alloc(klass);
    RARRAY(ary)->ptr = ALLOC_N(VALUE, len);
    RARRAY(ary)->aux.capa = len;

    return ary;
}

VALUE
rb_ary_new2(len)
    long len;
{
    return ary_new(rb_cArray, len);
}


VALUE
rb_ary_new()
{
    return rb_ary_new2(ARY_DEFAULT_SIZE);
}

#ifdef HAVE_STDARG_PROTOTYPES
#include <stdarg.h>
#define va_init_list(a,b) va_start(a,b)
#else
#include <varargs.h>
#define va_init_list(a,b) va_start(a)
#endif

VALUE
#ifdef HAVE_STDARG_PROTOTYPES
rb_ary_new3(long n, ...)
#else
rb_ary_new3(n, va_alist)
    long n;
    va_dcl
#endif
{
    va_list ar;
    VALUE ary;
    long i;

    ary = rb_ary_new2(n);

    va_init_list(ar, n);
    for (i=0; i<n; i++) {
	RARRAY(ary)->ptr[i] = va_arg(ar, VALUE);
    }
    va_end(ar);

    RARRAY(ary)->len = n;
    return ary;
}

VALUE
rb_ary_new4(n, elts)
    long n;
    const VALUE *elts;
{
    VALUE ary;

    ary = rb_ary_new2(n);
    if (n > 0 && elts) {
	MEMCPY(RARRAY(ary)->ptr, elts, VALUE, n);
    }
    RARRAY(ary)->len = n;

    return ary;
}

VALUE
#ifdef HAVE_STDARG_PROTOTYPES
rb_values_new(long n, ...)
#else
rb_values_new(n, va_alist)
    long n;
    va_dcl
#endif
{
    va_list ar;
    VALUE val;
    long i;

    val = ary_new(rb_cValues, n);
    va_init_list(ar, n);
    for (i=0; i<n; i++) {
	RARRAY(val)->ptr[i] = va_arg(ar, VALUE);
    }
    va_end(ar);
    RARRAY(val)->len = n;

    return val;
}

VALUE
rb_values_new2(n, elts)
    long n;
    const VALUE *elts;
{
    VALUE val;

    val = ary_new(rb_cValues, n);
    if (n > 0 && elts) {
	RARRAY(val)->len = n;
	MEMCPY(RARRAY(val)->ptr, elts, VALUE, n);
    }

    return val;
}

static VALUE
ary_make_shared(ary)
    VALUE ary;
{
    if (!FL_TEST(ary, ELTS_SHARED)) {
	NEWOBJ(shared, struct RArray);
	OBJSETUP(shared, rb_cArray, T_ARRAY);

	shared->len = RARRAY(ary)->len;
	shared->ptr = RARRAY(ary)->ptr;
	shared->aux.capa = RARRAY(ary)->aux.capa;
	RARRAY(ary)->aux.shared = (VALUE)shared;
	FL_SET(ary, ELTS_SHARED);
	OBJ_FREEZE(shared);
	return (VALUE)shared;
    }
    else {
	return RARRAY(ary)->aux.shared;
    }
}

static VALUE
ary_shared_array(klass, ary)
    VALUE klass, ary;
{
    VALUE val = ary_alloc(klass);

    ary_make_shared(ary);
    RARRAY(val)->ptr = RARRAY(ary)->ptr;
    RARRAY(val)->len = RARRAY(ary)->len;
    RARRAY(val)->aux.shared = RARRAY(ary)->aux.shared;
    FL_SET(val, ELTS_SHARED);
    return val;
}

VALUE
rb_values_from_ary(ary)
    VALUE ary;
{
    return ary_shared_array(rb_cValues, ary);
}

VALUE
rb_ary_from_values(val)
    VALUE val;
{
    return ary_shared_array(rb_cArray, val);
}

VALUE
rb_assoc_new(car, cdr)
    VALUE car, cdr;
{
    return rb_values_new(2, car, cdr);
}

static VALUE
to_ary(ary)
    VALUE ary;
{
    return rb_convert_type(ary, T_ARRAY, "Array", "to_ary");
}

static VALUE
to_a(ary)
    VALUE ary;
{
    return rb_convert_type(ary, T_ARRAY, "Array", "to_a");
}

VALUE
rb_check_array_type(ary)
    VALUE ary;
{
    return rb_check_convert_type(ary, T_ARRAY, "Array", "to_ary");
}

static VALUE rb_ary_replace _((VALUE, VALUE));

/*
 *  call-seq:
 *     Array.new(size=0, obj=nil)
 *     Array.new(array)
 *     Array.new(size) {|index| block }
 *
 *  Returns a new array. In the first form, the new array is
 *  empty. In the second it is created with _size_ copies of _obj_
 *  (that is, _size_ references to the same
 *  _obj_). The third form creates a copy of the array
 *  passed as a parameter (the array is generated by calling
 *  to_ary  on the parameter). In the last form, an array
 *  of the given size is created. Each element in this array is
 *  calculated by passing the element's index to the given block and
 *  storing the return value.
 *
 *     Array.new
 *     Array.new(2)
 *     Array.new(5, "A")
 * 
 *     # only one copy of the object is created
 *     a = Array.new(2, Hash.new)
 *     a[0]['cat'] = 'feline'
 *     a
 *     a[1]['cat'] = 'Felix'
 *     a
 * 
 *     # here multiple copies are created
 *     a = Array.new(2) { Hash.new }
 *     a[0]['cat'] = 'feline'
 *     a
 * 
 *     squares = Array.new(5) {|i| i*i}
 *     squares
 * 
 *     copy = Array.new(squares)
 */

static VALUE
rb_ary_initialize(argc, argv, ary)
    int argc;
    VALUE *argv;
    VALUE ary;
{
    long len;
    VALUE size, val;

    if (rb_scan_args(argc, argv, "02", &size, &val) == 0) {
	RARRAY(ary)->len = 0;
	if (rb_block_given_p()) {
	    rb_warning("given block not used");
	}
	return ary;
    }

    if (argc == 1 && !FIXNUM_P(size)) {
	val = rb_check_array_type(size);
	if (!NIL_P(val)) {
	    rb_ary_replace(ary, val);
	    return ary;
	}
    }

    len = NUM2LONG(size);
    if (len < 0) {
	rb_raise(rb_eArgError, "negative array size");
    }
    if (len > 0 && len * (long)sizeof(VALUE) <= len) {
	rb_raise(rb_eArgError, "array size too big");
    }
    rb_ary_modify(ary);
    if (len > RARRAY(ary)->aux.capa) {
	REALLOC_N(RARRAY(ary)->ptr, VALUE, len);
	RARRAY(ary)->aux.capa = len;
    }
    if (rb_block_given_p()) {
	long i;

	if (argc == 2) {
	    rb_warn("block supersedes default value argument");
	}
	for (i=0; i<len; i++) {
	    rb_ary_store(ary, i, rb_yield(LONG2NUM(i)));
	    RARRAY(ary)->len = i + 1;
	}
    }
    else {
	memfill(RARRAY(ary)->ptr, len, val);
	RARRAY(ary)->len = len;
    }

    return ary;
}


/* 
* Returns a new array populated with the given objects. 
*
*   Array.[]( 1, 'a', /^A/ )
*   Array[ 1, 'a', /^A/ ]
*   [ 1, 'a', /^A/ ]
*/

static VALUE
rb_ary_s_create(argc, argv, klass)
    int argc;
    VALUE *argv;
    VALUE klass;
{
    VALUE ary = ary_alloc(klass);

    if (argc > 0) {
	RARRAY(ary)->ptr = ALLOC_N(VALUE, argc);
	MEMCPY(RARRAY(ary)->ptr, argv, VALUE, argc);
    }
    RARRAY(ary)->len = RARRAY(ary)->aux.capa = argc;

    return ary;
}

void
rb_ary_store(ary, idx, val)
    VALUE ary;
    long idx;
    VALUE val;
{
    if (idx < 0) {
	idx += RARRAY(ary)->len;
	if (idx < 0) {
	    rb_raise(rb_eIndexError, "index %ld out of array",
		    idx - RARRAY(ary)->len);
	}
    }

    rb_ary_modify(ary);
    if (idx >= RARRAY(ary)->aux.capa) {
	long new_capa = RARRAY(ary)->aux.capa / 2;

	if (new_capa < ARY_DEFAULT_SIZE) {
	    new_capa = ARY_DEFAULT_SIZE;
	}
	new_capa += idx;
	if (new_capa * (long)sizeof(VALUE) <= new_capa) {
	    rb_raise(rb_eArgError, "index too big");
	}
	REALLOC_N(RARRAY(ary)->ptr, VALUE, new_capa);
	RARRAY(ary)->aux.capa = new_capa;
    }
    if (idx > RARRAY(ary)->len) {
	rb_mem_clear(RARRAY(ary)->ptr + RARRAY(ary)->len,
		     idx-RARRAY(ary)->len + 1);
    }

    if (idx >= RARRAY(ary)->len) {
	RARRAY(ary)->len = idx + 1;
    }
    RARRAY(ary)->ptr[idx] = val;
}

static VALUE
ary_shared_first(argc, argv, ary)
    int argc;
    VALUE *argv;
    VALUE ary;
{
    VALUE nv, result;
    long n;

    rb_scan_args(argc, argv, "1", &nv);
    n = NUM2LONG(nv);
    if (n > RARRAY(ary)->len) {
	n = RARRAY(ary)->len;
    }
    else if (n < 0) {
	rb_raise(rb_eArgError, "negative array size");
    }
    result = ary_shared_array(rb_cArray, ary);
    RARRAY(result)->len = n;
    return result;
}

static VALUE
ary_shared_last(argc, argv, ary)
    int argc;
    VALUE *argv;
    VALUE ary;
{
    VALUE result = ary_shared_first(argc, argv, ary);

    RARRAY(result)->ptr += RARRAY(ary)->len - RARRAY(result)->len;
    return result;
}

/*
 *  call-seq:
 *     array << obj            -> array
 *  
 *  Append---Pushes the given object on to the end of this array. This
 *  expression returns the array itself, so several appends
 *  may be chained together.
 *
 *     [ 1, 2 ] << "c" << "d" << [ 3, 4 ]
 *             #=>  [ 1, 2, "c", "d", [ 3, 4 ] ]
 *
 */

VALUE
rb_ary_push(ary, item)
    VALUE ary;
    VALUE item;
{
    rb_ary_store(ary, RARRAY(ary)->len, item);
    return ary;
}

/* 
 *  call-seq:
 *     array.push(obj, ... )   -> array
 *  
 *  Append---Pushes the given object(s) on to the end of this array. This
 *  expression returns the array itself, so several appends
 *  may be chained together.
 *
 *     a = [ "a", "b", "c" ]
 *     a.push("d", "e", "f")  
 *             #=> ["a", "b", "c", "d", "e", "f"]
 */

static VALUE
rb_ary_push_m(argc, argv, ary)
    int argc;
    VALUE *argv;
    VALUE ary;
{
    while (argc--) {
	rb_ary_push(ary, *argv++);
    }
    return ary;
}

VALUE
rb_ary_pop(ary)
    VALUE ary;
{
    rb_ary_modify_check(ary);
    if (RARRAY(ary)->len == 0) return Qnil;
    if (!FL_TEST(ary, ELTS_SHARED) &&
	    RARRAY(ary)->len * 2 < RARRAY(ary)->aux.capa &&
	    RARRAY(ary)->aux.capa > ARY_DEFAULT_SIZE) {
	RARRAY(ary)->aux.capa = RARRAY(ary)->len * 2;
	REALLOC_N(RARRAY(ary)->ptr, VALUE, RARRAY(ary)->aux.capa);
    }
    return RARRAY(ary)->ptr[--RARRAY(ary)->len];
}

/*
 *  call-seq:
 *     array.pop  -> obj or nil
 *  
 *  Removes the last element from <i>self</i> and returns it, or
 *  <code>nil</code> if the array is empty.
 *     
 *     a = [ "a", "b", "c", "d" ]
 *     a.pop     #=> "d"
 *     a.pop(2)  #=> ["b", "c"]
 *     a         #=> ["a"]
 */

static VALUE
rb_ary_pop_m(argc, argv, ary)
    int argc;
    VALUE *argv;
    VALUE ary;
{
    VALUE result;

    if (argc == 0) {
	return rb_ary_pop(ary);
    }

    rb_ary_modify_check(ary);

    result = ary_shared_last(argc, argv, ary);
    RARRAY(ary)->len -= RARRAY(result)->len;
    return result;
}

VALUE
rb_ary_shift(ary)
    VALUE ary;
{
    VALUE top;

    rb_ary_modify_check(ary);
    if (RARRAY(ary)->len == 0) return Qnil;
    top = RARRAY(ary)->ptr[0];
    ary_make_shared(ary);
    RARRAY(ary)->ptr++;		/* shift ptr */
    RARRAY(ary)->len--;

    return top;
}

/*
 *  call-seq:
 *     array.shift   ->   obj or nil
 *  
 *  Returns the first element of <i>self</i> and removes it (shifting all
 *  other elements down by one). Returns <code>nil</code> if the array
 *  is empty.
 *     
 *     args = [ "-m", "-q", "filename" ]
 *     args.shift     #=> "-m"
 *     args           #=> ["-q", "filename"]
 *
 *     args = [ "-m", "-q", "filename" ]
 *     args.shift(2)  #=> ["-m", "-q"]
 *     args           #=> ["filename"]
 */

static VALUE
rb_ary_shift_m(argc, argv, ary)
    int argc;
    VALUE *argv;
    VALUE ary;
{
    VALUE result;
    long n;

    if (argc == 0) {
	return rb_ary_shift(ary);
    }

    rb_ary_modify_check(ary);

    result = ary_shared_first(argc, argv, ary);
    n = RARRAY(result)->len;
    RARRAY(ary)->ptr += n;
    RARRAY(ary)->len -= n;

    return result;
}

VALUE
rb_ary_unshift(ary, item)
    VALUE ary, item;
{
    rb_ary_modify(ary);
    if (RARRAY(ary)->len == RARRAY(ary)->aux.capa) {
	long capa_inc = RARRAY(ary)->aux.capa / 2;
	if (capa_inc < ARY_DEFAULT_SIZE) {
	    capa_inc = ARY_DEFAULT_SIZE;
	}
	RARRAY(ary)->aux.capa += capa_inc;
	REALLOC_N(RARRAY(ary)->ptr, VALUE, RARRAY(ary)->aux.capa);
    }

    /* sliding items */
    MEMMOVE(RARRAY(ary)->ptr + 1, RARRAY(ary)->ptr, VALUE, RARRAY(ary)->len);

    RARRAY(ary)->len++;
    RARRAY(ary)->ptr[0] = item;

    return ary;
}

/*
 *  call-seq:
 *     array.unshift(obj, ...)  -> array
 *  
 *  Prepends objects to the front of <i>array</i>.
 *  other elements up one.
 *     
 *     a = [ "b", "c", "d" ]
 *     a.unshift("a")   #=> ["a", "b", "c", "d"]
 *     a.unshift(1, 2)  #=> [ 1, 2, "a", "b", "c", "d"]
 */

static VALUE
rb_ary_unshift_m(argc, argv, ary)
    int argc;
    VALUE *argv;
    VALUE ary;
{
    long len = RARRAY(ary)->len;

    if (argc == 0) return ary;

    /* make rooms by setting the last item */
    rb_ary_store(ary, len + argc - 1, Qnil);

    /* sliding items */
    MEMMOVE(RARRAY(ary)->ptr + argc, RARRAY(ary)->ptr, VALUE, len);
    MEMCPY(RARRAY(ary)->ptr, argv, VALUE, argc);
    
    return ary;
}

/* faster version - use this if you don't need to treat negative offset */
static inline VALUE
rb_ary_elt(ary, offset)
    VALUE ary;
    long offset;
{
    if (RARRAY(ary)->len == 0) return Qnil;
    if (offset < 0 || RARRAY(ary)->len <= offset) {
	return Qnil;
    }
    return RARRAY(ary)->ptr[offset];
}

VALUE
rb_ary_entry(ary, offset)
    VALUE ary;
    long offset;
{
    if (offset < 0) {
	offset += RARRAY(ary)->len;
    }
    return rb_ary_elt(ary, offset);
}

static VALUE
rb_ary_subseq(ary, beg, len)
    VALUE ary;
    long beg, len;
{
    VALUE klass, ary2, shared;
    VALUE *ptr;

    if (beg > RARRAY(ary)->len) return Qnil;
    if (beg < 0 || len < 0) return Qnil;

    if (beg + len > RARRAY(ary)->len) {
	len = RARRAY(ary)->len - beg;
	if (len < 0)
	    len = 0;
    }
    klass = rb_obj_class(ary);
    if (len == 0) return ary_new(klass, 0);

    shared = ary_make_shared(ary);
    ptr = RARRAY(ary)->ptr;
    ary2 = ary_alloc(klass);
    RARRAY(ary2)->ptr = ptr + beg;
    RARRAY(ary2)->len = len;
    RARRAY(ary2)->aux.shared = shared;
    FL_SET(ary2, ELTS_SHARED);

    return ary2;
}

/* 
 *  call-seq:
 *     array[index]                -> obj      or nil
 *     array[start, length]        -> an_array or nil
 *     array[range]                -> an_array or nil
 *     array.slice(index)          -> obj      or nil
 *     array.slice(start, length)  -> an_array or nil
 *     array.slice(range)          -> an_array or nil
 *
 *  Element Reference---Returns the element at _index_,
 *  or returns a subarray starting at _start_ and
 *  continuing for _length_ elements, or returns a subarray
 *  specified by _range_.
 *  Negative indices count backward from the end of the
 *  array (-1 is the last element). Returns nil if the index
 *  (or starting index) are out of range.
 *
 *     a = [ "a", "b", "c", "d", "e" ]
 *     a[2] +  a[0] + a[1]    #=> "cab"
 *     a[6]                   #=> nil
 *     a[1, 2]                #=> [ "b", "c" ]
 *     a[1..3]                #=> [ "b", "c", "d" ]
 *     a[4..7]                #=> [ "e" ]
 *     a[6..10]               #=> nil
 *     a[-3, 3]               #=> [ "c", "d", "e" ]
 *     # special cases
 *     a[5]                   #=> nil
 *     a[5, 1]                #=> []
 *     a[5..10]               #=> []
 *
 */

VALUE
rb_ary_aref(argc, argv, ary)
    int argc;
    VALUE *argv;
    VALUE ary;
{
    VALUE arg;
    long beg, len;

    if (argc == 2) {
	beg = NUM2LONG(argv[0]);
	len = NUM2LONG(argv[1]);
	if (beg < 0) {
	    beg += RARRAY(ary)->len;
	}
	return rb_ary_subseq(ary, beg, len);
    }
    if (argc != 1) {
	rb_scan_args(argc, argv, "11", 0, 0);
    }
    arg = argv[0];
    /* special case - speeding up */
    if (FIXNUM_P(arg)) {
	return rb_ary_entry(ary, FIX2LONG(arg));
    }
    /* check if idx is Range */
    switch (rb_range_beg_len(arg, &beg, &len, RARRAY(ary)->len, 0)) {
      case Qfalse:
	break;
      case Qnil:
	return Qnil;
      default:
	return rb_ary_subseq(ary, beg, len);
    }
    return rb_ary_entry(ary, NUM2LONG(arg));
}

/* 
 *  call-seq:
 *     array.at(index)   ->   obj  or nil
 *
 *  Returns the element at _index_. A
 *  negative index counts from the end of _self_.  Returns +nil+
 *  if the index is out of range. See also <code>Array#[]</code>.
 *  (<code>Array#at</code> is slightly faster than <code>Array#[]</code>,
 *  as it does not accept ranges and so on.)
 *
 *     a = [ "a", "b", "c", "d", "e" ]
 *     a.at(0)     #=> "a"
 *     a.at(-1)    #=> "e"
 */

static VALUE
rb_ary_at(ary, pos)
    VALUE ary, pos;
{
    return rb_ary_entry(ary, NUM2LONG(pos));
}

/*
 *  call-seq:
 *     array.first     ->   obj or nil
 *     array.first(n)  ->   an_array
 *  
 *  Returns the first element of the array. If the array is empty,
 *  returns <code>nil</code>.
 *     
 *     a = [ "q", "r", "s", "t" ]
 *     a.first     #=> "q"
 *     a.first(2)  #=> ["q", "r"]
 */

static VALUE
rb_ary_first(argc, argv, ary)
    int argc;
    VALUE *argv;
    VALUE ary;
{
    if (argc == 0) {
	if (RARRAY(ary)->len == 0) return Qnil;
	return RARRAY(ary)->ptr[0];
    }
    else {
	return ary_shared_first(argc, argv, ary);
    }
}

/*
 *  call-seq:
 *     array.last     ->  obj or nil
 *     array.last(n)  ->  an_array
 *  
 *  Returns the last element(s) of <i>self</i>. If the array is empty,
 *  the first form returns <code>nil</code>.
 *     
 *     a = [ "w", "x", "y", "z" ]
 *     a.last     #=> "z"
 *     a.last(2)  #=> ["y", "z"]
 */

static VALUE
rb_ary_last(argc, argv, ary)
    int argc;
    VALUE *argv;
    VALUE ary;
{
    if (argc == 0) {
	if (RARRAY(ary)->len == 0) return Qnil;
	return RARRAY(ary)->ptr[RARRAY(ary)->len-1];
    }
    else {
	return ary_shared_last(argc, argv, ary);
    }
}

/*
 *  call-seq:
 *     array.fetch(index)                    -> obj
 *     array.fetch(index, default )          -> obj
 *     array.fetch(index) {|index| block }   -> obj
 *  
 *  Tries to return the element at position <i>index</i>. If the index
 *  lies outside the array, the first form throws an
 *  <code>IndexError</code> exception, the second form returns
 *  <i>default</i>, and the third form returns the value of invoking
 *  the block, passing in the index. Negative values of <i>index</i>
 *  count from the end of the array.
 *     
 *     a = [ 11, 22, 33, 44 ]
 *     a.fetch(1)               #=> 22
 *     a.fetch(-1)              #=> 44
 *     a.fetch(4, 'cat')        #=> "cat"
 *     a.fetch(4) { |i| i*i }   #=> 16
 */

static VALUE
rb_ary_fetch(argc, argv, ary)
    int argc;
    VALUE *argv;
    VALUE ary;
{
    VALUE pos, ifnone;
    long block_given;
    long idx;

    rb_scan_args(argc, argv, "11", &pos, &ifnone);
    block_given = rb_block_given_p();
    if (block_given && argc == 2) {
	rb_warn("block supersedes default value argument");
    }
    idx = NUM2LONG(pos);

    if (idx < 0) {
	idx +=  RARRAY(ary)->len;
    }
    if (idx < 0 || RARRAY(ary)->len <= idx) {
	if (block_given) return rb_yield(pos);
	if (argc == 1) {
	    rb_raise(rb_eIndexError, "index %ld out of array", idx);
	}
	return ifnone;
    }
    return RARRAY(ary)->ptr[idx];
}

/*
 *  call-seq:
 *     array.index(obj)           ->  int or nil
 *     array.index {|item| block} ->  int or nil
 *  
 *  Returns the index of the first object in <i>self</i> such that is
 *  <code>==</code> to <i>obj</i>. If a block is given instead of an
 *  argument, returns first object for which <em>block</em> is true.
 *  Returns <code>nil</code> if no match is found.
 *     
 *     a = [ "a", "b", "c" ]
 *     a.index("b")        #=> 1
 *     a.index("z")        #=> nil
 *     a.index{|x|x=="b"}  #=> 1
 */

static VALUE
rb_ary_index(argc, argv, ary)
    int argc;
    VALUE *argv;
    VALUE ary;
{
    VALUE val;
    long i;

    if (rb_scan_args(argc, argv, "01", &val) == 0) {
	for (i=0; i<RARRAY(ary)->len; i++) {
	    if (RTEST(rb_yield(RARRAY(ary)->ptr[i]))) {
		return LONG2NUM(i);
	    }
	}
    }
    else {
	for (i=0; i<RARRAY(ary)->len; i++) {
	    if (rb_equal(RARRAY(ary)->ptr[i], val))
		return LONG2NUM(i);
	}
    }
    return Qnil;
}

/*
 *  call-seq:
 *     array.rindex(obj)    ->  int or nil
 *  
 *  Returns the index of the last object in <i>array</i>
 *  <code>==</code> to <i>obj</i>. If a block is given instead of an
 *  argument, returns first object for which <em>block</em> is
 *  true. Returns <code>nil</code> if no match is found.
 *     
 *     a = [ "a", "b", "b", "b", "c" ]
 *     a.rindex("b")        #=> 3
 *     a.rindex("z")        #=> nil
 *     a.rindex{|x|x=="b"}  #=> 3
 */

static VALUE
rb_ary_rindex(argc, argv, ary)
    int argc;
    VALUE *argv;
    VALUE ary;
{
    VALUE val;
    long i = RARRAY(ary)->len;

    if (rb_scan_args(argc, argv, "01", &val) == 0) {
	while (i--) {
	    if (RTEST(rb_yield(RARRAY(ary)->ptr[i])))
		return LONG2NUM(i);
	    if (i > RARRAY(ary)->len) {
		i = RARRAY(ary)->len;
	    }
	}
    }
    else {
	while (i--) {
	    if (rb_equal(RARRAY(ary)->ptr[i], val))
		return LONG2NUM(i);
	    if (i > RARRAY(ary)->len) {
		i = RARRAY(ary)->len;
	    }
	}
    }
    return Qnil;
}

VALUE
rb_ary_to_ary(obj)
    VALUE obj;
{
    if (TYPE(obj) == T_ARRAY) {
	return obj;
    }
    if (rb_respond_to(obj, rb_intern("to_ary"))) {
	return to_ary(obj);
    }
    return rb_ary_new3(1, obj);
}

static void
rb_ary_splice(ary, beg, len, rpl)
    VALUE ary;
    long beg, len;
    VALUE rpl;
{
    long rlen;

    if (len < 0) rb_raise(rb_eIndexError, "negative length (%ld)", len);
    if (beg < 0) {
	beg += RARRAY(ary)->len;
	if (beg < 0) {
	    beg -= RARRAY(ary)->len;
	    rb_raise(rb_eIndexError, "index %ld out of array", beg);
	}
    }
    if (beg + len > RARRAY(ary)->len) {
	len = RARRAY(ary)->len - beg;
    }

    if (rpl == Qundef) {
	rlen = 0;
    }
    else {
	rpl = rb_ary_to_ary(rpl);
	rlen = RARRAY(rpl)->len;
    }
    rb_ary_modify(ary);

    if (beg >= RARRAY(ary)->len) {
	len = beg + rlen;
	if (len >= RARRAY(ary)->aux.capa) {
	    REALLOC_N(RARRAY(ary)->ptr, VALUE, len);
	    RARRAY(ary)->aux.capa = len;
	}
	rb_mem_clear(RARRAY(ary)->ptr + RARRAY(ary)->len, beg - RARRAY(ary)->len);
	if (rlen > 0) {
	    MEMCPY(RARRAY(ary)->ptr + beg, RARRAY(rpl)->ptr, VALUE, rlen);
	}
	RARRAY(ary)->len = len;
    }
    else {
	long alen;

	if (beg + len > RARRAY(ary)->len) {
	    len = RARRAY(ary)->len - beg;
	}

	alen = RARRAY(ary)->len + rlen - len;
	if (alen >= RARRAY(ary)->aux.capa) {
	    REALLOC_N(RARRAY(ary)->ptr, VALUE, alen);
	    RARRAY(ary)->aux.capa = alen;
	}

	if (len != rlen) {
	    MEMMOVE(RARRAY(ary)->ptr + beg + rlen, RARRAY(ary)->ptr + beg + len,
		    VALUE, RARRAY(ary)->len - (beg + len));
	    RARRAY(ary)->len = alen;
	}
	if (rlen > 0) {
	    MEMMOVE(RARRAY(ary)->ptr + beg, RARRAY(rpl)->ptr, VALUE, rlen);
	}
    }
}

/* 
 *  call-seq:
 *     array[index]         = obj                     ->  obj
 *     array[start, length] = obj or an_array or nil  ->  obj or an_array or nil
 *     array[range]         = obj or an_array or nil  ->  obj or an_array or nil
 *
 *  Element Assignment---Sets the element at _index_,
 *  or replaces a subarray starting at _start_ and
 *  continuing for _length_ elements, or replaces a subarray
 *  specified by _range_.  If indices are greater than
 *  the current capacity of the array, the array grows
 *  automatically. A negative indices will count backward
 *  from the end of the array. Inserts elements if _length_ is
 *  zero. An +IndexError+ is raised if a negative index points
 *  past the beginning of the array. See also
 *  <code>Array#push</code>, and <code>Array#unshift</code>.
 * 
 *     a = Array.new
 *     a[4] = "4";                 #=> [nil, nil, nil, nil, "4"]
 *     a[0, 3] = [ 'a', 'b', 'c' ] #=> ["a", "b", "c", nil, "4"]
 *     a[1..2] = [ 1, 2 ]          #=> ["a", 1, 2, nil, "4"]
 *     a[0, 2] = "?"               #=> ["?", 2, nil, "4"]
 *     a[0..2] = "A"               #=> ["A", "4"]
 *     a[-1]   = "Z"               #=> ["A", "Z"]
 *     a[1..-1] = nil              #=> ["A", nil]
 *     a[1..-1] = []              #=> ["A"]
 */

static VALUE
rb_ary_aset(argc, argv, ary)
    int argc;
    VALUE *argv;
    VALUE ary;
{
    long offset, beg, len;

    if (argc == 3) {
	rb_ary_splice(ary, NUM2LONG(argv[0]), NUM2LONG(argv[1]), argv[2]);
	return argv[2];
    }
    if (argc != 2) {
	rb_raise(rb_eArgError, "wrong number of arguments (%d for 2)", argc);
    }
    if (FIXNUM_P(argv[0])) {
	offset = FIX2LONG(argv[0]);
	goto fixnum;
    }
    if (rb_range_beg_len(argv[0], &beg, &len, RARRAY(ary)->len, 1)) {
	/* check if idx is Range */
	rb_ary_splice(ary, beg, len, argv[1]);
	return argv[1];
    }

    offset = NUM2LONG(argv[0]);
fixnum:
    rb_ary_store(ary, offset, argv[1]);
    return argv[1];
}

/*
 *  call-seq:
 *     array.insert(index, obj...)  -> array
 *  
 *  Inserts the given values before the element with the given index
 *  (which may be negative).
 *     
 *     a = %w{ a b c d }
 *     a.insert(2, 99)         #=> ["a", "b", 99, "c", "d"]
 *     a.insert(-2, 1, 2, 3)   #=> ["a", "b", 99, "c", 1, 2, 3, "d"]
 */

static VALUE
rb_ary_insert(argc, argv, ary)
    int argc;
    VALUE *argv;
    VALUE ary;
{
    long pos;

    if (argc < 1) {
	rb_raise(rb_eArgError, "wrong number of arguments (at least 1)");
    }
    pos = NUM2LONG(argv[0]);
    if (pos == -1) {
	pos = RARRAY(ary)->len;
    }
    else if (pos < 0) {
	pos++;
    }

    if (argc == 1) return ary;
    rb_ary_splice(ary, pos, 0, rb_ary_new4(argc - 1, argv + 1));
    return ary;
}

/*
 *  call-seq:
 *     array.each {|item| block }   ->   array
 *  
 *  Calls <i>block</i> once for each element in <i>self</i>, passing that
 *  element as a parameter.
 *     
 *     a = [ "a", "b", "c" ]
 *     a.each {|x| print x, " -- " }
 *     
 *  produces:
 *     
 *     a -- b -- c --
 */

VALUE
rb_ary_each(ary)
    VALUE ary;
{
    long i;

    for (i=0; i<RARRAY(ary)->len; i++) {
	rb_yield(RARRAY(ary)->ptr[i]);
    }
    return ary;
}

/*
 *  call-seq:
 *     array.each_index {|index| block }  ->  array
 *  
 *  Same as <code>Array#each</code>, but passes the index of the element
 *  instead of the element itself.
 *     
 *     a = [ "a", "b", "c" ]
 *     a.each_index {|x| print x, " -- " }
 *     
 *  produces:
 *     
 *     0 -- 1 -- 2 --
 */

static VALUE
rb_ary_each_index(ary)
    VALUE ary;
{
    long i;

    for (i=0; i<RARRAY(ary)->len; i++) {
	rb_yield(LONG2NUM(i));
    }
    return ary;
}

/*
 *  call-seq:
 *     array.reverse_each {|item| block } 
 *  
 *  Same as <code>Array#each</code>, but traverses <i>self</i> in reverse
 *  order.
 *     
 *     a = [ "a", "b", "c" ]
 *     a.reverse_each {|x| print x, " " }
 *     
 *  produces:
 *     
 *     c b a
 */

static VALUE
rb_ary_reverse_each(ary)
    VALUE ary;
{
    long len = RARRAY(ary)->len;

    while (len--) {
	rb_yield(RARRAY(ary)->ptr[len]);
	if (RARRAY(ary)->len < len) {
	    len = RARRAY(ary)->len;
	}
    }
    return ary;
}

/*
 *  call-seq:
 *     array.length -> int
 *  
 *  Returns the number of elements in <i>self</i>. May be zero.
 *     
 *     [ 1, 2, 3, 4, 5 ].length   #=> 5
 */

static VALUE
rb_ary_length(ary)
    VALUE ary;
{
    return LONG2NUM(RARRAY(ary)->len);
}

/*
 *  call-seq:
 *     array.empty?   -> true or false
 *  
 *  Returns <code>true</code> if <i>self</i> array contains no elements.
 *     
 *     [].empty?   #=> true
 */

static VALUE
rb_ary_empty_p(ary)
    VALUE ary;
{
    if (RARRAY(ary)->len == 0)
	return Qtrue;
    return Qfalse;
}

VALUE
rb_ary_dup(ary)
    VALUE ary;
{
    VALUE dup = rb_ary_new2(RARRAY(ary)->len);

    DUPSETUP(dup, ary);
    MEMCPY(RARRAY(dup)->ptr, RARRAY(ary)->ptr, VALUE, RARRAY(ary)->len);
    RARRAY(dup)->len = RARRAY(ary)->len;
    return dup;
}

extern VALUE rb_output_fs;

static VALUE
recursive_join(ary, arg, recur)
    VALUE ary;
    VALUE *arg;
    int recur;
{
    if (recur) {
	return rb_str_new2("[...]");
    }
    return rb_ary_join(arg[0], arg[1]);
}

VALUE
rb_ary_join(ary, sep)
    VALUE ary, sep;
{
    long len = 1, i;
    int taint = Qfalse;
    VALUE result, tmp;

    if (RARRAY(ary)->len == 0) return rb_str_new(0, 0);
    if (OBJ_TAINTED(ary) || OBJ_TAINTED(sep)) taint = Qtrue;

    for (i=0; i<RARRAY(ary)->len; i++) {
	tmp = rb_check_string_type(RARRAY(ary)->ptr[i]);
	len += NIL_P(tmp) ? 10 : RSTRING(tmp)->len;
    }
    if (!NIL_P(sep)) {
	StringValue(sep);
	len += RSTRING(sep)->len * (RARRAY(ary)->len - 1);
    }
    result = rb_str_buf_new(len);
    for (i=0; i<RARRAY(ary)->len; i++) {
	tmp = RARRAY(ary)->ptr[i];
	switch (TYPE(tmp)) {
	  case T_STRING:
	    break;
	  case T_ARRAY:
	    {
		VALUE args[2];

		args[0] = tmp;
		args[1] = sep;
		tmp = rb_exec_recursive(recursive_join, ary, (VALUE)args);
	    }
	    break;
	  default:
	    tmp = rb_obj_as_string(tmp);
	}
	if (i > 0 && !NIL_P(sep))
	    rb_str_buf_append(result, sep);
	rb_str_buf_append(result, tmp);
	if (OBJ_TAINTED(tmp)) taint = Qtrue;
    }

    if (taint) OBJ_TAINT(result);
    return result;
}

/*
 *  call-seq:
 *     array.join(sep=$,)    -> str
 *  
 *  Returns a string created by converting each element of the array to
 *  a string, separated by <i>sep</i>.
 *     
 *     [ "a", "b", "c" ].join        #=> "abc"
 *     [ "a", "b", "c" ].join("-")   #=> "a-b-c"
 */

static VALUE
rb_ary_join_m(argc, argv, ary)
    int argc;
    VALUE *argv;
    VALUE ary;
{
    VALUE sep;

    rb_scan_args(argc, argv, "01", &sep);
    if (NIL_P(sep)) sep = rb_output_fs;
    
    return rb_ary_join(ary, sep);
}

/*
 *  call-seq:
 *     array.to_s -> string
 *  
 *  Returns _self_<code>.join</code>.
 *     
 *     [ "a", "e", "i", "o" ].to_s   #=> "aeio"
 *
 */

VALUE
rb_ary_to_s(ary)
    VALUE ary;
{
    if (RARRAY(ary)->len == 0) return rb_str_new(0, 0);
    
    return rb_ary_join(ary, rb_output_fs);
}

static VALUE
inspect_ary(ary, dummy, recur)
    VALUE ary;
    VALUE dummy;
    int recur;
{
    int tainted = OBJ_TAINTED(ary);
    long i;
    VALUE s, str;

    if (recur) return rb_tainted_str_new2("[...]");
    str = rb_str_buf_new2("[");
    for (i=0; i<RARRAY(ary)->len; i++) {
	s = rb_inspect(RARRAY(ary)->ptr[i]);
	if (OBJ_TAINTED(s)) tainted = Qtrue;
	if (i > 0) rb_str_buf_cat2(str, ", ");
	rb_str_buf_append(str, s);
    }
    rb_str_buf_cat2(str, "]");
    if (tainted) OBJ_TAINT(str);
    return str;
}

/*
 *  call-seq:
 *     array.inspect  -> string
 *
 *  Create a printable version of <i>array</i>.
 */

static VALUE
rb_ary_inspect(ary)
    VALUE ary;
{
    if (RARRAY(ary)->len == 0) return rb_str_new2("[]");
    return rb_exec_recursive(inspect_ary, ary, 0);
}

/*
 *  call-seq:
 *     array.to_a     -> array
 *  
 *  Returns _self_. If called on a subclass of Array, converts
 *  the receiver to an Array object.
 */

static VALUE
rb_ary_to_a(ary)
    VALUE ary;
{
    if (rb_obj_class(ary) != rb_cArray) {
	VALUE dup = rb_ary_new2(RARRAY(ary)->len);
	rb_ary_replace(dup, ary);
	return dup;
    }
    return ary;
}

/*
 *  call-seq:
 *     array.to_ary -> array
 *  
 *  Returns _self_.
 */

static VALUE
rb_ary_to_ary_m(ary)
    VALUE ary;
{
    return ary;
}

VALUE
rb_ary_reverse(ary)
    VALUE ary;
{
    VALUE *p1, *p2;
    VALUE tmp;

    rb_ary_modify(ary);
    if (RARRAY(ary)->len > 1) {
	p1 = RARRAY(ary)->ptr;
	p2 = p1 + RARRAY(ary)->len - 1;	/* points last item */

	while (p1 < p2) {
	    tmp = *p1;
	    *p1++ = *p2;
	    *p2-- = tmp;
	}
    }
    return ary;
}

/*
 *  call-seq:
 *     array.reverse!   -> array 
 *  
 *  Reverses _self_ in place.
 *     
 *     a = [ "a", "b", "c" ]
 *     a.reverse!       #=> ["c", "b", "a"]
 *     a                #=> ["c", "b", "a"]
 */

static VALUE
rb_ary_reverse_bang(ary)
    VALUE ary;
{
    return rb_ary_reverse(ary);
}

/*
 *  call-seq:
 *     array.reverse -> an_array
 *  
 *  Returns a new array containing <i>self</i>'s elements in reverse order.
 *     
 *     [ "a", "b", "c" ].reverse   #=> ["c", "b", "a"]
 *     [ 1 ].reverse               #=> [1]
 */

static VALUE
rb_ary_reverse_m(ary)
    VALUE ary;
{
    return rb_ary_reverse(rb_ary_dup(ary));
}

struct ary_sort_data {
    VALUE ary;
    VALUE *ptr;
    long len;
};

static void
ary_sort_check(data)
    struct ary_sort_data *data;
{
    if (RARRAY(data->ary)->ptr != data->ptr || RARRAY(data->ary)->len != data->len) {
	rb_raise(rb_eRuntimeError, "array modified during sort");
    }
}

static int
sort_1(a, b, data)
    VALUE *a, *b;
    struct ary_sort_data *data;
{
    VALUE retval = rb_yield_values(2, *a, *b);
    int n;

    n = rb_cmpint(retval, *a, *b);
    ary_sort_check(data);
    return n;
}

static int
sort_2(ap, bp, data)
    VALUE *ap, *bp;
    struct ary_sort_data *data;
{
    VALUE retval;
    VALUE a = *ap, b = *bp;
    int n;

    if (FIXNUM_P(a) && FIXNUM_P(b)) {
	if ((long)a > (long)b) return 1;
	if ((long)a < (long)b) return -1;
	return 0;
    }
    if (TYPE(a) == T_STRING && TYPE(b) == T_STRING) {
	return rb_str_cmp(a, b);
    }

    retval = rb_funcall(a, id_cmp, 1, b);
    n = rb_cmpint(retval, a, b);
    ary_sort_check(data);

    return n;
}

static VALUE
sort_internal(ary)
    VALUE ary;
{
    struct ary_sort_data data;

    data.ary = ary;
    data.ptr = RARRAY(ary)->ptr; data.len = RARRAY(ary)->len;
    qsort(RARRAY(ary)->ptr, RARRAY(ary)->len, sizeof(VALUE),
	  rb_block_given_p()?sort_1:sort_2, &data);
    return ary;
}

static VALUE
sort_unlock(ary)
    VALUE ary;
{
    FL_UNSET(ary, ARY_TMPLOCK);
    return ary;
}

/*
 *  call-seq:
 *     array.sort!                   -> array
 *     array.sort! {| a,b | block }  -> array 
 *  
 *  Sorts _self_. Comparisons for
 *  the sort will be done using the <code><=></code> operator or using
 *  an optional code block. The block implements a comparison between
 *  <i>a</i> and <i>b</i>, returning -1, 0, or +1. See also
 *  <code>Enumerable#sort_by</code>.
 *     
 *     a = [ "d", "a", "e", "c", "b" ]
 *     a.sort                    #=> ["a", "b", "c", "d", "e"]
 *     a.sort {|x,y| y <=> x }   #=> ["e", "d", "c", "b", "a"]
 */

VALUE
rb_ary_sort_bang(ary)
    VALUE ary;
{
    rb_ary_modify(ary);
    if (RARRAY(ary)->len > 1) {
	FL_SET(ary, ARY_TMPLOCK);	/* prohibit modification during sort */
	rb_ensure(sort_internal, ary, sort_unlock, ary);
    }
    return ary;
}

/*
 *  call-seq:
 *     array.sort                   -> an_array 
 *     array.sort {| a,b | block }  -> an_array 
 *  
 *  Returns a new array created by sorting <i>self</i>. Comparisons for
 *  the sort will be done using the <code><=></code> operator or using
 *  an optional code block. The block implements a comparison between
 *  <i>a</i> and <i>b</i>, returning -1, 0, or +1. See also
 *  <code>Enumerable#sort_by</code>.
 *     
 *     a = [ "d", "a", "e", "c", "b" ]
 *     a.sort                    #=> ["a", "b", "c", "d", "e"]
 *     a.sort {|x,y| y <=> x }   #=> ["e", "d", "c", "b", "a"]
 */

VALUE
rb_ary_sort(ary)
    VALUE ary;
{
    ary = rb_ary_dup(ary);
    rb_ary_sort_bang(ary);
    return ary;
}

/*
 *  call-seq:
 *     array.collect {|item| block }  -> an_array
 *     array.map     {|item| block }  -> an_array
 *  
 *  Invokes <i>block</i> once for each element of <i>self</i>. Creates a 
 *  new array containing the values returned by the block.
 *  See also <code>Enumerable#collect</code>.
 *     
 *     a = [ "a", "b", "c", "d" ]
 *     a.collect {|x| x + "!" }   #=> ["a!", "b!", "c!", "d!"]
 *     a                          #=> ["a", "b", "c", "d"]
 */

static VALUE
rb_ary_collect(ary)
    VALUE ary;
{
    long i;
    VALUE collect;

    if (!rb_block_given_p()) {
	return rb_ary_new4(RARRAY(ary)->len, RARRAY(ary)->ptr);
    }

    collect = rb_ary_new2(RARRAY(ary)->len);
    for (i = 0; i < RARRAY(ary)->len; i++) {
	rb_ary_push(collect, rb_yield(RARRAY(ary)->ptr[i]));
    }
    return collect;
}

/* 
 *  call-seq:
 *     array.collect! {|item| block }   ->   array
 *     array.map!     {|item| block }   ->   array
 *
 *  Invokes the block once for each element of _self_, replacing the
 *  element with the value returned by _block_.
 *  See also <code>Enumerable#collect</code>.
 *   
 *     a = [ "a", "b", "c", "d" ]
 *     a.collect! {|x| x + "!" }
 *     a             #=>  [ "a!", "b!", "c!", "d!" ]
 */

static VALUE
rb_ary_collect_bang(ary)
    VALUE ary;
{
    long i;

    rb_ary_modify(ary);
    for (i = 0; i < RARRAY(ary)->len; i++) {
	rb_ary_store(ary, i, rb_yield(RARRAY(ary)->ptr[i]));
    }
    return ary;
}

VALUE
rb_get_values_at(obj, olen, argc, argv, func)
    VALUE obj;
    long olen;
    int argc;
    VALUE *argv;
    VALUE (*func) _((VALUE,long));
{
    VALUE result = rb_ary_new2(argc);
    long beg, len, i, j;

    for (i=0; i<argc; i++) {
	if (FIXNUM_P(argv[i])) {
	    rb_ary_push(result, (*func)(obj, FIX2LONG(argv[i])));
	    continue;
	}
	/* check if idx is Range */
	switch (rb_range_beg_len(argv[i], &beg, &len, olen, 0)) {
	  case Qfalse:
	    break;
	  case Qnil:
	    continue;
	  default:
	    for (j=0; j<len; j++) {
		rb_ary_push(result, (*func)(obj, j+beg));
	    }
	    continue;
	}
	rb_ary_push(result, (*func)(obj, NUM2LONG(argv[i])));
    }
    return result;
}

/* 
 *  call-seq:
 *     array.values_at(selector,... )  -> an_array
 *
 *  Returns an array containing the elements in
 *  _self_ corresponding to the given selector(s). The selectors
 *  may be either integer indices or ranges. 
 *  See also <code>Array#select</code>.
 * 
 *     a = %w{ a b c d e f }
 *     a.values_at(1, 3, 5)
 *     a.values_at(1, 3, 5, 7)
 *     a.values_at(-1, -3, -5, -7)
 *     a.values_at(1..3, 2...5)
 */

static VALUE
rb_ary_values_at(argc, argv, ary)
    int argc;
    VALUE *argv;
    VALUE ary;
{
    return rb_get_values_at(ary, RARRAY(ary)->len, argc, argv, rb_ary_entry);
}

/*
 *  call-seq:
 *     array.select {|item| block } -> an_array
 *  
 *  Invokes the block passing in successive elements from <i>array</i>,
 *  returning an array containing those elements for which the block
 *  returns a true value (equivalent to <code>Enumerable#select</code>).
 *     
 *     a = %w{ a b c d e f }
 *     a.select {|v| v =~ /[aeiou]/}   #=> ["a", "e"]
 */

static VALUE
rb_ary_select(ary)
    VALUE ary;
{
    VALUE result;
    long i;

    result = rb_ary_new2(RARRAY(ary)->len);
    for (i = 0; i < RARRAY(ary)->len; i++) {
	if (RTEST(rb_yield(RARRAY(ary)->ptr[i]))) {
	    rb_ary_push(result, rb_ary_elt(ary, i));
	}
    }
    return result;
}

