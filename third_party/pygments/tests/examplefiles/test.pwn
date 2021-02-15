#include <core>

// Single line comment
/* Multi line
   comment */

/// documentation
/**

	documentation multi line
	
**/

public OnGameModeInit() {
    printf("Hello, World!");
}

enum info {
	Float:ex;
	exa,
	exam[5],
}
new arr[5][info];

stock Float:test_func()
{
	new a = 5, Float:b = 10.3;
	if (a == b) {
		
	} else {
		
	}
	
	for (new i = 0; i < 10; i++) {
		continue;
	}
	
	do {
		a--;
	} while (a > 0);
	
	while (a < 5) {
		a++;
		break;
	}
	
	switch (a) {
		case 0: {
		}
		case 0..4: {
		}
		case 5, 6: {
		}
	}
	
	static x;
	new xx = a > 5 ? 5 : 0;
	new array[sizeof arr] = {0};
	tagof a;
	state a;
	goto label;
	new byte[2 char];
	byte{0} = 'a';
	
	return (float(a) + b);
}


// float.inc
/* Float arithmetic
 *
 * (c) Copyright 1999, Artran, Inc.
 * Written by Greg Garner (gmg@artran.com)
 * Modified in March 2001 to include user defined
 * operators for the floating point functions.
 *
 * This file is provided as is (no warranties).
 */
#if defined _Float_included
  #endinput
#endif
#define _Float_included
#pragma library Float

/* Different methods of rounding */
enum floatround_method {
  floatround_round,
  floatround_floor,
  floatround_ceil,
  floatround_tozero,
  floatround_unbiased
}
enum anglemode {
  radian,
  degrees,
  grades
}

/**************************************************/
/* Convert an integer into a floating point value */
native Float:float(value);

/**************************************************/
/* Convert a string into a floating point value */
native Float:floatstr(const string[]);

/**************************************************/
/* Multiple two floats together */
native Float:floatmul(Float:oper1, Float:oper2);

/**************************************************/
/* Divide the dividend float by the divisor float */
native Float:floatdiv(Float:dividend, Float:divisor);

/**************************************************/
/* Add two floats together */
native Float:floatadd(Float:oper1, Float:oper2);

/**************************************************/
/* Subtract oper2 float from oper1 float */
native Float:floatsub(Float:oper1, Float:oper2);

/**************************************************/
/* Return the fractional part of a float */
native Float:floatfract(Float:value);

/**************************************************/
/* Round a float into a integer value */
native floatround(Float:value, floatround_method:method=floatround_round);

/**************************************************/
/* Compare two integers. If the two elements are equal, return 0.
   If the first argument is greater than the second argument, return 1,
   If the first argument is less than the second argument, return -1. */
native floatcmp(Float:oper1, Float:oper2);

/**************************************************/
/* Return the square root of the input value, same as floatpower(value, 0.5) */
native Float:floatsqroot(Float:value);

/**************************************************/
/* Return the value raised to the power of the exponent */
native Float:floatpower(Float:value, Float:exponent);

/**************************************************/
/* Return the logarithm */
native Float:floatlog(Float:value, Float:base=10.0);

/**************************************************/
/* Return the sine, cosine or tangent. The input angle may be in radian,
   degrees or grades. */
native Float:floatsin(Float:value, anglemode:mode=radian);
native Float:floatcos(Float:value, anglemode:mode=radian);
native Float:floattan(Float:value, anglemode:mode=radian);

/**************************************************/
/* Return the absolute value */
native Float:floatabs(Float:value);


/**************************************************/
#pragma rational Float

/* user defined operators */
native Float:operator*(Float:oper1, Float:oper2) = floatmul;
native Float:operator/(Float:oper1, Float:oper2) = floatdiv;
native Float:operator+(Float:oper1, Float:oper2) = floatadd;
native Float:operator-(Float:oper1, Float:oper2) = floatsub;
native Float:operator=(oper) = float;

stock Float:operator++(Float:oper)
    return oper+1.0;

stock Float:operator--(Float:oper)
    return oper-1.0;

stock Float:operator-(Float:oper)
    return oper^Float:cellmin;                  /* IEEE values are sign/magnitude */

stock Float:operator*(Float:oper1, oper2)
    return floatmul(oper1, float(oper2));       /* "*" is commutative */

stock Float:operator/(Float:oper1, oper2)
    return floatdiv(oper1, float(oper2));

stock Float:operator/(oper1, Float:oper2)
    return floatdiv(float(oper1), oper2);

stock Float:operator+(Float:oper1, oper2)
    return floatadd(oper1, float(oper2));       /* "+" is commutative */

stock Float:operator-(Float:oper1, oper2)
    return floatsub(oper1, float(oper2));

stock Float:operator-(oper1, Float:oper2)
    return floatsub(float(oper1), oper2);

stock bool:operator==(Float:oper1, Float:oper2)
    return floatcmp(oper1, oper2) == 0;

stock bool:operator==(Float:oper1, oper2)
    return floatcmp(oper1, float(oper2)) == 0;  /* "==" is commutative */

stock bool:operator!=(Float:oper1, Float:oper2)
    return floatcmp(oper1, oper2) != 0;

stock bool:operator!=(Float:oper1, oper2)
    return floatcmp(oper1, float(oper2)) != 0;  /* "!=" is commutative */

stock bool:operator>(Float:oper1, Float:oper2)
    return floatcmp(oper1, oper2) > 0;

stock bool:operator>(Float:oper1, oper2)
    return floatcmp(oper1, float(oper2)) > 0;

stock bool:operator>(oper1, Float:oper2)
    return floatcmp(float(oper1), oper2) > 0;

stock bool:operator>=(Float:oper1, Float:oper2)
    return floatcmp(oper1, oper2) >= 0;

stock bool:operator>=(Float:oper1, oper2)
    return floatcmp(oper1, float(oper2)) >= 0;

stock bool:operator>=(oper1, Float:oper2)
    return floatcmp(float(oper1), oper2) >= 0;

stock bool:operator<(Float:oper1, Float:oper2)
    return floatcmp(oper1, oper2) < 0;

stock bool:operator<(Float:oper1, oper2)
    return floatcmp(oper1, float(oper2)) < 0;

stock bool:operator<(oper1, Float:oper2)
    return floatcmp(float(oper1), oper2) < 0;

stock bool:operator<=(Float:oper1, Float:oper2)
    return floatcmp(oper1, oper2) <= 0;

stock bool:operator<=(Float:oper1, oper2)
    return floatcmp(oper1, float(oper2)) <= 0;

stock bool:operator<=(oper1, Float:oper2)
    return floatcmp(float(oper1), oper2) <= 0;

stock bool:operator!(Float:oper)
    return (_:oper & cellmax) == 0;

/* forbidden operations */
forward operator%(Float:oper1, Float:oper2);
forward operator%(Float:oper1, oper2);
forward operator%(oper1, Float:oper2);

