/* Auxilary functions that come in handy in certain parts
 * of the MP
 */
#include "str_aux.h"

string reverse(string s) {
	int c, i, j;
	for (i = 0, j = s.length() - 1; i<j; i++, j--) {
		c = s[i];
		s[i] = s[j];
		s[j] = c;
	}
	return s;
}

/* itoa:  convert n to characters in s */
string itoa(int n, string s) {
	int i = 0, sign;

	if ((sign = n) < 0)  /* record sign */
		n = -n;          /* make n positive */
	do {       /* generate digits in reverse order */
		s[i++] = n % 10 + '0';   /* get next digit */
	} while ((n /= 10) > 0);     /* delete it */
	if (sign < 0)
		s[i++] = '-';
	s[i] = '\0';
	return reverse(s);
} 

/* Return a string representation of n */
string itos(int n) {
	stringstream s;
	s << n;
	return s.str();
}
