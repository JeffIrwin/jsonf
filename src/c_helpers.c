#include <stdlib.h>
#include <errno.h>
#include <stdint.h>

/* Wrapper around strtoll that also reports whether ERANGE overflow occurred.
   Returns the parsed value; sets *overflow to 1 if errno == ERANGE. */
int64_t c_strtoll_checked(const char *nptr, char **endptr, int base, int *overflow)
{
	errno = 0;
	int64_t val = strtoll(nptr, endptr, base);
	*overflow = (errno == ERANGE) ? 1 : 0;
	return val;
}
