/* Simple routines for interfacing Fortran and C. */

#include <stddef.h>
#include <stdlib.h>
#include <string.h>

int white(k)
char k;
/* Tells if it's whitespace or not. */
{
  switch(k) {
    case ' ':
    case '\n':
    case '\t': return(1);
  }
  return(0);
} /* white */

char *ftocstring(fstring, len)
char *fstring;
long len;
{
  char *ret;
  int i;

  ret= (char *)malloc( sizeof(char) * (len+1) );
  if( ret== NULL )
    return(NULL);
  memcpy(ret,fstring, sizeof(char)*len);
  ret[len]=0;

  /* Now we look for the last non-whitespace character to
     re-terminate the string. */
  for(i=len; white(ret[i]) && i; i--) 
    ;
  ret[i+1]=0;
  return(ret);
} /* cstring */

