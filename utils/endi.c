#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <stdlib.h>

/* Function prototype */
void swapn(unsigned char *b, int N, int n);

int main(int iargc,char *argv[])
{
  int i,n,wdt;
  char *p;
  unsigned char *b;
  struct stat st;
  FILE *in,*out;

  if(iargc < 3) {
    printf("Usage: endi nwidth file1 [ file2 ... filen]\n");
    return 1;
  }
  for(i = 2; i < iargc; i++) {
    p = argv[i];
    if(stat(p,&st) != 0) {
       printf("File %s does no exists\n",p);
       continue;
    }
    wdt = atoi(argv[1]);    /* get swapping width */
    n = st.st_size/wdt;
    if((b = (unsigned char *)malloc(st.st_size)) == NULL) {
      printf(" Can not allocate memory\n");
    }
  /* open input   file      */
    if((in = fopen(p,"r")) == NULL) {
        fprintf(stderr,"File %s does nor exist.\n",p);
        return 1;
    }
    fread(b,st.st_size,1,in);
    fclose(in);
    swapn(b,wdt,n);
  /* open output   file      */
    if((out = fopen(p,"w")) == NULL) {
        fprintf(stderr,"File %s does nor exist.\n",p);
        return 1;
    }
    fwrite(b,st.st_size,1,in);
    fclose(out);
    free(b);
  }
  return 0;
}
