/*
 * MINEOS version 1.0 by Guy Masters, John Woodhouse, and Freeman Gilbert
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 ***************************************************************************
 *
 * endi program swaps with width 4 unlimited number of binary files in place.
 *
 ***************************************************************************
 */
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
  FILE *in;
  int icnt,icnte,irest,msize=1000000;

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
    msize = msize/wdt*wdt;
    if(st.st_size <= msize) msize = st.st_size;
    if((b = (unsigned char *)malloc(msize)) == NULL) {
      printf(" Can not allocate memory\n");
    }
  /* open input   file      */
    if((in = fopen(p,"r+")) == NULL) {
        fprintf(stderr,"File %s does nor exist.\n",p);
        return 1;
    }
    icnt = 0;
    do {
        icnte = icnt+msize;
        n = msize/wdt;
        fseek(in, icnt, SEEK_SET);
        fread(b,msize,1,in);
        swapn(b,wdt,n);
        fseek(in, icnt, SEEK_SET);
        fwrite(b,msize,1,in);
        irest = st.st_size-icnte;
        if(irest < msize && irest > 0) msize = irest;
        icnt = icnte;
    } while(icnte != st.st_size);
    fclose(in);
    free(b);
  }
  return 0;
}
