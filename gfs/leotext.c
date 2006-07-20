/* My interface for X-windows graphics
This file just has the text routines in it. */

/* This can be increased at the expense of client memory. */
#define maximum_number_fonts 30

/* The following fonts should be available with all X distributions:
(there are others, but these are the only interesting ones to me.)
family: Helvetica, Times, Courier
weights: medium, bold
slants: roman, italic/oblique
point sizes: 8, 10, 12, 14, 18, 24
*/

#include "rotated.h"  /* Alan Richardson's text rotation
		 	 library. */

XFontStruct *lfont[maximum_number_fonts];  /* This is an array of
				pointers to XFontStruc's. */
int howmanyfonts=0;  /* How many are loaded. */


int nowfont=0;

void F77_FUNC(lsetfont,LSETFONT)(which)
/* Picks which font to use for text. */
long *which;
{
  if( (*which>howmanyfonts) || (*which<1) ) {
    fprintf(stderr,"leolib: Ignoring attempt to select unloaded font #%d",which);
  } else {
    /* From fortran, fonts are numbered 1-n. */
    XSetFont(display,usergc, lfont[ *which - 1]->fid);
    nowfont= *which;
  }
} /* lsetfont_ */
 
 

void F77_FUNC(lloadfont,LLOADFONT)(num,fname,len)
/* Loads the font of the specified name.  num returns how many
fonts are laoded after having laoded this one, which also
gives the font index of the request font. */
long *num, len;
char *fname;
{
  char *cname;

  debug("Loading.\n");
  debug2("len= %ld\n",len);
  debug2("num= %ld\n",num);
  debug2("*num= %ld\n",*num);
  if( howmanyfonts == maximum_number_fonts ) {
    error("Unable to load font: font list full.");
    *num=0;
    return;
  }
  /* Null terminate the fortran string. */
  cname= (char *) malloc( sizeof(char) *(len+1) );
  if( cname==NULL ) {
    error("Unable to allocate memory for font name string.");
    *num=0;
    return;
  } 
  debug("Memcpy'ing.\n");
  memcpy(cname,fname, sizeof(char)*len );
  cname[len] =0;
  debug2("Loading font %s.\n",cname);
  lfont[howmanyfonts]= XLoadQueryFont(display,cname);
  debug("Yay!\n");
  howmanyfonts++;
  debug2("Setting num to %d.\n",howmanyfonts);
  *num= (long) howmanyfonts;
  debug("Freeing.\n");
  free(cname);
  debug("Setting.\n");
  lsetfont_(num);
  debug("Leaving.\n");
} /* lloadfont_ */  


int lalign=BLEFT;

void F77_FUNC(laligntext,LALIGNTEXT)(code)
/* Picks how the CP will determine text positioning. 
Codes are same as for EZLORG */
long *code;
{
  switch( *code ) {
    case 1: lalign= BLEFT;  	break;
    case 2: lalign= BCENTRE;  	break;  /* Damn Brits! */
    case 3: lalign= BRIGHT;  	break;
    case 4: lalign= MLEFT;	break;
    case 5: lalign= MCENTRE;	break;
    case 6: lalign= MRIGHT;	break;
    case 7: lalign= TLEFT;	break;
    case 8: lalign= TCENTRE;	break;
    case 9: lalign= TRIGHT;	break;
    default: 
       fprintf(stderr,"leolib: Ignoring bad text alignment code: %d\n",code );
  }
} /* laligntext_ */


float langle=0;

void F77_FUNC(ltextangle,LTEXTANGLE)(ang)
/* Sets the text drawing angle.  ang=0 is normal left to
right.  ang=90 draws up.  All angles are allowed.  Must be passed
as fortran type "REAL" */
float *ang;
{
  langle= *ang;
} /* ltextangle_ */


void F77_FUNC(lprint,LPRINT)(text, len)
/* Places the text on the screen at the CP.  
Note: len does not need to be passed from fortran.
(Does not move the CP).  Use lmoveto_, laligntext_ and ltextangle_ 
to change how and where text is displayed. */
char *text;
long int len;
{
  char *ctext;

  if( nowfont==0 ) {
    error("Cannot write text: no fonts loaded.");
    return;
  }
  ctext= (char *) malloc( sizeof(char) *(len+1) );
  if( ctext==NULL ) {
    error("Unable to allocate memory for printed string.");
  } else {
    /* C strings must be null terminated, so we have to make a new string
       with extra space for the terminator, and put it there. */
    memcpy(ctext,text, sizeof(char)*len );
    ctext[len] =0;
    XRotDrawAlignedString(display, lfont[nowfont-1], langle,
  	draw, usergc, (int)CPx, (int)CPy, ctext, lalign );
  }
  free(ctext);
}  /* lprint_ */
 


void losefonts()
/* Unloads the fonts during ldie_() */
{
  int i;

  for(i=0; i<howmanyfonts; i++) 
    XUnloadFont(display,(lfont[i])->fid);
}  /* losefonts */
