/* My X-windows graphics interface.
These have the drawing primitive routines in them. */


void F77_FUNC(lclearwindow,LCLEARWINDOW)()
/* Clears the Window. */
{
  XFillRectangle(display,draw,erasegc,0,0,window_width,window_height);
} /* lclearwindow_ */


void F77_FUNC(lclearpartwindow,LCLEARPARTWINDOW)(x, y, width, height)
long *x, *y, *width, *height;
/* Clears part of the Window, in a rectangle, from 
x to x+width, and y to y+height. */
{
  XFillRectangle(display,draw,erasegc, (int)*x, (int)*y,
		 (unsigned int)*width, (unsigned int)*height);
} /* lclearpartwindow_ */


void F77_FUNC(llinestyle,LLINESTYLE)(width, dash, join, cap)
/* Selects a linewidth, cap, and a line join, and whether or not to dash.
Width: 0: fast & sloppy
       >= 1: that many pixels wide
Dash: 0: No Dashing
      >=1: How many spaces between dashes
Join: 0: JoinBevel
      1: JoinRound
      2: JoinMiter (Pointy)
Cap: 0: CapButt
     1: CapNotLast
     2: CapRound (Extends)
     3: CapProjecting (Extends)
*/
long *width, *dash, *join, *cap;
{
  int c, j;
  char d[2];

  switch(*cap) {
    case 1: c= CapNotLast;	break;
    case 2: c= CapRound;	break;
    default: c= CapButt;
  }
  switch(*join) {
    case 1: j= JoinRound;	break;
    case 2: j= JoinMiter;	break;
    default: j= JoinBevel;
  }
  if( !(*dash) ) {
    XSetLineAttributes(display,usergc, (unsigned int)*width,
       LineSolid,c,j);
  } else {
    XSetLineAttributes(display,usergc, (unsigned int)*width,
       LineOnOffDash,c,j);
    d[0]= *dash;
    d[1]= *dash;
    XSetDashes(display,usergc,0,d,2);
  }
} /* llinestyle_ */


void F77_FUNC(ldrawhollowbox,LDRAWHOLLOWBOX)(x,y,width,height)
long *x, *y, *width, *height;
/* Draws the outline of a rectangle. */
{
  XDrawRectangle(display,draw,usergc, (int) *x, (int) *y,
		(unsigned int) *width, (unsigned int) *height);
} /* ldrawhollowbox_ */


void F77_FUNC(ldrawfilledbox,LDRAWFILLEDBOX)(x,y,width,height)
long *x, *y, *width, *height;
/* Draws and fills in the rectangle. */
{
  XFillRectangle(display,draw,usergc, (int) *x, (int) *y, 
	(unsigned int) *width, (unsigned int) *height);
} /* ldrawfilledbox_ */


void F77_FUNC(ldrawhollowcircle,LDRAWHOLLOWCIRCLE)(x,y,radius)
long *x, *y, *radius;
{
  XDrawArc(display,draw,usergc, (int)(*x-*radius), (int)(*y-*radius), 
		(unsigned int)(*radius)*2, (unsigned int)(*radius)*2, 
  		0,(int)360*64 );
} /* ldrawhollowcircle_ */



void F77_FUNC(ldrawfilledcircle,LDRAWFILLEDCIRCLE)(x,y,radius)
/* radius is the radius*sqrt(2) */
long *x, *y, *radius;
{
  XFillArc(display,draw,usergc, (int)(*x-*radius), (int)(*y-*radius), 
		(unsigned int)(*radius)*2, (unsigned int)(*radius)*2, 
	   0,(int)360*64 );
} /* ldrawfilledcircle_ */

long CPx=0, CPy=0;  /* The current position of the pointer. */

void F77_FUNC(lmoveto,LMOVETO)(x,y)
long *x, *y;
/* Moves the CP to a new place -- doesn't draw anything. */
{
  CPx=*x;
  CPy=*y;
} /* lmoveto_ */


void F77_FUNC(llineto,LLINETO)(x,y)
long *x, *y;
/* Draws a line from the CP to the specified point. */
{
  XDrawLine(display,draw,usergc,(int)CPx,(int)CPy,
	(int)*x, (int)*y);
  CPx=*x;
  CPy=*y;
} /* llineto_ */


void F77_FUNC(lline,LLINE)(x1, y1, x2, y2)
/* Note, this doesn't move the CP. */
long *x1, *y1, *x2, *y2;
{
  XDrawLine(display,draw,usergc,(int) *x1,(int) *y1,
  	(int) *x2, (int) *y2);
} /* lline_ */


long maxlines=0;  /* The maximum number of lines drawable 
		     at once. */

void F77_FUNC(llines,LLINES)(x,y,num)
/* Draws many lines.  x, and y are arrays of values,
and n says how big the array is.  Maximum n: 32767.
*/
long *x, *y, *num;
{
  XPoint *xp;
  int done;
  long i, offset, nxp;

  /* We must break the array up if it is too big. */
  if( maxlines==0 ) {
    /* We haven't checked the max yet.  Do so. */
    maxlines= (XMaxRequestSize(display)-3) /2 -1;
  }
  offset=0;
  if(*num > maxlines)
    nxp= maxlines;
  else
    nxp= *num;
  xp= (XPoint *) malloc(sizeof(XPoint) * nxp);
  if( xp==NULL ) {
    error("Insufficient memory for multiline draw.  Drawing separately.");
    for(i=0; i<*num-1; i++) 
      XDrawLine(display,draw,usergc,(int)x[i],(int)y[i],
	(int)x[i+1],(int)y[i+1]);
    return;
  }
  done=0;
  do { 
    for(i=0; i<nxp; i++) {
      xp[i].x= (int)x[i+offset];
      xp[i].y= (int)y[i+offset];
    }
    XDrawLines(display,draw,usergc,xp,(int)nxp,CoordModeOrigin);
    if( i+offset == *num ) {
      done=1;
    } else {
      offset+= i-2;  /* The last line segment is redrawn so
                  that we'll get the join right at the array split. */
      if( offset+nxp > *num ) {
	nxp= *num -offset;
      }
    }
  } while( !done );
  free(xp);
} /* llines_ */


void F77_FUNC(lbitblt,LBITBLT)(x1,y1,width,height,x2,y2)
/* Copies the contents of the box specified by
   x1,y1,width,height to x2,y2.  Note: it uses the
   user's choice of a raster operation. */
long *x1, *y1, *width, *height, *x2, *y2;
{
  XCopyArea(display, draw, draw, usergc, (int) *x1, (int) *y1,
	(unsigned int) *width, (unsigned int) *height, 
	(int) *x2, (int) *y2 );
} /* lbitblt_ */


void F77_FUNC(lclipbox,LCLIPBOX)(x,y,width,height)
/* Sets this as the current clipping box, and turns
   clipping on. */
long *x, *y, *width, *height;
{
  XRectangle R;

  R.x=(short) *x;
  R.y=(short) *y;
  R.width=(unsigned int) *width;
  R.height=(unsigned int) *height;
  XSetClipRectangles(display,usergc,0,0,&R,1,Unsorted);
} /* lclipbox_ */


void F77_FUNC(lnoclip,LNOCLIP)()
/* Turns off clipping. */
{
  XSetClipMask(display,usergc,None);
} /* lnoclip_ */

  
void F77_FUNC(lsetrasterops,LSETRASTEROPS)(code)
long *code;
/* For RasterOps Codes, see page 140 of XLib Programming Manual (Vol.1) 
   or page 689 of Volume 2.  
   Note: usually you just want code=3. 
   Note: These codes are the same as those used in the apollo graphics
	 primitives. */
{
  XSetFunction(display,usergc,(int) *code);
} /* lsetrasterops_ */


#include "leolib.col"  /* defines static char *color_names[] 
			  and number_colors */
unsigned long lcolor[number_colors];  /* The pixel values. */

void setBWcolors()
/* internal. */
{
  int i;
  lcolor[0]= WhitePixel(display, screen_num);
  lcolor[1]= BlackPixel(display, screen_num);
  for(i=2; i<number_colors; i++) 
    lcolor[i]=lcolor[1];
} /* setBWcolors */


int get_colors()
/* Internal -- taken from O'Reilly. 
Returns 1 on success (found colors) or 0 if had to default to B&W.
If it defaults to B&W, then color 0 is white, and all others are
black.
*/
{
        int default_depth;
        Visual *default_visual;
	XColor exact_def;
	unsigned int exactgrn, exactred, exactblu;
	Colormap default_cmap;
	int ncolors = 0;
	int i;
	XVisualInfo visual_info;
	char e[160];
	
	/* Try to allocate colors for PseudoColor, TrueColor, 
	 * DirectColor, and StaticColor.  Use black and white
	 * for StaticGray and GrayScale */
        default_visual = DefaultVisual(display, screen_num);
	default_depth = DefaultDepth(display, screen_num);
	default_cmap   = DefaultColormap(display, screen_num);
	if (default_depth == 1) {
		/* must be StaticGray, We give up, and use white for
		   color 0, and black for all others. */
		setBWcolors();
		return(0);
	}

	i=5;
	while (!XMatchVisualInfo(display, screen_num, default_depth, /* visual class */i--, &visual_info))
		;
	/*  found a %s class visual at default_depth.\n", 
	    visual_class[++i]); */
 	i++;
	if (i < 2) {
		/* No color visual available at default_depth.  */
		setBWcolors();
		return(0);
	}

	/* otherwise, got a color visual at default_depth */

	for (i = 0; i < number_colors; i++) {
		if (!XParseColor (display, default_cmap, color_names[i], &exact_def)) {
			sprintf(e,"color name %s not in database.  Using B&W.",color_names[i]);
			error(e);
			setBWcolors();
			return(0);
		}
		exactred=exact_def.red;
		exactgrn=exact_def.green;
		exactblu=exact_def.blue;
   		if (!XAllocColor(display, default_cmap, &exact_def)) {
			error("can't allocate color: all colorcells allocated and no matching cell found.  Using B&W.");
			setBWcolors();
			return(0);
		}
		if( (exact_def.green!=exactgrn) || (exact_def.red!=exactred) || (exact_def.blue!=exactblu) ) {
		  sprintf(e,"warning: Screen implementation of color %s differs from database.", color_names[i]);
		  error(e);
		}
		lcolor[i] = exact_def.pixel;
		ncolors++;
	}
	return(1);
} /* get_colors */


void F77_FUNC(lsetcolor,LSETCOLOR)(index)
/* Selects color number c as the current foreground drawing color. */
long *index;
{
  XSetForeground(display,usergc, lcolor[*index]);
} /* lsetcolor */
