/* My library for doing X-Windows graphics routines from Fortran.

This file just has the main control routines: Create window / destroy it...
*/

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xos.h>
#include <X11/Xatom.h>
#include "rotated.c"

#include <stdio.h>

#include "config.h"

#define NOTDEBUGGING

#ifdef DEBUGGING
#define debug(x)  fprintf(stderr,x)
#define debug2(x,y) fprintf(stderr,x,y)
#else
#define debug(x)  
#define debug2(x,y)
#endif

#define error(x)  fprintf(stderr,"leolib: %s\n",x)

/* These are used as arguments to nearly every Xlib routine, so it saves 
 * routine arguments to declare them global.  If there were 
 * additional source files, they would be declared extern there. */
Display *display=NULL;
int number_windows=0;
int screen_num;
Window win;
Pixmap background;
Drawable draw;
GC usergc, erasegc;
unsigned int window_width, window_height;

#include "ftoc.c"  /* Fortran to C string conversion */
#include "leodraw.c"  /* This has the drawing primitives. */
#include "leoinp.c"  /* This has the input routines. */
#include "leotext.c"  /* This has the text handling routines. */


#ifdef DEBUGGING
void F77_FUNC(lnum,LNUM)(x)
long *x;
{
  printf("x: %ld\n",*x);
} /* lnum_ */
#endif


void F77_FUNC(lfastmode,LFASTMODE)()
/* Sets fast drawing mode, in which drawing calls are made directly
   to the screen, with no backup in case of covering. */
{
  draw= win;
} /* lfastmode_ */


void F77_FUNC(lslowmode,LSLOWMODE)()
/* Sets the slow drawing mode, where drawing calls are done to the
   Background Pixmap, which is updated with an output buffer flush. */
{
  draw= background;
} /* lslowmode_ */


void update_window()
/* Internal to the library -- just copies the draw pixmap into
the window. */
{
  XSetWindowBackgroundPixmap(display,win,background);
  XClearWindow(display,win);
} /* update_window */


void set_backward_colors(gc)
GC *gc;
/* Internal: Sets Whitepixel as foreground, and blackpixel as background. */
{
  XSetBackground(display, *gc, BlackPixel(display,screen_num) );
  XSetForeground(display, *gc, WhitePixel(display,screen_num) );
} /* set_backward_colors */

void set_forward_colors(gc)
GC *gc;
/* Internal: Sets Blackpixel as foreground, and whitepixel as background. */
{
  XSetForeground(display, *gc, BlackPixel(display,screen_num) );
  XSetBackground(display, *gc, WhitePixel(display,screen_num) ); 
} /* set_forward_colors */


void connect_to_display()
/* Internal.  Does about what it says it should */
{
  char *display_name = NULL;

  if( display!=NULL ) /* already connected. */
    return;
  if ( (display=XOpenDisplay(display_name)) == NULL )
  {
    error("Fatal: cannot connect to X server.");
    exit( -1 );
  }
  screen_num = DefaultScreen(display);
} /* connect_to_display */


void F77_FUNC(linitfancywindow,LINITFANCYWINDOW)(fwidth,fheight,origx,origy,fwindow_name,ficon_name,wlen,ilen)
/* Here, we blindly assume that the window manager will give us a window
of the size we demanded.  (We specify minimum, maximum, and suggested size of
the window all to be the size we want it.)  This works under openwindows. 
A pixmap is created to store everything that we draw, so that we don't
have to redraw it everytime there's an expose.  This is at the cost of 
some server memory.
*/
long *fwidth, *fheight, *origx, *origy, wlen, ilen;
char *fwindow_name,*ficon_name;
{
	int x, y; 	/* window position */
	unsigned int border_width = 4;	/* four pixels */
	unsigned int display_width, display_height;
	unsigned int icon_width, icon_height;
	char *window_name;
	char *icon_name;
	XSizeHints size_hints;
	XIconSize *size_list;
	int count;
	XWMHints wm_hints;
	XClassHint class_hints;
	XTextProperty windowName, iconName;
	XSetWindowAttributes attrib;
	unsigned long attribvaluemask;
	XEvent report;
	XGCValues gcvalues;
	unsigned int width, height;
	long f1, f2;

	width= (unsigned int) *fwidth;
	height= (unsigned int) *fheight;

	if( number_windows == 1 ){
	  error("Ingoring attempt to create second window.");
	  return;
        }
	connect_to_display();

	/* get screen size from display structure macro */
	display_width = DisplayWidth(display, screen_num);
	display_height = DisplayHeight(display, screen_num);

	/* Window's starting position. */
	x = (int) *origx;
	y = (int) *origy;
	 
	/* create window -- this just creates the window data structure.
           This doesn't display it on the screen or anything useful
           like that. */
	win = XCreateSimpleWindow(display, RootWindow(display,screen_num), 
			x, y, width, height, border_width, BlackPixel(display,
			screen_num), WhitePixel(display,screen_num));

	/* Set size hints for window manager.  The window manager may
	 * override these settings.  Note that in a real
	 * application if size or position were set by the user
	 * the flags would be UPosition and USize, and these would
	 * override the window manager's preferences for this window. */
	size_hints.flags = PPosition | PSize | PMinSize | PMaxSize;;
	size_hints.x = x;
	size_hints.y = y;
	size_hints.width = width;
	size_hints.height = height;
	size_hints.min_width = width;
	size_hints.min_height = height;
	size_hints.max_width = width;
	size_hints.max_height = height;
	window_width = width;
	window_height = height;

	window_name= ftocstring(fwindow_name,wlen);
	if( window_name == NULL ) {
	  error("Unable to allocate memory for window name.");
	  window_name="LeoLib Window";
	}
	icon_name= ftocstring(ficon_name,ilen);
	if( window_name == NULL ) {
	  error("Unable to allocate memory for icon name.");
	  icon_name="LeoLib";
	}

	/* These calls store window_name and icon_name into
	 * XTextProperty structures and set their other 
	 * fields properly. */
	if (XStringListToTextProperty(&window_name, 1, &windowName) == 0) {
		error("Fatal: structure allocation for windowName failed" );
		exit(-1);
	}
		
	if (XStringListToTextProperty(&icon_name, 1, &iconName) == 0) {
		error("Fatal: structure allocation for iconName failed" );
		exit(-1);
	}

	wm_hints.initial_state = NormalState;
	wm_hints.input = True;
	wm_hints.flags = StateHint | InputHint;

	/* This tells the window manager how to get resources for
	   defaults on how to handle this window.  (if it wants it) */
	class_hints.res_name = "Leo Library";
	class_hints.res_class = "LeoLib";

	XSetWMProperties(display, win, &windowName, &iconName, 
			NULL, 0, &size_hints, &wm_hints, 
			&class_hints);

	/* Select event types wanted */
	XSelectInput(display, win, ExposureMask | KeyPressMask | 
			ButtonPressMask | StructureNotifyMask);

	/* Display window */
	XMapWindow(display, win);

	/* Now we wait for an expose event for our window.  The window won't
           be on the screen until we get this event.*/
	do {
  	  XNextEvent(display,&report);
        } while( report.type != Expose );

	/* Now we create the pixmap which is where we'll be
	drawing.  Then we copy it into the window. */
	background= XCreatePixmap(display,win, width,
         	height, XDefaultDepth(display,screen_num) );
	lslowmode_();  /* Set up draw. */
	gcvalues.graphics_exposures = False;
	usergc= XCreateGC(display, background, GCGraphicsExposures,
	 &gcvalues);  /* Take all default, but turn off graphics exposure 
			 events. */
        set_forward_colors(&usergc);
	f1=1;  f2=0;
	llinestyle_(&f1, &f2, &f2, &f2);  /* Simple solid width 1 line. */
	erasegc= XCreateGC(display, background, GCGraphicsExposures,
	  &gcvalues);  
        set_backward_colors(&erasegc);

        lclearwindow_();
	update_window();  

	get_colors();

	number_windows=1;

	/* Free allocated memory. */
	free(window_name);
	free(icon_name);
} /* linitfancywindow_ */


void F77_FUNC(linitializewindow,LINITIALIZEWINDOW)(fwidth, fheight)
/* Calls linitfancy window, with some default settings. */
long *fwidth, *fheight;
{
  long origx, origy;
  char *w = "LeoLib Window",
       *i = "LeoLib";

  origx=0;
  origy=0;
  linitfancywindow_(fwidth,fheight,&origx,&origy,w,i,strlen(w),strlen(i));
} /* linitializewindow_ */



void F77_FUNC(lscreentype,LSCREENTYPE)(width,height,color,planes)
/* Determines what type of display is available -- how big, is it color, how
many bit-planes */
long *width, *height, *color, *planes;
{
  int default_depth;
  XVisualInfo visual_info;
  Visual *default_visual;
  int i;
  
  connect_to_display();
  *width= DisplayWidth(display,screen_num);
  *height= DisplayHeight(display,screen_num);

  /* Now determine if there's color or not...  Code portion taken
     from O'Reilly's setcolor routines. */
  default_visual = DefaultVisual(display, screen_num);
  *planes = DefaultDepth(display, screen_num);
  if (*planes == 1) {
    /* must be StaticGray */
    *color=0;
  } else {
    i=5;
    while (!XMatchVisualInfo(display, screen_num, *planes, i--, &visual_info))
      ;
    /*  found a %s class visual at default_depth.\n", visual_class[++i]); */
    i++;
    if (i < 2) {
      /* No color visual available at default_depth.  */
      *color=0;
    } else {
      *color=1;
    }
  }
} /* lscreentype */
 
 

void F77_FUNC(lreset,LRESET)()
/* Reset's the drawing mode to something sensible */
{
  long l1=1, l0=0;
  float f0=0.0;

  XCopyGC(display,erasegc,0xffffffff,usergc);
  set_forward_colors(&usergc);
  laligntext_(&l1);
  ltextangle_(&f0);
  lmoveto_(&l0,&l0);
  lsetfont_(&l0);
} /* lreset_ */


void F77_FUNC(ldie,LDIE)()
/* Kills the window and unloads all the loaded stuff. */
{
  XFreePixmap(display,background);
  XFreeGC(display,erasegc);
  XFreeGC(display,usergc);
  losefonts();
  XCloseDisplay(display);
  number_windows--;
} /* ldie_ */


void F77_FUNC(ldoit,LDOIT)()
/* Flushes the output buffer. */
{
  if( draw == background ) /* i.e. we're in slow mode. */
    update_window();
  XFlush(display);
} /* ldoit */


void F77_FUNC(lfastscreendump,LFASTSCREENDUMP)(fname, len)
/* Dumps the screen contents to the specified file. 
Uses Images to run faster, but doesn't always work. */
char *fname;
long len;
{
  char *cname, line[80];
  FILE *out;
  unsigned int x, y;
  unsigned long col;
  long cnt;
  int outbyte;
  XImage *xi;
  int c;

  debug("Dumping.\n");
  /* First null-terminate the fortran string. */
  cname= ftocstring(fname,len);
  if( cname == NULL ) {
    error("Unable to allocate memory for PostScript filename.");
    return;
  }

  debug("Getting image.\n");
  debug("key");  scanf("%d\n",&c);
  debug("Goodbye, cruel world.\n");
  /* Copies the window contents to client-side memory. */
  xi= XGetImage(display, background, 0, 0, window_width, window_height, 1,
                XYPixmap);
  debug("Got it.  (maybe)\n");
  debug("key");  scanf("%d\n",&c);  debug("ack");
  if( xi== NULL ) {
    error("Unable to copy screen contents for screendump.");
    return;
  }
  debug("Got image.\n");
  debug("key");  scanf("%d\n",&c); debug("ACK");

  /* Output the PostScript header. */
  out= fopen(cname,"wt");
  if( out==NULL ) {
    error("Unable to open PostScript screendump file.");
    return;
  }
  fprintf(out,"%%! PS-Adobe\n");
  fprintf(out,"%% LeoLib Screen dump output.\n");
  fprintf(out,"%% Black and White.\n\n");
  fprintf(out,"/str %ld string def\n",(window_width+7)/8);
  fprintf(out,"/width %ld def\n",window_width);
  fprintf(out,"/height %ld def\n\n",window_height);
  fprintf(out,"/doit {\n");
  fprintf(out,"  %% Draws image in a unit square in user-space \n");
  fprintf(out,"  %ld %ld 1 [ %ld 0 0 %ld 0 %ld ]\n",
	    window_width,window_height,window_width,-window_height,
	    window_height );
  fprintf(out,"  { currentfile str readhexstring pop }\n");
  fprintf(out,"  image\n");
  fprintf(out,"} def\n\n");
  fprintf(out,"72 72 translate\n");
  fprintf(out,"500 500 scale\n");
  fprintf(out,"doit\n");

  cnt=0;
  for(y=0; y<window_height; y++) {
    outbyte=0;
    for(x=0; x<window_width; x++) {
      col= XGetPixel(xi,x,y);
      /* !! Here we're just doing B&W -- black unless it's white. */
      if( col != lcolor[0] ) 
	outbyte= (outbyte << 1);
      else
	outbyte= (outbyte << 1) +1;
      if( x%8 == 7 ) {
        fprintf(out,"%02X",outbyte);
        outbyte=0;
	cnt++;
        if( cnt%38 == 37 )
          fprintf(out,"\n");
      }
    } /* x loop */
    if( x %8 !=0 ) {
      /* Now we have to output the reset of the byte. */
      for(; x%8; x++) 
	outbyte= (outbyte << 1) +1;
      fprintf(out,"%02X",outbyte);
      cnt++;
      if( cnt%38 == 37 )
        fprintf(out,"\n");
    }
  } /* y loop */
  fprintf(out,"\nshowpage\n");
  fclose(out);
  free(cname);
  debug("Done dumping.\n");
} /* lscreendump_() */


void F77_FUNC(lscreendump,LSCREENDUMP)(fname, len)
/* Dumps the screen contents to the specified file. */
char *fname;
long len;
{
  char *cname, line[80];
  Pixmap bitmap;
  FILE *in, *out;
  int ix, ox, col;
  long num;

  printf("Dumping.\n");
  /* First null-terminate the fortran string. */
  cname= (char *) malloc( sizeof(char) *(len+1) );
  if( cname==NULL ) {
    error("Unable to allocate memory for filename string.");
    return;
  }
  memcpy(cname,fname, sizeof(char)*len);
  cname[len]=0;

  /* Now we get a bitmap from the background pixmap. */
  bitmap= XCreatePixmap(display,win,window_width,window_height,1);
  XCopyPlane(display,draw,bitmap,erasegc,0,0,window_width,window_height,0,0,1);
  printf("Prepare to die.\n");
  switch( 
    XWriteBitmapFile(display,"leolib.tmp.dump",bitmap,window_width,window_height,-1,-1)
  ) {
    case BitmapSuccess: break;
    case BitmapOpenFailed: error("Unable to open temporary bitmap file.");
			   return;
    case BitmapNoMemory: error("Insufficient memory for temporary bitmap file.");
			   return;
  }
  printf("Didn't die.\n");
  XFreePixmap(display,bitmap);

  /* Output the PostScript header. */
  out= fopen(cname,"wt");
  if( out==NULL ) {
    error("Unable to open PostScript screendump file.");
    return;
  }
  fprintf(out,"%%! PS-Adobe\n");
  fprintf(out,"%% LeoLib Screen dump output.\n");
  fprintf(out,"%% Black and White.\n\n");
  fprintf(out,"/str %ld string def\n",window_width/8);
  fprintf(out,"/width %ld def\n",window_width);
  fprintf(out,"/height %ld def\n\n",window_height);
  fprintf(out,"/doit {\n");
  fprintf(out,"  %% Draws image in a unit square in user-space \n");
  fprintf(out,"  %ld %ld 1 [ %ld 0 0 %ld 0 %ld ]\n",
	    window_width,window_height,window_width,-window_height,
	    window_height );
  fprintf(out,"  { currentfile str readhexstring pop }\n");
  fprintf(out,"  image\n");
  fprintf(out,"} def\n\n");
  fprintf(out,"72 72 translate\n");
  fprintf(out,"500 500 scale\n");
  fprintf(out,"doit\n");

  /* Now, we copy the data from leolib.tmp.dump */
  in= fopen("leolib.tmp.dump","rt");
  if( in==NULL ) {
    error("Read error opening leolib.tmp.dump");
    return;
  }
  /* Ignore all lines until one starts with "static" */
  do {
    fgets(line,80,in);
    printf("Skipping: %s\n",line);
    line[6]=0;  /* Terminate for easy comparison. */
  } while( strcmp("static",line) );

  /* Now the data start streaming in, looking like this:
   0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
    */
  for(num=0; num< window_width * window_height; num++ ) {
    fscanf(in," 0x%x,",&ix);
    /* Now we invert the bit order. */
    ox= ((ix & 0x80) >> 7)
      + ((ix & 0x40) >> 5)
      + ((ix & 0x20) >> 3)
      + ((ix & 0x10) >> 1)
      + ((ix & 0x08) << 1)
      + ((ix & 0x04) << 3)
      + ((ix & 0x02) << 5)
      + ((ix & 0x01) << 7) ;
    fprintf(out,"%02X",ox);
    if( num%38 == 37 )
      fprintf(out,"\n");
  }
  fclose(in);
  fprintf(out,"\nshowpage\n");
  fclose(out);
  system("rm leolib.tmp.dump");
} /* lscreendump_() */
