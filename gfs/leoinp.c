/* My library for doing X-Windows graphics routines from Fortran.
   These are just the input related routines. */

/* Interfacing ideas:
XQueryPointer -- find out present location. (Why?)
*/

void F77_FUNC(lwaitforbutton,LWAITFORBUTTON)(but, x, y, mod)
/* Responds to either button presses or releases.
Returns the position where the
button was pressed within the window, and which
button was pressed, and which modifier keys were held.  
(The button must be pressed within the window, else it's 
none of our business.) 
Returns a positive value for button press, and a negative value
for buttonrelease.  Buttons are numbered from Left to Right, starting at
1.  */
long *but, *x, *y, *mod;
{
  XEvent report;

  XSelectInput(display, win, ButtonPressMask | ButtonReleaseMask);
				 
  do {
    XNextEvent(display, &report);
  } while ( (report.type!=ButtonPress) && (report.type!=ButtonRelease) );
  /* Note: all other events are ignored. */
  *x= report.xbutton.x;
  *y= report.xbutton.y;
  switch( report.xbutton.button ) {
    case Button1: *but=1;	break;
    case Button2: *but=2;	break;
    case Button3: *but=3;	break;
    case Button4: *but=4;	break;
    case Button5: *but=5;	break;
    default: *but=6;		break;
  }
  *mod= report.xbutton.state;
  if( report.type==ButtonRelease )
    *but= 0- *but;
} /* lwaitforbutton_ */


void F77_FUNC(lwaitforbuttonup,LWAITFORBUTTONUP)(but, x, y, mod)
/* Same as lwaitforbutton_ but only responds to button releases. */
long *but, *x, *y, *mod;
{
  do {
    lwaitforbutton_(but,x,y,mod);
  } while( *but > 0 );
  *but= 0-*but;  /* Return a positive value. */
} /* lwaitforbuttonup_ */
  

void F77_FUNC(lwaitforbuttondown,LWAITFORBUTTONDOWN)(but, x, y, mod)
/* Same as lwaitforbutton_ but only responds to button presses. */
long *but, *x, *y, *mod;
{
  do {
    lwaitforbutton_(but,x,y,mod);
  } while( *but < 0 );
} /* lwaitforbuttondown_ */
  

void F77_FUNC(lmodmasks,LMODMASKS)(shift,lock,control, mod1,mod2,mod3,mod4,mod5, 
     but1,but2,but3,but4,but5)
/* Returns the bitmask values for each of these things.  These
   give the bit positions for the mod field of lwaitforbutton_ */
long *shift, *lock, *control, *mod1, *mod2, *mod3, *mod4, *mod5, 
	*but1, *but2, *but3, *but4, *but5;
{
  *shift= (long) ShiftMask;
  *lock= (long) LockMask;
  *control= (long) ControlMask;
  *mod1= (long) Mod1Mask;
  *mod2= (long) Mod2Mask;
  *mod3= (long) Mod3Mask;
  *mod4= (long) Mod4Mask;
  *mod5= (long) Mod5Mask;
  *but1= (long) Button1Mask;
  *but2= (long) Button2Mask;
  *but3= (long) Button3Mask;
  *but4= (long) Button4Mask;
  *but5= (long) Button5Mask;
} /* lmodmasks_ */


void F77_FUNC(lwarp,LWARP)(x,y)
long *x, *y;
/* Suddenly moves the mouse pointer to the point x,y within our window. 
   Warning: This is very confusing to the user.  Never use. */
{
  XWarpPointer(display,None,win,0,0,1,1,(int) *x, (int) *y);
} /* lwarp_ */


int silly_cursor_exists=0;
Cursor silly;

void F77_FUNC(lcursor,LCURSOR)(which)
/* Picks which cursor symbol should be put on the screen.  
0 gives the default cursor.
1 Gives the "silly cursor" which is a single black pixel.
For a list of valid values, refer to the include file X11/cursorfont.h,
found in the $OPENWINHOME/include/X11 directory.  For what these things
look like, see Appendix I, in the Xlib reference manual, (volume 2) */
long *which;
{
  Cursor cursor;
  Pixmap dot;
  XColor blk;

  switch( *which ) {
    case 0: XDefineCursor(display,win,None);  /* Inherits parent's. */
	    break;
    case 1: if( !silly_cursor_exists ) {
	      dot= XCreatePixmap(display,win,1,1,1); /* Create a 1x1x1 pixmap */
	      /* We don't care about the contents, since we set both
  	         foreground and background to the same color. */
	      XParseColor(display, DefaultColormap(display,screen_num), 
			  "black", &blk );
	      silly= XCreatePixmapCursor(display,dot,dot,&blk,&blk, 0, 0 );
	      XFreePixmap(display,dot);
	      silly_cursor_exists=1;
            } 
	    XDefineCursor(display,win,silly);
	    break;
    default: /* Predefined. */
             cursor= XCreateFontCursor(display,(unsigned int) *which);
             XDefineCursor(display,win,cursor);
	     break;
  }
} /* lcursor_ */


long user_event_mask = KeyPressMask;
  
long getbit(which)
/* Internal: Gets the bit corresponding to which. */
long which;
{
  long r;
  switch( which ) {
    case 1: r = KeyPressMask;	break;
    case 2: r = KeyReleaseMask;	break;
    case 3: r = ButtonPressMask;	break;
    case 4: r = ButtonReleaseMask;	break;
    case 5: r = PointerMotionMask;	break;
    case 6: r = PointerMotionHintMask;	break;
    default: r=0;  break;
  }
  return(r);
}
/* getbit */ 


void F77_FUNC(leventtypeon,LEVENTTYPEON)(which)
/* Causes these event types to be received by leventwait_ */
long *which;
{
  user_event_mask |= getbit(*which);
} /* leventtypeon_ */


void F77_FUNC(leventtypeoff,LEVENTTYPEOFF)(which)
/* Causes these event types to be ignored by leventwait_ */
long *which;
{
  if( *which == 0 )
    user_event_mask = 0;
  else
    user_event_mask &= (~getbit(*which));
} /* leventtypeoff_ */


void F77_FUNC(leventwait,LEVENTWAIT)(main, x, y, mod, typ)
/* Waits for an event of the selected types, and then returns
information about it.  Main is the main thingy returned.  x, and y are
the position of the pointer when the button is pressed.  mod is the
state of the modifier keys when the event occured */
long *main, *x, *y, *mod, *typ;
{
  XEvent report;
  char k;
  int done=1, howlong;

  if( user_event_mask == 0 ) {
    error("Attempt to wait for event, when no event types selected.");
    return;
  }
  XSelectInput(display,win,user_event_mask); /* Just to make sure.  */

  do {
    XNextEvent(display,&report);
    switch( report.type ) {
      case ButtonPress: /* Trickle through to ButtonRelease */
      case ButtonRelease: 
		if( report.type == ButtonPress )
		  *typ= 3;
		else
		  *typ= 4;
    		switch( report.xbutton.button ) {
  		  case Button1: *main=1;	break;
  		  case Button2: *main=2;	break;
  		  case Button3: *main=3;	break;
  		  case Button4: *main=4;	break;
  		  case Button5: *main=5;	break;
  		  default: *main=6;		break;
  		}
  		if( report.type== ButtonRelease )
  		  *main= 0- *main;
  		*x= report.xbutton.x;
  		*y= report.xbutton.y;
  		*mod= report.xbutton.state;
  		break;
      case MotionNotify:
		*typ= 5;
  		/* main is unset. */
  		*x= report.xbutton.x;
  		*y= report.xbutton.y;
  		*mod= report.xbutton.state;
  		break;
      case KeyPress: /* Trickle through to KeyRelease */
      case KeyRelease:
		if( report.type == KeyPress )
		  *typ= 1;
		else
		  *typ= 2;
  		/* Set main to which key.  Negative for release. */
  		debug("Key.\n");
  		howlong=XLookupString(&report,&k,1,NULL,NULL);
  		if( howlong==0 ) 
  		  done=0; /* This is not a normal key -- probably shift or
  			  something.  keep trying. */
		else
		  done=1;
  		if( report.type==KeyPress )
  		  *main= (long) (k);
  		else
  		  *main= 0- (long) (k);
  		*x= report.xbutton.x;
  		*y= report.xbutton.y;
  		*mod= report.xbutton.state;
  		break;
      default: error("Ignoring unknown event type in leventwait");
  		break;
    }
  } while( !done );
} /* leventwait_ */

void F77_FUNC(lwherecursor,LWHERECURSOR)(x,y,mod)
/* Queries the pointer's location. */
long *x, *y, *mod;
{
  int rx, ry, wx, wy;
  unsigned int m;
  Window r, c;

  XQueryPointer(display,win,&r,&c,&rx,&ry,&wx,&wy,&m);
  *mod=m;
  *x=wx;
  *y=wy;
} /* lwherecursor_ */

