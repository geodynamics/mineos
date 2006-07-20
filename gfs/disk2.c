/* this is a modification of Paul Henkarts diskio
 *    The following entry points are in this c program.
 * GETFIL(mode, lun, name, istat)   assigns disk files and unit numbers
 * FREFIL(lun)    releases unit
 * PODISCB(lun, mode, nbyte)    positions lun to byte nbyte
 * RDDISCB(lun, buffer, nbytes, istat)   reads nbytes bytes from disk unit lun
 * WRDISCB(lun, buffer, nbytes,  istat)    writes nbytes bytes to disk unit lun
 *
****   NOTE  ****   lun is an index to an array of file descriptors within this
                   subroutine, thus any I/O must be done through this subroutine
                   (since the other I/O doesn't have the file descriptor!)

   Call GETFIL(MODE, LUN, NAME, ISTAT)
 
      GETFIL KEEPS TRACK OF, AND OPTIONALLY ASSIGNS, DISK FILE UNIT NUMBERS.
   THE PURPOSE IS TO ASSIGN UNIT NUMBERS TO SUBROUTINES AS THEY ARE NEEDED,
   RATHER THAN EACH SUBROUTINE ASSIGNING A PARTICULAR NUMBER FOR A PARTICULAR
   TASK, AND THUS NO OTHER ROUTINES BEING ABLE TO USE THAT UNIT NUMBER EVEN
   THOUGH THE ORIGINAL TASK IS DONE.  MOST FORTRANS ALSO HAVE A LIMIT AS TO
   THE LARGEST UNIT NUMBER ALLOWED, THUS GETFIL OPTIMIZES THE TOTAL NUMBER OF UNIT
   NUMBERS USED IN A COMPUTER RUN.  FURTHER MOTIVATION FOR USE OF GETFIL IS THAT
   SYSTEM CONFIGURATIONS ALLOW CERTAIN UNIT NUMBERS TO BE USED FOR SYSTEM
   SOFTWARE AND THE POOR PROGRAMMER HAS TO KEEP TRACK OF WHAT THE SYSTEM USES
   (such as stdin=0, stdout=1, errout=2).
     GETFIL WILL FIND AN UNUSED UNIT NUMBER AND RETURN IT'S NUMBER, AS WELL AS
   ASSIGNING A DISK FILE TO THE UNIT NUMBER.
   ARGUMENTS:
    MODE   - THE TYPE OF DISK ASSIGNMENT TO MAKE. INTEGER*4
           =3,  FINDS A FREE UNIT NUMBER AND CREATES THE FILE GIVEN IN NAME TO
                THE UNIT.  NAME MUST BE GIVEN.
           =4,  Finds a free unit number and opens the existing file name. NAME
                must be given.  The file is opened for reading and writing
                unless permission is denied, in which case the file is opened
                for reading only.
    LUN     - THE FILE UNIT NUMBER. INTEGER*4, lun is set by getfil
    NAME    - A CHARACTER FILE NAME 
    ISTAT   - The return status of the file assignment.
            =0, File opened properly.
            =-1, TOO MANY FILES ALREADY EXIST, UNIT NUMBER NOT RESERVED.
            =-2, LUN IS ALREADY ASSIGNED.

  CALL FREFIL( LUN )
  Frefil releases and closes  the file
  associated with lun.  The file must have been assigned via GETFIL. 

  arguments:
    lun   - The logical unit number of the file



  CALL PODISCB( LUN, MODE, NBYTE )
  PODISC positions and open the disc file associated with lun.  The positioning
may be to an absolute address or relative to the current file pointer.
The first address is 0, the second adress is 1, etc.
  ARGUMENTS:
    LUN  - The unit number of the file to be positioned.
    MODE - The mode of positioning.
         =1, The file is positioned to the ABSOLUTE word address.
         =2, The file is positioned nwrd RELATIVE to the current file pointer.
    NBYTE - The byte number to postion to. 


  CALL RDDISCB( LUN, BUFFER, NBYTES, ISTAT )
  RDDISCB reads nbytes from the disc file associated with the file on
unit lun.  
  ARGUMENTS:
    LUN    - The logical unit number of the file to be read.
    BUFFER - The array to receive the results of the read.  Buffer must be at
             laest nwrds long.
    NBYTES - The number of bytes to read into buffer.
    ISTAT  - The return status of the read.
           >0, ISTAT words/bytes were read into buffer (No problems).
           =-1,  An end of file was detected.
           <-1, A problem occurred.

  CALL WRDISCB( LUN, BUFFER, NBYTES)
  WRDISCB write nbytes bytes from buffer to disc file associated with lun.
  ARGUMENTS:
   LUN    - The logical unit number of the file to write to.
   BUFFER - The array in memory to write to disc.
   NBYTES  - The number of bytes to write to disc.
   istat  - the number of bytes written


*/

#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include "config.h"


#define   MAXFDS    40  /* the most files allowed in UNIX */
#define   PMODE     0775 /* read, write, execute for owner, read and exec for group */
void mknamc( nam )
	char 	*nam;
{
	while( *nam != ' ' && *nam != '\0' ) *nam++;  /* find a blank or the end */
        *nam = '\0';
}

/* 0 is stdin
   1 is stdout
   2 is stderr
   5 is fortran reader
   6 is fortran printer
*/
static    int       fd[MAXFDS] = {0, 1, 2, -1, -1, 5, 6, -1, -1, -1,
                               -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
                               -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
                               -1, -1, -1, -1, -1, -1, -1, -1, -1, -1};
static    int       reserved[MAXFDS] = {0, 1, 2, -1, -1, 5, 6, -1, -1, -1,
                               -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
                               -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
                               -1, -1, -1, -1, -1, -1, -1, -1, -1, -1};
          off_t    offset;  /* the number of bytes to move relative to the origin */
          int       origin;
          char      fname[80];
          int       nbytes;
          int       status;
          int       i;

F77_FUNC(ggetfil,GGETFIL)(mode, lun, name, istat)
          int       *mode;
          int       *lun;
          char      *name;
          int       *istat;

{
      *istat = 0;
      if(*mode > 0 ) { 
          /* find a free unit by searching fd for a -1 */
           *lun = -1;
           for (i = MAXFDS-1; fd[i] == -1 && i > 0; i--)  *lun = i;
           if ( *lun == -1 ){
               if( fd[3] == -1 ) *lun = 3;
                   else if( fd[4] == -1 ) *lun = 4 ;
               if(  *lun == -1) {
                   printf(" ***  ERROR  ***  Too many units for UNIX (%d max).\n",MAXFDS);
                   *istat = -1;
                   exit(0) ; }
                }
           mknamc( name );   /* make sure the name terminates with a NULL */
           if( *mode == 3 ) {
                status = creat( name, PMODE );   /* open with read and write privileges */
                close(status);
                status = open(name,2);  }
           if( *mode == 4 ) { status = open( name, 2);
                if( status == -1 )  status = open( name, 0);  /* open it for read only if read and write fails */
                if( status == -1 ) status = open( name, 1); }  /* open for write only if read only failed */
           if( status != -1 ) {  /* if it created successfully, then carry on */
                fd[*lun] = status ;  /* create returns the file desriptor  */
                *istat = 0;  /* tell the caller that everything is ok */
                return ;  }
           else  {   /* create didn't create! */
                    perror("getfil");
                    perror(name);
                    *istat = status ;
                    return ;  }
           }
}


F77_FUNC(gfrefil,GFREFIL)( lun )
        int    *lun;

{

            status = close( fd[*lun] );
            fd[*lun] = -1;
            return;
}


F77_FUNC(gfilsiz,GFILSIZ)(name, bsize)
          char      *name;
          int       *bsize;
{
      struct stat stbuf;

      mknamc( name );   /* make sure the name terminates with a NULL */
      stat(name, &stbuf);
      *bsize = stbuf.st_size;
      return;
}


F77_FUNC(gpodiscb,GPODISCB)( lun,  addres)
      int     *lun;
      int     *addres;

{
      offset = *addres ;  /* the addres was given in units of bytes */
      origin = 0;  /* preset to origin of the beginning of the file */
      lseek( fd[*lun], offset, origin) ;
      return;
}


F77_FUNC(grddiscb,GRDDISCB)(lun, buffer, n, istat)
       int     *lun;
       int     *buffer;
       int     *n;
       int     *istat;

{
       nbytes = *n ; 
       status = read( fd[*lun], buffer, nbytes);
       if( status < 0 ) {
              printf(" ***  ERROR  ***  disc file read error on unit %d, status = %d\n",*lun,status);
              perror("rddisc"); }
       *istat = status;
       if ( *istat == 0 ) *istat = -1 ;  /* istat=-1 means end of file  */
       return;
}

F77_FUNC(gwrdiscb,GWRDISCB)(lun, buffer, n, istat)
       int     *lun;
       int     *buffer;
       int     *n;
       int     *istat;

{
       nbytes = *n ;
       status = write( fd[*lun], buffer, nbytes);
       if( status != nbytes ) {
             printf(" ***  ERROR  ***  disc file write error on unit %d, status = %d\n",*lun,status);
             perror("wrdisc"); }
       *istat = status;
       return;
}


/* end */

