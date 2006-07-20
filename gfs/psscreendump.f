***************************************************************
*
*  This is a subroutine to dump some stuff from a X screen 
*  (LeoLib) to a PostScript file.
*  It is simply a system call to a couple of X-routines
*  When this subroutine is called the cursor changes to a +
*  then the user has to click on the window he wants.
*  There will be on beep when the dump starts and two when
*  it ends and then some time lag while the files is
*  converted to PostScript.
*
***************************************************************
      subroutine psscreendump(filename)
      character*(*) filename
      character*200 cmd

      cmd= 'rm ' // filename
      call system(cmd)
      cmd= 'xwd | xpr -device ps > ' // filename
      call system(cmd)

      return
      end
