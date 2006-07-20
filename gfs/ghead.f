c*** gfs version of unix head
      common/wkXXXX/hed(10000)
      character*256 name,arg
      data lines/10/
      name=' '
      numarg=iargc()
      if(numarg.lt.1) then
        print *,' '
        print *,' usage: ghead [filename] ([lines])'
        print *,' '
        stop
      end if
      call getarg(1,arg)
      name=arg
      if(numarg.eq.2) then
        call getarg(2,arg)
        read(arg,*,err=1000) lines
        goto 5
 1000   lines=10
      end if
c*** list first 10 headers in file(s)               
    5 itype=-1
      call gfs_open(1,name,itype,iret)
      if(iret.lt.0) stop
      print 900,iret
  900 format(i6,' entries in this file')
      jret=min(iret,lines)
      do i=1,jret
        call gfs_rwdir(1,hed,i,'r',ierr)
        call prhdr(i,hed,itype)
      enddo
      call gfs_close(1)
      stop
      end
