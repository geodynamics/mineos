c*** gfs untility for listing gfs files
      common/wkXXXX/hed(10000)
      logical search
      character*256 name,arg
      data lines/-1/
      search=.false.
      name=' '
      numarg=iargc()
      if(numarg.lt.1) then
        print *,' '
        print *,' usage: list [filename]  [-s]  [-lxx]'
        print *,' '
        stop
      end if
      do 1 i=1,numarg
      call getarg(i,arg)
      if(arg(1:2).eq.'-s') then
         search=.true.
      else if(arg(1:2).eq.'-l') then
         read(arg(3:33),*,err=1) lines
      else
         name=arg
      end if
    1 continue
    5 itype=-1
      call gfs_open(1,name,itype,iret)
      if(iret.lt.0) stop
  900 format(i6,' entries in this file')
      if(lines.eq.-1) lines=iret
      if(.not.search) then
        print 900,iret
        do 10 i=1,lines
        call gfs_rwdir(1,hed,i,'r',ierr)
   10   call prhdr(i,hed,itype)
      else
        call gfs_phdef(1) 
        call gfs_sdef(1) 
        kfl=0
        print 900,iret
   20     call gfs_search(1,hed,kfl,0)
          if(kfl.lt.0) goto 30
          call prhdr(kfl,hed,itype)
          if(kfl.lt.lines) goto 20
   30     continue
      end if
      call gfs_close(1)
      stop
      end
