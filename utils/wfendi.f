c================================================================
c Reverse order of bytes for whole .wfdisc relation
c Usage: fwendi ivel ipc INFILE OUTFILE
c ivel - =1, make multiplication by omega, =0 - No
c ipc  - =1, rdseed for PC, add f4 always, =0 - No
c================================================================
      implicit none
      include "../fdb/fdb_wfdisc.h"
      include "../fdb/fdb_io.h"
      integer*4 i,ivel,ierr,ipc,n,narg,iargc,lnblnk
      real*4    pi2,data(400000)
      character*256 named,cmd
      character*2 endian,datatype
      logical tf

      pi2 = datan(1.0d0)*8.0d0
      narg=iargc()
      if (narg.ne.4) STOP ' Usage: wfehdi ivel ipc INFILE OUTFILE'
      call getarg(1,dbin)
      read(dbin,*) ivel
      call getarg(2,dbin)
      read(dbin,*) ipc
      call getarg(3,dbin)
      call getarg(4,dbout)
c ---
      named = dbout(1:lnblnk(dbout))//'.wfdisc'
        call read_wfdisc
        do i = 1,nrowwfdisc
          if(ipc.eq.1) datatype = datatype_wfdisc(i)
          if(ipc.eq.1) datatype_wfdisc(i) = 'f4'
          n = nsamp_wfdisc(i)
          call get_wfdisc(i,n,data,ierr)
          if(ivel.eq.1) then
             calib_wfdisc(i) = calib_wfdisc(i)*pi2/calper_wfdisc(i)
          endif
          datatype_wfdisc(i) = endian()
          if(datatype_wfdisc(i).eq.'f4') then
             datatype_wfdisc(i) = 't4'
             if(ipc.eq.1) datatype_wfdisc(i)=datatype
             call swap1(data,4,n)
             call put_wfdisc(i,n,data,ierr)
          endif
        enddo
        inquire(file=named,exist=tf)
        if(tf) then
           cmd = 'rm -f '//named
           call system(cmd)
        endif
        call write_wfdisc
      end
