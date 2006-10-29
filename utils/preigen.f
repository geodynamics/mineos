c read eigen functions
      implicit none
      integer*4 mk
      parameter (mk=350)
c ---  eigen relation common block
      real*4    per_eigen,phvel_eigen,grvel_eigen,attn_eigen
      integer*4 norder_eigen,lorder_eigen,eigid_eigen,
     +          nraw_eigen,ncol_eigen,npar_eigen,foff_eigen,
     +          commid_eigen
      character*2 datatype_eigen
      character*64 dir_eigen
      character*32 dfile_eigen
      character*17 lddate_eigen
      character*1 typeo_eigen
      common/c_eigen/norder_eigen,lorder_eigen,
     +      eigid_eigen,per_eigen,phvel_eigen,grvel_eigen,
     +      attn_eigen,nraw_eigen,ncol_eigen,npar_eigen,
     +      foff_eigen,commid_eigen,typeo_eigen,
     +      datatype_eigen,dir_eigen,dfile_eigen,lddate_eigen
c --- other variables
      character*64 dir
      character*256 fout
      real*4      rout(mk),buf(6,mk)
      real*4      pi2,rn,vn,accn,ts,t1,t2
      real*4      t3,ww,qq
      integer*4   narg,iargc,nrecl,ieig,idat,ierr
      integer*4   indreq,nn,ll,lll,i,j,mods

      pi2 = atan(1.0)*8.0
c get input arguments
      narg=iargc()
      if (narg.ne.0) then
        write(*,*) ' Usage: peigcon '
        call exit(1)
      endif
c ---
      nrecl = 2000
      ieig = 9
      idat = 10
      write(*,*) 'c Enter eigen database name'
      read(*,*,end =98) fout
c get record length of direct eigen file
      call open_eigen(fout,ieig,idat,nrecl,dir,'r',ierr)
      call read_eigen(ieig,ierr)
      nrecl = (ncol_eigen*nraw_eigen+npar_eigen)*4
      call close_eigen(ieig,idat)
   1  call open_eigen(fout,ieig,idat,nrecl,dir,'r',ierr)
c find record by index indreq and print it out   
      write(*,*) 'c Enter mode and Period:'
      read(*,*,end=999) mods,ts
      indreq = 0
    4 indreq = indreq+1
      call read_eigen(ieig,ierr)
      if(ierr.ne.0) goto 99
      if(norder_eigen.ne.mods) goto 4
      t1 = abs(per_eigen-ts)
      indreq = indreq+1
      call read_eigen(ieig,ierr)
      if(ierr.ne.0) goto 3
      if(norder_eigen.ne.mods) goto 3
      t2 = abs(per_eigen-ts)
      if(t2.gt.t1) goto 3
    2 indreq = indreq+1
      call read_eigen(ieig,ierr)
      if(ierr.ne.0) goto 3
      if(norder_eigen.ne.mods) goto 3
      t3 = abs(per_eigen-ts)
      if((t1-t2)*(t2-t3).le.0.0) goto 3
      t1 = t2
      t2 = t3
      goto 2
    3 indreq = indreq-1
      call close_eigen(ieig,idat)
      call open_eigen(fout,ieig,idat,nrecl,dir,'r',ierr)
  10  call read_eigen(ieig,ierr)
      if(eigid_eigen.ne.indreq) goto 10
      read(idat,rec=eigid_eigen) nn,ll,ww,qq,rn,vn,accn,
     +     (rout(lll),(buf(ll,lll),ll=1,ncol_eigen-1),lll=1,nraw_eigen)
      do i=1,nraw_eigen
        rout(i) = (1.0-rout(i))*rn/1000.0
      enddo
c output data
      write(*,*) 'c eigid= ',indreq, ', datatype= ',datatype_eigen
      write(*,*) 'c n order= ',norder_eigen,', mode= ',typeo_eigen,
     *        ', l order= ',lorder_eigen
      write(*,*) 'c T = ',per_eigen,', Q = ',attn_eigen, ', nrow= ',
     *           nraw_eigen,', ncol= ',ncol_eigen
      write(*,*) 'c rn = ',rn
      do i = nraw_eigen,1,-1
        j = nraw_eigen+1-i
        if(ncol_eigen.eq.3) then
          write(*,1000) j,rout(i),(buf(lll,i),lll=1,ncol_eigen-1)
        else
          write(*,1001) j,rout(i),(buf(lll,i),lll=1,ncol_eigen-1)
        endif
      enddo
      call close_eigen(ieig,idat)
      goto 1
  98  continue
  99  call close_eigen(ieig,idat)
      goto 1
 999  continue
      call close_eigen(ieig,idat)
1000  format(i4,f10.3,2e15.6)
1001  format(i4,f10.3,6e15.6)
      end
