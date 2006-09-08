c ******************************************************************
c Relation: eigen
c Description: Eigenfunction and eigenvalue.
c attribute field storage external character  attribure
c name      no    type    format   position   description
c norder    1      i4      i8        1-8      order number n
c lorder    2      i4      i8        10-17    order number l
c typeo     3      i4      a1        19-19    type of oscillations
c eigid     4      i4      i8        21-28    eigen id
c per       5      f4      f10.5     30-45    eigenvalue, period, sec
c phvel     6      f4      f10.5     47-62    phase velocity, km/s
c grvel     7      f4      f10.5     64-79    group velocity, km/s
c attn      8      f4      f10.5     81-96    attenuation
c nraw      9      i4      i8        98-105   number of raws
c ncol     10      i4      i4       107-110   number of columns
c npar     11      i4      i4       112-115   number of parameters
c datatype 12      c2      a2       117-118   numeric storage
c foff     13      i4      i10      120-129   byte offset
c dir      14      c64     a64      131-194   directory
c dfile    15      c32     a32      196-227   file name
c commid   16      i4      i8       229-236   comment id
c lddate   17     date     a17      238-254   load date
c ******************************************************************
      subroutine open_eigen(name,n,ieig,idat,dir,mode,ierr)
      implicit none
      character mode *(*)
      character*256 name,namep,named,nameb,cmd
      character*64 dir
      integer*4 i,n,ieig,idat,l,lnblnk,ierr
      logical tf
c ---
      ierr = 0
      l = lnblnk(name)
      namep = name(1:l)//'.eigen'
      named = name(1:l)//'.eigen.dat'
      inquire(file=named,exist=tf)
      if(.not.tf) then
c create new dir for data
        cmd = 'mkdir -p '//name(1:l)//'.eigen.dat'
        call system(cmd)
c       write(*,*) 'dir created'
      endif
      nameb = name(1:l)//'.eigen.dat/eigen'
      inquire(file=nameb,exist=tf)
      if(tf.and.mode.eq.'w') then
c delete file eigen, if exists
        cmd = 'rm -f '//nameb
        call system(cmd)
c       write(*,*) 'file deleted'
      endif
      l = lnblnk(named)
      do i = l,1,-1
        if(named(i:i).eq.'/'.or.named(i:i).eq.' ') then
          dir = named(i+1:l)
          goto 1
        endif
      enddo
      dir =  named(1:l)
  1   open(ieig,err=99,file=namep,form='formatted',status='unknown')
      open(idat,err=99,file=nameb,access='direct',recl=n)
      return
 99   ierr = 1
      return
      end
c **********************************************************************
      subroutine close_eigen(ieig,idat)
      close(idat)
      close(ieig)
      return
      end
c **********************************************************************
      subroutine write_eigen(ieig,ierr)
      implicit none
c ---
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
c ---
      integer*4 ieig,ierr
c ---
      write(ieig,1000) norder_eigen,lorder_eigen,typeo_eigen,
     +      eigid_eigen,per_eigen,phvel_eigen,grvel_eigen,
     +      attn_eigen,nraw_eigen,ncol_eigen,npar_eigen,
     +      datatype_eigen,foff_eigen,dir_eigen,
     +      dfile_eigen,commid_eigen,lddate_eigen
      return
 1000 format(i8,1x,i8,1x,a1,1x,i8,1x,4(f16.5,1x),i8,1x,2(i4,1x),
     +       a2,1x,i10,1x,a64,1x,a32,1x,i8,1x,a17)
      end
c **********************************************************************
      subroutine read_eigen(ieig,ierr)
      implicit none
      common/c_eigen/norder_eigen,lorder_eigen,
     +      eigid_eigen,per_eigen,phvel_eigen,grvel_eigen,
     +      attn_eigen,nraw_eigen,ncol_eigen,npar_eigen,
     +      foff_eigen,commid_eigen,typeo_eigen,
     +      datatype_eigen,dir_eigen,dfile_eigen,lddate_eigen
      real*4    per_eigen,phvel_eigen,grvel_eigen,attn_eigen
      integer*4 norder_eigen,lorder_eigen,eigid_eigen,
     +          nraw_eigen,ncol_eigen,npar_eigen,foff_eigen,
     +          commid_eigen
      character*2 datatype_eigen
      character*64 dir_eigen
      character*32 dfile_eigen
      character*17 lddate_eigen
      character*1 typeo_eigen
      integer*4 ieig,ierr
c ---
      ierr = 0
      read(ieig,1000,end=9) norder_eigen,lorder_eigen,typeo_eigen,
     +      eigid_eigen,per_eigen,phvel_eigen,grvel_eigen,
     +      attn_eigen,nraw_eigen,ncol_eigen,npar_eigen,
     +      datatype_eigen,foff_eigen,dir_eigen,
     +      dfile_eigen,commid_eigen,lddate_eigen
      return
    9 ierr = 1
      return
 1000 format(i8,1x,i8,1x,a1,1x,i8,1x,4(f16.5,1x),i8,1x,2(i4,1x),
     +       a2,1x,i10,1x,a64,1x,a32,1x,i8,1x,a17)
      end
c **********************************************************************
      subroutine null_eigen
      implicit none
      common/c_eigen/norder_eigen,lorder_eigen,
     +      eigid_eigen,per_eigen,phvel_eigen,grvel_eigen,
     +      attn_eigen,nraw_eigen,ncol_eigen,npar_eigen,
     +      foff_eigen,commid_eigen,typeo_eigen,
     +      datatype_eigen,dir_eigen,dfile_eigen,lddate_eigen
      real*4    per_eigen,phvel_eigen,grvel_eigen,attn_eigen
      integer*4 norder_eigen,lorder_eigen,eigid_eigen,
     +          nraw_eigen,ncol_eigen,npar_eigen,foff_eigen,
     +          commid_eigen
      character*2 datatype_eigen
      character*64 dir_eigen
      character*32 dfile_eigen
      character*17 lddate_eigen
      character*1 typeo_eigen
c---
c t4 - SUN IEEE real*4, t4 - VAX/PC IEEE real*4
      norder_eigen = -1
      lorder_eigen = -1
      typeo_eigen = 'P'
      eigid_eigen = 0
      per_eigen = -1.0
      phvel_eigen = -1.0
      grvel_eigen = -1.0
      attn_eigen = -1.0
      nraw_eigen = 0
      ncol_eigen = 0
      npar_eigen = 0
      datatype_eigen = 't4'
      foff_eigen = 0
      dir_eigen = '-'
      dfile_eigen = '-'
      commid_eigen = -1
      lddate_eigen = '-'
      return
      end
