c
c MINEOS version 1.0 by Guy Masters, John Woodhouse, and Freeman Gilbert
c
c This program is free software; you can redistribute it and/or modify
c it under the terms of the GNU General Public License as published by
c the Free Software Foundation; either version 2 of the License, or
c (at your option) any later version.
c
c This program is distributed in the hope that it will be useful,
c but WITHOUT ANY WARRANTY; without even the implied warranty of
c MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
c GNU General Public License for more details.
c
c You should have received a copy of the GNU General Public License
c along with this program; if not, write to the Free Software
c Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
c
c**************************************************************************
c
c Utilities routines for eigen relation
c
c**************************************************************************
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
c**************************************************************************
      subroutine open_eigen(name,ieig,idat,n,dir,mode,ierr)
      implicit none
      character mode *(*)
      character*256 name,namep,named,nameb,cmd
      character*64 dir
      integer*4 i,ieig,idat,l,lnblnk,ierr,n
      logical tf
c ---
      ierr = 0
      l = lnblnk(name)
c relation itself
      namep = name(1:l)//'.eigen'
      inquire(file=namep,exist=tf)
      if((.not.tf).and.mode.eq.'r') then
        write(*,*) 'ERR030: fdb: file ',namep(1:lnblnk(namep)),
     *             ' does not exist.'
        ierr = 2
        return
      endif
c directory for data
      named = name(1:l)//'.eigen.dat'
c data file
      nameb = name(1:l)//'.eigen.dat/eigen'
c relative directory - dir
      l = lnblnk(named)
      do i = l,1,-1
        if(named(i:i).eq.'/'.or.named(i:i).eq.' ') then
          dir = named(i+1:l)
          goto 1
        endif
      enddo
      dir =  named(1:l)
  1   inquire(file=named,exist=tf)
      if(.not.tf) then
c create new dir for data
        cmd = 'mkdir -p '//named
        call system(cmd)
      endif
      inquire(file=nameb,exist=tf)
      if(tf.and.mode.eq.'w') then
c delete file eigen, if exists
        cmd = 'rm -f '//nameb
        call system(cmd)
      endif
      open(ieig,err=98,file=namep,form='formatted',status='unknown')
      open(idat,err=99,file=nameb,access='direct',recl=n)
      return
 98   nameb = namep
 99   ierr = 1
      write(*,*) 'ERR031: fdb: Can not open file: ',
     *       nameb(1:lnblnk(nameb))
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
      include "fdb_eigen.h"
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
      include "fdb_eigen.h"
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
      include "fdb_eigen.h"
c---
      character*17 loctime
      character*2  endian
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
      datatype_eigen = endian()
      foff_eigen = 0
      dir_eigen = '-'
      dfile_eigen = '-'
      commid_eigen = -1
      lddate_eigen = loctime()
      return
      end
c **********************************************************************
      subroutine get_eigen(idat,npar,param,nraw,ncol,eig,ieig,ierr)
      implicit none
      integer*4 idat,npar,nraw,ncol,ieig,ierr,l,lll
      real*4      param(npar),eig(7,nraw)

      ierr = 0
      read(idat,rec=ieig,err=9) (param(l),l=1,npar),
     +      ((eig(l,lll),l=1,ncol),lll=1,nraw)
      return
   9  ierr = 1
      return
      end
c **********************************************************************
      subroutine put_eigen(idat,npar,param,nraw,ncol,eig,ieig,ierr)
      implicit none
      integer*4 idat,npar,nraw,ncol,ieig,ierr,l,lll
      real*4      param(npar),eig(7,nraw)
cxx   real*4      paramw(npar),eigw(7,nraw)
cxx   character*2 type,endian

      ierr = 0
cxx   type = endian()
cxx   if(type.ne.'t4') then
cxx     call swap(param,paramw,4,npar)
cxx     call swap(eig,eigw,4,7*nraw)
cxx   endif
      write(idat,rec=ieig,err=9) (param(l),l=1,npar),
     +      ((eig(l,lll),l=1,ncol),lll=1,nraw)
      return
   9  ierr = 1
      return
      end
c **********************************************************************
c Estimate hardware platform:
c endian = 't4' - BIG_ENDIAN,
c endian = 'f4' - LOW_ENDIAN,
c **********************************************************************
      character*2 function endian()
      implicit none
      integer*4 i,is
      character*4 s
      equivalence (is,s)
c ---
      s = '+   '
      i = is/256
      i = is-i*256
      endian = 'f4'
      if(i.eq.32) endian = 't4'
      return
      end
