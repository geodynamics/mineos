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
c  Header: fdb_eigen.h
c  eigen declarations
c
c**************************************************************************
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
