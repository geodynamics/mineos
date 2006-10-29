c********************************************************************
c  Header: fdb_eigen.h
c  eigen declarations (3.0)
c  (R) University of Colorado at Boulder,CIEI.      Date: 08/15/2006
c********************************************************************
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
