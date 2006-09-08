      program eigcon
c
c=====converts eigenfunction files ( output from minos )
c      into gfs-file containing only info about the
c      upper most 700 km.
c         ---this version for spheroidal and toroidal modes
c   
***************************************************************************
*     designed:                                                           *
*     changes:       15.08.91 spheroidal and toroidal modes               *
*                             parameter mnl for max no. layers in model   *
*                             variable  irecl for reclength of eigfun file*
*     latest change: 19.08.91 subroutines    for sph, rad and tor modes   *
***************************************************************************
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
      real*8    r(mk),pi
      real*4    rout(mk),buf(6,mk)
      real*4    bigg,tau,rhobar,con,accn
      real*4    tref,rx,r1,r2,dr,dum,rn,gn,vn,wn
      real*4    dmax,rmax,ww,qq,qc,tf,fl,fl1,fl3
      integer*4 i,ieig,idat,iin,ifanis,ifdeck,j,jj,jcom,l,ll,lll
      integer*4 n,nic,noc,nreg,nn,nlay,nstart,nrad,ni,nrecl,nrest
      integer*4 ierr,iflag,ititle(20)
      character*20 str
      character*64 dir
      character*1 typeo
      character*256 fmodel,fflatin,fbinin,fout  
c ---
      data bigg,tau,rhobar/6.6723e-11,1000.0,5515.0/
      pi=4.0d0*datan(1.d0)          
      con=pi*bigg
      nstart = 0
c
c read control parameters from stdin
c
c  read jcom : flag to indicate mode type
      print*,' spheroidals (3) or toroidals (2) or radial (1) or'
      print*,' inner core toroidals (4) modes'
      read(*,*)jcom
c read model file name
      print*,' enter name of model file'
      read(*,'(a)')fmodel
c read max depth for eigenvectors
      print *,'enter max depth [km] : '
      read(5,*) dmax
c read minos_bran output text file
      write(*,*) ' enter name of minos_bran output text file'
      read(*,'(a)') fflatin
c read minos_bran output binary unformatted file
      write(*,*) ' minos_bran output binary unformatted file name'
      read(*,'(a)') fbinin
c read dbase_name. There might be two form of input string:
c path/dbase_name  or dbase_name, where path is relative or absolute
c path to existing directory. Under this directory or working
c directory (no path/ in input string) will be created db relation file
c dbase_name.eigen, directory dbase_name.eigen.dat, and
c under dbase_name.eigen.dat binary data file eigen.
      print*,
     * ' enter path/dbase_name or dbase_name to store eigenfunctions:'
      read(*,'(a)')fout
c
c  read in radial knots from model
c
      iin = 7
      open(iin,file=fmodel,status='old')
      read(iin,101) (ititle(i),i=1,20)
  101 format(20a4)
      read(iin,*) ifanis,tref,ifdeck
      if(ifdeck.eq.0) go to 1000
c*** card deck model ***
      read(iin,*) n,nic,noc
      read(iin,105) (r(i),i=1,n)
  105 format(f8.0)
      go to 2000
c*** polynomial model ***
 1000 read(iin,*) nreg,nic,noc,rx
      n=0
      jj=5
      if(ifanis.ne.0) jj=8
      do 10 nn=1,nreg
      read(iin,*) nlay,r1,r2
      r1=r1*tau
      r2=r2*tau
      dr=(r2-r1)/float(nlay-1)
      do 15 i=1,nlay
      n=n+1
   15 r(n)=r1+dr*float(i-1)
      do 10 j=1,jj
   10    read(iin,110) dum
  110 format(f9.5)
 2000 close(iin)
c
c  rn     : radius at surface
c  wn     : frequency normalization
c  vn     : velocity normalisation
c  gn     : acceleration normalisation
c  accn   : ??????
c  n      : index of surface grid point
c  nstart : index of lowest grid point
c  nrad   : # of gridpoints of reduced eigenfunctions
c
      rn=r(n)
      gn=con*rhobar*rn
      vn=sqrt(gn*rn)
      wn=vn/rn
      accn=1.e+20/(rhobar*rn**4)
c  normalize radius
      do i=1,n
         r(i)=r(i)/rn
         if(i.gt.1.and.dabs(r(i)-r(i-1)).lt.1.d-7) r(i)=r(i-1)
      enddo
c cut radius knots lower than max depth
      rmax=1.-dmax*tau/rn
      j=0
      do i=1,n
         j=j+1
         if(r(i).ge.rmax) goto 30
      enddo
   30 nstart=max0(j-1,1)
      j=0
      do i=nstart,n
         j=j+1
         rout(j)=r(i)
      enddo
      nrad=j
      write(*,*) 'n,nstart,nrad = ',n,nstart,nrad
c open minos_bran plane output file and search mode part
      open(7,file=fflatin,form='formatted',status='old')
  1   read(7,'(a)',end=9) str
      ni=0
      do i = 1,20
        if(str(i:i).eq.'m') ni=i
      enddo
      if(ni.eq.0) goto 1
      if(str(ni:ni+3).eq.'mode') goto 2
      goto 1
  9   stop 'Wrong minos_bran output text file'
  2   read(7,'(a)') str
c open minos_bran binary unformatted file
      open(8,file=fbinin,form='unformatted',status='old')
c***************************************************************
c Create .eigen relations and binary eigenfunctions file
c***************************************************************
      ieig = 9
      idat = 10
      call null_eigen
      eigid_eigen = 1
      foff_eigen = 0
      ncol_eigen = 2
      if(jcom.eq.3) ncol_eigen = 6
      npar_eigen = 4
      nraw_eigen = nrad
      nrecl = max0(ncol_eigen*nraw_eigen+npar_eigen,10)*4
      call open_eigen(fout,nrecl,ieig,idat,dir,'w',ierr)
      dir_eigen = dir
      dfile_eigen = 'eigen'
      call write_eigen(ieig,ierr)
      nrest=nrecl/4-nrad-9
      write(idat,rec=eigid_eigen) 
     +        jcom,bigg,tau,rhobar,rn,wn,vn,gn,accn,
     +        (rout(l),l=1,nrad),(foff_eigen,lll=1,nrest)
c
c Main loop ----
c
 200  eigid_eigen = eigid_eigen+1
      foff_eigen = foff_eigen+nrecl
      read(8,end=99) nn,ll,ww,qq,qc,((buf(l,lll),lll=1,n),
     +               l=1,ncol_eigen)
      read(7,*) norder_eigen,typeo,lorder_eigen,phvel_eigen,
     +          tf,per_eigen,grvel_eigen,attn_eigen
      if(nn.ne.norder_eigen.or.ll.ne.lorder_eigen) then
        write(*,*) 
     +      'ERR001: eigcon: Input plane and binary files differ: ',
     +      nn,ll,norder_eigen,lorder_eigen
        stop
      endif
c check that jcom corresponds to minos_bran mode
      iflag = 0
      if(jcom.eq.4) then
        if(typeo.ne.'c') iflag = 1
        typeo_eigen = 'C'
      else if(jcom.eq.3)then
        if(typeo.ne.'s') iflag = 1
        typeo_eigen = 'S'
      else if(jcom.eq.2) then
        if(typeo.ne.'t') iflag = 1
        typeo_eigen = 'T'
      else if(jcom.eq.1) then 
        if(typeo.ne.'s') iflag = 1
        typeo_eigen = 'R'
        grvel_eigen = -1.0
      else
        write(*,*) 'ERR002: eigcon: Unknown jcom ',jcom
        stop
      endif
      if(iflag.eq.1) then
        write(*,*) 'ERR003: eigcon: jcom=',jcom,
     +            ' does not fit mode ',typeo_eigen
        stop
      endif
c additional normalization V, V' or W, W'
      if(typeo_eigen.eq.'S'.or.typeo_eigen.eq.'T') then
        fl=ll
        fl1=fl+1.0
        fl3=sqrt(fl*fl1)
        do i=nstart,n
          do j =1,2
             if(typeo_eigen.eq.'T') then
               buf(j,i)=buf(j,i)/fl3
             else
               buf(j+2,i)=buf(j+2,i)/fl3
             endif
          enddo
        enddo
      endif
      qq=0.5*ww/qq
cxx   ww = ww/wn
      call write_eigen(ieig,ierr)
      write(idat,rec=eigid_eigen) nn,ll,ww,qq,
     +     ((buf(l,lll),l=1,ncol_eigen),lll=nstart,n)
      goto 200
  99  close(7)
      close(8)
      call close_eigen(ieig,idat)
      end                         
