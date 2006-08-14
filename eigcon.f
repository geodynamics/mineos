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
      dimension ititle(20),hed6(1554)
      real*8 r(300),pi
      character atyp*4, model*8,filen*80  
      common/hedX/nhed,model,jcom,nz,atyp,lz,w,q,g,nrad,
     *           rn,wn,accn,rout(1540)
      equivalence (nhed,hed6(1))
      data bigg,tau,rhobar/6.6723e-11,1000.0,5515.0/
      data rout/1540*0./                               
      irecl=5600
      iin=11
      pi=4.*atan(1.d0)          
      con=pi*bigg
      print*,'enter name of model : '
      read (*,'(a)') model
c...  hed6 must have dimension of rout+14 (not 13, because of model*8!)
c  jcom : flag to indicate mode type
      print*,' spheroidals (3) or toroidals (2) or radial (1)'
      read(*,*)jcom
c
c  read in radial knots
c
      print*,' enter name of model file'
      read(*,'(a)')filen 
      open(iin,file=filen,status='old')
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
c  n      : index of surface grid point
c  nstart :   "   "  lowest   "     "
c  nrad   : # of gridpoints of reduced eigenfunctions
c
      rn=r(n)
      gn=con*rhobar*rn
      vn=sqrt(gn*rn)
      wn=vn/rn
      accn=1.e+20/(rhobar*rn**4)
c  normalize radius
      do 55 i=1,n
         r(i)=r(i)/rn
         if(i.gt.1.and.dabs(r(i)-r(i-1)).lt.1.d-7) r(i)=r(i-1)
   55    continue
      print *,'enter max depth [km] : '
      read(5,*) dmax
      rmax=1.-dmax*tau/rn
      j=0
      do 25 i=1,n
         if(r(i).gt.rmax) goto 30
   25    j=j+1
   30 nstart=j
      j=0
      do 35 i=nstart,n
         j=j+1
   35    rout(j)=r(i)
      nrad=j
      print *,'n,nstart,nrad = ',n,nstart,nrad

      ityp = 6
      print*,' enter name of eigenfunction file to be reduced'
      read(*,'(a)')filen
      open(2,file=filen,
     *       form='unformatted',iostat=iret)
      call gfs_opena(3,'output gfs-file name :',ityp,ierr)
c  write first header containing radial grid
      kfl  = 1
      nz   = 0
      atyp = 'grid'
      lz   = 0           
      w    = 0.
      q    = 0.
      g    = 0.
      nhed = 0                       
ccc
      print 40,nhed,model,jcom,nz,atyp,lz,w,q,g,nrad,
     *           rn,wn,accn
   40 format(i6,1x,a8,2i6,1x,a4,i4,3g15.5,i6,3g15.5)
      print 41,(rout(i),i=1,nrad)
   41 format(5g15.5)
ccc
      call gfs_rwdir(3,hed6,kfl,'w',ierr)                              
c...  i/o of eigenfunctions
      if(jcom.eq.3)then
        call conv4(nstart,n,kfl)   
      else  if(jcom.eq.2) then
        call conv2(nstart,n,kfl)   
      else
        call conv1(nstart,n,kfl)
      endif
      close(2)
      call gfs_close(3)
      stop
      end                         

      subroutine conv4(nstart,n,kfl)
      parameter(mnl=323)          
      common/bufz/nn,ll,ww,qq,cg,buf(6*mnl)
      common/hedX/nhed,model,jcom,nz,atyp,lz,w,q,g,nrad,
     *            rn,wn,accn,x(1540)
      dimension abuf(6*mnl+5),a(4,350),aa(1),hed6(1554)
      character*4 atyp                                       
      character*8 model
      equivalence (nn,abuf),(a,aa),(hed6,nhed)
        atyp    ='S   ' 
        nvec = 6*n + 5     
        nloop=4
        nlen = nloop*nrad                                 
        nout = nloop*nrad+5
      print *,'nlen (must be .le. 1540 or else change defhdr) : ',nlen,nvec
      if(nlen.gt.1540)stop
   50 read(2,end=99) (abuf(i),i=1,nvec)
    5 do 1 i=1,nrad
         do 1 j=1,nloop
    1       a(j,i)=0.0
      fl=ll
      fl1=fl+1.0
      fl3=fl*fl1
      sfl3=sqrt(fl3)
      print 900,nn,atyp,ll,ww,qq,cg,sfl3,buf(5*n)
  900 format(i4,a4,i4,5g14.6)
      qq=0.5*ww/qq
c*** cg is really the perturbation of the grav potl at the surface!
      cg=buf(5*n)
      if(ll.eq.0) cg=0.
      k=0
      do 10 i=nstart,n
         k=k+1
         a(1,k)=buf(i)
         j=i+n
         a(2,k)=buf(j)
         j=j+n
         a(3,k)=buf(j)/sfl3
         j=j+n
         a(4,k)=buf(j)/sfl3
   10    continue
      do 20 i=1,nlen
   20    x(i)=aa(i)
      nz  = nn
      lz  = ll                
      w   = ww
      q   = qq
      g   = cg
      kfl = kfl + 1
      call gfs_rwdir(3,hed6,kfl,'w',ierr)
      goto 50
   99 return
      end

      subroutine conv2(nstart,n,kfl)
      parameter(mnl=323)          
      common/bufz/nn,ll,ww,qq,cg,buf(6*mnl)
      common/hedX/nhed,model,jcom,nz,atyp,lz,w,q,g,nrad,
     *            rn,wn,accn,x(1540)
      dimension abuf(6*mnl+5),a(2,350),aa(1),hed6(1554)
      character*4 atyp                                       
      character*8 model
      equivalence (nn,abuf(1)),(a(1,1),aa(1)),(hed6(1),nhed)
        atyp    ='T   '
        nvec = 2*n + 5                                
        nloop=2
        nlen = nloop*nrad                                 
        nout = nloop*nrad+5
      print *,'nlen (must be .le. 1540 or else change defhdr) : ',nlen
      if(nlen.gt.1540)stop
   50 read(2,end=99) (abuf(i),i=1,nvec)
    5 do 1 i=1,nrad
         do 1 j=1,nloop
    1       a(j,i)=0.0
      fl=ll
      fl1=fl+1.0
      fl3=fl*fl1
      sfl3=sqrt(fl3)
      print 900,nn,atyp,ll,ww,qq,cg,sfl3
  900 format(i4,a4,i4,5g14.6)
      qq=0.5*ww/qq
      k=0
      do 10 i=nstart,n
         k=k+1
         a(1,k)=buf(i)/sfl3
         a(2,k)=buf(i+n)/sfl3
   10    continue    
      do 20 i=1,nlen
   20    x(i)=aa(i)
      nz  = nn
      lz  = ll                
      w   = ww
      q   = qq
      g   = 0.
      kfl = kfl + 1
      call gfs_rwdir(3,hed6,kfl,'w',ierr)
      goto 50
   99 return
      end


      subroutine conv1(nstart,n,kfl)
c*** special for radial modes
      parameter(mnl=323)          
      common/bufz/nn,ll,ww,qq,cg,buf(6*mnl)
      common/hedX/nhed,model,jcom,nz,atyp,lz,w,q,g,nrad,
     *            rn,wn,accn,x(1540)
      dimension abuf(6*mnl+5),a(4,350),aa(1),hed6(1554)
      character*4 atyp                                       
      character*8 model
      equivalence (nn,abuf),(a,aa),(hed6,nhed)
        atyp    ='S   ' 
        nvec = 2*n + 5     
        nloop=4
        nlen = nloop*nrad                                 
        nout = nloop*nrad+5
      print *,'nlen (must be .le. 1540 or else change defhdr) : ',nlen
      if(nlen.gt.1540)stop
   50 read(2,end=99) (abuf(i),i=1,nvec)
    5 do 1 i=1,nrad
         do 1 j=1,nloop
    1       a(j,i)=0.0
      fl=ll
      fl1=fl+1.0
      fl3=fl*fl1
      sfl3=sqrt(fl3)
      print 900,nn,atyp,ll,ww,qq
  900 format(i4,a4,i4,5g14.6)
      qq=0.5*ww/qq
      k=0
      do 10 i=nstart,n
         k=k+1
         a(1,k)=buf(i)
         a(2,k)=buf(i+n)
         a(3,k)=0.
         a(4,k)=0.
   10    continue
      do 20 i=1,nlen
   20    x(i)=aa(i)
      nz  = nn
      lz  = ll                
      w   = ww
      q   = qq
      g   = 0.
      kfl = kfl + 1
      call gfs_rwdir(3,hed6,kfl,'w',ierr)
      goto 50
   99 return
      end
