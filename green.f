
c.....program green
c       common blocks :
c          dheadX : source info
c          myhedX : record header
c          modeX  : complex eigenfrequencies
c          vecnlX : exitations of modes
c
      program green
      include "green.h"
      integer n1(100),n2(100)
cxx (MB)      real*4 hed1(30),alocal(9),scalrs(8),dcg(3)
      real*4 hed1(30),alocal(9),scalrs(8)
      real*8 co,si,c1,c2,s1,s2,z,zp,cz,sz,caz,saz,prp
      common/dheadX/d0,th,ph,jy,jd,jh,jm,sec,tstart
      common/zfXX/z(3,ml),zp(3,ml)
      common/rcrds/dat(mseis)
      common/myhed/nscan,nsta,nchn,ntyp,iy,id,ih,im,ss,dt,slat,slon,
     &             sdep,jys,jds,jhs,jms,sss,spare(12)

      common/grnX/grn(12*mseis)
      common/modeX/om(meig),a0(meig),omt(meig),a0t(meig)
      common/vecnlX/vecnl(8,meig),vecnlt(4,meig)
      common/wts/wt(6,meig)
      common/sclrX/n,l,e1,e2,e3,e4,au,av
      common/propX/prp(6*mseis)
      character*4 nchn,nsta,ksta
c (MB) added for consistency
      character*4 ntyp

      equivalence (n,scalrs),(hed1,nscan),(alocal,d0)
      data pi,rad,zero,tstart/3.14159265359,57.29578,0.,0./
      data icomp,itype1,ksta/0,1,'xxxx'/
c
c.....open event file for which you want to compute green's functions
c.....read first header to get source info
c
      open(12,file='green_in',status='old')
      read(12,*) nscan,nsta,nchn,ntyp,iy,id,ih,im,ss,dt,slat,slon,
     &             sdep,jys,jds,jhs,jms,sss
      do i=1,12
         spare(i) =0.0
      enddo
      close (12)
c--
      d0=sdep
      th=90.-slat
      ph=slon
      jy=jys
      jd=jds
      jh=jhs
      jm=jms
      sec=sss
c
c.....open output file for green's functions
c
      call gfs_opena(2,'greens function file (output) :',itype1,jret2)
      call gfs_erase(2)
      print*,'min and max frequencies to be considered (mHz) : '
      read*,fmin,fmax
      fmin = fmin*pi/500.
      fmax = fmax*pi/500.

      call source(fmin,fmax,lmax,nmodes,nmodet)

      if(lmax.lt.meig) goto 5
      print*,'max l =',lmax,' must be .le.',meig
      goto 99
    5 print 105,d0,th,ph
  105 format('source depth =',f5.0,' km, colat. and long. =',2f7.2)
      print*,'# pts in greens fns ( .le. 9000) :'
      read*,iscan
      iscan = iscan/2
      call factor(iscan)
      iscan = 2*iscan
      iscan=min0(iscan,9000)
      t0=th/rad
      p0=ph/rad
      c0=cos(t0)
      s0=sin(t0)
c
c====loop over records
c........read event header
c
      open(12,file='green_in',status='old')
cxx   do 90 ifl=1,jret1
      do 90 ifl=1,3
cxx   call gfs_rwdir(1,hed1,ifl,'r',ierr)
      read(12,*) nscan,nsta,nchn,ntyp,iy,id,ih,im,ss,dt,slat,slon,
     &             sdep,jys,jds,jhs,jms,sss
      ierr=0
      write(*,*) nscan,nsta,nchn,ntyp,iy,id,ih,im,ss,dt,slat,slon,
     &             sdep,jys,jds,jhs,jms,sss
      if (ierr.ne.0) stop 'could not read event header'
      if (nscan.gt.9000) stop 'Data array too long! '
      if (nsta.eq.ksta) then
         if (nchn(1:4).eq.'MODE') icomp = 0
         if (nchn(1:3).eq.'VHZ') icomp = 0
         told = tstart
      else
         icomp = 0
         told = 0.
      endif
      ksta = nsta
      mchn=1
      if(nchn(1:3).eq.'VHN ') mchn=2
      if(nchn(1:3).eq.'VH1 ') mchn=2
      if(nchn(1:3).eq.'VHE ') mchn=3
      if(nchn(1:3).eq.'VH2 ') mchn=3
      if(mchn.eq.2) call newsta(nsta,nchn,iy,id,t1,p1,az1)
      if(mchn.eq.3) call newsta(nsta,nchn,iy,id,t1,p1,az2)

      print 110,nsta,mchn
  110 format(' Station ',a4,'   Channel',i2)
cxx   call inresp(iscan)
      tstart=((((iy-jy)*366.+(id-jd))*24.+(ih-jh))*60.+(im-jm))*60.+
     +ss-sec
      if(tstart.ne.told.and.icomp.ne.0) icomp=1
      if(icomp.eq.2) goto 500
      len=0
      if(icomp.eq.1) goto 35
      call allnsta(nsta,t1,p1)
c
c....do some trigonometry
c      epicentral distance: co, si
c      azimuth of source:   caz,saz
c
      c1=cos(t1)
      s1=sin(t1)
      dp=p1-p0
      co=c0*c1+s0*s1*cos(dp)
      si=dsqrt(1.d0-co*co)

      del = atan2(si,co)
      del = del /pi*180.
      write(*,'(a,f10.3)')' Epicentral Distance : ',del

      sz=s1*sin(dp)/si
      cz=(c0*co-c1)/(s0*si)
      saz=-s0*sin(dp)/si
      caz=(c0-co*c1)/(si*s1)

      azim = atan2(saz,caz)
      azim = azim/pi*180.
      write(*,'(a,f10.3)')' Azimuth of Source   : ',azim

      c2=2.d0*(cz**2-sz**2)
      s2=8.d0*cz*sz
      c1=2.d0*cz
      s1=2.d0*sz
c
c....generate the spherical harmonics
c
      call zfcns(lmax,co,si)
      if(mchn.ne.1) goto 35
c*** vertical component ***
      icomp=1
      do 25 i=1,nmodes
      do 30 j=1,8
   30    scalrs(j) = vecnl(j,i)
      lp1=l+1
c
c....initial amplitudes at time t=0
c
      wt(1,i)=e1*z(1,lp1)*au
      wt(2,i)=e2*z(1,lp1)*au
      wt(3,i)=e3*z(2,lp1)*au
      wt(4,i)=e4*z(3,lp1)*au
   25 continue
      call prop(om,a0,dt,tstart,nmodes,len,iscan,4,prp)
      call s4t6(grn,iscan,c1,s1,c2,s2)
      goto 500
c*** theta and phi components ***
c*** spheroidal modes ***
   35 icomp=2
      do 40 i=1,nmodes
      do 45 j=1,8
   45 scalrs(j)=vecnl(j,i)
      lp1=l+1
c
c....initial amplitudes at time t=0
c
      wt(1,i)=e1*zp(1,lp1)*av
      wt(2,i)=e2*zp(1,lp1)*av
      wt(3,i)=e3*zp(2,lp1)*av
      wt(4,i)=e4*zp(3,lp1)*av
      wt(5,i)=e3*z(2,lp1)*av/si
   40 wt(6,i)=e4*z(3,lp1)*av/si
      call prop(om,a0,dt,tstart,nmodes,len,iscan,6,prp)
      len=6*iscan
c
c*** toroidal modes ***
c
      if(nmodet.eq.0) goto 61
      do 60 i=1,nmodet
      do 65 j=1,4
   65 scalrs(j)=vecnlt(j,i)
      lp1=l+1
c
c....initial amplitudes at time t=0
c
      wt(1,i)=e1*z(2,lp1)/si
      wt(2,i)=e2*z(3,lp1)/si
      wt(3,i)=e1*zp(2,lp1)
   60 wt(4,i)=e2*zp(3,lp1)

      call prop(omt,a0t,dt,tstart,nmodet,len,iscan,4,prp)
   61 continue
      call s10t12(grn,iscan,c1,s1,c2,s2)
      call rotate(grn,iscan,caz,saz,az1,az2)
c
c....read in data record
c
cxx  500 call gfs_rwentry(1,hed1,dat,ifl,'r')
 500  do ii = 1,nscan
       dat(ii) = 0.5*sin(0.31415926*(ii-1)*dt)
      enddo
c
c....greens function longer than data ? : add flags to data !
c
      if(iscan.le.nscan) goto 75
      ip1=nscan+1
      do 70 i=ip1,iscan
   70 dat(i)=5.e15
   75 nscan=iscan
c
c....write out record
c
      do 77 i=1,9
   77    hed1(10+i)=alocal(i)
      igr = 2*ifl - 1
      call prhdr(igr,hed1,itype1)
      call gfs_rwentry(2,hed1,dat,igr,'w')
c
c....find gaps in data
c
      call panlsp(dat,nscan,n1,n2,np)
      indx=0
      if(mchn.eq.3) indx=6*iscan
c
c....convolve green's function by instrument response
c
cxx   call grnflt(grn(indx+1),iscan,6)
      len=6*iscan
c      if(dcg(mchn).eq.1.0) goto 87
c
c....multiply green's function by calibration factor
c
c      do 86 i1=1,len
c         jlong=indx+i1
c   86    grn(jlong)=grn(jlong)*dcg(mchn)
c
c....write out green's functions
c
   87 nscan = 6 * iscan
      igr = 2*ifl
      call prhdr(igr,hed1,itype1)
      call gfs_rwentry(2,hed1,grn(indx+1),igr,'w')
   90 continue
      close(12)
cxx   99 call gfs_close(1)
99    continue
      call gfs_close(2)
      stop
      end


      subroutine prop(om,a0,dt,tst,nmodes,len,npts,nfun,prp)
      include "green.h"
      real*8 ddt,dts,dt0,dt1,prp(nfun,npts),phi,ain
      real*8 b0,b1,c0,c1,c2
      common/propc/ddt,dts,dt0,dt1,phi,ain,b0,b1,c0,c1,c2
      common/grnX/grn(12*mseis)
      common/wts/wt(6,meig)
      dimension om(*),a0(*)
c
c....stores green's function in multiplexed form
c
      ddt = dt
      dts = tst
      dt0 = dts - 2*ddt
      dt1 = dts - ddt
      do 10 j=1,npts
      do 10 i=1,nfun
   10    prp(i,j) = 0.d0
c
c====loop over modes
c
      do 50 j=1,nmodes
c
c....initialize propagator
c
       b0 =                   -exp(-2*a0(j)*ddt)
       b1 = 2*cos(om(j)*ddt) * exp(  -a0(j)*ddt)
       c0 =   cos(om(j)*dt0) * exp(  -a0(j)*dt0)
       c1 =   cos(om(j)*dt1) * exp(  -a0(j)*dt1)
c
c====loop over time points
c
         do 40 i=1,npts
            c2 = b1*c1 + b0*c0
            do 30 n=1,nfun
   30       prp(n,i) = prp(n,i) + wt(n,j)*c2
            c0 = c1
            c1 = c2
   40       continue
   50    continue
c
c....demultiplex green's function
c
      k=len
      do 60 i=1,npts
         k  = k+1
         do 60 n=1,nfun
            grn((n-1)*npts+k) = prp(n,i)
   60 continue

      return
      end
c***************************************************************
c source sub reads eigen functions from flat databases.
c List of dbnames are defined in dbnames.dat file. source read 
c .eigen relations and select eigen functions for S & T modes
c****************************************************************
      subroutine source(fmin,fmax,lmax,nmodes,nmodet)
      implicit none
      include "green.h"
      integer*4 mk,lmax,nmodes,nmodet
      real*4    fmin,fmax
c --- common blocks -------------------------------
      real*4    d0,t0,p0,sec,tstart
      integer*4 jy,jd,jh,jm
      common/dheadX/d0,t0,p0,jy,jd,jh,jm,sec,tstart
      real*4 x1,r0,x2,f
      common/sclXXX/x1,r0,x2,f(4,3)
      real*4 vecnl,vecnlt
      common/vecnlX/vecnl(8,meig),vecnlt(4,meig)
      real*4 om,a0,omt,a0t
      common/modeX/om(meig),a0(meig),omt(meig),a0t(meig)
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
      real* 4   w,q,p,rn,wn,accn
      real*4    pi2,fot,bigg,tau,rhobar,vn,gn,rs,fl,fl1,fl3,u,v
      real*4    wsq,e14,au,av,wr,aw
      integer*4 jcom,npts,nrecl,ieig,idat,ierr,ifl,i,is,j,js
      integer*4 ll,lll,m,l,n
c ---
      character*64 dir
      character*256 dbname
      real*4      r(mk),buf(6,mk)
c ---
      real*4    fnl(2)
      equivalence (fnl(1),n),(fnl(2),l)
      data fot/1.33333333333/
cxx (MB) data name1/'/home/guy/lfsyn.dir/sph20m.gfs'/
cxx (MB) data name2/'/home/guy/lfsyn.dir/tor20m.gfs'/
      pi2 = datan(1.0d0)*8.0d0
      nmodes = 0
      nmodet = 0
      lmax=0
      ieig = 9
      idat = 10
c===================================================================
c Main loop by dbase names
c===================================================================
      open(7,file='dbnames.dat',status='old')
c*** read dbnames list
   8  read(7,'(a256)',end=9) dbname
      nrecl = 2000
      call open_eigen(dbname,nrecl,ieig,idat,dir,'r',ierr)
      call read_eigen(ieig,ierr)
      nrecl = (ncol_eigen*nraw_eigen+npar_eigen)*4
c
c....read in parameters and radial grid
c
      ifl=1
      read(idat,rec=ifl) jcom,bigg,tau,rhobar,rn,wn,vn,gn,accn,
     +                        (r(ll),ll=1,nraw_eigen)
      call close_eigen(ieig,idat)
      if(typeo_eigen.ne.'P') stop 
     *         'ERR010:eigen: radius pints are not found'
      npts = nraw_eigen
      rs=rn/1000.
      r0=1.-(d0/rs)
      do 5 i=1,npts
         if(r(i).lt.r0)is=i
    5    continue
      x1=r(is)
      js=is+1
      x2=r(js)
c
c....read in spheroidal and toroidal modes 
c....in frequency band fmin < f < fmax
c
      call open_eigen(dbname,nrecl,ieig,idat,dir,'r',ierr)
      call read_eigen(ieig,ierr)
      
  10  ifl = ifl+1
      call read_eigen(ieig,ierr)
      if(ierr.ne.0) goto 30
      if(ifl.ne.eigid_eigen) stop 
     *          'ERR011:eigen: flat and bin indices are different.'
         w = pi2/per_eigen
         if (w.lt.fmin) goto 10
         if (w.le.fmax) goto 11
          goto 10
  11  read(idat,rec=eigid_eigen) n,l,w,q,
     +     ((buf(ll,lll),ll=1,ncol_eigen),lll=1,nraw_eigen)
      npts = nraw_eigen
      if (typeo_eigen.eq.'S') then
c
c  get source scalars for S mode
c
         nmodes=nmodes+1
         om(nmodes)=w
         a0(nmodes)=q
         lmax = max0(l,lmax)
         fl=float(l)
         fl1=fl+1.
         fl3=fl*fl1
         m=-1
         do i=is,js
            m=m+2
            do j=1,4
               f(j,m)=buf(j,i)
            enddo
         enddo
         call cubic(2)
         u=f(1,2)/r0
         v=f(3,2)/r0
         wsq=(w/wn)**2
         e14=f(4,2)+u-v
         if(l.eq.0) e14=0.

         p = buf(5,npts)
         au=-((wsq+2.*fot)*buf(1,npts)+fl1*p)*accn
         av=-(wsq*buf(3,npts)-buf(1,npts)*fot-p)*accn

         if(l.eq.0) av=0.
         vecnl(1,nmodes)=fnl(1)
         vecnl(2,nmodes)=fnl(2)
         vecnl(3,nmodes)=f(2,2)
         vecnl(4,nmodes)=u-.5*fl3*v
         vecnl(5,nmodes)=e14
         vecnl(6,nmodes)=2*v
         vecnl(7,nmodes)=au
         vecnl(8,nmodes)=av
c         print 800,n,l,(vecnl(i,nmodes),i=3,8)
  800    format(2i4,6e15.7)
      else if (typeo_eigen.eq.'T')  then
c
c  get source scalars for T mode
c
         nmodet=nmodet+1
         omt(nmodet)=w
         a0t(nmodet)=q
         lmax = max0(l,lmax)
         m=-1
         do i=is,js
            m=m+2
            do j=1,2
               f(j,m)=buf(j,i)
            enddo
         enddo
         call cubic(1)
         wr=f(1,2)/r0
         wsq=(w/wn)**2

         aw=-buf(1,npts)*wsq*accn

         vecnlt(1,nmodet)=fnl(1)
         vecnlt(2,nmodet)=fnl(2)
         vecnlt(3,nmodet)=aw*(wr-f(2,2))
         vecnlt(4,nmodet)=-4.*aw*wr
c         print 801,n,l,(vecnlt(i,nmodet),i=3,4)
  801    format(2i4,6e15.7)
      endif
      goto 10
  30  goto 8
   9  close(7)
      print *,'# sph. modes in band =',nmodes,'  must be .le. ',meig
      print *,'# tor. modes in band =',nmodet,'  must be .le. ',meig
      return
      end


      subroutine grnflt(grn,iscan,ifilt)
c
c....multiply green's functions by instrument response
c
      real*8 s,sh
      complex resp,zed
      common/respX/resp(4501)
      common/workX/s(9002)
      common/myhed/nscan,nsta,nchn,ntyp,iy,id,ih,im,ss,dt,spare(20)
      dimension grn(*)
      nh=iscan/2
      nha=nh+1
      sh=dt
      do 10 j=1,ifilt
         k1=(j-1)*iscan
         do 15 i=1,iscan
   15       s(i) = grn(k1+i)
         isn = -1
         call fftldp(s,iscan,isn,sh,kerr)
         if (kerr.ne.0) stop 'fft failed'
         i = 1
         do 20 jj=2,nha
            i=i+2
            zed   = cmplx(s(i),s(i+1)) * resp(jj)
            s(i)  = real(zed)
   20       s(i+1)= aimag(zed)
         s(1)=0.0d0
         s(2)=0.0d0
         isn=-2
         call fftldp(s,iscan,isn,sh,kerr)
         if (kerr.ne.0) stop 'fft failed'
         do 25 i=1,iscan
   25       grn(k1+i)=s(i)
   10    continue
      return
      end

      subroutine inresp(iscan)
c
c....instrument response
c
      real*8 df,pi,sh
cxx (MB)      complex evresh,resp,z
      complex evresh,resp
      common/respX/resp(4501)
      common/myhed/nscan,nsta,nchn,ntyp,iy,id,ih,im,ss,dt,spare(20)
      data pi/3.14159265358979d0/
      character*4 ntyp,nsta,nchn
      call pznall(nsta,nchn,ntyp,iy,id,istat)
      nh=iscan/2
      nha=nh+1
      sh=dt
      df=2.d0*pi/(sh*iscan)
      resp(1)=(1.,0.)
      do i=2,nha
         wr      = (i-1)*df
         resp(i) = evresh(wr)/cmplx(0.,wr)
      enddo
      return
      end

      subroutine s4t6(grn,nscan,c1,s1,c2,s2)
      implicit real*8(a-h,o-z)
      real*4 grn(nscan,*)
      do 1 i=1,nscan
         f2=grn(i,2)
         f5=grn(i,3)
         f6=grn(i,4)
         grn(i,2)=f2+f6*c2
         grn(i,3)=f2-f6*c2
         grn(i,4)=f5*c1
         grn(i,5)=f5*s1
    1    grn(i,6)=f6*s2
      return
      end


      subroutine s10t12(grn,nscan,c1,s1,c2,s2)
      implicit real*8(a-h,o-z)
      real*4 grn(nscan,*)
      s3=s2/4.d0
      do 1 i=1,nscan
         f11=grn(i,5)-grn(i,9)
         f12=4.d0*grn(i,6)-grn(i,10)
         grn(i,9)=f12*s3
         grn(i,10)=-f11*s1
         grn(i,11)=f11*c1
         grn(i,12)=f12*c2
         f2=grn(i,2)
         f5=grn(i,3)-grn(i,7)
         f6=grn(i,4)-grn(i,8)
         grn(i,2)=f2+f6*c2
         grn(i,3)=f2-f6*c2
         grn(i,4)=f5*c1
         grn(i,5)=f5*s1
         grn(i,6)=f6*s2
         grn(i,7)=0.d0
    1    grn(i,8)=-f12*s3
      return
      end


      subroutine rotate(grn,nscan,cz,sz,az1,az2)
      implicit real*8(a-h,o-z)
      real*4 grn(*),az1,az2
      data pi/3.14159265358979d0/
      pi2=0.5d0*pi
      len=6*nscan
      if(az1.eq.0.0.and.abs(az2-pi2).lt.1.d-4) then
        do i=1,len
           j=len+i
           d1=-grn(i)*cz-grn(j)*sz
           d2=-grn(i)*sz+grn(j)*cz
           grn(i)=d1
           grn(j)=d2
        enddo
      else
        fdel=cos(az2-az1-pi2)
        sb=sin(az2-pi2)/fdel
        cb=cos(az2-pi2)/fdel
        sa=sin(az1)/fdel
        ca=cos(az1)/fdel
        do i=1,len
           j=len+i
           d1=-grn(i)*cz-grn(j)*sz
           d2=-grn(i)*sz+grn(j)*cz
           grn(i)=d1*cb+d2*sb
           grn(j)=-d1*sa+d2*ca
        enddo
      end if
      return
      end

      subroutine cubic(itype)
      real*8 a1,a2,c0,c1,c2,c3,x,y,y2,y3
      common/sclXXX/x1,r0,x2,f(4,3)
      common/cubXXX/y,y2,y3,x,a1,a2
      data isw/1/
      if(isw.ne.1) goto 10
      isw=0
      y=x2-x1
      y2=y**2
      y3=y*y2
      x=r0-x1
      a1=3./y2
      a2=2./y3
   10 continue
      do 20 i=1,itype
         k=2*i
         j=k-1
         c0=f(j,1)
         c1=f(k,1)
         c3=f(j,3)-c0
         c2=a1*c3-(2.*c1+f(k,3))/y
         c3=(c1+f(k,3))/y2-a2*c3
         f(j,2)=c0+x*(c1+x*(c2+x*c3))
   20    f(k,2)=c1+x*(2.*c2+3.*x*c3)
      return
      end


      subroutine zfcns(lmax,c,s)
c  zfcns computes z(m,l,theta) and dz(m,l,theta)/dtheta,
c  denoted z and p resp. all functions for 0 le m le
c  max(2,l) and 0 le l le lmax are computed. c is cos(theta)
c  and s is sin(theta).
c    Z(m,l,theta) = b(m,l) * X(m,l,theta) where
c        b(m,l) is given in G&D (1975) equation (21)
c  and   X(m,l,theta) is given in G&D (1975) equation (2)
c          G&D = Gilbert & Dziewonski
      include "green.h"
      implicit real*8(a-h,o-z)
      common/zfXX/z(3,ml),p(3,ml)
      data pi/3.14159265358979d0/
      z(1,1)=1d0/(4.d0*pi)
      z(2,1)=0.d0
      z(3,1)=0.d0
      z(1,2)=3d0*c*z(1,1)
      z(2,2)=1.5d0*s*z(1,1)
      z(3,2)=0.d0
      z(1,3)=2.5d0*(c*z(1,2)-z(1,1))
      z(2,3)=5.d0*c*z(2,2)
      z(3,3)=1.25d0*s*z(2,2)
      p(1,1)=0.d0
      p(2,1)=0.d0
      p(3,1)=0.d0
      p(1,2)=-2.d0*z(2,2)
      p(2,2)=0.5d0*z(1,2)
      p(3,2)=0.d0
      p(1,3)=-2.d0*z(2,3)
      p(2,3)=1.5d0*z(1,3)-2.d0*z(3,3)
      p(3,3)=0.5d0*z(2,3)
      if(lmax.le.2) return
      lmaxp1=lmax+1
      tlp1=5d0
      do 10 lp1=4,lmaxp1
      l=lp1-1
      lm1=l-1
      elmm=l
      elpmm1=lm1
      tlp1=tlp1+2d0
      tlm3=tlp1-4d0
      do 10 mp1=1,3
      r=tlp1/elmm
      q=elpmm1/tlm3
      z(mp1,lp1)=r*(c*z(mp1,l)-q*z(mp1,lm1))
      p(mp1,lp1)=r*(c*p(mp1,l)-s*z(mp1,l)-q*p(mp1,lm1))
      elmm=elmm-1d0
   10 elpmm1=elpmm1+1d0
      return
      end
      subroutine panlsp(a,len,n1,n2,np)
c   find gaps of bad data, previously flagged, zero them and index them.
c   the resulting time series consists of np panels of good data indexed
c   from n1(i) to n2(i), i=1 to np.
      dimension a(*),n1(*),n2(*)
      data zero/0./,badun/3.5e15/
      id=2
      np=0
      do 30 i=1,len
      go to (15,20),id
   15 if(a(i).lt.badun) goto 30
      a(i)=zero
      n2(np)=i-1
      id=2
      go to 30
   20 if(a(i).gt.badun) goto 25
      np=np+1
      n1(np)=i
      id=1     
      goto 30
   25 a(i)=zero
   30 continue
      if(id.eq.2) return
      n2(np)=len
      return
      end


      integer function lenb(pathname)
c counts number of non-blank characters in the name and left justifies
c if there are leading blanks
      character*(*) pathname
      lenb=0
      jchar=len(pathname)
      do 5 i=1,jchar
        k=i
        if(pathname(i:i).ne.' ') goto 10
    5   continue
      return
   10 do 20 i=k,jchar
        if(pathname(i:i).eq.' ') goto 30
   20   lenb=lenb+1
   30 if(k.eq.1) return
      do 40 i=1,lenb
        j=k+i-1
   40   pathname(i:i)=pathname(j:j)
      return
      end

      subroutine newsta(nsta,nchn,iy,id,th,ph,azim)
      character*90 filename
      character*4 tsta,nchn,nsta
      character*3 tchn
      integer ty1,td1,ty2,td2,nix
      parameter(ix=100000)
      common/staXXX/nix,tsta(ix),tchn(ix),
     &    ty1(ix),td1(ix),ty2(ix),td2(ix),
     &    tlat(ix),tlon(ix),tele(ix),azi(ix)
      DATA RAD/.0174532/
      data iflag/1/
      if(iflag.eq.1) then
cxx (MB)  filename='/Users/guy/resp.dir/LIST.STA'
        filename='LIST.STA'
        open(99,file=filename(1:lenb2(filename)))
        n=1
   98   read(99,96,end=97) tsta(n),tchn(n),ty1(n),td1(n),
     &      ty2(n),td2(n),tlat(n),tlon(n),tele(n),azi(n)
cxx (MB) 96   format(a4,1x,a3,2x,i4,1x,i3,1x,i4,1x,i3,1x,f10.5,1x,f10.5,1x,
   96   format(a4,2x,a3,2x,i4,1x,i3,1x,i4,1x,i3,1x,f10.5,1x,f10.5,1x,
     +       f6.0,1x,f10.5)
        n=n+1
        go to 98
   97   close(99)
        nix=n-1
        if(nix.gt.ix) stop 'resize dimension of newsta'
        iflag=0
      end if
      do 94 i=1,nix
        if(nsta.ne.tsta(i)) goto 94
        if(nchn(1:3).ne.tchn(i)) goto 94
        if(iy.lt.ty1(i)) goto 94
        if(iy.gt.ty2(i)) goto 94
        if(iy.eq.ty1(i).and.id.lt.td1(i)) goto 94
        if(iy.eq.ty2(i).and.id.ge.td2(i)) goto 94
        th=(90.-tlat(i))*rad
        ph=tlon(i)
        if(ph.lt.0.) ph=360+ph
        ph=ph*rad
        azim=azi(i)*rad
        return
   94 continue
      print *, 'station not found in newsta',nsta
      return
      end

