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
c  green program computes for a single event and a given set of stations,
c  the Green functions.
c
c**************************************************************************
      program green
      implicit none
      include "green.h"
      include "fdb/fdb_io.h"
      include "fdb/fdb_site.h"
      include "fdb/fdb_sitechan.h"
      include "fdb/fdb_wfdisc.h"
c---
      real*4 scalrs(8)
      real*8 co,si,c1,c2,s1,s2,z,zp,cz,sz,caz,saz,prp
      common/dheadX/d0,th,ph,jy,jd,jh,jm,sec,tstart
      common/zfXX/z(3,ml),zp(3,ml)
      common/grnX/grn(12*mseis)
      common/modeX/om(meig),a0(meig),omt(meig),a0t(meig)
      common/vecnlX/vecnl(8,meig),vecnlt(4,meig)
      common/wts/wt(6,meig)
      common/sclrX/n,l,e1,e2,e3,e4,au,av
      common/propX/prp(6*mseis)
c--- local variables ---
      integer*4 lnblnk,numchan(msitechan),numsta(msite)
      real*8 time,endtime,htoepoch
      real*4    d0,th,ph,sec,tstart,ss,dt,slat,slon,sdep,
     *          sss, grn, om, a0, omt, a0t, vecnl, vecnlt,
     *          wt,e1,e2,e3,e4,au,av,pi,rad,fmin,fmax,
     *          t0,p0,c0,s0,t1,p1,dp,del,azim
      integer*4 i,lmax,nmodes,nmodet,j,lp1,indx,
     *          jy,jd,jh,jm,nscan,iy,id,ih,im,jys,jds,jhs,jms,
     *          n,l,len,mchn,icomp,itype1,iscan,ifl,isq,ii
      integer*4 nchan,inchn,iseq,jdate,ierr,nc(100),ns(100)
      real*4    ang(3)
      character*8 ename
      character*256 efname
c--- equivalence and data ---
      equivalence (n,scalrs)
      data pi,rad,tstart/3.14159265359,57.29578,0./
      data icomp,itype1/0,1/
c
c    read input parameters
c
c....read in dbname with static relations: sta, stachan
      write(*,*) '============= Program green ===================='
      write(*,*) 'enter path to db with sta & stachan:'
      read(*,'(a256)') dbin
      write(*,*) dbin(1:lnblnk(dbin))
c....read in name of file containing list of dbnames.
c....each dbname in list refferes ito db with .eigen relation.
      write(*,*) 'enter name of file within list of nmodes db:'
      read(*,'(a256)') fname3
      write(*,*) fname3(1:lnblnk(fname3))
c....read in file within event and moment tensor info
      write(*,*) 'enter input CMT file name:'
      read(*,'(a256)') fname4
      write(*,*) fname4(1:lnblnk(fname4))
c....read in frequency range in mHz
      write(*,*) 'min and max frequencies to be considered (mHz) : '
      read(*,*) fmin,fmax
      write(*,*) fmin,fmax
c....read in number of samples in Green function
      write(*,*) 'enter # pts in greens fns .le. ',mseis,' :'
      read(*,*) iscan
      write(*,*) iscan
c.... read in output path to gsf name to store Green functions
      write(*,*) 'enter Green functions output db file name:'
      read(*,'(a256)') dbout
      write(*,*) dbout(1:lnblnk(dbout))
      write(*,*) '===================================================='
c
c.....open event file for which you want to compute green's functions
c.....read first line to get source  and moment tensor info
c
      open(12,file=fname4,status='old')
      read(12,*) ename,jys,jds,jhs,jms,sss,slat,slon,sdep,dt
      close (12)
      write(*,1001) ename,jys,jds,jhs,jms,sss,slat,slon
      write(*,1002) sdep
 1001 format(' green: Event: ',a8,2x,i4,1x,i3,1x,i2,':',i2,':',f6.3,1x,
     *       'lat = ',f8.3,', lon = ',f9.3)
 1002 format(' green:        source depth =',f5.1,' km')
      write(*,1003) dt,iscan
 1003 format(' green: step = ',f8.3,' sec, nsamples =',i7)
c convert human time to epoch time
      time = htoepoch (jys,jds,jhs,jms,dble(sss))
      endtime = time+dt*(iscan-1)
      jdate = jys*1000+jds
c--
      d0=sdep
cxx   th=90.-slat
c.....convert event geographic latitude to geocentric
      th = 90.0-atan(0.99329534*tan(slat/rad))*rad
      ph=slon
      jy=jys
      jd=jds
      jh=jhs
      jm=jms
      sec=sss
c
c.....open output file for green's functions
c
      fmin = fmin*pi/500.
      fmax = fmax*pi/500.

      call source(fmin,fmax,lmax,nmodes,nmodet)

      if(lmax.le.ml) goto 5
      write(*,*) 'ERR010: green: max l =',lmax,' must be .le.',ml
      stop '  '
cxx   goto 99
    5 iscan = iscan/2
      call factor(iscan)
      iscan = 2*iscan
      if(iscan.gt.mseis) then
         write(*,*) 'WARNING: green: # of points in Green ',
     *      'functions is stripped to ',mseis
      endif
      iscan=min0(iscan,mseis)
      t0=th/rad
      p0=ph/rad
      c0=cos(t0)
      s0=sin(t0)
c
c====loop over records
c........read event header
c
c.....open file with station & channel info ---
      write(*,*) 'green: Input dbname : ',dbin(1:lnblnk(dbin))
      call read_site
      call read_sitechan
c.....select channel sequence ---
      call select_sitechan(jdate,nchan,numchan,numsta)
      open(12,file=fname4,status='old')
      inchn = 1
      ifl = 0
c.....choose new single or triple of station(s)
  6   iseq = 1
cxx
      do ii = 1,12*iscan
        grn(ii) = 0.0
      enddo
cxx   if(ifl.ge.30) goto 88
  66  if(inchn.gt.nchan) goto 88
      nc(iseq) = iabs(numchan(inchn))
      ns(iseq) = numsta(inchn)
      inchn = inchn+1
      iseq = iseq+1
      if(numchan(inchn-1).le.0) goto 7
      goto 66
   7  iseq = iseq-1
c.....print station and channel info ---
      write(*,1004) sta_site(ns(1)),lat_site(ns(1)),
     *           lon_site(ns(1)),iseq
 1004 format(' green: Station: ',a6,1x,f9.4,f10.4,' , Channels: ',i1)
      do i = 1,iseq
        write(*,*) 'green: Channel: # ',i,'  ',sta_sitechan(nc(i)),
     *       chan_sitechan(nc(i)),hang_sitechan(nc(i)),
     *       vang_sitechan(nc(i))
      enddo
c....check channels sequence: Z,N,E ---
      if(iseq.gt.3) then
         write(*,*) 'WARNING: green: # of channels is stripped to 3'
         iseq = 3
      endif
      if(iseq.eq.2) then
         write(*,*) 'WARNING: green: # of channels is stripped to 1'
         iseq = 1
      endif
      ang(1) = (vang_sitechan(nc(1))-90.0)/rad
      if(abs(vang_sitechan(nc(1))).gt.0.5) then
         write(*,*)
     *   'WARNING: green: Channel: # ',1,' is not vertical. ',
     *            'Sequence ignored'
         goto 6
      endif
      do i = 2,iseq
        ang(i) = hang_sitechan(nc(i))/rad
        if(abs(vang_sitechan(nc(i))-90.0).gt.0.5) then
           write(*,*)
     *     'WARNING: green: Channel: # ',i,' is not horizontal. ',
     *             'Sequence ignored'
           goto 6
        endif
      enddo
c
c compute green functions for selected channels
c
      icomp = 0
      do 90 isq=1,iseq
c.....extract channel parameters from relation tables 
      ifl = ifl+1
      mchn = isq
      iy = jy
      id = jd
      ih = jh
      im = jm
      ss = sec
cxx   t1 = (90.0-lat_site(ns(1)))/rad
c.....convert station geographic latitude to geocentric
      t1 = (90.0-atan(0.99329534*tan(lat_site(ns(1))/rad))*rad)/rad
      p1 = lon_site(ns(1))
      if(p1.lt.0.0) p1 = 360.0+p1
      p1 = p1/rad

      if(icomp.eq.2) goto 500
      len=0
      if(icomp.eq.1) goto 35
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

      del = datan2(si,co)
      del = del/pi*180.
      write(*,'(a,f10.3)')' green: Epicentral Distance : ',del

      sz=s1*sin(dp)/si
      cz=(c0*co-c1)/(s0*si)
      saz=-s0*sin(dp)/si
      caz=(c0-co*c1)/(si*s1)

      azim = datan2(saz,caz)
      azim = azim/pi*180.
      write(*,'(a,f10.3)')' green: Azimuth of Source   : ',azim

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
      call rotate(grn,iscan,caz,saz,ang(2),ang(3))
c
c....greens function longer than data ? : add flags to data !
c
cxx  500 nscan=iscan
  500 indx=0
      if(mchn.eq.3) indx=6*iscan
      len=6*iscan
c
c....write out green's functions
c
      nscan = 6 * iscan
      write(*,1000) ifl,sta_sitechan(nc(1)),chan_sitechan(nc(isq)),
     *           jys,jds,jhs,jms,sss,dt,nscan
 1000 format(' green: ',i4,1x,a6,1x,a8,i4,1x,i3,1x,i2,':',i2,':',
     *       f6.3,1x,f8.3,1x,i6)
c write wfdisc relation with green functions
      call default_wfdisc(1)
      nrowwfdisc = 1
      sta_wfdisc(1) = sta_sitechan(nc(1))
      chan_wfdisc(1) = chan_sitechan(nc(isq))
      chanid_wfdisc(1) = nc(isq)
      time_wfdisc(1) = time
      wfid_wfdisc(1) = ifl
      jdate_wfdisc(1) = jdate
      endtime_wfdisc(1) = time+(nscan-1)/dt
      nsamp_wfdisc(1) = nscan
      calib_wfdisc(1) = 1.0
      calper_wfdisc(1) = 20.0
      samprate_wfdisc(1) = 1.0/dt
      segtype_wfdisc(1) = 'g'
      foff_wfdisc(1) = 0
      dir_wfdisc(1) = '.'
      write(efname,'("g.",i5)') ifl
      do i = 1,7
        if(efname(i:i).eq.' ') efname(i:i) = '0'
      enddo
      dfile_wfdisc(1) = efname
      do i = 1,nscan
         grn(indx+i) = -grn(indx+i)
      enddo
      call put_wfdisc(1,nscan,grn(indx+1),ierr)
      call write_wfdisc
   90 continue
      goto 6
   88 continue
      close(12)
99    continue
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
      include "fdb/fdb_eigen.h"
      integer*4 lmax,nmodes,nmodet
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
c --- other variables
      real* 4   w,q,p,rn,wn,accn
      real*4    pi2,fot,vn,rs,fl,fl1,fl3,u,v
      real*4    wsq,e14,au,av,wr,aw
      integer*4 npts,nrecl,ieig,idat,ierr,ifl,i,is,j,js
      integer*4 ll,lll,m,l,n,ik
      character*2 tendia,endian
c ---
      character*64 dir
      character*256 dbname
      real*4      fnl(2),r(mk),buf(6,mk)
c ---
      equivalence (fnl(1),n),(fnl(2),l)
      data fot/1.33333333333/

      pi2 = datan(1.0d0)*8.0d0
      nmodes = 0
      nmodet = 0
      lmax=0
      ieig = 9
      idat = 10
c===================================================================
c Main loop by dbase names
c===================================================================
      open(7,file=fname3,status='old')
c*** read dbnames list
   8  read(7,'(a256)',end=9) dbname
      nrecl = 2000
      call open_eigen(dbname,ieig,idat,nrecl,dir,'r',ierr)
      call read_eigen(ieig,ierr)
      nrecl = (ncol_eigen*nraw_eigen+npar_eigen)*4
      npts = nraw_eigen
      call close_eigen(ieig,idat)
c
c....read in spheroidal and toroidal modes 
c....in frequency band fmin < f < fmax
c
      ifl = 0
      call open_eigen(dbname,ieig,idat,nrecl,dir,'r',ierr)
      
  10  ifl = ifl+1
      call read_eigen(ieig,ierr)
      if(ierr.ne.0) goto 30
      if(ifl.ne.eigid_eigen) stop 
     *          'ERR011:eigen: flat and bin indices are different.'
         w = pi2/per_eigen
         if (w.lt.fmin) goto 10
         if (w.le.fmax) goto 11
          goto 10
  11  read(idat,rec=eigid_eigen) n,l,w,q,rn,vn,accn,
     +     (r(lll),(buf(ll,lll),ll=1,ncol_eigen-1),lll=1,nraw_eigen)
      if(ncol_eigen.eq.3) then
         do ik = 1,nraw_eigen
            buf(3,ik) = 0.0
            buf(4,ik) =0.0
         enddo
      endif
c swap bytes if necessary
      tendia = endian()
      if(datatype_eigen.ne.tendia) then
        call swap1(fnl,4,2)
        call swap1(w,4,1)
        call swap1(q,4,1)
        call swap1(rn,4,1)
        call swap1(vn,4,1)
        call swap1(accn,4,1)
        call swap1(r,4,mk)
        call swap1(buf,4,6*mk)
      endif
      npts = nraw_eigen
      wn = vn/rn
c
c  find radius interpolation points
c
      rs=rn/1000.
      r0=1.-(d0/rs)
      do 5 i=1,npts
         if(r(i).lt.r0)is=i
    5    continue
      x1=r(is)
      js=is+1
      x2=r(js)

      if (typeo_eigen.eq.'S') then
C
c  get source scalars for S mode
c
         nmodes=nmodes+1
         if(nmodes.ge.meig) then
           write(*,*) 'ERR012: green: # sph. modes in band exceed ',
     *           'max allowed number ',meig
         stop '  '
         endif
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
         if(nmodet.ge.meig) then
           write(*,*) 'ERR012: green: # tor. modes in band exceed ',
     *           'max allowed number ',meig
         stop '  '
         endif
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
c 665    continue
      endif
      goto 10
  30  goto 8
   9  close(7)
      write(*,*) 'green: # sph. modes in band =',nmodes,
     *          '  must be .le. ',meig
      write(*,*) 'green: # tor. modes in band =',nmodet,
     *          '  must be .le. ',meig
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
cxx     write(*,*)'Standard position.'
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
      implicit real*8(a-h,o-z)
      include "green.h"
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
