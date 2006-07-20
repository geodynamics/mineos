      program syndat
c
c.....syndat makes up synthetic seismograms from green's functions
c
      implicit real*8(a-h,o-z)
      real*4 ss,dt,a,flag,hed1(30),spare,zero
      character*1l2,k2,k3
      common/rcrds/a(108000)
      common/myhed/nscan,nsta,nchn,ntyp,iy,id,ih,im,ss,dt,spare(20)
      common/rhs/rh1(10000)
      dimension sol(6),n1(100),n2(100)
      equivalence (nscan,hed1)
      data pi/3.14159265358979d0/            
      data itype1,zero,flag/1,0.,5.e15/
      open(7,file='/home/guy/lfsyn.dir/locale.cmt.big')
   21 write(*,101)
  101 format('Do you want to use moment tensor in locale ? (y/n) : ',$)
      read(*,'(a1)',err=21)l2               
      if(l2.ne.'y'.and.l2.ne.'Y') goto 1
      goto 22
    1 write(*,102)
  102 format('Do you want to use strike,dip and slip ? (y/n)     : ',$)
      read(*,'(a1)',err=1)k2
      if(k2.ne.'y'.and.k2.ne.'Y') goto 10
      call fault(sol)
      goto 20
   10 write(*,103)
  103 format('enter moment tensor elements f(x6) : ',$)
      read(*,*,err=10)sol
   20 write(*,104)
  104 format('enter time constant : ',$)
      read(*,*,err=20)tconst                
   22 call gfs_opena(1,'greens function file name : ',itype1,jret1)
      call gfs_opena(2,'output file name          : ',itype1,jret2)
      call gfs_erase(2)
    2 write(*,105) 
  105 format('Do you want synthetics only ? (y/n)                : ',$)
      read(*,'(a1)',err=2)k2
    3 write(*,106)
  106 format('Do you want the synthetics to be gap free ? (y/n)  : ',$)
      read(*,'(a1)',err=3)k3
      ifl=1
      call gfs_rwdir(1,hed1,ifl,'r',ierr)
      if (ierr.ne.0) goto 50
      if(l2.ne.'y'.and.l2.ne.'Y') goto 34
c
c.... locate source parameters
c
   31 read(7,100,end=32) jy,jd,jh,jm,sec,t0,p0,d0,fm,
     +     (sol(i),i=1,6),tconst,fscal
  100 format(i4,i4,2i3,f5.1,2f9.2,f4.0,g11.3,7f7.2,g11.3)
      if(iy.eq.jy.and.id.eq.jd.and.jh.eq.ih) goto 33
      goto 31
   32 print*,'***** event not found in locale! *****'
      close(7)
      l2 = 'n   '
      goto 1
   33 close(7)
      fm=fm*1.e-27
      fscal=fscal*1.e-27
      do i=1,6
         sol(i)=sol(i)*fscal
      enddo
c
c....loop over records
c      
   34 isy = 0
      do 30 ifl=1,jret1,2       
      igr = ifl + 1
c
c....read in record
c
      call gfs_rwentry(1,hed1,a,ifl,'r')
      call panlsp(a,nscan,n1,n2,np)
      if(k2.eq.'y'.or.k2.eq.'Y') goto 60
      call fgapsp(a,nscan,n1,n2,np,flag)
c
c....write record to disk
c             
      isy = isy + 1
      call gfs_rwentry(2,hed1,a,isy,'w')
c
c....read in green's functions
c
   60 nold = nscan
      call gfs_rwentry(1,hed1,a,igr,'r')
      len = nscan
      nscan = nold
      do 71 i=1,nscan
  71     rh1(i)=0.d0
      do 72 j=1,6
         ib=(j-1)*nscan
         if(k3.ne.'y'.and.k3.ne.'Y')
     *      call fgapsp(a(ib+1),nscan,n1,n2,np,zero)
         do 72 i=1,nscan
   72       rh1(i)=rh1(i)+a(ib+i)*sol(j)
      do 81 i=1,nscan
   81    a(i)=rh1(i)

      if(tconst.le.0.d0) goto 90

      si=dt
      nh=(nscan+1)/2
      call facdwn(nh)
      iscan=2*nh
      iscan2=iscan+2
      np1=nscan+1
      do 82 i=np1,iscan2
   82    a(i)=0.
      isn=-1
      call fftl(a,iscan,isn,kerr)
      df=2.d0*pi/(iscan*si)
      j=0
      do 83 i=3,iscan2,2
         j=j+1
         wr=j*df
         con=0.5d0*tconst*wr
         arg=0.5d0*con
         sinc2=(dsin(arg)/arg)**2
         frp=sinc2*dcos(con)
         fip=-sinc2*dsin(con)
         datr=a(i)*frp-a(i+1)*fip
         dati=a(i)*fip+a(i+1)*frp
         a(i)=datr
   83    a(i+1)=dati
      isn=-2
      call fftl(a,iscan,isn,kerr)
   90 if(k3.ne.'y'.and.k3.ne.'Y') call fgapsp(a,nscan,n1,n2,np,flag)
c
c....write out synthetics 
c                                  
      isy  = isy + 1 
c....force SNR flag of synthetic to be zero
      hed1(30)=0.
      call gfs_rwentry(2,hed1,a,isy,'w') 
      call prhdr(isy,hed1,itype1)
   30 continue                      
   50 call gfs_close(1)
      call gfs_close(2)
      stop
      end


      subroutine fault(sol)
      implicit real*8(a-h,o-z)
      common/faultX/un(3),us(3)
      dimension sol(*)
    1 write(*,100)
  100 format('enter strike, dip and slip [deg] : ',$)
      read(*,*,err=1)sig,del,gam
      call udc(sig,del,gam,un,us,sol)
    2 write(*,101)
  101 format('enter moment : ',$)
      read(*,*,err=2)fmom
      do 5 i=1,6
    5 sol(i)=fmom*sol(i)
      return
      end


      subroutine udc(sig,del,gam,un,us,f)
c
c  udc computes the unit normal, unit slip and unit moment tensor
c  given strike(sig), dip(del) and slip(gam) in degrees.
c
      implicit real*8(a-h,o-z)
      dimension un(*),us(*),f(*)
      data rad/57.29578/
      s=sig/rad
      d=del/rad
      g=gam/rad
      cs=dcos(s)
      ss=dsin(s)
      cd=dcos(d)
      sd=dsin(d)
      cg=dcos(g)
      sg=dsin(g)
      un(1)=cd
      un(2)=sd*ss
      un(3)=sd*cs
      us(1)=sg*sd
      us(2)=-sg*cd*ss-cg*cs
      us(3)=-sg*cd*cs+cg*ss
      f(1)=2.*un(1)*us(1)
      f(2)=2.*un(2)*us(2)
      f(3)=2.*un(3)*us(3)
      f(4)=un(1)*us(2)+us(1)*un(2)
      f(5)=un(1)*us(3)+us(1)*un(3)
      f(6)=un(2)*us(3)+us(2)*un(3)
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
      subroutine fgapsp(a,len,n1,n2,np,flag)
c  fgapsp flags bad gaps in a time series with flag(usually zero
c  or 5.e15). fgapsp presupposes a call to panlsp to put
c  panel structure in n1 and n2.
      dimension a(*),n1(*),n2(*)
      if(np.gt.0) goto 15
      do 10 i=1,len
   10 a(i) = flag
      return 
   15 if(n1(1).eq.1) go to 25
      mm1=n1(1)-1
      do 20 i=1,mm1
   20 a(i)=flag
   25 if(n2(np).eq.len) go to 35
      mp1=n2(np)+1
      do 30 i=mp1,len
   30 a(i)=flag
   35 if(np.eq.1) return
      nbad=np-1
      do 45 nb=1,nbad
      m1=n2(nb)+1
      m2=n1(nb+1)-1
      if(m2.lt.m1) go to 45
      do 40 m=m1,m2
   40 a(m)=flag
   45 continue
      return
      end
