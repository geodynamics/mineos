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
c  syndat program makes synthetic seismograms by convolutions of
c  Green functions with the seismic moment tensor of the choosen event.
c
c**************************************************************************
      program syndat
      implicit none
      include 'fdb/fdb_wfdisc.h'
      include 'fdb/fdb_io.h'
      include 'green.h'
c----
      real*4 a
      common/rcrds/a(6*mseis)
      real*8 rh1
      common/rhs/rh1(mseis)
      real*4 dt,fnorm
      integer*4 nscan, itptens, jys, jds, jhs, jms
      integer*4 l, i, nfw, ifl, ierr, j, ib, nh, idout
      integer*4 iscan, iscan2, np1, isn, kerr,lnblnk
      real*8    pi,sss,slat,slon,sdep,tconst,dm0,fscal,ww
      real*8    si,df,wr,con,arg,sinc2,frp,dati,fip,datr,sol(6)
      real*8    p1(3),p2(3),un(3),us(3)
      character*256 efname
      character*8 ename
      character*2 endian
c--
      data pi/3.14159265358979d0/            
      data fnorm/1.0e-27/
c
c    read input parameters
c
c....read in file name within event and moment tensor info
      write(*,*) '============== Program syndat =================='
      write(*,*) 'enter input CMT file name:'
      read(*,'(a256)') fname1
      write(*,*) fname1(1:lnblnk(fname1))
c.... setup tensor type: moment/Mo,nodal2/Mo,nodal2
      write(*,*) 'enter tensor type: 0 - moment, ',
     *     '1 - nodal plane 1, 2 - nodal plane 2'
      read(*,*) itptens
      write(*,*) itptens
c.... i/o database name
      write(*,*) 'enter input dbname'
      read(*,'(a256)') dbin
      write(*,*) dbin(1:lnblnk(dbin))
      write(*,*) 'enter output dbname'
      read(*,'(a256)') dbout
      write(*,*) dbout(1:lnblnk(dbout))
c.... type of output data
      write(*,*) 'Enter output datatype: 0: acceleration, 1: velocity,',
     *     '2: displacement'
      read(*,*)  idout
      if(idout.lt.0.or.idout.gt.2) then
         write(*,*) 'WARNING: syndat: wrong output datatype = ',
     *        idout,', assumed accereration = 0'
         idout = 0
      endif
      write(*,*) idout
c
c    read source and moment
c
      open(12,file=fname1,status='old')
      read(12,*) ename,jys,jds,jhs,jms,sss,slat,slon,sdep,dt,
     *           tconst,dM0,(sol(l),l=1,6),fscal,(p1(l),l=1,3),
     *           (p2(l),l=1,3)
      close(12)
      write(*,1001) ename,jys,jds,jhs,jms,sss,slat,slon,sdep,dt,
     *           tconst,dM0,(fscal*sol(l),l=1,6),(p1(l),l=1,3),
     *           (p2(l),l=1,3)
 1001 format('syndat: CMT solution:',/
     * 'syndat: Event: ',a8,i5,i4,i3,':',i3,':',f5.1,', lat/lon = ',
     * f8.2,f9.2,',',/
     * 'syndat: depth = ',f6.1,' km, step = ',f8.3,
     * ' sec, Duration = ',f5.1, ' sec,',/
     * 'syndat: M0 = ',e11.3,','/
     * 'syndat: comp: ',6e11.3,/
     * 'syndat: plane1: ',3f5.0,' plane2: ',3f5.0)
      if(itptens.eq.0) then
        do i = 1,6
          sol(i) = sol(i)*fscal*fnorm
        enddo
      else if(itptens.eq.1) then
        call udc(p1(1),p1(2),p1(3),un,us,sol)  
        do i = 1,6
          sol(i) = sol(i)*dM0*fnorm
        enddo
      else
        call udc(p2(1),p2(2),p2(3),un,us,sol)  
        do i = 1,6
          sol(i) = sol(i)*dM0*fnorm
        enddo
      endif
      write(*,1002) (sol(l)/fnorm,l=1,6)
 1002 format('syndat: Selected moment tensor components:',/
     *       'syndat:',6e11.3,//'syndat: Synthetic waveforms: ')
c
c....loop over records
c      
c.....read input wfdisc relation
      call read_wfdisc
      nfw = nrowwfdisc
      do 30 ifl=1,nfw    
        nscan =nsamp_wfdisc(ifl)
        write(*,1000) ifl,sta_wfdisc(ifl),chan_wfdisc(ifl),
     *                1.0/samprate_wfdisc(ifl), nscan/6
 1000 format('syndat: ',i4,1x,a6,1x,a8,1x,f8.3,1x,i7)
        call get_wfdisc(ifl,nscan,a,ierr)
        nscan = nscan/6
        do i=1,nscan
          rh1(i) = 0.d0
        enddo
        do j = 1,6
          ib=(j-1)*nscan
          do i = 1,nscan
            rh1(i)=rh1(i)+a(ib+i)*sol(j)
          enddo
        enddo
        do i=1,nscan
          a(i)=rh1(i)
        enddo
c
c make correction for halfduratio, if tconst > 0
c convert data to velocity or displacement, if it necessarily
c  
        if(tconst.le.0.d0.and.idout.eq.0) goto 90
        si=dt
        nh=(nscan+1)/2
        call facdwn(nh)
        iscan=2*nh
        iscan2=iscan+2
        np1=nscan+1
        do i=np1,iscan2
          a(i)=0.
        enddo
        isn=-1
        call fftl(a,iscan,isn,kerr)
        df=2.d0*pi/(iscan*si)
        j=0
        do i=3,iscan2,2
           j=j+1
           wr=j*df
           if(tconst.ge.0.d0) then
             con=0.5d0*tconst*wr
             arg=0.5d0*con
             sinc2=(dsin(arg)/arg)**2
             frp=sinc2*dcos(con)
             fip=-sinc2*dsin(con)
             datr=a(i)*frp-a(i+1)*fip
             dati=a(i)*fip+a(i+1)*frp
             a(i)=datr
             a(i+1)=dati
           endif
c convert from acceleration to velocity
           if(idout.gt.0) then
             ww = a(i)
             a(i) = a(i+1)/wr
             a(i+1) = -ww/wr
           endif
c convert from velocity to displacement
           if(idout.gt.1) then
             ww = a(i)
             a(i) = a(i+1)/wr
             a(i+1) = -ww/wr
           endif
        enddo
        isn=-2
        call fftl(a,iscan,isn,kerr)
   90   continue
c write new wfdisc relation
        nrowwfdisc = 1
        sta_wfdisc(1) = sta_wfdisc(ifl)
        chan_wfdisc(1) = chan_wfdisc(ifl)
        time_wfdisc(1) = time_wfdisc(ifl)
        wfid_wfdisc(1) = ifl
        jdate_wfdisc(1) = jdate_wfdisc(ifl)
        endtime_wfdisc(1) = time_wfdisc(ifl)+(nscan-1)/
     *        samprate_wfdisc(ifl)
        nsamp_wfdisc(1) = nscan
        chanid_wfdisc(1) = chanid_wfdisc(ifl)
        samprate_wfdisc(1) = samprate_wfdisc(ifl)
        segtype_wfdisc(1) = 'w'
        foff_wfdisc(1) = 0
        dir_wfdisc(1) = '.'
        write(efname,'("w.",i5)') ifl
        do i = 1,7
          if(efname(i:i).eq.' ') efname(i:i) = '0'
        enddo
        dfile_wfdisc(1) = efname
        calper_wfdisc(1) = 20.0
        calib_wfdisc(1) = 1.0
        datatype_wfdisc(1) = endian()
        do i = 1,nscan
           a(i) = a(i)*1.0e9
        enddo
        call put_wfdisc(1,nscan,a,ierr)
        call write_wfdisc
   30 continue                      
      end

c*********************************************************************
c  udc computes the unit normal, unit slip and unit moment tensor
c  given strike(sig), dip(del) and slip(gam) in degrees.
c*********************************************************************
      subroutine udc(sig,del,gam,un,us,f)
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
