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
c time conversation routines
c
c**************************************************************************
c
c  convert epoch time to human time
c
      subroutine epochtoh(t, year,doy,hour,miin,sec)
      implicit none
      real*8 t,sec,fsec
      integer*4 year,doy,hour,miin
      integer*4 idate, itime, irest,iysupp,idsupp

c...  date parts
      idate = t/86400.0
      iysupp = idate/365
      idsupp = iysupp*365+(iysupp+1)/4
      if(idate.lt.idsupp) iysupp = iysupp-1
      idsupp = iysupp*365+(iysupp+1)/4
c... extract year
      year = 1970+iysupp
c... extract doy
      doy = idate - idsupp+1
c... time part
      fsec = t-idate*86400.0d0
      irest = fsec
      fsec = fsec - irest
      itime = irest
      irest = itime/60
c... extract seconds
      sec = itime - irest*60 + fsec
      itime = irest
      irest = itime/60
c... extract mininutes
      miin = itime - irest*60
c... extract hours
      hour = irest
      return
      end
c
c  convert human time to epoch time
c
      real*8 function  htoepoch (year,doy,hour,miin,sec)
      implicit none
      integer*4 year,doy,hour,miin
      integer*4 vis, ydelay
      real*8    sec
      ydelay = year - 1970
      vis = (ydelay+1)/4
      htoepoch = (365.0d0*ydelay+vis+doy-1)*86400.0d0+
     *       (hour*60.0+miin)*60.0d0+sec
      return
      end
c
c... convert month and day to day of year - doy
c
      integer*4 function mtodoy(year,mon,day)
      implicit none
      integer*4 year,mon,day,i,jday,days(12)
      data days/31,28,31,30,31,30,31,31,30,31,30,31/
c ---
      days(2) = 28
      if(mod(year,4).eq.0) days(2) = 29
      jday = 0
      if(mon.gt.1) then
        do i = 1,mon-1
            jday = jday+days(i)
        enddo
      endif
      mtodoy = jday+day
      return
      end
c
c... convert day of year to month and day
c
      subroutine doytom(year,doy,mon,day)
      implicit none
      integer*4 year,doy,mon,day,i,days(12)
      data days/31,28,31,30,31,30,31,31,30,31,30,31/
c ---
      days(2) = 28
      if(mod(year,4).eq.0) days(2) = 29
      mon = 1
      day = doy
      do i = 1,11
        if(day.le.days(i)) goto 2
        mon = mon+1
        day = day-days(i)
      enddo
   2  continue
      return
      end
c
c get local time in form mm/dd/yy-hh:mm:ss
c
      character*17 function loctime()
      implicit none
      integer*4 itime,ia(9),time,i
      itime=time()
      call ltime(itime,ia)
      write(loctime,1000) ia(5)+1,ia(4),mod(1900+ia(6),100),
     +                    ia(3),ia(2),ia(1)
      do i = 1,17
        if(loctime(i:i).eq.' ') loctime(i:i) = '0'
      enddo
      return
 1000 format(i2,'/',i2,'/',i2,'-',i2,':',i2,':',i2)
      end
