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
c  Header: fdb_sitechan.h
c  sitechan declarations (3.0)
c
c**************************************************************************
        integer*4 msitechan, nrowsitechan
	parameter(msitechan=10000)
	character*4 ctype_sitechan
	character*6 sta_sitechan
	character*8 chan_sitechan
	character*17 lddate_sitechan
	character*50 descrip_sitechan
	integer*4 ondate_sitechan, offdate_sitechan, chanid_sitechan
	real*4    edepth_sitechan, hang_sitechan, vang_sitechan
	common/csitechan/sta_sitechan(msitechan), 
     *     chan_sitechan(msitechan),
     *     ondate_sitechan(msitechan), chanid_sitechan(msitechan), 
     *     offdate_sitechan(msitechan), ctype_sitechan(msitechan), 
     *     edepth_sitechan(msitechan), hang_sitechan(msitechan), 
     *     vang_sitechan(msitechan), descrip_sitechan(msitechan),
     *     lddate_sitechan(msitechan), nrowsitechan
