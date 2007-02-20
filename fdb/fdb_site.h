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
c  Header: fdb_site.h
c  site declarations (3.0)
c
c**************************************************************************
        integer*4 msite, nrowsite
	parameter (msite = 10000)
	character*4 statype_site 
	character*6 sta_site, refsta_site
	character*17 lddate_site 
	character*50 staname_site
	integer*4 ondate_site, offdate_site	
	real*4    lat_site,lon_site,elev_site, deast_site, dnorth_site
	common/c_site/sta_site(msite), ondate_site(msite), 
     *        offdate_site(msite),lat_site(msite), lon_site(msite), 
     *        elev_site(msite), staname_site(msite), 
     *        statype_site(msite), refsta_site(msite), 
     *        dnorth_site(msite), deast_site(msite), 
     *        lddate_site(msite), nrowsite
