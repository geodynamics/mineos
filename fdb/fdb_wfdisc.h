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
c  Header: fdb_wfdisc.h
c  wfdisc declarations (3.0)
c
c**************************************************************************
        integer*4 mwfdisc, nrowwfdisc
	parameter(mwfdisc = 10000)
	character segtype_wfdisc, clip_wfdisc
	character*2 datatype_wfdisc
	character*6 sta_wfdisc, instype_wfdisc
	character*8 chan_wfdisc
	character*17 lddate_wfdisc
	character*32 dfile_wfdisc
	character*64 dir_wfdisc
	integer*4 wfid_wfdisc, chanid_wfdisc, jdate_wfdisc, 
     *      nsamp_wfdisc, foff_wfdisc, commid_wfdisc
	real*4   samprate_wfdisc, calib_wfdisc, calper_wfdisc
	real*8   time_wfdisc, endtime_wfdisc
	common/c_wfdisc/sta_wfdisc(mwfdisc), chan_wfdisc(mwfdisc), 
     *     time_wfdisc(mwfdisc), wfid_wfdisc(mwfdisc), 
     *     chanid_wfdisc(mwfdisc), jdate_wfdisc(mwfdisc),
     *     endtime_wfdisc(mwfdisc), nsamp_wfdisc(mwfdisc), 
     *     samprate_wfdisc(mwfdisc), calib_wfdisc(mwfdisc), 
     *     calper_wfdisc(mwfdisc), instype_wfdisc(mwfdisc), 
     *     segtype_wfdisc(mwfdisc), datatype_wfdisc(mwfdisc), 
     *     clip_wfdisc(mwfdisc), dir_wfdisc(mwfdisc), 
     *     dfile_wfdisc(mwfdisc), foff_wfdisc(mwfdisc), 
     *     commid_wfdisc(mwfdisc), lddate_wfdisc(mwfdisc), nrowwfdisc
