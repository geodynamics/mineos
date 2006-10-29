c********************************************************************
c  Header: fdb_wfdisc.h
c  wfdisc declarations (3.0)
c  (R) University of Colorado at Boulder,CIEI.      Date: 02/24/2004
c********************************************************************
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
