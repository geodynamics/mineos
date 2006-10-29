c********************************************************************
c  Header: fdb_sitechan.h
c  sitechan declarations (3.0)
c  (R) University of Colorado at Boulder,CIEI.      Date: 02/24/2004
c********************************************************************
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
