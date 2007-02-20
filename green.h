c============================================================
c green program. Parameters for the length of arrays:
c mk - maximum number knots in 1-D model
c ml - maximum value of angular mode number l
c meig - maximum total number of eigen functions involved
c mseis - maximun length of seismogram
c============================================================
      integer*4 mk
      parameter (mk=350)
c ---
      integer*4 ml
      parameter (ml=6000)
c ---
      integer*4 meig
      parameter (meig=300000)
c ---
      integer*4 mseis
      parameter (mseis=30000)
c ---
c coomon block /names/ stores in/out file names
      character*256 fname1,fname2,fname3,fname4,fname5
      common/names/fname1,fname2,fname3,fname4,fname5
