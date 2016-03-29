\name{MMLPDEA-package}
\alias{MMLPDEA-package}
\docType{package}
\title{
 MMLPDEA 
}
\description{
 Uses FORTRAN to bootstrap input/output DEA models.

 Also provides an interface to Morris and Miller's open source linear programming Fortran code.
 
}
\details{
\tabular{ll}{
Package: \tab MMLPDEA\cr
Type: \tab Package\cr
Version: \tab 2.1\cr
Date: \tab 2016-3-21\cr
License: \tab  LGPL-2.1\cr
LazyLoad: \tab yes\cr
}

}
\author{

---------------------------------------------------------------

R-Fortran bootstrapping and random number generation interface/fortran code: Joe Atwood
Correction to Miller's code: Joe Atwood  August 2015 

---------------------------------------------------------------

Alan Miller fortran code obtained from: http://jblevins.org/mirror/amiller/


Site Statement: "This is an archived copy of the Fortran source code repository of Alan Miller previously located at http://users.bigpond.net.au/amiller/. 
It is hosted by Jason Blevins with permission. The site has been slightly reformatted, but the source code and descriptions below have not been modified.
All code written by Alan Miller is released into the public domain."     

--------------------

Fortran linear programming code listing:

"smplx.f90 Linear programming using the simplex algorithm. This is a translation of the Fortran 66 program from the NSWC (Naval Surface Warfare Center) 
library written by Alfred Morris. There is also a simple test program t_smplx.f90. Needs the module constant.f90 which defines the precision and returns certain machine constants."


--------------------

Fortran random number generation code listing:

"mt19937.f90 The 'Mersenne Twister' random number generator from Japan with a cycle of length (2^19937 - 1). mt19937a.f90 is a version for compilers which stop when there are 
integer overflows, as some do when compiler check options are enabled for debugging purposes. .... . mt19937.f90 was revised on 5 February 2002;" 

GPL license statement contained in mt19937.f90 code:

! A Fortran-program for MT19937: Real number version
 
! Code converted using TO_F90 by Alan Miller
! Date: 1999-11-26  Time: 17:09:23
! Latest revision - 5 February 2002
! A new seed initialization routine has been added based upon the new
! C version dated 26 January 2002.
! This version assumes that integer overflows do NOT cause crashes.
! This version is compatible with Lahey's ELF90 compiler,
! and should be compatible with most full Fortran 90 or 95 compilers.
! Notice the strange way in which umask is specified for ELF90.
 
!   genrand() generates one pseudorandom real number (double) which is
! uniformly distributed on [0,1]-interval, for each call.
! sgenrand(seed) set initial values to the working area of 624 words.
! Before genrand(), sgenrand(seed) must be called once.  (seed is any 32-bit
! integer except for 0).
! Integer generator is obtained by modifying two lines.
!   Coded by Takuji Nishimura, considering the suggestions by
! Topher Cooper and Marc Rieffel in July-Aug. 1997.

! This library is free software; you can redistribute it and/or modify it
! under the terms of the GNU Library General Public License as published by
! the Free Software Foundation; either version 2 of the License, or (at your
! option) any later version.   This library is distributed in the hope that
! it will be useful, but WITHOUT ANY WARRANTY; without even the implied
! warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
! See the GNU Library General Public License for more details.
! You should have received a copy of the GNU Library General Public License
! along with this library; if not, write to the Free Foundation, Inc.,
! 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA

! Copyright (C) 1997 Makoto Matsumoto and Takuji Nishimura.
! When you use this, send an email to: matumoto@math.keio.ac.jp
! with an appropriate reference to your work.

!***********************************************************************
! Fortran translation by Hiroshi Takano.  Jan. 13, 1999.

}
