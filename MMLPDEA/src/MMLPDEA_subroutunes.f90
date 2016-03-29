!____________________________________________________________________
Module m_refsor
!____________________________________________________________________
! Statement from Michael Olagnon with respect to Module m_refsor 
! "Code available from: http://www.fortran-2000.com/
!  in a version authored by Michel Olagnon. It may be used, copied,
!  modified and distributed as desired provided his initial authorship
!  is not questionned and that he does not bear any responsability for
!  the uses that are made of this code and further versions."
!_____________________________________________________________________
Integer, Parameter :: kdp = selected_real_kind(15)
public :: refsor
private :: kdp
private :: R_refsor, I_refsor, D_refsor
private :: R_inssor, I_inssor, D_inssor
private :: R_subsor, I_subsor, D_subsor
interface refsor
  module procedure d_refsor, r_refsor, i_refsor
end interface refsor
contains

Subroutine D_refsor(XDONT)
!  Sorts XDONT into ascending order - Quicksort
! __________________________________________________________
!  Quicksort chooses a "pivot" in the set, and explores the
!  array from both ends, looking for a value > pivot with the
!  increasing index, for a value <= pivot with the decreasing
!  index, and swapping them when it has found one of each.
!  The array is then subdivided in 2 ([3]) subsets:
!  { values <= pivot} {pivot} {values > pivot}
!  One then call recursively the program to sort each subset.
!  When the size of the subarray is small enough, one uses an
!  insertion sort that is faster for very small sets.
!  Michel Olagnon - Apr. 2000
! __________________________________________________________
! __________________________________________________________
      Real (kind=kdp), Dimension (:), Intent (InOut) :: XDONT
! __________________________________________________________
!
!
      Call D_subsor (XDONT, 1, Size (XDONT))
      Call D_inssor (XDONT)
      Return
End Subroutine D_refsor
Recursive Subroutine D_subsor (XDONT, IDEB1, IFIN1)
!  Sorts XDONT from IDEB1 to IFIN1
! __________________________________________________________
      Real(kind=kdp), dimension (:), Intent (InOut) :: XDONT
      Integer, Intent (In) :: IDEB1, IFIN1
! __________________________________________________________
      Integer, Parameter :: NINS = 16 ! Max for insertion sort
      Integer :: ICRS, IDEB, IDCR, IFIN, IMIL
      Real(kind=kdp) :: XPIV, XWRK
!
      IDEB = IDEB1
      IFIN = IFIN1
!
!  If we don't have enough values to make it worth while, we leave
!  them unsorted, and the final insertion sort will take care of them
!
      If ((IFIN - IDEB) > NINS) Then
         IMIL = (IDEB+IFIN) / 2
!
!  One chooses a pivot, median of 1st, last, and middle values
!
         If (XDONT(IMIL) < XDONT(IDEB)) Then
            XWRK = XDONT (IDEB)
            XDONT (IDEB) = XDONT (IMIL)
            XDONT (IMIL) = XWRK
         End If
         If (XDONT(IMIL) > XDONT(IFIN)) Then
            XWRK = XDONT (IFIN)
            XDONT (IFIN) = XDONT (IMIL)
            XDONT (IMIL) = XWRK
            If (XDONT(IMIL) < XDONT(IDEB)) Then
               XWRK = XDONT (IDEB)
               XDONT (IDEB) = XDONT (IMIL)
               XDONT (IMIL) = XWRK
            End If
         End If
         XPIV = XDONT (IMIL)
!
!  One exchanges values to put those > pivot in the end and
!  those <= pivot at the beginning
!
         ICRS = IDEB
         IDCR = IFIN
         ECH2: Do
            Do
               ICRS = ICRS + 1
               If (ICRS >= IDCR) Then
!
!  the first  >  pivot is IDCR
!  the last   <= pivot is ICRS-1
!  Note: If one arrives here on the first iteration, then
!        the pivot is the maximum of the set, the last value is equal
!        to it, and one can reduce by one the size of the set to process,
!        as if XDONT (IFIN) > XPIV
!
                  Exit ECH2
!
               End If
               If (XDONT(ICRS) > XPIV) Exit
            End Do
            Do
               If (XDONT(IDCR) <= XPIV) Exit
               IDCR = IDCR - 1
               If (ICRS >= IDCR) Then
!
!  The last value < pivot is always ICRS-1
!
                  Exit ECH2
               End If
            End Do
!
            XWRK = XDONT (IDCR)
            XDONT (IDCR) = XDONT (ICRS)
            XDONT (ICRS) = XWRK
         End Do ECH2
!
!  One now sorts each of the two sub-intervals
!
         Call D_subsor (XDONT, IDEB1, ICRS-1)
         Call D_subsor (XDONT, IDCR, IFIN1)
      End If
      Return
   End Subroutine D_subsor
   Subroutine D_inssor (XDONT)
!  Sorts XDONT into increasing order (Insertion sort)
! __________________________________________________________
      Real(kind=kdp), dimension (:), Intent (InOut) :: XDONT
! __________________________________________________________
      Integer :: ICRS, IDCR
      Real(kind=kdp) :: XWRK
!
      Do ICRS = 2, Size (XDONT)
         XWRK = XDONT (ICRS)
         If (XWRK >= XDONT(ICRS-1)) Cycle
         XDONT (ICRS) = XDONT (ICRS-1)
         Do IDCR = ICRS - 2, 1, - 1
            If (XWRK >= XDONT(IDCR)) Exit
            XDONT (IDCR+1) = XDONT (IDCR)
         End Do
         XDONT (IDCR+1) = XWRK
      End Do
!
      Return
!
End Subroutine D_inssor
!
Subroutine R_refsor (XDONT)
!  Sorts XDONT into ascending order - Quicksort
! __________________________________________________________
!  Quicksort chooses a "pivot" in the set, and explores the
!  array from both ends, looking for a value > pivot with the
!  increasing index, for a value <= pivot with the decreasing
!  index, and swapping them when it has found one of each.
!  The array is then subdivided in 2 ([3]) subsets:
!  { values <= pivot} {pivot} {values > pivot}
!  One then call recursively the program to sort each subset.
!  When the size of the subarray is small enough, one uses an
!  insertion sort that is faster for very small sets.
!  Michel Olagnon - Apr. 2000
! __________________________________________________________
! _________________________________________________________
      Real, Dimension (:), Intent (InOut) :: XDONT
! __________________________________________________________
!
!
      Call R_subsor (XDONT, 1, Size (XDONT))
      Call R_inssor (XDONT)
      Return
End Subroutine R_refsor
Recursive Subroutine R_subsor (XDONT, IDEB1, IFIN1)
!  Sorts XDONT from IDEB1 to IFIN1
! __________________________________________________________
      Real, dimension (:), Intent (InOut) :: XDONT
      Integer, Intent (In) :: IDEB1, IFIN1
! __________________________________________________________
      Integer, Parameter :: NINS = 16 ! Max for insertion sort
      Integer :: ICRS, IDEB, IDCR, IFIN, IMIL
      Real :: XPIV, XWRK
!
      IDEB = IDEB1
      IFIN = IFIN1
!
!  If we don't have enough values to make it worth while, we leave
!  them unsorted, and the final insertion sort will take care of them
!
      If ((IFIN - IDEB) > NINS) Then
         IMIL = (IDEB+IFIN) / 2
!
!  One chooses a pivot, median of 1st, last, and middle values
!
         If (XDONT(IMIL) < XDONT(IDEB)) Then
            XWRK = XDONT (IDEB)
            XDONT (IDEB) = XDONT (IMIL)
            XDONT (IMIL) = XWRK
         End If
         If (XDONT(IMIL) > XDONT(IFIN)) Then
            XWRK = XDONT (IFIN)
            XDONT (IFIN) = XDONT (IMIL)
            XDONT (IMIL) = XWRK
            If (XDONT(IMIL) < XDONT(IDEB)) Then
               XWRK = XDONT (IDEB)
               XDONT (IDEB) = XDONT (IMIL)
               XDONT (IMIL) = XWRK
            End If
         End If
         XPIV = XDONT (IMIL)
!
!  One exchanges values to put those > pivot in the end and
!  those <= pivot at the beginning
!
         ICRS = IDEB
         IDCR = IFIN
         ECH2: Do
            Do
               ICRS = ICRS + 1
               If (ICRS >= IDCR) Then
!
!  the first  >  pivot is IDCR
!  the last   <= pivot is ICRS-1
!  Note: If one arrives here on the first iteration, then
!        the pivot is the maximum of the set, the last value is equal
!        to it, and one can reduce by one the size of the set to process,
!        as if XDONT (IFIN) > XPIV
!
                  Exit ECH2
!
               End If
               If (XDONT(ICRS) > XPIV) Exit
            End Do
            Do
               If (XDONT(IDCR) <= XPIV) Exit
               IDCR = IDCR - 1
               If (ICRS >= IDCR) Then
!
!  The last value < pivot is always ICRS-1
!
                  Exit ECH2
               End If
            End Do
!
            XWRK = XDONT (IDCR)
            XDONT (IDCR) = XDONT (ICRS)
            XDONT (ICRS) = XWRK
         End Do ECH2
!
!  One now sorts each of the two sub-intervals
!
         Call R_subsor (XDONT, IDEB1, ICRS-1)
         Call R_subsor (XDONT, IDCR, IFIN1)
      End If
      Return
   End Subroutine R_subsor
   Subroutine R_inssor (XDONT)
!  Sorts XDONT into increasing order (Insertion sort)
! __________________________________________________________
      Real, dimension (:), Intent (InOut) :: XDONT
! __________________________________________________________
      Integer :: ICRS, IDCR
      Real :: XWRK
!
      Do ICRS = 2, Size (XDONT)
         XWRK = XDONT (ICRS)
         If (XWRK >= XDONT(ICRS-1)) Cycle
         XDONT (ICRS) = XDONT (ICRS-1)
         Do IDCR = ICRS - 2, 1, - 1
            If (XWRK >= XDONT(IDCR)) Exit
            XDONT (IDCR+1) = XDONT (IDCR)
         End Do
         XDONT (IDCR+1) = XWRK
      End Do
!
      Return
!
End Subroutine R_inssor
!
Subroutine I_refsor (XDONT)
!  Sorts XDONT into ascending order - Quicksort
! __________________________________________________________
!  Quicksort chooses a "pivot" in the set, and explores the
!  array from both ends, looking for a value > pivot with the
!  increasing index, for a value <= pivot with the decreasing
!  index, and swapping them when it has found one of each.
!  The array is then subdivided in 2 ([3]) subsets:
!  { values <= pivot} {pivot} {values > pivot}
!  One then call recursively the program to sort each subset.
!  When the size of the subarray is small enough, one uses an
!  insertion sort that is faster for very small sets.
!  Michel Olagnon - Apr. 2000
! __________________________________________________________
! __________________________________________________________
      Integer, Dimension (:), Intent (InOut)  :: XDONT
! __________________________________________________________
!
!
      Call I_subsor (XDONT, 1, Size (XDONT))
      Call I_inssor (XDONT)
      Return
End Subroutine I_refsor
Recursive Subroutine I_subsor (XDONT, IDEB1, IFIN1)
!  Sorts XDONT from IDEB1 to IFIN1
! __________________________________________________________
      Integer, dimension (:), Intent (InOut) :: XDONT
      Integer, Intent (In) :: IDEB1, IFIN1
! __________________________________________________________
      Integer, Parameter :: NINS = 16 ! Max for insertion sort
      Integer :: ICRS, IDEB, IDCR, IFIN, IMIL
      Integer :: XPIV, XWRK
!
      IDEB = IDEB1
      IFIN = IFIN1
!
!  If we don't have enough values to make it worth while, we leave
!  them unsorted, and the final insertion sort will take care of them
!
      If ((IFIN - IDEB) > NINS) Then
         IMIL = (IDEB+IFIN) / 2
!
!  One chooses a pivot, median of 1st, last, and middle values
!
         If (XDONT(IMIL) < XDONT(IDEB)) Then
            XWRK = XDONT (IDEB)
            XDONT (IDEB) = XDONT (IMIL)
            XDONT (IMIL) = XWRK
         End If
         If (XDONT(IMIL) > XDONT(IFIN)) Then
            XWRK = XDONT (IFIN)
            XDONT (IFIN) = XDONT (IMIL)
            XDONT (IMIL) = XWRK
            If (XDONT(IMIL) < XDONT(IDEB)) Then
               XWRK = XDONT (IDEB)
               XDONT (IDEB) = XDONT (IMIL)
               XDONT (IMIL) = XWRK
            End If
         End If
         XPIV = XDONT (IMIL)
!
!  One exchanges values to put those > pivot in the end and
!  those <= pivot at the beginning
!
         ICRS = IDEB
         IDCR = IFIN
         ECH2: Do
            Do
               ICRS = ICRS + 1
               If (ICRS >= IDCR) Then
!
!  the first  >  pivot is IDCR
!  the last   <= pivot is ICRS-1
!  Note: If one arrives here on the first iteration, then
!        the pivot is the maximum of the set, the last value is equal
!        to it, and one can reduce by one the size of the set to process,
!        as if XDONT (IFIN) > XPIV
!
                  Exit ECH2
!
               End If
               If (XDONT(ICRS) > XPIV) Exit
            End Do
            Do
               If (XDONT(IDCR) <= XPIV) Exit
               IDCR = IDCR - 1
               If (ICRS >= IDCR) Then
!
!  The last value < pivot is always ICRS-1
!
                  Exit ECH2
               End If
            End Do
!
            XWRK = XDONT (IDCR)
            XDONT (IDCR) = XDONT (ICRS)
            XDONT (ICRS) = XWRK
         End Do ECH2
!
!  One now sorts each of the two sub-intervals
!
         Call I_subsor (XDONT, IDEB1, ICRS-1)
         Call I_subsor (XDONT, IDCR, IFIN1)
      End If
      Return
   End Subroutine I_subsor
   Subroutine I_inssor (XDONT)
!  Sorts XDONT into increasing order (Insertion sort)
! __________________________________________________________
      Integer, dimension (:), Intent (InOut) :: XDONT
! __________________________________________________________
      Integer :: ICRS, IDCR
      Integer :: XWRK
!
      Do ICRS = 2, Size (XDONT)
         XWRK = XDONT (ICRS)
         If (XWRK >= XDONT(ICRS-1)) Cycle
         XDONT (ICRS) = XDONT (ICRS-1)
         Do IDCR = ICRS - 2, 1, - 1
            If (XWRK >= XDONT(IDCR)) Exit
            XDONT (IDCR+1) = XDONT (IDCR)
         End Do
         XDONT (IDCR+1) = XWRK
      End Do
!
      Return
!
End Subroutine I_inssor
!
end module m_refsor
!************************************************************************



!-----------------------------------------------------------------------
MODULE constants_NSWC
! Contains the NSWC functions SPMPAR, DPMPAR, EPSLN, DEPSLN, EXPARG & DXPARG
!-----------------------------------------------------------------------
!     WRITTEN using F90 intrinsics by
!        Alan Miller
!        CSIRO Mathematical & Information Sciences
!        CLAYTON, VICTORIA, AUSTRALIA 3169
!     Latest revision - 1 February 1997
!-----------------------------------------------------------------------

IMPLICIT NONE
INTEGER, PARAMETER     :: dp = SELECTED_REAL_KIND(15, 60)

CONTAINS

FUNCTION spmpar (i) RESULT(fn_val)
!-----------------------------------------------------------------------

!     SPMPAR PROVIDES THE SINGLE PRECISION MACHINE CONSTANTS FOR
!     THE COMPUTER BEING USED. IT IS ASSUMED THAT THE ARGUMENT
!     I IS AN INTEGER HAVING ONE OF THE VALUES 1, 2, OR 3. IF THE
!     SINGLE PRECISION ARITHMETIC BEING USED HAS M BASE B DIGITS AND
!     ITS SMALLEST AND LARGEST EXPONENTS ARE EMIN AND EMAX, THEN

!        SPMPAR(1) = B**(1 - M), THE MACHINE PRECISION,

!        SPMPAR(2) = B**(EMIN - 1), THE SMALLEST MAGNITUDE,

!        SPMPAR(3) = B**EMAX*(1 - B**(-M)), THE LARGEST MAGNITUDE.
!-----------------------------------------------------------------------

IMPLICIT NONE
INTEGER, INTENT(IN) :: i
REAL                :: fn_val

! Local variable
REAL                :: one = 1.0

SELECT CASE (i)
  CASE (1)
    fn_val = EPSILON(one)
  CASE (2)
    fn_val = TINY(one)
  CASE (3)
    fn_val = HUGE(one)
END SELECT

RETURN
END FUNCTION spmpar



FUNCTION dpmpar (i) RESULT(fn_val)
!-----------------------------------------------------------------------

!     DPMPAR PROVIDES THE DOUBLE PRECISION MACHINE CONSTANTS FOR
!     THE COMPUTER BEING USED. IT IS ASSUMED THAT THE ARGUMENT
!     I IS AN INTEGER HAVING ONE OF THE VALUES 1, 2, OR 3. IF THE
!     DOUBLE PRECISION ARITHMETIC BEING USED HAS M BASE B DIGITS AND
!     ITS SMALLEST AND LARGEST EXPONENTS ARE EMIN AND EMAX, THEN

!        DPMPAR(1) = B**(1 - M), THE MACHINE PRECISION,

!        DPMPAR(2) = B**(EMIN - 1), THE SMALLEST MAGNITUDE,

!        DPMPAR(3) = B**EMAX*(1 - B**(-M)), THE LARGEST MAGNITUDE.
!-----------------------------------------------------------------------

IMPLICIT NONE
INTEGER, INTENT(IN) :: i
REAL (dp)           :: fn_val

! Local variable
REAL (dp)    :: one = 1._dp

SELECT CASE (i)
  CASE (1)
    fn_val = EPSILON(one)
  CASE (2)
    fn_val = TINY(one)
  CASE (3)
    fn_val = HUGE(one)
END SELECT

RETURN
END FUNCTION dpmpar


FUNCTION epsln () RESULT(fn_val)
!--------------------------------------------------------------------
!     THE EVALUATION OF LN(EPS) WHERE EPS IS THE SMALLEST NUMBER
!     SUCH THAT 1.0 + EPS .GT. 1.0 .  L IS A DUMMY ARGUMENT.
!--------------------------------------------------------------------
IMPLICIT NONE
REAL                :: fn_val

! Local variable
REAL                :: one = 1.0

fn_val = LOG( EPSILON(one) )
RETURN
END FUNCTION epsln


FUNCTION exparg (l) RESULT(fn_val)
!--------------------------------------------------------------------
!     IF L = 0 THEN  EXPARG(L) = THE LARGEST POSITIVE W FOR WHICH
!     EXP(W) CAN BE COMPUTED.
!
!     IF L IS NONZERO THEN  EXPARG(L) = THE LARGEST NEGATIVE W FOR
!     WHICH THE COMPUTED VALUE OF EXP(W) IS NONZERO.
!
!     NOTE... ONLY AN APPROXIMATE VALUE FOR EXPARG(L) IS NEEDED.
!--------------------------------------------------------------------
IMPLICIT NONE
INTEGER, INTENT(IN) :: l
REAL                :: fn_val

! Local variable
REAL                :: one = 1.0

IF (l == 0) THEN
  fn_val = LOG( HUGE(one) )
ELSE
  fn_val = LOG( TINY(one) )
END IF
RETURN
END FUNCTION exparg


FUNCTION depsln () RESULT(fn_val)
!--------------------------------------------------------------------
!     THE EVALUATION OF LN(EPS) WHERE EPS IS THE SMALLEST NUMBER
!     SUCH THAT 1.D0 + EPS .GT. 1.D0 .  L IS A DUMMY ARGUMENT.
!--------------------------------------------------------------------
IMPLICIT NONE
REAL (dp)           :: fn_val

! Local variable
REAL (dp)    :: one = 1._dp

fn_val = LOG( EPSILON(one) )
RETURN
END FUNCTION depsln


FUNCTION dxparg (l) RESULT(fn_val)
!--------------------------------------------------------------------
!     IF L = 0 THEN  DXPARG(L) = THE LARGEST POSITIVE W FOR WHICH
!     DEXP(W) CAN BE COMPUTED.
!
!     IF L IS NONZERO THEN  DXPARG(L) = THE LARGEST NEGATIVE W FOR
!     WHICH THE COMPUTED VALUE OF DEXP(W) IS NONZERO.
!
!     NOTE... ONLY AN APPROXIMATE VALUE FOR DXPARG(L) IS NEEDED.
!--------------------------------------------------------------------
IMPLICIT NONE
INTEGER, INTENT(IN) :: l
REAL (dp)           :: fn_val

! Local variable
REAL (dp)    :: one = 1._dp

IF (l == 0) THEN
  fn_val = LOG( HUGE(one) )
ELSE
  fn_val = LOG( TINY(one) )
END IF
RETURN
END FUNCTION dxparg

END MODULE constants_NSWC



SUBROUTINE smplx (a, b0, c, ka, m, n0, ind, ibasis, x, z, iter, mxiter,   &
                  numle, numge, bi, rerr, MMLPV)
!-----------------------------------------------------------------------
!     SIMPLEX PROCEDURE FOR SOLVING LINEAR PROGRAMMING PROBLEMS
!-----------------------------------------------------------------------
! Finds non-negative x's to maximize:
!     c1.x1 + ... + cn.xn
!
! Subject to the constraints:
!     a11.x1 + ... + a1n.xn {<=,=,>=} b1
!              ...
!     am1.x1 + ... + amn.xn {<=,=,>=} bm
!
! The constraints must be ordered so that the <= constraints, if any, come
! first, then any >= constraints, with the equality constraints last.
!
! Arguments:
! a(ka,n0)   INPUT   coefficients in the constraints.
! b0(m)      INPUT   right-hand side of the constraints.
! c(n0)      INPUT   vector of `costs' in the objective function.
! ka         INPUT   first dimension of array a.
! m          INPUT   dimension of array b0.
! n0         INPUT   2nd dimension of array a.
! ind        IN/OUT  If IND = 0 on input, the routine selects its own initial
!                    basis, else if IND = 1 then the indices of the initial
!                    basis should be in IBASIS.
!                    On output:
!                    IND = 0 the problem was solved
!                    IND = 1 the problem has no solution
!                    IND = 2 MXITER iterations were performed; more needed
!                    IND = 3 sufficient accuracy could not be maintained to
!                            solve the problem
!                    IND = 4 the problem has an unbounded solution
!                    IND = 5 input error detected
!                    IND = 6 the solution may have been obtained
! ibasis(m)  IN/OUT  indices of the variables in the basis.
! x()        OUTPUT  Dimension must be >= n + numle + numge.
!                    If IND = 0 or 6, it contains the values of the original,
!                    slack and surplus variables.
! z          OUTPUT  If IND = 0 or 6, contains the value of the objective.
! iter       OUTPUT  number of iterations used.
! mxiter     INPUT   maximum number of iterations.
! numle      INPUT   number of <= constraints.
! numge      INPUT   number of >= constraints.
! bi(m,m)    OUTPUT  the inverse of the basis matrix.
! rerr       OUTPUT  the estimated relative error achieved.
! N.B. The last two arguments in the NSWC routine have been eliminated.
!      These were work spaces.   They are now declared as automatic arrays.
!-----------------------------------------------------------------------
!     WRITTEN BY ALFRED H. MORRIS JR.
!        NAVAL SURFACE WEAPONS CENTER
!        DAHLGREN, VIRGINIA
!------------------------
!     INITIAL VERSION  DEC  1977
!     LAST UPDATE      OCT  1990
!-----------------------------------------------------------------------
!     Converted using F90 intrinsics by
!        Alan Miller
!        CSIRO Mathematical & Information Sciences
!        CLAYTON, VICTORIA, AUSTRALIA 3169
!     Latest revision - 5 February 1997
!-----------------------------------------------------------------------
!     Variable MMLPV=1 or MMLPV=2 added by Joe Atwood 8/20/2015
!     Setting MMLPV=1 uses Miller's original code and can be used to reproduce the error found by Atwood
!     Setting MMLPV=2 uses Miller's original code with minor corrections by Atwood
!-----------------------------------------------------------------------

USE constants_NSWC
IMPLICIT NONE

INTEGER, INTENT(IN)                    :: ka, m, n0, mxiter, numle, numge
INTEGER, INTENT(IN OUT)                :: ind
INTEGER, DIMENSION(:), INTENT(IN OUT)  :: ibasis

INTEGER, INTENT(OUT)                   :: iter
REAL (dp), DIMENSION(:,:), INTENT(IN)  :: a
REAL (dp), DIMENSION(:), INTENT(IN)    :: b0, c
REAL (dp), INTENT(OUT)                 :: z, rerr
REAL (dp), DIMENSION(:), INTENT(OUT)   :: x
REAL (dp), DIMENSION(:,:), INTENT(OUT) :: bi
!******************************************************
INTEGER, INTENT(IN)                    :: MMLPV    !atwood 8/20/2015
!******************************************************

INTERFACE
  SUBROUTINE smplx1 (a, b0, c, ka, m, n0, ind, ibasis, r, z, iter, mxiter,  &
                     eps0, rerrmn, rerrmx, rerr, numle, numge, bi)
    USE constants_NSWC
    IMPLICIT NONE
    INTEGER, INTENT(IN)                    :: ka, m, n0, mxiter, numle, numge
    INTEGER, INTENT(OUT)                   :: ind, iter
    REAL (dp), DIMENSION(:,:), INTENT(IN)  :: a
    REAL (dp), DIMENSION(:), INTENT(IN)    :: b0, c
    REAL (dp), DIMENSION(:,:), INTENT(OUT) :: bi
    REAL (dp), DIMENSION(:), INTENT(OUT)   :: r
    REAL (dp), INTENT(OUT)                 :: z, rerr
    REAL (dp), INTENT(IN)                  :: eps0, rerrmn, rerrmx
    INTEGER, DIMENSION(:), INTENT(IN OUT)  :: ibasis
  END SUBROUTINE smplx1

  SUBROUTINE smplx2 (a, b0, c, ka, m, n0, ind, ibasis, r, z, iter, mxiter,  &
                     eps0, rerrmn, rerrmx, rerr, numle, numge, bi)
    USE constants_NSWC
    IMPLICIT NONE
    INTEGER, INTENT(IN)                    :: ka, m, n0, mxiter, numle, numge
    INTEGER, INTENT(OUT)                   :: ind, iter
    REAL (dp), DIMENSION(:,:), INTENT(IN)  :: a
    REAL (dp), DIMENSION(:), INTENT(IN)    :: b0, c
    REAL (dp), DIMENSION(:,:), INTENT(OUT) :: bi
    REAL (dp), DIMENSION(:), INTENT(OUT)   :: r
    REAL (dp), INTENT(OUT)                 :: z, rerr
    REAL (dp), INTENT(IN)                  :: eps0, rerrmn, rerrmx
    INTEGER, DIMENSION(:), INTENT(IN OUT)  :: ibasis
  END SUBROUTINE smplx2

END INTERFACE

!------------------------
!     DIMENSION X(N0+NUMLE+NUMGE)
!------------------------

!     ********** EPS0 IS A MACHINE DEPENDENT PARAMETER. ASSIGN EPS0
!                THE VALUE U WHERE U IS THE SMALLEST POSITIVE FLOATING
!                POINT NUMBER SUCH THAT 1.0 + U .GT. 1.0.

!     Local variables
REAL (dp) :: eps0, rerrmn, rerrmx

eps0 = dpmpar(1)

!------------------------
rerrmn = 10.0_dp*eps0
rerrmx = 1.d-4
IF (eps0 < 1.d-13) rerrmx = 1.d-5


if(MMLPV.eq.1) then
 CALL smplx1(a, b0, c, ka, m, n0, ind, ibasis, x, z, iter, mxiter, eps0,   &
      rerrmn, rerrmx, rerr, numle, numge, bi)
end if

if(MMLPV.ne.1) then
 CALL smplx2(a, b0, c, ka, m, n0, ind, ibasis, x, z, iter, mxiter, eps0,   &
      rerrmn, rerrmx, rerr, numle, numge, bi)
end if
        
RETURN
END SUBROUTINE smplx



!***************************************************************************
SUBROUTINE smplx1 (a, b0, c, ka, m, n0, ind, ibasis, r, z, iter, mxiter,  &
                   eps0, rerrmn, rerrmx, rerr, numle, numge, bi)
!----------------------
!     NSTEP = 1   ELIMINATE THE NEGATIVE VARIABLES
!     NSTEP = 2   PHASE 1 OF THE SIMPLEX ALGORITHM
!     NSTEP = 3   PHASE 2 OF THE SIMPLEX ALGORITHM
!----------------------
!     MXITER = THE MAXIMUM NUMBER OF ITERATIONS PERMITTED
!     ITER = THE NUMBER OF THE CURRENT ITERATION
!     ICOUNT = THE NUMBER OF ITERATIONS SINCE THE LAST INVERSION
!----------------------
!     NUMLE = THE NUMBER OF .LE. CONSTRAINTS
!     NUMGE = THE NUMBER OF .GE. CONSTRAINTS
!----------------------
!     THE ROUTINE ASSUMES THAT THE .LE. CONSTRAINTS PRECEDE THE .GE.
!     CONSTRAINTS AND THAT THE .EQ. CONSTRAINTS COME LAST. THERE ARE
!     M CONSTRAINTS. X(N0+I) IS THE SLACK, SURPLUS, OR ARTIFICIAL
!     VARIABLE FOR THE I-TH CONSTRAINT (I=1, ..., M).
!----------------------
!     N0 = THE NUMBER OF ORGINAL VARIABLES
!     NS = THE NUMBER OF ORGINAL AND SLACK VARIABLES
!     N  = THE NUMBER OF ORGINAL, SLACK, AND SURPLUS VARIABLES
!     NUM = THE TOTAL NUMBER OF VARIABLES
!----------------------
!     RERRMN = THE SMALLEST RELATIVE ERROR TOLERANCE USED
!     RERRMX = THE LARGEST RELATIVE ERROR TOLERACE USED
!     RERR   = THE ESTIMATED CURRENT RELATIVE ERROR
!----------------------
!     ASSUME THAT
!         B0 = (B0(1), ..., B0(M))
!         C  = (C(1), ..., C(N0))
!         Z  = C(1)*X(1)+...+C(N0)*X(N0)
!     THE PROBLEM IS TO MAXIMIZE Z SUBJECT TO
!         AX(LE,EQ,GE)B0
!         X.GE.0
!----------------------
!     ON INPUT IND CAN HAVE THE VALUES
!         IND = 0   NO BEGINNING BASIS IS PROVIDED BY THE USER
!         IND = 1   THE ARRAY IBASIS HAS BEEN SET BY THE USER
!     ON OUTPUT IND IS ASSIGNED ONE OF THE VALUES
!         IND = 0   Z WAS SUCCESSFULLY MAXIMIZED
!         IND = 1   THE PROBLEM HAS NO FEASIBLE SOLUTION
!         IND = 2   MXITER ITERATIONS WERE PERFORMED
!         IND = 3   SUFFICIENT ACCURACY CANNOT BE MAINTAINED
!         IND = 4   THE PROBLEM HAS AN UNBOUNDED SOLUTION
!         IND = 5   THERE IS AN INPUT ERROR
!         IND = 6   Z WAS POSSIBLY MAXIMIZED
!----------------------
!     BASIS IS AN INTEGER ARRAY OF DIMENSION N0+M. FOR J.LE.N
!         BASIS(J) = 1  IF X(J) IS A BASIC VARIABLE
!         BASIS(J) = 0  IF X(J) IS NOT A BASIC VARIABLE
!     IF THE BASIC VARIABLES ARE X(I1), ..., X(IM) THEN
!         IBASIS = (I1, ..., IM)
!     ALSO XB(1), ..., XB(M) ARE THE CORRESPONDING VALUES OF THE
!     BASIC VARIABLES.
!----------------------
!     BI IS AN MXM ARRAY CONTAINING THE INVERSE OF THE BASIS MATRIX.
!----------------------
!     R IS AN ARRAY OF DIMENSION N. ON OUTPUT R CONTAINS THE CURRENT
!     VALUE OF X. DURING COMPUTATION R NORMALLY CONTAINS THE REDUCED
!     COSTS USED FOR THE SELECTION OF THE VARIABLE TO BE MADE BASIC.
!----------------------
USE constants_NSWC
IMPLICIT NONE
INTEGER, INTENT(IN)                    :: ka, m, n0, mxiter, numle, numge
INTEGER, INTENT(OUT)                   :: ind, iter
REAL (dp), DIMENSION(:,:), INTENT(IN)  :: a
REAL (dp), DIMENSION(:), INTENT(IN)    :: b0, c
REAL (dp), DIMENSION(:,:), INTENT(OUT) :: bi
REAL (dp), DIMENSION(:), INTENT(OUT)   :: r
REAL (dp), INTENT(OUT)                 :: z, rerr
REAL (dp), INTENT(IN)                  :: eps0, rerrmn, rerrmx
INTEGER, DIMENSION(:), INTENT(IN OUT)  :: ibasis
!----------------------

INTERFACE
  SUBROUTINE crout1(a, ka, n, iend, indx, temp, ierr)
    USE constants_NSWC
    IMPLICIT NONE
    INTEGER, INTENT(IN)                     :: ka, n, iend
    INTEGER, INTENT(OUT)                    :: ierr
    REAL (dp), DIMENSION(:), INTENT(IN OUT) :: a, temp
    INTEGER, DIMENSION(:), INTENT(IN OUT)   :: indx
  END SUBROUTINE crout1
END INTERFACE

!     Local variables
INTEGER   :: i, ibeg, icount, iend, ierr, ii, il, imin, iout, ip, j, jj,  &
             jmin, jp, k, ki, kj, l, ll, lrow, m0, mcheck, ms, n, npos,   &
             nrow, ns, nstep, num, bflag
REAL (dp) :: xb(m), y(m)
INTEGER   :: basis(m+n0), indx(m)
REAL (dp) :: amax, binorm, bmax, bmin, bnorm, cmin, const, eps, epsi, ratio, &
             rerr1, rmin, rtol, s, sgn, t, tol, total, w, xmax, zero = 0._dp, &
             dsum, dsump, dsumn, dt

!     ****** XMAX IS A MACHINE DEPENDENT CONSTANT. XMAX IS THE
!            LARGEST POSITIVE FLOATING POINT NUMBER.

xmax = dpmpar(3)

!----------------------
iter = 0
icount = 0
mcheck = MIN(5, 1 + m/15)
z = zero

!                CHECK FOR INPUT ERRORS

ms = numle + numge
IF (m < 2 .OR. n0 < 2 .OR. ms > m .OR. ka < m)  &
GO TO 12
DO i = 1, m
  IF (b0(i) < zero) GO TO 12
  xb(i) = zero
END DO
rtol = xmax
DO i = 1, n0
  IF (c(i) /= zero) rtol = MIN(ABS(c(i)), rtol)
END DO
rtol = rerrmx*rtol
GO TO 20

12 ind = 5
RETURN

!     FORMATION OF THE IBASIS AND BASIS ARRAYS.
!     (IF IND = 1  THEN THE IBASIS ARRAY IS DEFINED BY THE USER.)

20 ns = n0 + numle
n = ns + numge
IF (ind == 0) GO TO 30
num = n
DO i = 1, m
  IF (ibasis(i) > n) num = num + 1
END DO
GO TO 32
22 IF (ind == 0) GO TO 590
ind = 0

30 num = n0 + m
DO i = 1, m
  ibasis(i) = n0 + i
END DO
32 bflag = 0
basis(1:n) = 0
DO i = 1, m
  ki = ibasis(i)
  basis(ki) = 1
END DO
IF (ind == 1) GO TO 100

!          CALCULATION OF XB AND BI WHEN IND = 0

rerr = rerrmn
DO j = 1, m
  xb(j) = b0(j)
  bi(1:m,j) = zero
  bi(j,j) = 1.0_dp
END DO
IF (numge == 0) GO TO 630
jmin = numle + 1
DO j = jmin, ms
  xb(j) = -xb(j)
  bi(j,j) = -1.0_dp
END DO
GO TO 601

!                  REORDER THE BASIS

100 ibeg = 1
iend = m
DO i = 1, m
  IF (ibasis(i) > n0) THEN
    indx(ibeg) = ibasis(i)
    ibeg = ibeg + 1
  ELSE
    indx(iend) = ibasis(i)
    iend = iend - 1
  END IF
END DO
IF (iend == m) GO TO 22
ibasis(1:m) = indx(1:m)

!            REINVERSION OF THE BASIS MATRIX

DO j = 1, m
  kj = ibasis(j)
  IF (kj <= n0) GO TO 110
  IF (kj <= ns) GO TO 120
  IF (kj <= n) GO TO 130
  GO TO 120
  
  110 bi(1:m,j) = a(1:m,kj)
  CYCLE
  
  120 l = kj - n0
  bi(1:m,j) = zero
  bi(l,j) = 1.0_dp
  CYCLE
  
  130 l = kj - n0
  bi(1:m,j) = zero
  bi(l,j) = -1.0_dp
END DO

icount = 0
CALL crout1 (bi(1:,1), m, m, iend, indx, y, ierr)
IF (ierr /= 0) GO TO 580

!         CHECK THE ACCURACY OF BI AND RESET RERR

bnorm = zero
DO j = 1, m
  kj = ibasis(j)
  IF (kj <= n0) GO TO 140
  total = 1.0_dp
  GO TO 142
  140 total = SUM( ABS(a(1:m,kj)) )
  142 bnorm = MAX(bnorm, total)
END DO

binorm = zero
DO j = 1, m
  total = SUM( ABS(bi(1:m,j)) )
  binorm = MAX(binorm, total)
END DO
rerr = MAX(rerrmn, eps0*bnorm*binorm)
IF (rerr > 1.d-2) GO TO 580
bflag = 0

!                 RECALCULATION OF XB

DO i = 1, m
  dsump = zero
  dsumn = zero
  DO l = 1, m
    dt = bi(i,l)*b0(l)
    IF (dt > zero) THEN
      dsump = dsump + dt
    ELSE
      dsumn = dsumn + dt
    END IF
  END DO
  xb(i) = dsump + dsumn
  s = dsump
  t = dsumn
  tol = rerrmx*MAX(s, -t)
  IF (ABS(xb(i)) < tol) xb(i) = zero
END DO
GO TO 601

!     FIND THE NEXT VECTOR A(--, JP) TO BE INSERTED INTO THE BASIS

200 jp = 0
rmin = zero
IF (nstep == 3) rmin = -rtol
DO j = 1, n0
  IF (basis(j) /= 0) CYCLE
  IF (r(j) >= rmin) CYCLE
  jp = j
  rmin = r(j)
END DO
IF (n0 == n) GO TO 203
jmin = n0 + 1
rmin = rmin*1.1_dp
DO j = jmin, n
  IF (basis(j) /= 0) CYCLE
  IF (r(j) >= rmin) CYCLE
  jp = j
  rmin = r(j)
END DO
203 IF (jp /= 0) GO TO 300
IF (nstep < 2) THEN
   GO TO 800
ELSE IF (nstep == 2) THEN
  GO TO 230
ELSE
  GO TO 250
END IF

!     INSERT THE VALUES OF THE ORGINAL, SLACK, AND SURPLUS
!             VARIABLES INTO R, THEN TERMINATE.

220 r(1:n) = zero
DO i = 1, m
  ki = ibasis(i)
  IF (ki <= n) r(ki) = xb(i)
END DO
RETURN

!             COMPLETION OF THE NSTEP = 2 CASE

230 DO i = 1, m
  IF (ibasis(i) <= n) CYCLE
  IF (xb(i) > zero) GO TO 800
END DO
GO TO 680

240 IF (icount >= 5) GO TO 100
ind = 1
GO TO 220

!             COMPLETION OF THE NSTEP = 3 CASE

250 IF (rerr > 1.d-2) GO TO 251
ind = 0
GO TO 800
251 IF (icount >= 5) GO TO 100
ind = 6
GO TO 800

!     IF MXITER ITERATIONS HAVE NOT BEEN PERFORMED THEN BEGIN THE
!     NEXT ITERATION. COMPUTE THE JP-TH COLUMN OF BI*A AND STORE IT IN Y.

300 IF (iter < mxiter) GO TO 301
ind = 2
GO TO 220
301 iter = iter + 1
icount = icount + 1
IF (jp > ns) GO TO 330
IF (jp > n0) GO TO 320

nrow = 0
amax = zero
DO i = 1, m
  IF (a(i,jp) == zero) CYCLE
  nrow = nrow + 1
  indx(nrow) = i
  amax = MAX(ABS(a(i,jp)), amax)
END DO
IF (nrow /= 0) GO TO 310
ind = 4
GO TO 220

310 rerr1 = rerrmx*amax
DO i = 1, m
  dsum = zero
  DO ll = 1, nrow
    l = indx(ll)
    dsum = dsum + bi(i,l)*a(l,jp)
  END DO
  y(i) = dsum
  IF (ABS(y(i)) >= 5.d-3) CYCLE
  bmax = zero
  DO l = 1, m
    bmax = MAX(ABS(bi(i,l)), bmax)
  END DO
  tol = rerr1*bmax
  IF (ABS(y(i)) < tol) y(i) = zero
END DO
GO TO 350

320 l = jp - n0
y(1:m) = bi(1:m,l)
GO TO 350

330 l = jp - n0
y(1:m) = -bi(1:m,l)

350 DO i = 1, m
  IF (y(i) /= zero) GO TO 360
END DO
r(jp) = zero
iter = iter - 1
icount = icount - 1
GO TO 200

360 IF (nstep == 2) GO TO 430
IF (nstep > 2) GO TO 440

!     FINDING THE VARIABLE XB(IP) TO BE MADE NONBASIC FOR THE NSTEP = 1 CASE

npos = 0
ip = 0
eps = zero
epsi = xmax
DO i = 1, m
  IF (xb(i) < zero .OR. y(i) <= zero) CYCLE
  ratio = xb(i)/y(i)
  IF (ratio < epsi) THEN
    epsi = ratio
    npos = 1
    indx(1) = i
    CYCLE
  ELSE IF (ratio > epsi) THEN
    CYCLE
  END IF
  npos = npos + 1
  indx(npos) = i
END DO
IF (npos == 0) GO TO 420
IF (epsi == zero) GO TO 460

DO i = 1, m
  IF (xb(i) >= zero .OR. y(i) >= zero) CYCLE
  ratio = xb(i)/y(i)
  IF (ratio > epsi) CYCLE
  IF (ratio < eps) CYCLE
  eps = ratio
  ip = i
END DO
IF (ip /= 0) GO TO 500
GO TO 460

420 DO i = 1, m
  IF (xb(i) >= zero .OR. y(i) >= zero) CYCLE
  ratio = xb(i)/y(i)
  IF (ratio < eps) CYCLE
  eps = ratio
  ip = i
END DO
GO TO 500

!     FINDING THE VARIABLE XB(IP) TO BE MADE NONBASIC FOR THE NSTEP = 2 CASE

430 npos = 0
epsi = xmax
DO i = 1, m
  IF (y(i) <= zero) CYCLE
  ratio = xb(i)/y(i)
  IF (ratio < epsi) THEN
    epsi = ratio
    npos = 1
    indx(1) = i
  ELSE IF (ratio > epsi) THEN
    CYCLE
  END IF
  npos = npos + 1
  indx(npos) = i
END DO
GO TO 450

!     FINDING THE VARIABLE XB(IP) TO BE MADE NONBASIC FOR THE NSTEP = 3 CASE

440 npos = 0
epsi = xmax
DO i = 1, m
  IF (y(i) < zero) THEN
    IF (ibasis(i) <= n) CYCLE
    ip = i
    GO TO 500
  ELSE IF (y(i) > zero) THEN
    ratio = xb(i)/y(i)
    IF (ratio < epsi) THEN
      epsi = ratio
      npos = 1
      indx(1) = i
    ELSE IF (ratio > epsi) THEN
      CYCLE
    END IF
    npos = npos + 1
    indx(npos) = i
  END IF
END DO

450 IF (npos /= 0) GO TO 460
IF (icount >= 5) GO TO 100
ind = 4
GO TO 220

!              TIE BREAKING PROCEDURE

460 ip = indx(1)
IF (npos == 1) GO TO 500
ip = 0
bmin = xmax
cmin = xmax
DO ii = 1, npos
  i = indx(ii)
  l = ibasis(i)
  IF (l > n0) GO TO 461
  IF (c(l) <= zero) cmin = MIN(zero, cmin)
  IF (c(l) > cmin) CYCLE
  imin = i
  cmin = c(l)
  CYCLE
  461 IF (l <= n) GO TO 462
  ip = i
  GO TO 500
  462 lrow = l - n0
  s = b0(lrow)
  IF (lrow <= numle) THEN
    IF (s > bmin) CYCLE
    ip = i
    bmin = s
  ELSE
    s = -s
    bmin = MIN(zero, bmin)
    IF (s > bmin) CYCLE
    ip = i
    bmin = s
  END IF
END DO
IF (cmin <= zero .OR. ip == 0) ip = imin

!               TRANSFORMATION OF XB

500 IF (xb(ip) == zero) GO TO 510
const = xb(ip)/y(ip)
DO i = 1, m
  s = xb(i)
  xb(i) = xb(i) - const*y(i)
  IF (xb(i) >= zero) CYCLE
  IF (s >= zero .OR. xb(i) >= rerrmx*s) xb(i) = zero
END DO
xb(ip) = const

!               TRANSFORMATION OF BI

510 DO j = 1, m
  IF (bi(ip,j) == zero) CYCLE
  const = bi(ip,j)/y(ip)
  bi(1:m,j) = bi(1:m,j) - const*y(1:m)
  bi(ip,j) = const
END DO

!             UPDATING IBASIS AND BASIS

iout = ibasis(ip)
ibasis(ip) = jp
basis(iout) = 0
basis(jp) = 1
IF (iout > n) num = num - 1

!        CHECK THE ACCURACY OF BI AND RESET RERR

IF (rerr > 1.d-2) GO TO 530
k = 0
DO j = 1, m
  kj = ibasis(j)
  IF (kj > n0) CYCLE
  total = DOT_PRODUCT( bi(j,1:m), a(1:m, kj) )
  rerr = MAX(rerr, ABS(1.0_dp - total))
  k = k + 1
  IF (k >= mcheck) GO TO 522
END DO
522 IF (rerr <= 1.d-2) GO TO 600

!        THE ACCURACY CRITERIA ARE NOT SATISFIED

530 IF (icount < 5) GO TO 600
bflag = 1
GO TO 100

580 IF (iter == 0) GO TO 12
IF (bflag == 0) GO TO 590
bflag = 0
DO ip = 1, m
  IF (jp == ibasis(ip)) GO TO 582
END DO
582 ibasis(ip) = iout
basis(jp) = 0
basis(iout) = 1
IF (iout > n) num = num + 1
GO TO 100

590 ind = 3
GO TO 220

!       SET UP THE R ARRAY FOR THE NSTEP = 1 CASE

600 IF (nstep == 2) GO TO 630
IF (nstep > 2) GO TO 700
601 DO j = 1, m
  IF (xb(j) < zero) GO TO 610
END DO
GO TO 630

610 nstep = 1
m0 = 0
DO l = 1, m
  IF (xb(l) >= zero) CYCLE
  m0 = m0 + 1
  indx(m0) = l
END DO

DO j = 1, m
  dsump = zero
  dsumn = zero
  DO ll = 1, m0
    l = indx(ll)
    IF (bi(l,j) < zero) THEN
      dsumn = dsumn + bi(l,j)
    ELSE IF (bi(l,j) > zero) THEN
      dsump = dsump + bi(l,j)
    END IF
  END DO
  y(j) = dsump + dsumn
  s = dsump
  t = dsumn
  tol = rerrmx*MAX(s, -t)
  IF (ABS(y(j)) < tol) y(j) = zero
END DO
GO TO 650

!       SET UP THE R ARRAY FOR THE NSTEP = 2 CASE

630 IF (n == num) GO TO 680
nstep = 2
m0 = 0
DO l = 1, m
  IF (ibasis(l) <= n) CYCLE
  m0 = m0 + 1
  indx(m0) = l
END DO

DO j = 1, m
  dsump = zero
  dsumn = zero
  DO ll = 1, m0
    l = indx(ll)
    IF (bi(l,j) < zero) THEN
      dsumn = dsumn + bi(l,j)
    ELSE IF (bi(l,j) > zero) THEN
      dsump = dsump + bi(l,j)
    END IF
  END DO
  y(j) = -(dsump + dsumn)
  s = dsump
  t = dsumn
  tol = rerrmx*MAX(s, -t)
  IF (ABS(y(j)) < tol) y(j) = zero
END DO

650 DO j = 1, n0
  IF (basis(j) == 0) THEN
    r(j) = DOT_PRODUCT( y(1:m), a(1:m,j) )
  ELSE
    r(j) = zero
  END IF
END DO

660 IF (n0 == ns) GO TO 670
jmin = n0 + 1
DO j = jmin, ns
  r(j) = zero
  IF (basis(j) /= 0) CYCLE
  jj = j - n0
  r(j) = y(jj)
END DO

670 IF (ns == n) GO TO 200
jmin = ns + 1
DO j = jmin, n
  r(j) = zero
  IF (basis(j) /= 0) CYCLE
  jj = j - n0
  r(j) = -y(jj)
END DO
GO TO 200

!      SET UP A NEW R ARRAY FOR THE NSTEP = 3 CASE

680 nstep = 3
DO j = 1, m
  dsum = zero
  DO l = 1, m
    il = ibasis(l)
    IF (il <= n0) dsum = dsum + c(il)*bi(l,j)
  END DO
  y(j) = dsum
END DO

DO j = 1, n0
  r(j) = zero
  IF (basis(j) /= 0) CYCLE
  dsum = -c(j) + DOT_PRODUCT( y(1:m), a(1:m,j) )
  r(j) = dsum
  IF (r(j) >= zero) CYCLE
  tol = rerrmx*ABS(c(j))
  IF (ABS(r(j)) < tol) r(j) = zero
END DO
GO TO 660

!       UPDATE THE R ARRAY FOR THE NSTEP = 3 CASE

700 const = r(jp)
DO j = 1, n0
  IF (basis(j) /= 0) THEN
    r(j) = zero
  ELSE
    total = DOT_PRODUCT( bi(ip,1:m), a(1:m,j) )
    r(j) = r(j) - const*total
    IF (r(j) >= zero) CYCLE
    tol = rerrmx*ABS(c(j))
    IF (ABS(r(j)) < tol) r(j) = zero
  END IF
END DO

IF (n0 == ns) GO TO 720
jmin = n0 + 1
DO j = jmin, ns
  IF (basis(j) /= 0) THEN
    r(j) = zero
  ELSE
    jj = j - n0
    r(j) = r(j) - const*bi(ip,jj)
  END IF
END DO

720 IF (ns == n) GO TO 200
jmin = ns + 1
DO j = jmin, n
  IF (basis(j) /= 0) THEN
    r(j) = zero
  ELSE
    jj = j - n0
    r(j) = r(j) + const*bi(ip,jj)
  END IF
END DO
GO TO 200
!-----------------------------------------------------------------------
!               REFINE XB AND STORE THE RESULT IN Y
!-----------------------------------------------------------------------
800 y(1:m) = zero

m0 = 0
DO j = 1, m
  kj = ibasis(j)
  IF (kj <= n0) GO TO 810
  IF (kj <= ns) GO TO 820
  IF (kj <= n) GO TO 830
  GO TO 820
  
  810 m0 = m0 + 1
  indx(m0) = j
  CYCLE
  
  820 l = kj - n0
  y(l) = xb(j)
  CYCLE
  
  830 l = kj - n0
  y(l) = -xb(j)
END DO

IF (m0 == 0) THEN
  r(1:m) = b0(1:m) - y(1:m)
ELSE
  DO i = 1, m
    dsum = y(i)
    DO jj = 1, m0
      j = indx(jj)
      kj = ibasis(j)
      dsum = dsum + a(i,kj)*xb(j)
    END DO
    r(i) = b0(i) - dsum
  END DO
END IF

rerr1 = MIN(rerrmx, rerr)
DO i = 1, m
  y(i) = zero
  IF (xb(i) < zero) THEN
    sgn = -1.0_dp
    dsump = zero
    dsumn = xb(i)
  ELSE IF (xb(i) > zero) THEN
    sgn = 1.0_dp
    dsump = xb(i)
    dsumn = zero
  ELSE
    CYCLE
  END IF
  DO l = 1, m
    dt = bi(i,l)*r(l)
    IF (dt > zero) THEN
      dsump = dsump + dt
    ELSE
      dsumn = dsumn + dt
    END IF
  END DO
  w = dsump + dsumn
  IF (w == zero) CYCLE
  IF (sgn /= SIGN(1.0_dp, w)) CYCLE
  s = dsump
  t = dsumn
  tol = rerr1*MAX(s, -t)
  IF (ABS(w) > tol) y(i) = w
END DO
IF (nstep == 2) GO TO 870
IF (nstep > 2) GO TO 880

!         CHECK THE REFINEMENT (NSTEP = 1)

DO i = 1, m
  IF (y(i) >= zero) GO TO 861
  IF (y(i) < -rerrmx) GO TO 240
  y(i) = zero
  861 xb(i) = y(i)
END DO
GO TO 630

!         CHECK THE REFINEMENT (NSTEP = 2)

870 DO i = 1, m
  IF (ibasis(i) <= n) GO TO 871
  IF (y(i) > rerrmx) GO TO 240
  y(i) = zero
  871 xb(i) = y(i)
END DO
GO TO 680

!              COMPUTE Z  (NSTEP = 3)

880 dsum = zero
DO i = 1, m
  ki = ibasis(i)
  IF (ki > n0) GO TO 881
  dsum = dsum + c(ki)*y(i)
  881 xb(i) = y(i)
END DO
z = dsum
GO TO 220
END SUBROUTINE smplx1
!**************************************************************************




!**************************************************************************
SUBROUTINE smplx2 (a, b0, c, ka, m, n0, ind, ibasis, r, z, iter, mxiter,  &
                   eps0, rerrmn, rerrmx, rerr, numle, numge, bi)
!----------------------
!     NSTEP = 1   ELIMINATE THE NEGATIVE VARIABLES
!     NSTEP = 2   PHASE 1 OF THE SIMPLEX ALGORITHM
!     NSTEP = 3   PHASE 2 OF THE SIMPLEX ALGORITHM
!----------------------
!     MXITER = THE MAXIMUM NUMBER OF ITERATIONS PERMITTED
!     ITER = THE NUMBER OF THE CURRENT ITERATION
!     ICOUNT = THE NUMBER OF ITERATIONS SINCE THE LAST INVERSION
!----------------------
!     NUMLE = THE NUMBER OF .LE. CONSTRAINTS
!     NUMGE = THE NUMBER OF .GE. CONSTRAINTS
!----------------------
!     THE ROUTINE ASSUMES THAT THE .LE. CONSTRAINTS PRECEDE THE .GE.
!     CONSTRAINTS AND THAT THE .EQ. CONSTRAINTS COME LAST. THERE ARE
!     M CONSTRAINTS. X(N0+I) IS THE SLACK, SURPLUS, OR ARTIFICIAL
!     VARIABLE FOR THE I-TH CONSTRAINT (I=1, ..., M).
!----------------------
!     N0 = THE NUMBER OF ORGINAL VARIABLES
!     NS = THE NUMBER OF ORGINAL AND SLACK VARIABLES
!     N  = THE NUMBER OF ORGINAL, SLACK, AND SURPLUS VARIABLES
!     NUM = THE TOTAL NUMBER OF VARIABLES
!----------------------
!     RERRMN = THE SMALLEST RELATIVE ERROR TOLERANCE USED
!     RERRMX = THE LARGEST RELATIVE ERROR TOLERACE USED
!     RERR   = THE ESTIMATED CURRENT RELATIVE ERROR
!----------------------
!     ASSUME THAT
!         B0 = (B0(1), ..., B0(M))
!         C  = (C(1), ..., C(N0))
!         Z  = C(1)*X(1)+...+C(N0)*X(N0)
!     THE PROBLEM IS TO MAXIMIZE Z SUBJECT TO
!         AX(LE,EQ,GE)B0
!         X.GE.0
!----------------------
!     ON INPUT IND CAN HAVE THE VALUES
!         IND = 0   NO BEGINNING BASIS IS PROVIDED BY THE USER
!         IND = 1   THE ARRAY IBASIS HAS BEEN SET BY THE USER
!     ON OUTPUT IND IS ASSIGNED ONE OF THE VALUES
!         IND = 0   Z WAS SUCCESSFULLY MAXIMIZED
!         IND = 1   THE PROBLEM HAS NO FEASIBLE SOLUTION
!         IND = 2   MXITER ITERATIONS WERE PERFORMED
!         IND = 3   SUFFICIENT ACCURACY CANNOT BE MAINTAINED
!         IND = 4   THE PROBLEM HAS AN UNBOUNDED SOLUTION
!         IND = 5   THERE IS AN INPUT ERROR
!         IND = 6   Z WAS POSSIBLY MAXIMIZED
!----------------------
!     BASIS IS AN INTEGER ARRAY OF DIMENSION N0+M. FOR J.LE.N
!         BASIS(J) = 1  IF X(J) IS A BASIC VARIABLE
!         BASIS(J) = 0  IF X(J) IS NOT A BASIC VARIABLE
!     IF THE BASIC VARIABLES ARE X(I1), ..., X(IM) THEN
!         IBASIS = (I1, ..., IM)
!     ALSO XB(1), ..., XB(M) ARE THE CORRESPONDING VALUES OF THE
!     BASIC VARIABLES.
!----------------------
!     BI IS AN MXM ARRAY CONTAINING THE INVERSE OF THE BASIS MATRIX.
!----------------------
!     R IS AN ARRAY OF DIMENSION N. ON OUTPUT R CONTAINS THE CURRENT
!     VALUE OF X. DURING COMPUTATION R NORMALLY CONTAINS THE REDUCED
!     COSTS USED FOR THE SELECTION OF THE VARIABLE TO BE MADE BASIC.
!----------------------
USE constants_NSWC
IMPLICIT NONE
INTEGER, INTENT(IN)                    :: ka, m, n0, mxiter, numle, numge
INTEGER, INTENT(OUT)                   :: ind, iter
REAL (dp), DIMENSION(:,:), INTENT(IN)  :: a
REAL (dp), DIMENSION(:), INTENT(IN)    :: b0, c
REAL (dp), DIMENSION(:,:), INTENT(OUT) :: bi
REAL (dp), DIMENSION(:), INTENT(OUT)   :: r
REAL (dp), INTENT(OUT)                 :: z, rerr
REAL (dp), INTENT(IN)                  :: eps0, rerrmn, rerrmx
INTEGER, DIMENSION(:), INTENT(IN OUT)  :: ibasis
!----------------------

INTERFACE
  SUBROUTINE crout1(a, ka, n, iend, indx, temp, ierr)
    USE constants_NSWC
    IMPLICIT NONE
    INTEGER, INTENT(IN)                     :: ka, n, iend
    INTEGER, INTENT(OUT)                    :: ierr
    REAL (dp), DIMENSION(:), INTENT(IN OUT) :: a, temp
    INTEGER, DIMENSION(:), INTENT(IN OUT)   :: indx
  END SUBROUTINE crout1
END INTERFACE

!     Local variables
INTEGER   :: i, ibeg, icount, iend, ierr, ii, il, imin, iout, ip, j, jj,  &
             jmin, jp, k, ki, kj, l, ll, lrow, m0, mcheck, ms, n, npos,   &
             nrow, ns, nstep, num, bflag
REAL (dp) :: xb(m), y(m)
INTEGER   :: basis(m+n0), indx(m)
REAL (dp) :: amax, binorm, bmax, bmin, bnorm, cmin, const, eps, epsi, ratio, &
             rerr1, rmin, rtol, s, sgn, t, tol, total, w, xmax, zero = 0._dp, &
             dsum, dsump, dsumn, dt

!     ****** XMAX IS A MACHINE DEPENDENT CONSTANT. XMAX IS THE
!            LARGEST POSITIVE FLOATING POINT NUMBER.

xmax = dpmpar(3)

!----------------------
iter = 0
icount = 0
mcheck = MIN(5, 1 + m/15)
z = zero

!                CHECK FOR INPUT ERRORS

ms = numle + numge
IF (m < 2 .OR. n0 < 2 .OR. ms > m .OR. ka < m)  &
GO TO 12
DO i = 1, m
  IF (b0(i) < zero) GO TO 12
  xb(i) = zero
END DO
rtol = xmax
DO i = 1, n0
  IF (c(i) /= zero) rtol = MIN(ABS(c(i)), rtol)
END DO
rtol = rerrmx*rtol
GO TO 20

12 ind = 5
RETURN

!     FORMATION OF THE IBASIS AND BASIS ARRAYS.
!     (IF IND = 1  THEN THE IBASIS ARRAY IS DEFINED BY THE USER.)

20 ns = n0 + numle
n = ns + numge
IF (ind == 0) GO TO 30
num = n
DO i = 1, m
  IF (ibasis(i) > n) num = num + 1
END DO
GO TO 32
22 IF (ind == 0) GO TO 590
ind = 0

30 num = n0 + m
DO i = 1, m
  ibasis(i) = n0 + i
END DO
32 bflag = 0
basis(1:n) = 0
DO i = 1, m
  ki = ibasis(i)
  basis(ki) = 1
END DO
IF (ind == 1) GO TO 100

!          CALCULATION OF XB AND BI WHEN IND = 0

rerr = rerrmn
DO j = 1, m
  xb(j) = b0(j)
  bi(1:m,j) = zero
  bi(j,j) = 1.0_dp
END DO
IF (numge == 0) GO TO 630
jmin = numle + 1
DO j = jmin, ms
  xb(j) = -xb(j)
  bi(j,j) = -1.0_dp
END DO
GO TO 601

!                  REORDER THE BASIS

100 ibeg = 1
iend = m
DO i = 1, m
  IF (ibasis(i) > n0) THEN
    indx(ibeg) = ibasis(i)
    ibeg = ibeg + 1
  ELSE
    indx(iend) = ibasis(i)
    iend = iend - 1
  END IF
END DO
IF (iend == m) GO TO 22
ibasis(1:m) = indx(1:m)

!            REINVERSION OF THE BASIS MATRIX

DO j = 1, m
  kj = ibasis(j)
  IF (kj <= n0) GO TO 110
  IF (kj <= ns) GO TO 120
  IF (kj <= n) GO TO 130
  GO TO 120
  
  110 bi(1:m,j) = a(1:m,kj)
  CYCLE
  
  120 l = kj - n0
  bi(1:m,j) = zero
  bi(l,j) = 1.0_dp
  CYCLE
  
  130 l = kj - n0
  bi(1:m,j) = zero
  bi(l,j) = -1.0_dp
END DO

icount = 0
CALL crout1 (bi(1:,1), m, m, iend, indx, y, ierr)
IF (ierr /= 0) GO TO 580

!         CHECK THE ACCURACY OF BI AND RESET RERR

bnorm = zero
DO j = 1, m
  kj = ibasis(j)
  IF (kj <= n0) GO TO 140
  total = 1.0_dp
  GO TO 142
  140 total = SUM( ABS(a(1:m,kj)) )
  142 bnorm = MAX(bnorm, total)
END DO

binorm = zero
DO j = 1, m
  total = SUM( ABS(bi(1:m,j)) )
  binorm = MAX(binorm, total)
END DO
rerr = MAX(rerrmn, eps0*bnorm*binorm)
IF (rerr > 1.d-2) GO TO 580
bflag = 0

!                 RECALCULATION OF XB

DO i = 1, m
  dsump = zero
  dsumn = zero
  DO l = 1, m
    dt = bi(i,l)*b0(l)
    IF (dt > zero) THEN
      dsump = dsump + dt
    ELSE
      dsumn = dsumn + dt
    END IF
  END DO
  xb(i) = dsump + dsumn
  s = dsump
  t = dsumn
  tol = rerrmx*MAX(s, -t)
  IF (ABS(xb(i)) < tol) xb(i) = zero
END DO
GO TO 601

!     FIND THE NEXT VECTOR A(--, JP) TO BE INSERTED INTO THE BASIS

200 jp = 0
rmin = zero
IF (nstep == 3) rmin = -rtol
DO j = 1, n0
  IF (basis(j) /= 0) CYCLE
  IF (r(j) >= rmin) CYCLE
  jp = j
  rmin = r(j)
END DO
IF (n0 == n) GO TO 203
jmin = n0 + 1
rmin = rmin*1.1_dp
DO j = jmin, n
  IF (basis(j) /= 0) CYCLE
  IF (r(j) >= rmin) CYCLE
  jp = j
  rmin = r(j)
END DO
203 IF (jp /= 0) GO TO 300
IF (nstep < 2) THEN
   GO TO 800
ELSE IF (nstep == 2) THEN
  GO TO 230
ELSE
  GO TO 250
END IF

!     INSERT THE VALUES OF THE ORGINAL, SLACK, AND SURPLUS
!             VARIABLES INTO R, THEN TERMINATE.

220 r(1:n) = zero
DO i = 1, m
  ki = ibasis(i)
  IF (ki <= n) r(ki) = xb(i)
END DO
RETURN

!             COMPLETION OF THE NSTEP = 2 CASE

230 DO i = 1, m
  IF (ibasis(i) <= n) CYCLE
  IF (xb(i) > zero) GO TO 800
END DO
GO TO 680

240 IF (icount >= 5) GO TO 100
ind = 1
GO TO 220

!             COMPLETION OF THE NSTEP = 3 CASE

250 IF (rerr > 1.d-2) GO TO 251
ind = 0
GO TO 800
251 IF (icount >= 5) GO TO 100
ind = 6
GO TO 800

!     IF MXITER ITERATIONS HAVE NOT BEEN PERFORMED THEN BEGIN THE
!     NEXT ITERATION. COMPUTE THE JP-TH COLUMN OF BI*A AND STORE IT IN Y.

300 IF (iter < mxiter) GO TO 301
ind = 2
GO TO 220
301 iter = iter + 1
icount = icount + 1
IF (jp > ns) GO TO 330
IF (jp > n0) GO TO 320

nrow = 0
amax = zero
DO i = 1, m
  IF (a(i,jp) == zero) CYCLE
  nrow = nrow + 1
  indx(nrow) = i
  amax = MAX(ABS(a(i,jp)), amax)
END DO
IF (nrow /= 0) GO TO 310
ind = 4
GO TO 220

310 rerr1 = rerrmx*amax
DO i = 1, m
  dsum = zero
  DO ll = 1, nrow
    l = indx(ll)
    dsum = dsum + bi(i,l)*a(l,jp)
  END DO
  y(i) = dsum
  IF (ABS(y(i)) >= 5.d-3) CYCLE
  bmax = zero
  DO l = 1, m
    bmax = MAX(ABS(bi(i,l)), bmax)
  END DO
  tol = rerr1*bmax
  IF (ABS(y(i)) < tol) y(i) = zero
END DO
GO TO 350

320 l = jp - n0
y(1:m) = bi(1:m,l)
GO TO 350

330 l = jp - n0
y(1:m) = -bi(1:m,l)

350 DO i = 1, m
  IF (y(i) /= zero) GO TO 360
END DO
r(jp) = zero
iter = iter - 1
icount = icount - 1
GO TO 200

360 IF (nstep == 2) GO TO 430
IF (nstep > 2) GO TO 440

!     FINDING THE VARIABLE XB(IP) TO BE MADE NONBASIC FOR THE NSTEP = 1 CASE

npos = 0
ip = 0
eps = zero
epsi = xmax
DO i = 1, m
!  IF (xb(i) < zero .OR. y(i) <= zero) CYCLE
  IF (xb(i) < zero .OR. y(i) <= eps0) CYCLE   ! joe atwood 8/20/2015
  ratio = xb(i)/y(i)
  IF (ratio < epsi) THEN
    epsi = ratio
    npos = 1
    indx(1) = i
    CYCLE
  ELSE IF (ratio > epsi) THEN
    CYCLE
  END IF
  npos = npos + 1
  indx(npos) = i
END DO
IF (npos == 0) GO TO 420
IF (epsi == zero) GO TO 460

DO i = 1, m
  IF (xb(i) >= zero .OR. y(i) >= zero) CYCLE
  ratio = xb(i)/y(i)
  IF (ratio > epsi) CYCLE
  IF (ratio < eps) CYCLE
  eps = ratio
  ip = i
END DO
IF (ip /= 0) GO TO 500
GO TO 460

420 DO i = 1, m
  IF (xb(i) >= zero .OR. y(i) >= zero) CYCLE
  ratio = xb(i)/y(i)
  IF (ratio < eps) CYCLE
  eps = ratio
  ip = i
END DO
GO TO 500

!     FINDING THE VARIABLE XB(IP) TO BE MADE NONBASIC FOR THE NSTEP = 2 CASE

430 npos = 0
epsi = xmax
DO i = 1, m
!  IF (y(i) <= zero) CYCLE
  IF (y(i) <= eps0) CYCLE       ! joe atwood 8/20/2015
  ratio = xb(i)/y(i)
  IF (ratio < epsi) THEN
    epsi = ratio
    npos = 1
    indx(1) = i
  ELSE IF (ratio > epsi) THEN
    CYCLE
  END IF
  npos = npos + 1
  indx(npos) = i
END DO
GO TO 450

!     FINDING THE VARIABLE XB(IP) TO BE MADE NONBASIC FOR THE NSTEP = 3 CASE

440 npos = 0
epsi = xmax
DO i = 1, m
!  IF (y(i) < zero) THEN
  IF (y(i) < eps0) THEN        ! joe atwood 8/20/2014
    IF (ibasis(i) <= n) CYCLE
    ip = i
    GO TO 500
!  ELSE IF (y(i) > zero) THEN
  ELSE IF (y(i) > eps0) THEN   ! joe atwood 8/20/2014
    ratio = xb(i)/y(i)
    IF (ratio < epsi) THEN
      epsi = ratio
      npos = 1
      indx(1) = i
    ELSE IF (ratio > epsi) THEN
      CYCLE
    END IF
    npos = npos + 1
    indx(npos) = i
  END IF
END DO

450 IF (npos /= 0) GO TO 460
IF (icount >= 5) GO TO 100
ind = 4
GO TO 220

!              TIE BREAKING PROCEDURE

460 ip = indx(1)
IF (npos == 1) GO TO 500
ip = 0
bmin = xmax
cmin = xmax
DO ii = 1, npos
  i = indx(ii)
  l = ibasis(i)
  IF (l > n0) GO TO 461
  IF (c(l) <= zero) cmin = MIN(zero, cmin)
  IF (c(l) > cmin) CYCLE
  imin = i
  cmin = c(l)
  CYCLE
  461 IF (l <= n) GO TO 462
  ip = i
  GO TO 500
  462 lrow = l - n0
  s = b0(lrow)
  IF (lrow <= numle) THEN
    IF (s > bmin) CYCLE
    ip = i
    bmin = s
  ELSE
    s = -s
    bmin = MIN(zero, bmin)
    IF (s > bmin) CYCLE
    ip = i
    bmin = s
  END IF
END DO
IF (cmin <= zero .OR. ip == 0) ip = imin

!               TRANSFORMATION OF XB

500 IF (xb(ip) == zero) GO TO 510
const = xb(ip)/y(ip)
DO i = 1, m
  s = xb(i)
  xb(i) = xb(i) - const*y(i)
  IF (xb(i) >= zero) CYCLE
  IF (s >= zero .OR. xb(i) >= rerrmx*s) xb(i) = zero
END DO
xb(ip) = const

!               TRANSFORMATION OF BI

510 DO j = 1, m
  IF (bi(ip,j) == zero) CYCLE
  const = bi(ip,j)/y(ip)
  bi(1:m,j) = bi(1:m,j) - const*y(1:m)
  bi(ip,j) = const
END DO

!             UPDATING IBASIS AND BASIS

iout = ibasis(ip)
ibasis(ip) = jp
basis(iout) = 0
basis(jp) = 1
IF (iout > n) num = num - 1

!        CHECK THE ACCURACY OF BI AND RESET RERR

IF (rerr > 1.d-2) GO TO 530
k = 0
DO j = 1, m
  kj = ibasis(j)
  IF (kj > n0) CYCLE
  total = DOT_PRODUCT( bi(j,1:m), a(1:m, kj) )
  rerr = MAX(rerr, ABS(1.0_dp - total))
  k = k + 1
  IF (k >= mcheck) GO TO 522
END DO
522 IF (rerr <= 1.d-2) GO TO 600

!        THE ACCURACY CRITERIA ARE NOT SATISFIED

530 IF (icount < 5) GO TO 600
bflag = 1
GO TO 100

580 IF (iter == 0) GO TO 12
IF (bflag == 0) GO TO 590
bflag = 0
DO ip = 1, m
  IF (jp == ibasis(ip)) GO TO 582
END DO
582 ibasis(ip) = iout
basis(jp) = 0
basis(iout) = 1
IF (iout > n) num = num + 1
GO TO 100

590 ind = 3
GO TO 220

!       SET UP THE R ARRAY FOR THE NSTEP = 1 CASE

600 IF (nstep == 2) GO TO 630
IF (nstep > 2) GO TO 700
601 DO j = 1, m
  IF (xb(j) < zero) GO TO 610
END DO
GO TO 630

610 nstep = 1
m0 = 0
DO l = 1, m
  IF (xb(l) >= zero) CYCLE
  m0 = m0 + 1
  indx(m0) = l
END DO

DO j = 1, m
  dsump = zero
  dsumn = zero
  DO ll = 1, m0
    l = indx(ll)
    IF (bi(l,j) < zero) THEN
      dsumn = dsumn + bi(l,j)
    ELSE IF (bi(l,j) > zero) THEN
      dsump = dsump + bi(l,j)
    END IF
  END DO
  y(j) = dsump + dsumn
  s = dsump
  t = dsumn
  tol = rerrmx*MAX(s, -t)
  IF (ABS(y(j)) < tol) y(j) = zero
END DO
GO TO 650

!       SET UP THE R ARRAY FOR THE NSTEP = 2 CASE

630 IF (n == num) GO TO 680
nstep = 2
m0 = 0
DO l = 1, m
  IF (ibasis(l) <= n) CYCLE
  m0 = m0 + 1
  indx(m0) = l
END DO

DO j = 1, m
  dsump = zero
  dsumn = zero
  DO ll = 1, m0
    l = indx(ll)
    IF (bi(l,j) < zero) THEN
      dsumn = dsumn + bi(l,j)
    ELSE IF (bi(l,j) > zero) THEN
      dsump = dsump + bi(l,j)
    END IF
  END DO
  y(j) = -(dsump + dsumn)
  s = dsump
  t = dsumn
  tol = rerrmx*MAX(s, -t)
  IF (ABS(y(j)) < tol) y(j) = zero
END DO

650 DO j = 1, n0
  IF (basis(j) == 0) THEN
    r(j) = DOT_PRODUCT( y(1:m), a(1:m,j) )
  ELSE
    r(j) = zero
  END IF
END DO

660 IF (n0 == ns) GO TO 670
jmin = n0 + 1
DO j = jmin, ns
  r(j) = zero
  IF (basis(j) /= 0) CYCLE
  jj = j - n0
  r(j) = y(jj)
END DO

670 IF (ns == n) GO TO 200
jmin = ns + 1
DO j = jmin, n
  r(j) = zero
  IF (basis(j) /= 0) CYCLE
  jj = j - n0
  r(j) = -y(jj)
END DO
GO TO 200

!      SET UP A NEW R ARRAY FOR THE NSTEP = 3 CASE

680 nstep = 3
DO j = 1, m
  dsum = zero
  DO l = 1, m
    il = ibasis(l)
    IF (il <= n0) dsum = dsum + c(il)*bi(l,j)
  END DO
  y(j) = dsum
END DO

DO j = 1, n0
  r(j) = zero
  IF (basis(j) /= 0) CYCLE
  dsum = -c(j) + DOT_PRODUCT( y(1:m), a(1:m,j) )
  r(j) = dsum
  IF (r(j) >= zero) CYCLE
  tol = rerrmx*ABS(c(j))
  IF (ABS(r(j)) < tol) r(j) = zero
END DO
GO TO 660

!       UPDATE THE R ARRAY FOR THE NSTEP = 3 CASE

700 const = r(jp)
DO j = 1, n0
  IF (basis(j) /= 0) THEN
    r(j) = zero
  ELSE
    total = DOT_PRODUCT( bi(ip,1:m), a(1:m,j) )
    r(j) = r(j) - const*total
    IF (r(j) >= zero) CYCLE
    tol = rerrmx*ABS(c(j))
    IF (ABS(r(j)) < tol) r(j) = zero
  END IF
END DO

IF (n0 == ns) GO TO 720
jmin = n0 + 1
DO j = jmin, ns
  IF (basis(j) /= 0) THEN
    r(j) = zero
  ELSE
    jj = j - n0
    r(j) = r(j) - const*bi(ip,jj)
  END IF
END DO

720 IF (ns == n) GO TO 200
jmin = ns + 1
DO j = jmin, n
  IF (basis(j) /= 0) THEN
    r(j) = zero
  ELSE
    jj = j - n0
    r(j) = r(j) + const*bi(ip,jj)
  END IF
END DO
GO TO 200
!-----------------------------------------------------------------------
!               REFINE XB AND STORE THE RESULT IN Y
!-----------------------------------------------------------------------
800 y(1:m) = zero

m0 = 0
DO j = 1, m
  kj = ibasis(j)
  IF (kj <= n0) GO TO 810
  IF (kj <= ns) GO TO 820
  IF (kj <= n) GO TO 830
  GO TO 820
  
  810 m0 = m0 + 1
  indx(m0) = j
  CYCLE
  
  820 l = kj - n0
  y(l) = xb(j)
  CYCLE
  
  830 l = kj - n0
  y(l) = -xb(j)
END DO

IF (m0 == 0) THEN
  r(1:m) = b0(1:m) - y(1:m)
ELSE
  DO i = 1, m
    dsum = y(i)
    DO jj = 1, m0
      j = indx(jj)
      kj = ibasis(j)
      dsum = dsum + a(i,kj)*xb(j)
    END DO
    r(i) = b0(i) - dsum
  END DO
END IF

rerr1 = MIN(rerrmx, rerr)
DO i = 1, m
  y(i) = zero
  IF (xb(i) < zero) THEN
    sgn = -1.0_dp
    dsump = zero
    dsumn = xb(i)
  ELSE IF (xb(i) > zero) THEN
    sgn = 1.0_dp
    dsump = xb(i)
    dsumn = zero
  ELSE
    CYCLE
  END IF
  DO l = 1, m
    dt = bi(i,l)*r(l)
    IF (dt > zero) THEN
      dsump = dsump + dt
    ELSE
      dsumn = dsumn + dt
    END IF
  END DO
  w = dsump + dsumn
  IF (w == zero) CYCLE
  IF (sgn /= SIGN(1.0_dp, w)) CYCLE
  s = dsump
  t = dsumn
  tol = rerr1*MAX(s, -t)
  IF (ABS(w) > tol) y(i) = w
END DO
IF (nstep == 2) GO TO 870
IF (nstep > 2) GO TO 880

!         CHECK THE REFINEMENT (NSTEP = 1)

DO i = 1, m
  IF (y(i) >= zero) GO TO 861
  IF (y(i) < -rerrmx) GO TO 240
  y(i) = zero
  861 xb(i) = y(i)
END DO
GO TO 630

!         CHECK THE REFINEMENT (NSTEP = 2)

870 DO i = 1, m
  IF (ibasis(i) <= n) GO TO 871
  IF (y(i) > rerrmx) GO TO 240
  y(i) = zero
  871 xb(i) = y(i)
END DO
GO TO 680

!              COMPUTE Z  (NSTEP = 3)

880 dsum = zero
DO i = 1, m
  ki = ibasis(i)
  IF (ki > n0) GO TO 881
  dsum = dsum + c(ki)*y(i)
  881 xb(i) = y(i)
END DO
z = dsum
GO TO 220
END SUBROUTINE smplx2
!***********************************************************************




!***********************************************************************
SUBROUTINE crout1(a, ka, n, iend, indx, temp, ierr)
!     ******************************************************************
!     CROUT PROCEDURE FOR INVERTING MATRICES
!     ******************************************************************
!     A IS A MATRIX OF ORDER N WHERE N IS GREATER THAN OR EQUAL TO 1.
!     THE INVERSE OF A IS COMPUTED AND STORED IN A.

!     KA = LENGTH OF THE COLUMNS OF THE ARRAY A

!     IEND MAY BE 0, 1, ..., N-1.  IT IS ASSUMED THAT EACH OF THE FIRST
!     IEND COLUMNS OF THE MATRIX A CONTAINS ONLY ONE NONZERO ELEMENT
!     AND THAT THE NONZERO ELEMENT IS 1 OR -1.

!     indx IS AN ARRAY OF DIMENSION N-1 OR LARGER THAT IS USED BY THE
!     ROUTINE FOR KEEPING TRACK OF THE ROW INTERCHANGES THAT ARE MADE.

!     TEMP IS A TEMPORARY STORAGE ARRAY.

!     IERR REPORTS THE STATUS OF THE RESULTS. IF A IS NONSINGULAR THEN
!     THE INVERSE OF A IS COMPUTED AND IERR=0. OTHERWISE IF A IS FOUND
!     TO BE SINGULAR THEN IERR=1 AND THE ROUTINE TERMINATES.
!     --------------------
USE constants_NSWC
IMPLICIT NONE
INTEGER, INTENT(IN)                     :: ka, n, iend
INTEGER, INTENT(OUT)                    :: ierr
REAL (dp), DIMENSION(:), INTENT(IN OUT) :: a, temp
INTEGER, DIMENSION(:), INTENT(IN OUT)   :: indx

!     Local variables
INTEGER   :: i, ibeg, ij, ik, il, j, jcol, jj, k, kcol, kcount, kj,  &
             kj0, kk, kl, km1, kp1, l, lj, lj0, lk, lmin, maxdim, mcol,  &
             ncol, nk, nm1, nmj, nmk, nn
REAL (dp) :: zero = 0._dp
REAL (dp) :: dsum, c, pmin, s

maxdim = ka*n
mcol = iend*ka
IF (iend == 0) GO TO 100

!           PROCESS THE FIRST IEND COLUMNS OF A

kcol = 0
DO k = 1, iend
  kk = kcol + k
  nk = kcol + n
  DO lk = kk, nk
    IF (a(lk) < zero) GO TO 20
    IF (a(lk) > zero) GO TO 30
  END DO
  GO TO 300
  
  20 l = lk - kcol
  lj0 = mcol + l
  DO lj = lj0, maxdim, ka
    a(lj) = -a(lj)
  END DO
  
  30 l = lk - kcol
  indx(k) = l
  IF (k == l) GO TO 32
  lj = lk
  DO kj = kk, maxdim, ka
    c = a(kj)
    a(kj) = a(lj)
    a(lj) = c
    lj = lj + ka
  END DO
  32 kcol = kcol + ka
END DO

!           PROCESS THE REMAINING COLUMNS OF A

100 nm1 = n - 1
ierr = 0
pmin = zero
ibeg = iend + 1
IF (ibeg == n) GO TO 190

k = ibeg
km1 = iend
kp1 = k + 1
kcol = mcol
kk = kcol + k
DO kcount = ibeg, nm1
  
!     SEARCH FOR THE K-TH PIVOT ELEMENT (K=IBEG, ..., N-1)
  
  l = k
  s = ABS(a(kk))
  DO i = kp1, n
    ik = kcol + i
    c = ABS(a(ik))
    IF (s >= c) CYCLE
    l = i
    s = c
  END DO
  
  IF (k > ibeg .AND. s >= pmin) GO TO 120
  pmin = s
  IF (s == zero) GO TO 300
  
!              INTERCHANGING ROWS K AND L
  
  120 indx(k) = l
  IF (k == l) GO TO 130
  kj0 = mcol + k
  lj  = mcol + l
  DO kj = kj0, maxdim, ka
    c = a(kj)
    a(kj) = a(lj)
    a(lj) = c
    lj = lj + ka
  END DO
  
!       COMPUTE THE K-TH ROW OF U (K=IBEG, ..., N-1)
  
  130 c = a(kk)
  IF (k > ibeg) GO TO 140
  kj0 = kk + ka
  DO kj = kj0, maxdim, ka
    a(kj) = a(kj)/c
  END DO
  GO TO 160
  
  140 kl = mcol + k
  DO l = ibeg, km1
    temp(l) = a(kl)
    kl = kl + ka
  END DO
  
  kj0 = kk + ka
  DO kj = kj0, maxdim, ka
    jcol = kj - k
    dsum = -a(kj)
    DO l = ibeg, km1
      lj = jcol + l
      dsum = dsum + temp(l)*a(lj)
    END DO
    a(kj) = -dsum/c
  END DO
  
!      COMPUTE THE K-TH COLUMN OF L (K=IBEG+1, ..., N)
  
  160 km1 = k
  k = kp1
  kp1 = k + 1
  kcol = kcol + ka
  kk = kcol + k
  DO l = ibeg, km1
    lk = kcol + l
    temp(l) = a(lk)
  END DO
  
  DO i = k, n
    il = mcol + i
    dsum = zero
    DO l = ibeg, km1
      dsum = dsum + a(il)*temp(l)
      il = il + ka
    END DO
    a(il) = a(il) - dsum
  END DO
END DO

!           CHECK THE N-TH PIVOT ELEMENT

190 ncol = maxdim - ka
nn = ncol + n
c = ABS(a(nn))
IF (c > pmin) GO TO 200
IF (c == zero) GO TO 300

!          REPLACE L WITH THE INVERSE OF L

200 IF (ibeg == n) GO TO 213
jj = mcol + ibeg
i = ka + 1
DO j = ibeg, nm1
  a(jj) = 1.0_dp/a(jj)
  temp(j) = a(jj)
  kj = jj
  DO km1 = j, nm1
    k = km1 + 1
    kj = kj + 1
    dsum = zero
    kl = kj
    DO l = j, km1
      dsum = dsum + a(kl)*temp(l)
      kl = kl + ka
    END DO
    a(kj) = -dsum/a(kl)
    temp(k) = a(kj)
  END DO
  jj = jj + i
END DO
213 a(nn) = 1.0_dp/a(nn)
IF (n == 1) RETURN

!       SOLVE UX = Y WHERE Y IS THE INVERSE OF L

DO nmk = 1, nm1
  k = n - nmk
  lmin = MAX(ibeg, k+1)
  kl = (lmin-1)*ka + k
  DO l = lmin, n
    temp(l) = a(kl)
    a(kl) = zero
    kl = kl + ka
  END DO
  
  kj0 = mcol + k
  DO kj = kj0, maxdim, ka
    dsum = -a(kj)
    lj = (kj - k) + lmin
    DO l = lmin, n
      dsum = dsum + temp(l)*a(lj)
      lj = lj + 1
    END DO
    a(kj) = -dsum
  END DO
END DO

!                 COLUMN INTERCHANGES

jcol = ncol - ka
DO nmj = 1, nm1
  j = n - nmj
  k = indx(j)
  IF (j == k) GO TO 251
  ij = jcol
  ik = (k-1)*ka
  DO i = 1, n
    ij = ij + 1
    ik = ik + 1
    c = a(ij)
    a(ij) = a(ik)
    a(ik) = c
  END DO
  251 jcol = jcol - ka
END DO
RETURN

!                    ERROR RETURN

300 ierr = 1
RETURN
END SUBROUTINE crout1
!*********************************************************



!*********************************************************
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

!   genrand()      -> double precision function grnd()
!   sgenrand(seed) -> subroutine sgrnd(seed)
!                     integer seed

! This program uses the following standard intrinsics.
!   ishft(i,n): If n > 0, shifts bits in i by n positions to left.
!               If n < 0, shifts bits in i by n positions to right.
!   iand (i,j): Performs logical AND on corresponding bits of i and j.
!   ior  (i,j): Performs inclusive OR on corresponding bits of i and j.
!   ieor (i,j): Performs exclusive OR on corresponding bits of i and j.

!***********************************************************************

!***********************************************************************

MODULE rgen
IMPLICIT NONE
INTEGER, PARAMETER :: dp = SELECTED_REAL_KIND(12, 60)

! Period parameters
INTEGER, PARAMETER :: n = 624, n1 = n+1, m = 397, mata = -1727483681
!                                    constant vector a
INTEGER, PARAMETER :: umask = -2147483647 - 1
!                                    most significant w-r bits
INTEGER, PARAMETER :: lmask =  2147483647
!                                    least significant r bits
! Tempering parameters
INTEGER, PARAMETER :: tmaskb= -1658038656, tmaskc= -272236544

!                     the array for the state vector
INTEGER, SAVE      :: mt(0:n-1), mti = n1
!                     mti==N+1 means mt[N] is not initialized

PRIVATE
PUBLIC :: dp, sgrnd, grnd, init_genrand
!PUBLIC :: ugen,normgen

CONTAINS


SUBROUTINE sgrnd(seed)
! This is the original version of the seeding routine.
! It was replaced in the Japanese version in C on 26 January 2002
! It is recommended that routine init_genrand is used instead.

INTEGER, INTENT(IN)   :: seed

!    setting initial seeds to mt[N] using the generator Line 25 of Table 1 in
!    [KNUTH 1981, The Art of Computer Programming Vol. 2 (2nd Ed.), pp102]

mt(0)= IAND(seed, -1)
DO  mti=1,n-1
  mt(mti) = IAND(69069 * mt(mti-1), -1)
END DO

RETURN
END SUBROUTINE sgrnd
!***********************************************************************

SUBROUTINE init_genrand(seed)
! This initialization is based upon the multiplier given on p.106 of the
! 3rd edition of Knuth, The Art of Computer Programming Vol. 2.

! This version assumes that integer overflow does NOT cause a crash.

INTEGER, INTENT(IN)  :: seed

INTEGER  :: latest

mt(0) = seed
latest = seed
DO mti = 1, n-1
  latest = IEOR( latest, ISHFT( latest, -30 ) )
  latest = latest * 1812433253 + mti
  mt(mti) = latest
END DO

RETURN
END SUBROUTINE init_genrand
!***********************************************************************


!************************************************************************
FUNCTION grnd() RESULT(fn_val)
REAL (dp) :: fn_val

INTEGER, SAVE :: mag01(0:1) = (/ 0, mata /)
!                        mag01(x) = x * MATA for x=0,1
INTEGER       :: kk, y

! These statement functions have been replaced with separate functions
! tshftu(y) = ISHFT(y,-11)
! tshfts(y) = ISHFT(y,7)
! tshftt(y) = ISHFT(y,15)
! tshftl(y) = ISHFT(y,-18)

IF(mti >= n) THEN
!                       generate N words at one time
  IF(mti == n+1) THEN
!                            if sgrnd() has not been called,
    CALL sgrnd(4357)
!                              a default initial seed is used
  END IF
  
  DO  kk = 0, n-m-1
    y = IOR(IAND(mt(kk),umask), IAND(mt(kk+1),lmask))
    mt(kk) = IEOR(IEOR(mt(kk+m), ISHFT(y,-1)),mag01(IAND(y,1)))
  END DO
  DO  kk = n-m, n-2
    y = IOR(IAND(mt(kk),umask), IAND(mt(kk+1),lmask))
    mt(kk) = IEOR(IEOR(mt(kk+(m-n)), ISHFT(y,-1)),mag01(IAND(y,1)))
  END DO
  y = IOR(IAND(mt(n-1),umask), IAND(mt(0),lmask))
  mt(n-1) = IEOR(IEOR(mt(m-1), ISHFT(y,-1)),mag01(IAND(y,1)))
  mti = 0
END IF

y = mt(mti)
mti = mti + 1
y = IEOR(y, tshftu(y))
y = IEOR(y, IAND(tshfts(y),tmaskb))
y = IEOR(y, IAND(tshftt(y),tmaskc))
y = IEOR(y, tshftl(y))

IF(y < 0) THEN
  fn_val = (DBLE(y) + 2.0D0**32) / (2.0D0**32 - 1.0D0)
ELSE
  fn_val = DBLE(y) / (2.0D0**32 - 1.0D0)
END IF

RETURN
END FUNCTION grnd


FUNCTION tshftu(y) RESULT(fn_val)
INTEGER, INTENT(IN) :: y
INTEGER             :: fn_val

fn_val = ISHFT(y,-11)
RETURN
END FUNCTION tshftu


FUNCTION tshfts(y) RESULT(fn_val)
INTEGER, INTENT(IN) :: y
INTEGER             :: fn_val

fn_val = ISHFT(y,7)
RETURN
END FUNCTION tshfts


FUNCTION tshftt(y) RESULT(fn_val)
INTEGER, INTENT(IN) :: y
INTEGER             :: fn_val

fn_val = ISHFT(y,15)
RETURN
END FUNCTION tshftt


FUNCTION tshftl(y) RESULT(fn_val)
INTEGER, INTENT(IN) :: y
INTEGER             :: fn_val

fn_val = ISHFT(y,-18)
RETURN
END FUNCTION tshftl
!***************************************************************************

!************************************************************
END MODULE rgen

!*************************************************************



!***************************************************************************
subroutine ugen(n,seedval,x)
!written/modified by Joe Atwood
! 
 use rgen   
 implicit none
 integer n,seedval
 integer i 
 real (dp) x(n) 
 CALL init_genrand(seedval)
 do i=1,n
  x(i)=grnd()
 end do
end subroutine ugen
!**************************************************************

!****************************************************************************!      
subroutine normgen(nobs,seedum,E)                                           !
!written/modified by Joe Atwood
 implicit none                                                         !
 ! Variables                                                            !
 INTEGER nobs,seedum                                             !
 real*8 E(nobs)
 integer seed
 real*8 uniformRand1(nobs), uniformRand2(nobs)                          !
 real*8 pi                                                        !
 !! **** NORMAL  DRAWS **** !                                                 !
 if(seedum.eq.0) seedum=1
 seed=seedum                                                            !
 call ugen(nobs,seed,uniformRand1)                                       !
 seed=-1*seedum
 call ugen(nobs,seed,uniformRand2)                                       !
 pi = 4.0d0 * atan ( 1.0d0 )                                            !
 E=sqrt(-2.0d0*log(uniformRand1))*cos(2.0d0*pi*uniformRand2)            !
end subroutine normgen                                                   !
 !****************************************************************************!   

!***********************************************************
function newseed(seedval)
! written by Joe Atwood
 implicit none
 integer seedval,newseed
 real*8  u(1)

 call ugen(1,seedval,u)
 newseed=floor(u(1)*2147483645)
 if(newseed==0) newseed=1
end function newseed
!***********************************************************



       
!***********************************************************
subroutine sampleum(n,nobs,x,samples,seedval)
!***********************************************************
! written by Joe Atwood
 implicit none
!input variables
 integer n,nobs,seedval
 real*8  x(n)
!output variables
  real*8 samples(nobs)
!internal variables
 integer pickum(nobs)
 real*8  u(nobs)
    
 call ugen(nobs,seedval,u)
 pickum=floor(u*float(n))+1
 where(pickum>n) pickum=n
 
 samples=x(pickum)


end subroutine sampleum
!***********************************************************


!***********************************************************
subroutine sdcalc(nobs,x,xsd)
!***********************************************************
! written by Joe Atwood
 implicit none
 integer nobs
 real*8 x(nobs),xsd
 real*8 xbar,x2(nobs),xvar
 real*8 eps
 
 eps=1E-16
 xsd=0.0
 xbar=sum(x)/float(nobs)
 x2=x*x
 xvar= sum(x2)/float(nobs) - xbar*xbar
 if(xvar<eps) xvar=eps
 xsd=sqrt(xvar)
 
end subroutine sdcalc
!***********************************************************  


!***********************************************************       
subroutine sdcalc_nm1(nobs,x,xsd)
!***********************************************************
! written by Joe Atwood 
 implicit none
 integer nobs
 real*8 x(nobs),xsd
 real*8 xbar,x2(nobs),xvar
 real*8 eps
       
 eps=1E-16
 xsd=0.0
 xbar=sum(x)/float(nobs)
 x2=x*x
 xvar= sum(x2)/float(nobs) - xbar*xbar
 if(xvar<eps) xvar=eps
 xsd=sqrt(xvar)*sqrt(float(nobs)/float(nobs-1))
 
end subroutine sdcalc_nm1       
!***********************************************************

!***********************************************************
subroutine quantile(n,x,prob,qval)
!***********************************************************
! written by Joe Atwood
use m_refsor
 implicit none
 integer n
 real*8 x(n),prob,qval

 !internal variables
 integer order(n)
 integer ih
 real*8  h,fh
 real*8  xsort(n)
 
 
 h=(float(n)-1.0d0)*prob+1.0d0
 ih=floor(h)
 fh=h-float(ih)
 
 xsort=x
! call quicksort(n,xsort,order)
 call refsor(xsort)

 qval=xsort(n)
 if(prob.lt.(1.0d0)) then
  qval=xsort(ih)+fh*(xsort(ih+1)-xsort(ih))
 end if 

end subroutine quantile
!***********************************************************


!***********************************************************
subroutine IQR(n,x,iqrange)
!***********************************************************
! written by Joe Atwood
 implicit none
 integer n
 real*8 x(n),iqrange

 !internal variables
 integer order(n)
 real*8 q25,q75
 
 call quantile(n,x,0.25d0,q25) 
 call quantile(n,x,0.75d0,q75) 
 iqrange=q75-q25

end subroutine IQR
!************************************************************


!************************************************************
subroutine hcalc_output(nDMU,effvals,h)
!************************************************************
! written by Joe Atwood
! modified (with permission) from Benchmarking package's R code
use constants_NSWC
implicit none
!******************* 
!input variables
 integer nDMU
 real (dp) effvals(nDMU)
!******************** 
!output variables
 real (dp) h0,h
!******************** 
!internal variables
 integer   nzeff,nneff
 integer   i,i1,i2,i3
 integer   j,j1,j2,j3
 
 real (dp)    dist(nDMU)
 real (dp)    zeff(nDMU),zeff2(nDMU),neff(2*nDMU)
 real (dp)    sd_dist,sd_neff,adjust
 real (dp)    std,iqrange
!*************************************
 h0=0.0d0
 h=0.0d0
 
! dist=1.0d0/effvals !input model 
 dist=effvals        !output model 
 zeff=0.0d0; zeff2=0.0d0
 nzeff=0
 do i=1,nDMU
  if(dist(i)>=(1.0d0+1.0e-6)) then
   nzeff=nzeff+1
   zeff(nzeff)=effvals(i)
   zeff2(nzeff)=2.0d0-effvals(i)
  end if
 end do

 nneff=2*nzeff
 neff(1:nzeff)=zeff(1:nzeff)
 i1=nzeff+1
 neff(i1:nneff)=zeff2(1:nzeff)
 call sdcalc_nm1(nDMU,dist,sd_dist)
 call sdcalc_nm1(nneff,neff(1:nneff),sd_neff)
 adjust=(sd_dist/sd_neff)*(float(nneff)/float(nDMU))**(1.0d0/5.0d0)

 call sdcalc_nm1(nneff,neff(1:nneff),std)
 call IQR(nneff,neff(1:nneff),iqrange)
 iqrange=iqrange/1.349
 if(iqrange>(1e-6) .and. iqrange<std) std=iqrange
 h0=0.9d0*std*float(nneff)**(-1.0d0/5.0d0)
 h=adjust*h0
 !write(*,*) 'h = ',h
  
 end subroutine hcalc_output
!*****************************************************************



!*****************************************************************
subroutine hcalc_input(nDMU,effvals,h)
!*****************************************************************
! written by Joe Atwood
! modified (with permission) from Benchmarking package's R code
use constants_NSWC
implicit none
!******************* 
!input variables
 integer nDMU
 real (dp) effvals(nDMU)
!******************** 
!output variables
 real (dp) h0,h
!******************** 
!internal variables
 integer   nzeff,nneff
 integer   i,i1,i2,i3
 integer   j,j1,j2,j3
 
 real (dp)    dist(nDMU)
 real (dp)    zeff(nDMU),zeff2(nDMU),neff(2*nDMU)
 real (dp)    sd_dist,sd_neff,adjust
 real (dp)    std,iqrange
!*************************************
 h0=0.0d0
 h=0.0d0
 dist=1.0d0/effvals 
 zeff=0.0d0; zeff2=0.0d0
 nzeff=0
 do i=1,nDMU
  if(dist(i)>=(1.0d0+1.0e-6)) then
   nzeff=nzeff+1
   zeff(nzeff)=effvals(i)
   zeff2(nzeff)=2.0d0-effvals(i)
  end if
 end do

 nneff=2*nzeff
 neff(1:nzeff)=zeff(1:nzeff)
 i1=nzeff+1
 neff(i1:nneff)=zeff2(1:nzeff)
 call sdcalc_nm1(nDMU,dist,sd_dist)
 call sdcalc_nm1(nneff,neff(1:nneff),sd_neff)
 adjust=(sd_dist/sd_neff)*(float(nneff)/float(nDMU))**(1.0d0/5.0d0)

 call sdcalc_nm1(nneff,neff(1:nneff),std)
 call IQR(nneff,neff(1:nneff),iqrange)
 iqrange=iqrange/1.349
 if(iqrange>(1e-6) .and. iqrange<std) std=iqrange
 h0=0.9d0*std*float(nneff)**(-1.0d0/5.0d0)
 h=adjust*h0
 !write(*,*) 'h = ',h
 
 
 end subroutine hcalc_input
!*******************************************
 

!************************************************************
subroutine DEA_out_sample(nDMU,evals,h,seedval,estar)
!************************************************************
! written by Joe Atwood
! modified (with permission) from Benchmarking package's R code
 use constants_NSWC
 implicit none
!input variables
 integer nDMU,seedval
 real (dp)  evals(nDMU),h
!output variables 
 real (dp) estar(nDMU)
!internal variables
 integer newseed
 integer i,i1,i2,i3,i4
 real (dp) espej1(2*nDMU)
 real (dp) beta(nDMU),z(nDMU)
 real (dp) etilde(nDMU)
 real (dp) betahat,espvar


 !evals=1.0d0/evals
 espej1(1:nDMU)=evals
 i1=nDMU+1
 i2=2*nDMU
 espej1(i1:i2)=2.0d0-evals
! seedval=seedval+1
 seedval=newseed(seedval)
 call sampleum(i2,nDMU,espej1,beta,seedval)
! seedval=seedval+1
 seedval=newseed(seedval)
 call normgen(nDMU,seedval,z)
 etilde=beta+h*z
 
! open(10,file='espejl_Fortran.csv',status='unknown')
! do i3=1,i2
!  write(10,*) espej1(i3)
! end do
! close(10)
 
 betahat=sum(beta)/float(nDMU)
 call sdcalc_nm1(i2,espej1,espvar)
 espvar=espvar*espvar
 
 estar=betahat+(etilde-betahat)/sqrt(1+(h*h)/espvar)
 where(estar<1) estar=2.0d0-estar
 !estar=1/estar
 !evals=1.0d0/evals

end subroutine DEA_out_sample
!***********************************************************
 
!************************************************************
subroutine DEA_in_sample(nDMU,evals,h,seedval,estar)
!************************************************************
! written by Joe Atwood
! modified (with permission) from Benchmarking package's R code
 use constants_NSWC
 implicit none
!input variables
 integer nDMU,seedval
 real (dp)  evals(nDMU),h
!output variables 
 real (dp) estar(nDMU)
!internal variables
 integer newseed
 integer i,i1,i2,i3,i4
 real (dp) espej1(2*nDMU)
 real (dp) beta(nDMU),z(nDMU)
 real (dp) etilde(nDMU)
 real (dp) betahat,espvar


 evals=1.0d0/evals
 espej1(1:nDMU)=evals
 i1=nDMU+1
 i2=2*nDMU
 espej1(i1:i2)=2.0d0-evals
! seedval=seedval+1
 seedval=newseed(seedval)
 call sampleum(i2,nDMU,espej1,beta,seedval)
! seedval=seedval+1
 seedval=newseed(seedval)
 call normgen(nDMU,seedval,z)
 etilde=beta+h*z
 
! open(10,file='espejl_Fortran.csv',status='unknown')
! do i3=1,i2
!  write(10,*) espej1(i3)
! end do
! close(10)
 
 betahat=sum(beta)/float(nDMU)
 call sdcalc_nm1(i2,espej1,espvar)
 espvar=espvar*espvar
 
 estar=betahat+(etilde-betahat)/sqrt(1+(h*h)/espvar)
 where(estar<1) estar=2.0d0-estar
 estar=1/estar
 evals=1.0d0/evals

end subroutine DEA_in_sample
!***********************************************************
 


!************************************************************************
subroutine MMLP(nsims,nr,nc,objmax,obj,A,restcodes,rhs,itermax,objval,xvals,duals,indstat,MMLPV)
!************************************************************************
! MMLP wrapper written/modified by Joe Atwood
!************************************************************************
use constants_NSWC
IMPLICIT NONE
!***********************************************************************
!input variables
integer                :: nsims,nr,nc,objmax,restcodes(nr),itermax
integer                :: MMLPV
real (dp)              :: obj(nc),A(nr,nc),rhs(nr)
!output variables
integer                :: indstat
real (dp)              :: objval,xvals(nc+nr),duals(nr)
!***********************************************************************
!smplx subroutine variables
CHARACTER (LEN=3)      :: objtype
CHARACTER (LEN=1)      :: rest(nr)

INTEGER                :: i,j,k 
INTEGER                :: ka,m,n 
INTEGER                :: constr_type(nr), ind, ibasis(nr)
INTEGER                :: iter, mxiter, numle, numge
INTEGER                :: nx

REAL (dp)              :: b(nr), c(nc), x(nc+nr), z, temp(nc), rerr
REAL (dp)              :: bi(nr,nr)
!***********************************************************************
!other internal variables
CHARACTER (LEN=1)      :: restold(nr)
integer                :: isim
integer                :: index1(nr),index2(nr),itmp
real (dp)              :: objold(nc),Aold(nr,nc),bold(nr)
!duals info
real (dp)              :: cold(nc+nr)
!***********************************************************************
INTERFACE
  SUBROUTINE smplx (a,b0,c,ka,m,n0,ind,ibasis, x, z, iter, mxiter,   &
                    numle, numge, bi, rerr,MMLPV)
    USE constants_NSWC
    IMPLICIT NONE
    INTEGER, INTENT(IN)                    :: ka, m, n0, mxiter, numle, numge
    INTEGER, INTENT(IN OUT)                :: ind
    INTEGER, DIMENSION(:), INTENT(IN OUT)  :: ibasis
    INTEGER, INTENT(OUT)                   :: iter
    REAL (dp), DIMENSION(:,:), INTENT(IN)  :: a
    REAL (dp), DIMENSION(:), INTENT(IN)    :: b0, c
    REAL (dp), INTENT(OUT)                 :: z, rerr
    REAL (dp), DIMENSION(:), INTENT(OUT)   :: x
    REAL (dp), DIMENSION(:,:), INTENT(OUT) :: bi
    !******************************************************
    INTEGER, INTENT(IN)                    :: MMLPV    !atwood 8/20/2015
    !******************************************************

  END SUBROUTINE smplx
END INTERFACE
!************************************************************************
do 100 isim=1,nsims
ka=nr
m=nr
n=nc
mxiter=itermax
ind=0
ibasis=0
iter=0
!*********************************************
!set up smplx
objtype='min'
if(objmax==1) objtype='max'
c=obj
b=rhs
do i=1,nr
 if(restcodes(i)==1) rest(i)='<' 
 if(restcodes(i)==2) rest(i)='>' 
 if(restcodes(i)==3) rest(i)='=' 
end do 
!*********************************************
!if(nr<=100) then
! open(10,file='LP-MODEL.txt',status='unknown')
!  WRITE(10,*) 'c = ',c(1:n),'obj type',objtype
!  write(10,*)'  '
!  do i=1,m
!   WRITE(10,*) 'A = ',A(i,1:n),rest(i),b(i)
!  end do
! close(10)
!end if
!********************************************

!********************************************
objold=obj
if(objtype=='min') then
 where(abs(obj)>1E-6) obj=-1.0d0*obj
end if

b=rhs
c=obj
Aold=A
bold=b
restold=rest
do i=1,nr
 index1(i)=i
 index2(i)=i
end do
!***************************************************
! Reordering Code Modified from Miller's LP example
!***************************************************
z=0
rerr=0
x=0
bi=0

numle=0
numge=0


DO i = 1,m
  IF(rest(i) == '<') THEN
    constr_type(i) = 1
    numle = numle + 1
  ELSE IF (rest(i) == '>') THEN
    constr_type(i) = 2
    numge = numge + 1
  ELSE
    constr_type(i) = 3
  END IF
END DO


!     Re-order the constraints if necessary
DO i = 1, numle
  IF (constr_type(i) /= 1) THEN
    DO j = numle+1, m
      IF (constr_type(j) == 1) THEN
        temp(1:n) = a(i, 1:n)
        a(i, 1:n) = a(j, 1:n)
        a(j, 1:n) = temp(1:n)
        z = b(i)
        b(i) = b(j)
        b(j) = z
        itmp=index2(i)
        index2(i)=index2(j)
        index2(j)=itmp
        constr_type(j) = constr_type(i)
        constr_type(i) = 1
        EXIT
      END IF
    END DO
  END IF
END DO
DO i = numle+1, numle+numge
  IF (constr_type(i) /= 2) THEN
    DO j = numle+numge+1, m
      IF (constr_type(j) == 2) THEN
        temp(1:n) = a(i, 1:n)
        a(i, 1:n) = a(j, 1:n)
        a(j, 1:n) = temp(1:n)
        z = b(i)
        b(i) = b(j)
        b(j) = z
        itmp=index1(i)
        index2(i)=index2(j)
        index2(j)=itmp
        constr_type(j) = constr_type(i)
        constr_type(i) = 2
        EXIT
      END IF
    END DO
  END IF
END DO

!     Enter the coefficients in the objective function.
nx=n+numle+numge



!*********************************************************************
!*************************************************************************

!*************************************************************************
!*************************************************************************

 CALL smplx(A, b, c, ka, m, n, ind, ibasis, x, z, iter, mxiter,   &
            numle, numge, bi, rerr,MMLPV)

!************************************************
!compute duals
cold=0.0d0
cold(1:nc)=objold
duals=0.0d0
duals=cold(ibasis)
duals=matmul(duals,bi)
duals(index2)=duals
duals=abs(duals)


objval=z
xvals=x
indstat=ind

!*******************************************
if(objtype=='min') then
 objval=-objval
end if  
!************************************************************************
!reset initial variables
A=Aold
b=bold
rest=restold
100 continue

!*************************************************************************
end subroutine MMLP
!*************************************************************************







!**********************************************
subroutine nCm(n,m,nvals,mvals,replace,seedval)
!**********************************************
! written by Joe Atwood
 implicit none
 !input variables
 integer n,m,nvals(n),replace,seedval
 !output variables
 integer mvals(m)
 !internal variables
 integer ivals(n),nleft,i,k,pick(m)
 real*8 u(m)
!***********
 ivals=nvals
 call ugen(m,seedval,u)

 if(replace>0) then
  pick=floor(u*float(n))+1
  where(pick>n) pick=n
  mvals=ivals(pick)
 end if ! on if(replace==0)

 if(replace==0) then
  do i=1,m
   nleft=n-i+1
   k=floor(u(i)*float(nleft))+1
   if(k>nleft) k=nleft
   mvals(i)=ivals(k)
   ivals(k)=ivals(nleft)
  end do
 end if ! on if(replace>0)

!************************************************************************
end subroutine nCm
!************************************************************************


!************************************************************************
subroutine Bsets(orient,nDMUboot,nDMU,nYM,nXM,YM,XM,nS1,nS2,Slist1,Slist2)
!************************************************************************
! written by Joe Atwood
USE constants_NSWC
implicit none
!************************************************************************
!input variables
integer                :: orient         ! 1=in,  2=out
integer                :: nDMUboot
integer                :: nDMU,nYM,nXM
real (dp)              :: YM(nDMU,nYM),XM(nDMU,nXM)
!************************************************************************
!output variables
integer                :: nS1,nS2
integer                :: Slist1(nDMU),Slist2(nDMU)
!************************************************************************
!internal variables
integer                :: i,j,k
real (dp)              :: YDIFF(nDMU,nYM),XDIFF(nDMU,nXM)
real (dp)              :: Rtmp,Rtmpv(nDMU)
!************************************************************************
Slist1=0; Slist2=0
Slist2(1)=nDMUboot
nS1=0;nS2=1

if(orient==1) then
 do i=1,nYM
  YDIFF(1:nDMU,i)=YM(1:nDMU,i)-YM(nDMUboot,i)
 end do
Rtmpv=minval(YDIFF,dim=2)
 do j=1,nDMU
  if(j.ne.nDMUboot) then
   if(Rtmpv(j)<(0.0d0)) then
    nS1=nS1+1
    Slist1(nS1)=j
   end if
   
   if(Rtmpv(j)>=(0.0d0)) then
    nS2=nS2+1
    Slist2(nS2)=j
   end if
      
  end if 
 end do   
end if !on if(orient==1) then


if(orient==2) then
 do i=1,nXM
  XDIFF(1:nDMU,i)=XM(1:nDMU,i)-XM(nDMUboot,i)
 end do
 Rtmpv=maxval(XDIFF,dim=2)

 do j=1,nDMU
  if(j.ne.nDMUboot) then
   if(Rtmpv(j)>(0.0d0)) then
    nS1=nS1+1
    Slist1(nS1)=j
   end if
   
   if(Rtmpv(j)<=(0.0d0)) then
    nS2=nS2+1
    Slist2(nS2)=j
   end if
     
  end if 
 end do   
end if !on if(orient==2) then

!*************************************************************************
end subroutine Bsets
!*************************************************************************


!*************************************************************************

subroutine Bsample(m,nDMU,replace,nS1,nS2,Slist1,Slist2,jpick1,jpick2,seedval)    
!************************************************************************
! written by Joe Atwood
!************************************************************************
implicit none
!************************************************************************
!input variables
integer                :: m,nDMU,replace,nS1,nS2 
integer                :: jpick1(nDMU),jpick2(nDMU)
integer                :: Slist1(nDMU),Slist2(nDMU)

integer                :: seedval
!************************************************************************
!output variables
!integer                :: jpick1(nDMU),jpick2(nDMU) 
!************************************************************************
!internal variables
integer                :: i,j,k
integer                :: m1,m2
integer                :: nlist1(:),nlist2(:),mlist1(:),mlist2(:)
integer                :: newseed 
!************************************************************************
allocatable            :: nlist1,nlist2,mlist1,mlist2
!************************************************************************
m2=floor(float(m*nS2)/float(nDMU))
if(m2==0) m2=1
m1=m-m2

allocate(nlist1(nS1),nlist2(nS2),mlist1(m1),mlist2(m2))
nlist1=Slist1(1:nS1)
nlist2=Slist2(1:nS2)
mlist1=0
mlist2=0
seedval=newseed(seedval) 
call nCm(nS1,m1,nlist1,mlist1,replace,seedval)
jpick1(1:m1)=mlist1
seedval=newseed(seedval) 
call nCm(nS2,m2,nlist2,mlist2,replace,seedval)
jpick2(1:m2)=mlist2

deallocate(nlist1,nlist2,mlist1,mlist2)
!*************************************************************************
end subroutine Bsample
!*************************************************************************


!************************************************************************
subroutine DEAboot(orient,RTS,nboot,nDMU,nYM,nXM,nDMUboot,bootlist,YM,XM,h,effvals,effstatus,boot,bootstatus,seedval,MMLPV)
!************************************************************************
! written/modified by Joe Atwood
!************************************************************************
USE constants_NSWC
IMPLICIT NONE
!************************************************************************
!input variables
integer                :: orient         ! 1=in,  2=out
integer                :: RTS            ! 1=VRS, 2=DRS, 3=CRS, 4=IRS
integer                :: nboot 
integer                :: seedval
integer                :: nDMU,nYM,nXM,nDMUboot
integer                :: MMLPV
integer                :: bootlist(nDMUboot)
real (dp)              :: YM(nDMU,nYM),XM(nDMU,nXM)
!************************************************************************
!output variables
integer                :: effstatus(nDMU),bootstatus(nDMUboot,nboot)
real (dp)              :: h
real (dp)              :: effvals(nDMU)
real (dp)              :: boot(nDMUboot,nboot)
!************************************************************************

!************************************************************************
!internal variables
!to construct smplx subroutine variables
integer                :: nr,nc
real (dp)              :: obj(nDMU+1),rhs(nYM+nXM+1),a(nYM+nXM+1,nDMU+1)
real (dp)              :: objval

CHARACTER (LEN=3)      :: objtype
CHARACTER (LEN=2)      :: cons(nYM+nXM+1)
CHARACTER (LEN=1)      :: rest(nYM+nXM+1)

INTEGER                :: i,j,k 
INTEGER                :: ka,m,n 
INTEGER                :: constr_type(nYM+nXM+1), ind, ibasis(nYM+nXM+1)
INTEGER                :: iter, mxiter, numle, numge
INTEGER                :: nx

REAL (dp)              :: b(nYM+nXM+1), c(nDMU+1), x(nYM+nXM+1+nDMU+1), z, temp(nDMU+1), rerr
REAL (dp)              :: bi(nYM+nXM+1,nYM+nXM+1)
REAL                   :: t1,t2,t3,t4,t5,t6,t7,t8,t9
!***********************************************************************
CHARACTER (LEN=1)      :: restold(nYM+nXM+1)
integer                :: i1,i2,i3,i4,i5
integer                :: index1(nYM+nXM+1),index2(nYM+nXM+1),itmp

real (dp)              :: objold(nDMU+1),Aold(nYM+nXM+1,nDMU+1),bold(nYM+nXM+1)
!duals info
real (dp)              :: cold(nDMU+1+nDMU+1)
real (dp)              :: duals(nYM+nXM+1)
!***********************************************************************
!deaboot variables 
integer                :: nb,jb,jbcount
real (dp)              :: estar(nDMU)
real (dp)              :: YMSTAR(nDMU,nYM),XMSTAR(nDMU,nXM)
real (dp)              :: effvalsjb(nDMUboot) 
!***********************************************************************
INTERFACE
  SUBROUTINE smplx (a,b0,c,ka,m,n0,ind,ibasis, x, z, iter, mxiter,   &
                    numle, numge, bi, rerr,MMLPV)
    USE constants_NSWC
    IMPLICIT NONE
    INTEGER, INTENT(IN)                    :: ka, m, n0, mxiter, numle, numge
    INTEGER, INTENT(IN OUT)                :: ind
    INTEGER, DIMENSION(:), INTENT(IN OUT)  :: ibasis
    INTEGER, INTENT(OUT)                   :: iter
    REAL (dp), DIMENSION(:,:), INTENT(IN)  :: a
    REAL (dp), DIMENSION(:), INTENT(IN)    :: b0, c
    REAL (dp), INTENT(OUT)                 :: z, rerr
    REAL (dp), DIMENSION(:), INTENT(OUT)   :: x
    REAL (dp), DIMENSION(:,:), INTENT(OUT) :: bi
!******************************************************
    INTEGER, INTENT(IN)                    :: MMLPV    !atwood 8/20/2015
!******************************************************

  END SUBROUTINE smplx
END INTERFACE
!************************************************************************



!************************************************************************
!write(*,*)
!write(*,*) (YM(i,1),i=1,nDMU)
!write(*,*)
!write(*,*) (XM(i,1),i=1,nDMU)
!write(*,*) (XM(i,2),i=1,nDMU)
!write(*,*)
!

boot=0.0d0
nr=nYM+nXM+1
nc=nDMU+1

ka=nr
m=nr
n=nc
mxiter=1000
ind=0
ibasis=0
iter=0
!**********************************

!**********************************
! construct LP data for DEA model
obj=0.0d0
A=0.0d0
rhs=0.0d0
cons='>='

obj(nc)=1.0d0
if(orient == 1) objtype='min'
if(orient == 2) objtype='max'

do i=1,nYM
 A(i,1:nDMU)=YM(:,i)
 if(orient == 1) A(i,nc)=0.0d0
 if(orient == 2) A(i,nc)=-YM(1,i)
 cons(i)='>='
 if(orient == 1) rhs(i)=YM(1,i)    ! input model
 if(orient == 2) rhs(i)=0.0d0      ! output model
end do

do i=1,nXM
 i1=nYM+i
 A(i1,1:nDMU)=XM(:,i)
 if(orient == 1) A(i1,nc)=-XM(1,i)  !input model
 if(orient == 2) A(i1,nc)=0.0d0     !output model
 cons(i1)='<='
 if(orient == 1) rhs(i1)=0.0d0      !input model
 if(orient == 2) rhs(i1)=XM(1,i)    !output model
end do

A(nr,1:nDMU)=1.0d0

!set CRS as default  RTS=3
 cons(nr)='>='   
 rhs(nr)=0.0d0


if(RTS==1) then  !VRS
 cons(nr)='=='  
 rhs(nr)=1.0d0
end if    


if(RTS==2) then  !DRS
 cons(nr)='<='  
 rhs(nr)=1.0d0
end if    


if(RTS==4) then  !IRS
 cons(nr)='>='  
 rhs(nr)=1.0d0
end if    


!set up smplx
c=obj
b=rhs
rest=cons
!*********************************************
!if(nr<=100) then
! open(10,file='DEA-MODEL.txt',status='unknown')
!  WRITE(10,*) 'c = ',c(1:n),'obj type',objtype
!  write(10,*)'  '
!  do i=1,m
!   WRITE(10,*) 'A = ',A(i,1:n),rest(i),b(i)
!  end do
! close(10)
!end if
!********************************************






!********************************************
do i=1,nr
 if(cons(i)=='<=') rest(i) = '<'
 if(cons(i)=='>=') rest(i) = '>'
 if(cons(i)=='==') rest(i) = '='
end do 

objold=obj
if(objtype=='min') then
 do i=1,nc
  if(abs(obj(i))>1E-6)  then
   obj(i)=obj(i)*(-1.0d0)
  end if 
 end do
end if

b=rhs
c=obj
Aold=A
bold=b
restold=rest
do i=1,nr
 index1(i)=i
 index2(i)=i
end do

!!*********************************************
!!*********************************************
!if(nDMU<=100) then
! open(10,file='DEA-MODEL-1.txt',status='unknown')
!  WRITE(10,*) 'c = ',c(1:n),'obj type',objtype
!  write(10,*)'  '
!  do i=1,m
!   WRITE(10,*) 'A = ',A(i,1:n),rest(i),b(i)
!  end do
! close(10)
!end if
!!********************************************


!*****************************************************
! sorting code borrowed from Alan Miller's lp example
!*****************************************************
z=0
rerr=0
x=0
bi=0

numle=0
numge=0


DO i = 1,m
  IF(rest(i) == '<') THEN
    constr_type(i) = 1
    numle = numle + 1
  ELSE IF (rest(i) == '>') THEN
    constr_type(i) = 2
    numge = numge + 1
  ELSE
    constr_type(i) = 3
  END IF
END DO

!     Re-order the constraints if necessary
DO i = 1, numle
  IF (constr_type(i) /= 1) THEN
    DO j = numle+1, m
      IF (constr_type(j) == 1) THEN
        temp(1:n) = a(i, 1:n)
        a(i, 1:n) = a(j, 1:n)
        a(j, 1:n) = temp(1:n)
        z = b(i)
        b(i) = b(j)
        b(j) = z
        itmp=index2(i)
        index2(i)=index2(j)
        index2(j)=itmp
        constr_type(j) = constr_type(i)
        constr_type(i) = 1
        EXIT
      END IF
    END DO
  END IF
END DO
DO i = numle+1, numle+numge
  IF (constr_type(i) /= 2) THEN
    DO j = numle+numge+1, m
      IF (constr_type(j) == 2) THEN
        temp(1:n) = a(i, 1:n)
        a(i, 1:n) = a(j, 1:n)
        a(j, 1:n) = temp(1:n)
        z = b(i)
        b(i) = b(j)
        b(j) = z
        itmp=index1(i)
        index2(i)=index2(j)
        index2(j)=itmp
        constr_type(j) = constr_type(i)
        constr_type(i) = 2
        EXIT
      END IF
    END DO
  END IF
END DO

nx=n+numle+numge
!*********************************************************************

!*************************************************************************
cold=0.0d0
cold(1:nc)=objold
!*************************************************************************
!estimate eff scores for each dmu
call cpu_time(t7) 
do i=1,nDMU
 i1=nYM+1
 i2=i1+nXM-1
 
 if(orient == 1) A(index2(i1:i2),nc)=-1.0d0*XM(i,1:nXM) !input model
 if(orient == 2) A(index2(1:nYM),nc)=-1.0d0*YM(i,1:nYM) !output model

 if(orient == 1) b(index2(1:nYM))=YM(i,1:nYM)  ! input model
 if(orient == 2) b(index2(i1:i2))=XM(i,1:nXM)  ! output model
 
!!*********************************************
!if(nDMU<=100 .and. i==1) then
! open(10,file='DEA-MODEL-2.txt',status='unknown')
!  WRITE(10,*) 'c = ',c(1:n),'obj type',objtype
!  write(10,*)'  '
!  do i1=1,m
!   WRITE(10,*) 'A = ',A(i1,1:n),rest(i1),b(i1)
!  end do
! close(10)
!end if
!!********************************************
 ind=0
 CALL smplx(A, b, c, ka, m, n, ind, ibasis, x, z, iter, mxiter,   &
            numle, numge, bi, rerr,MMLPV)
 effstatus(i)=ind
 effvals(i)=z
 if(ind.ne.0) then
    effvals(i)=1.0 
    if(objtype=='min') effvals(i)=-1.0
 end if    
!
! duals=0.0d0
! duals=cold(ibasis)
! duals=matmul(duals,bi)
! duals(index2)=duals
! duals=abs(duals)
 
end do
!*******************************************
if(objtype=='min') then
 effvals=-effvals
end if  
call cpu_time(t8) 
!************************************************************************
!**************************************************************************
!t9=t8-t7
!write(*,'(a,f15.6)') 'DEA DMU runtime = ',t9
!**************************************************************************
if(nboot>0) then
!bootstrap section
!estimate h
if(orient == 1) call hcalc_input(nDMU,effvals,h)
if(orient == 2) call hcalc_output(nDMU,effvals,h)

!write(*,*) 'h = ',h
!write(*,*) ' '
!******************
jbcount=0
call cpu_time(t7)
!********************************************
do 100 jb=1,nboot
 if(orient == 1) call DEA_in_sample(nDMU,effvals,h,seedval,estar)
 if(orient == 2) call DEA_out_sample(nDMU,effvals,h,seedval,estar)
!***********************************
  if(orient == 1) then
   XMSTAR=XM
   do j=1,nXM
    XMSTAR(:,j)=XM(:,j)*effvals/estar
    i1=nYM+j
    A(index2(i1),1:nDMU)=XMSTAR(:,j)
    A(index2(i1),nc)=-XM(1,j)
   end do 
  end if 
!***********************************
  if(orient == 2) then 
   YMSTAR=YM
   do j=1,nYM
    YMSTAR(:,j)=YM(:,j)*effvals/estar
    A(index2(j),1:nDMU)=YMSTAR(:,j) !output model
    A(index2(j),nc)=-YM(1,j)        ! output model
   end do
  end if
!******************************************* 
 effvalsjb=-100.0d0
  do i=1,nDMUboot
   nb=bootlist(i)   
   i1=nYM+1
   i2=i1+nXM-1
   if(orient == 1) b(index2(1:nYM))=YM(nb,1:nYM)  ! input model
   if(orient == 2) b(index2(i1:i2))=XM(nb,1:nXM)  ! output moel

   if(orient == 1) A(index2(i1:i2),nc)=-1.0d0*XM(nb,1:nXM) !input model
   if(orient == 2) A(index2(1:nYM),nc)=-1.0d0*YM(nb,1:nYM) !output model

   ind=0
   CALL smplx(A, b, c, ka, m, n, ind, ibasis, x, z, iter, mxiter,   &
            numle, numge, bi, rerr,MMLPV)

   if(ind==0) effvalsjb(i)=z
   bootstatus(i,jb)=ind

   !duals=0.0d0
   !duals=cold(ibasis)
   !duals=matmul(duals,bi)
   !duals(index2)=duals
   !duals=abs(duals)
  end do
!*******************************************
 if(objtype=='min') then
  effvalsjb=-effvalsjb
 end if  
 call cpu_time(t8) 
!*******************************************



!********************************************
 boot(1:nDMUboot,jb)=effvalsjb

!***************************************************
! jbcount=jbcount+1
! if(jbcount>=25) then
!  call cpu_time(t8) 
!  t9=t8-t7
!  write(*,'(a,i6,f15.6)') 'jbboot,DEA DMUout runtime = ',jb,t9
!  jbcount=0
! end if    
!**************************************************
100 continue
end if ! end if(nboot>0) then
!**************************************************************************

!**************************************************************************
call cpu_time(t8) 


!open(10,file='effvals-DEAout-boot.out',status='unknown')
!do i=1,nDMU
! write(10,102) (boot(i,k),k=1,nboot)
!end do
!close(10)
!
!t9=t8-t7
!write(*,'(a,f15.6)') 'DEA DMUout runtime = ',t9
!write(*,*) ' '
!*************************************************************************
END subroutine DEAboot
!*************************************************************************

!************************************************************************
 subroutine DDEAboot(RTS,nboot,nDMU,nYM,nXM,nDMUboot,bootlist,YM,XM,DYM,DXM,h,effvals,effstatus,boot,bootstatus,seedval,MMLPV)
!************************************************************************
USE constants_NSWC
IMPLICIT NONE
!************************************************************************
! input variables
 integer                :: RTS              ! 1=CRS, 2=VRS, 3=DRS, 4=IRS
 integer                :: nboot
 integer                :: seedval
 integer                :: nDMU,nYM,nXM,nDMUboot
 integer                :: MMLPV
 integer                :: bootlist(nDMUboot)
 real (dp)              :: YM(nDMU,nYM),XM(nDMU,nXM)
 real (dp)              :: DYM(nDMU,nYM),DXM(nDMU,nXM)
!*************************************************************************
!output variables
 integer                :: effstatus(nDMU),bootstatus(nDMUboot,nboot)
 real (dp)              :: h
 real (dp)              :: effvals(nDMU)
 real (dp)              :: boot(nDMUboot,nboot)
!************************************************************************
!internal variables
!to construct smplx subroutine variables
 integer                :: nr,nc
 real (dp)              :: obj(nDMU+1),rhs(nYM+nXM+1),a(nYM+nXM+1,nDMU+1)
 real (dp)              :: objval
 
 CHARACTER (LEN=3)      :: objtype
 CHARACTER (LEN=2)      :: cons(nYM+nXM+1)
 CHARACTER (LEN=1)      :: rest(nYM+nXM+1)
 
 INTEGER                :: i,j,k 
 INTEGER                :: ka,m,n 
 INTEGER                :: constr_type(nYM+nXM+1), ind, ibasis(nYM+nXM+1)
 INTEGER                :: iter, mxiter, numle, numge
 INTEGER                :: nx
 
 REAL (dp)              :: b(nYM+nXM+1), c(nDMU+1), x(nYM+nXM+1+nDMU+1), z, temp(nDMU+1), rerr
 REAL (dp)              :: bi(nYM+nXM+1,nYM+nXM+1)
 REAL                   :: t1,t2,t3,t4,t5,t6,t7,t8,t9
 
 real (dp)              :: scalevals(nDMU)
 real (dp)              :: ssdX,ssdY
 
 
!***********************************************************************
 CHARACTER (LEN=1)      :: restold(nYM+nXM+1)
 integer                :: i1,i2,i3,i4,i5
 integer                :: index1(nYM+nXM+1),index2(nYM+nXM+1),itmp
 
 real (dp)              :: objold(nDMU+1),Aold(nYM+nXM+1,nDMU+1),bold(nYM+nXM+1)
 !duals info
 real (dp)              :: cold(nDMU+1+nDMU+1)
 real (dp)              :: duals(nYM+nXM+1)
!***********************************************************************
 !ddeaboot variables 
 integer                      :: jb,jbcount
 real (dp)                    :: TDeffvals(nDMU)
 real (dp)                    :: estar(nDMU)
 real (dp)                    :: YMSTAR(nDMU,nYM),XMSTAR(nDMU,nXM)
 real (dp)                    :: effvalsjb(nDMUboot)
!***********************************************************************
INTERFACE
  SUBROUTINE smplx (a,b0,c,ka,m,n0,ind,ibasis, x, z, iter, mxiter,   &
                    numle, numge, bi, rerr,MMLPV)
    USE constants_NSWC
    IMPLICIT NONE
    INTEGER, INTENT(IN)                    :: ka, m, n0, mxiter, numle, numge
    INTEGER, INTENT(IN OUT)                :: ind
    INTEGER, DIMENSION(:), INTENT(IN OUT)  :: ibasis
    INTEGER, INTENT(OUT)                   :: iter
    REAL (dp), DIMENSION(:,:), INTENT(IN)  :: a
    REAL (dp), DIMENSION(:), INTENT(IN)    :: b0, c
    REAL (dp), INTENT(OUT)                 :: z, rerr
    REAL (dp), DIMENSION(:), INTENT(OUT)   :: x
    REAL (dp), DIMENSION(:,:), INTENT(OUT) :: bi
!******************************************************
    INTEGER, INTENT(IN)                    :: MMLPV    !atwood 8/20/2015
!******************************************************

  END SUBROUTINE smplx
END INTERFACE
!************************************************************************

!!write(*,*)
!!write(*,*) (YM(i,1),i=1,nDMU)
!!write(*,*)
!!write(*,*) (XM(i,1),i=1,nDMU)
!!write(*,*) (XM(i,2),i=1,nDMU)
!!write(*,*)
!
!
! do i=1,nDMU
!  !write(*,*) (DXM(i,k),k=1,nXM),(DYM(i,k),k=1,nYM)
! end do
!

nr=nYM+nXM+1
nc=nDMU+1

ka=nr
m=nr
n=nc
mxiter=1000
ind=0
ibasis=0
iter=0
!***********************************
! scale the direction vectors
scalevals=1.0d0
 do i=1,nDMU
  ssdX=sum(DXM(i,1:nXM)*DXM(i,1:nXM))
  ssdY=sum(DYM(i,1:nYM)*DYM(i,1:nYM))
  scalevals(i)=1.0d0/sqrt(ssdX+ssdY)
  DXM(i,1:nxM)=scalevals(i)*DXM(i,1:nxM)
  DYM(i,1:nYM)=scalevals(i)*DYM(i,1:nYM)
 end do
!***********************************
! construct LP data for 
obj=0.0d0
A=0.0d0
rhs=0.0d0
cons='>='

obj(nc)=1.0d0
objtype='max'

do i=1,nYM
 A(i,1:nDMU)=YM(:,i)
 A(i,nc)=-DYM(1,i)
 cons(i)='>='
 rhs(i)=YM(1,i)       
end do

do i=1,nXM
 i1=nYM+i
 A(i1,1:nDMU)=XM(:,i)
 A(i1,nc)=DXM(1,i)  
 cons(i1)='<='
 rhs(i1)=XM(1,i)    
end do

A(nr,1:nDMU)=1.0d0

cons(nr)='>='
rhs(nr)=0.0d0

if(RTS==2) then
 cons(nr)='=='
 rhs(nr)=1.0d0
end if

if(RTS==3) then
 cons(nr)='<='
 rhs(nr)=1.0d0
end if

if(RTS==4) then
 cons(nr)='>='
 rhs(nr)=1.0d0
end if


!set up smplx
c=obj
b=rhs
rest=cons
!*********************************************
if(nr<=100) then
 open(10,file='DEA-MODEL.txt',status='unknown')
  !write(10,*) 'c = ',c(1:n),'obj type',objtype
  !write(10,*)'  '
  do i=1,m
   !write(10,*) 'A = ',A(i,1:n),rest(i),b(i)
  end do
 close(10)
end if
!********************************************

!********************************************
do i=1,nr
 if(cons(i)=='<=') rest(i) = '<'
 if(cons(i)=='>=') rest(i) = '>'
 if(cons(i)=='=') rest(i) = '='
end do 

objold=obj
if(objtype=='min') then
 do i=1,nc
  if(abs(obj(i))>1E-6)  then
   obj(i)=obj(i)*(-1.0d0)
  end if 
 end do
end if

b=rhs
c=obj
Aold=A
bold=b
restold=rest
do i=1,nr
 index1(i)=i
 index2(i)=i
end do

!*********************************************
!open(10,file='DEA-MODEL.txt',status='unknown')
! !write(10,*) 'c = ',c(1:n)
! !write(10,*)'  '
! do i=1,m
!  !write(10,*) 'A = ',A(i,1:n),rest(i),b(i)
! end do
!close(10)
!********************************************
z=0
rerr=0
x=0
bi=0

numle=0
numge=0


DO i = 1,m
  IF(rest(i) == '<') THEN
    constr_type(i) = 1
    numle = numle + 1
  ELSE IF (rest(i) == '>') THEN
    constr_type(i) = 2
    numge = numge + 1
  ELSE
    constr_type(i) = 3
  END IF
END DO


!     Re-order the constraints if necessary
DO i = 1, numle
  IF (constr_type(i) /= 1) THEN
    DO j = numle+1, m
      IF (constr_type(j) == 1) THEN
        temp(1:n) = a(i, 1:n)
        a(i, 1:n) = a(j, 1:n)
        a(j, 1:n) = temp(1:n)
        z = b(i)
        b(i) = b(j)
        b(j) = z
        itmp=index2(i)
        index2(i)=index2(j)
        index2(j)=itmp
        constr_type(j) = constr_type(i)
        constr_type(i) = 1
        EXIT
      END IF
    END DO
  END IF
END DO
DO i = numle+1, numle+numge
  IF (constr_type(i) /= 2) THEN
    DO j = numle+numge+1, m
      IF (constr_type(j) == 2) THEN
        temp(1:n) = a(i, 1:n)
        a(i, 1:n) = a(j, 1:n)
        a(j, 1:n) = temp(1:n)
        z = b(i)
        b(i) = b(j)
        b(j) = z
        itmp=index1(i)
        index2(i)=index2(j)
        index2(j)=itmp
        constr_type(j) = constr_type(i)
        constr_type(i) = 2
        EXIT
      END IF
    END DO
  END IF
END DO

!     Enter the coefficients in the objective function.
nx=n+numle+numge
!*********************************************************************
!k=0
!call cpu_time(t1) 
!DO j = 1,500000
! k=k+1
! z=0
!CALL smplx (a, b, c, ka, m, n, ind, ibasis, x, z, iter, mxiter,   &
!            numle, numge, bi, rerr)
! if(k==25000) then
!  !write(*, '(a, 1i10,1f16.6)') 'loop and obj value =  ', j,z
!  k=0
! end if    
!
!END DO
!call cpu_time(t2) 
!*********************************************************************
!
!CALL smplx (a, b, c, ka, m, n, ind, ibasis, x, z, iter, mxiter,   &
!            numle, numge, bi, rerr)
!
!
!!write(*, '(a, i2, a, i3)') ' IND = ', ind, '  No. of iterations = ', iter
!IF (ind == 0 .OR. i== 6) THEN
!!  !write(*, '(a, 9f7.3)') ' Solution: ', x(1:nx)
!  !write(*, '(a, f8.3)') ' Value of objective = ', z
!
!!  !write(*,*) 'ibasis'
!!  !write(*, '(9i5)') ibasis(1:m)
!
!!  !write(*, *) 'The inverse matrix:'
!!  DO i = 1, m
!!    !write(*, '(9f8.3)') bi(i,1:m)
!!  END DO
!END IF
!
!!write(*, '(a, g12.4)') ' Relative error = ', rerr
!
!t3=t2-t1
!!write(*,'(a,f15.6)') '500000 rep runtime = ',t3
!*************************************************************************

!*************************************************************************
effvals=-1.0d0
cold=0.0d0
cold(1:nc)=objold
!*************************************************************************
call cpu_time(t7) 
do i=1,nDMU
 i1=nYM+1
 i2=i1+nXM-1
 b(index2(1:nYM))=YM(i,1:nYM)  
 b(index2(i1:i2))=XM(i,1:nXM)  
 

 A(index2(1:nYM),nc)=-1.0d0*DYM(i,1:nYM) 
 A(index2(i1:i2),nc)= 1.0d0*DXM(i,1:nXM) 
 
!!*********************************************
!if(i==1) then
! open(10,file='DEA-MODEL-1.txt',status='unknown')
! !write(10,*) 'c = ',c(1:n)
! !write(10,*)'  '
! do i2=1,m
!  !write(10,*) 'A = ',A(i2,1:n),rest(i2),b(i2)
! end do
!close(10)
!end if
!!********************************************
!
 CALL smplx(A, b, c, ka, m, n, ind, ibasis, x, z, iter, mxiter,   &
            numle, numge, bi, rerr,MMLPV)
 if(ind==0) effvals(i)=z
 effstatus(i)=ind
duals=0.0d0
duals=cold(ibasis)
duals=matmul(duals,bi)
duals(index2)=duals
duals=abs(duals)
end do
!*******************************************
if(objtype=='min') then
 effvals=-effvals
end if  
call cpu_time(t8) 
!************************************************************************

!**************************************************************************
!bootstrap
TDeffvals=1.0d0+effvals  ! transform directional scores to "output" type scores >=1
call hcalc_output(nDMU,TDeffvals,h)
!write(*,*) 'h = ',h
!write(*,*) ' '


!******************
seedval=1001
jbcount=0
call cpu_time(t7)
do 100 jb=1,nboot

call DEA_out_sample(nDMU,TDeffvals,h,seedval,estar)
estar=estar-1.0d0     ! transform 'output type scores (>=1) to directional scores (>=0)

YMSTAR=YM
do j=1,nYM
 YMSTAR(:,j)=YM(:,j)+(effvals-estar)*DYM(:,j)
 A(index2(j),1:nDMU)=YMSTAR(:,j) 
 A(index2(j),nc)=-DYM(1,j)       
end do

XMSTAR=XM
do j=1,nXM
 XMSTAR(:,j)=XM(:,j)+(estar-effvals)*DXM(:,j)
 i1=nYM+j
 A(index2(i1),1:nDMU)=XMSTAR(:,j)
 A(index2(i1),nc)=DXM(1,j)
end do

!******************************************* 
effvalsjb=-1.0d0
do j=1,nDMUboot
 i=bootlist(j)      
 i1=nYM+1
 i2=i1+nXM-1
  b(index2(1:nYM))=YM(i,1:nYM)  
  b(index2(i1:i2))=XM(i,1:nXM)  

  A(index2(1:nYM),nc)=-1.0d0*DYM(i,1:nYM) 
  A(index2(i1:i2),nc)= 1.0d0*DXM(i,1:nXM) 
 
   CALL smplx(A, b, c, ka, m, n, ind, ibasis, x, z, iter, mxiter,   &
            numle, numge, bi, rerr,MMLPV)
   if(ind==0) effvalsjb(j)=z
   bootstatus(j,jb)=ind

   !duals=0.0d0
   !duals=cold(ibasis)
   !duals=matmul(duals,bi)
   !duals(index2)=duals
   !duals=abs(duals)
end do
!*******************************************
if(objtype=='min') then
 effvalsjb=-effvalsjb
end if  
call cpu_time(t8) 
!************************************************************************




!********************************************
boot(1:nDMUboot,jb)=effvalsjb
jbcount=jbcount+1
if(jbcount>=25) then
 call cpu_time(t8) 
 t9=t8-t7
 !write(*,'(a,i6,f15.6)') 'jbboot,DEA DMUout runtime = ',jb,t9
 jbcount=0
end if    

100 continue

!***********************************
! scale the direction vectors
do i=1,nDMU
  effvals(i)=scalevals(i)*effvals(i)
end do

do j=1,nDMUboot
 i=bootlist(j)   
 boot(j,1:nboot)=scalevals(i)*boot(j,1:nboot)
end do
!***********************************

call cpu_time(t8) 


t9=t8-t7
!write(*,'(a,f15.6)') 'DEA DMUout runtime = ',t9
!write(*,*) ' '
!*************************************************************************
END subroutine DDEAboot
!*************************************************************************


!************************************************************************
subroutine DEAnCm(orient,RTS,nDMUboot,bootlist,mNUM,mlist,nboot,balance,   &
replace,nDMU,nYM,nXM,YM,XM,effvals,effstatus,boot,bootstatus,seedval,MMLPV)
!************************************************************************
! written/modified by Joe Atwood
!************************************************************************
USE constants_NSWC
IMPLICIT NONE
!************************************************************************
!input variables
integer                :: orient         ! 1=in,  2=out
integer                :: RTS            ! 1=VRS, 2=DRS, 3=CRS, 4=IRS
integer                :: nDMUboot,bootlist(nDMUboot),mNUM,mlist(mnum),nboot
integer                :: balance,replace  
integer                :: nDMU,nYM,nXM
integer                :: seedval 
integer                :: MMLPV
real (dp)              :: YM(nDMU,nYM),XM(nDMU,nXM)
!************************************************************************
!output variables
integer                :: effstatus(nDMUboot),bootstatus(nDMUboot,mNUM,nboot)
real (dp)              :: effvals(nDMUboot),boot(nDMUboot,mNUM,nboot)
!************************************************************************
!internal variables
!to construct smplx subroutine variables
integer                :: nr,nc
real (dp)              :: obj(nDMU+1),rhs(nYM+nXM+1),a(nYM+nXM+1,nDMU+1)

CHARACTER (LEN=3)      :: objtype
CHARACTER (LEN=2)      :: cons(nYM+nXM+1)
CHARACTER (LEN=1)      :: rest(nYM+nXM+1)

INTEGER                :: i,j,k 
INTEGER                :: ka,m,n 
INTEGER                :: constr_type(nYM+nXM+1), ind, ibasis(nYM+nXM+1)
INTEGER                :: iter, mxiter, numle, numge
INTEGER                :: nx

REAL (dp)              :: b(nYM+nXM+1), c(nDMU+1), x(nYM+nXM+1+nDMU+1), z, temp(nDMU+1), rerr
REAL (dp)              :: bi(nYM+nXM+1,nYM+nXM+1)
REAL                   :: t1,t2,t3,t4,t5,t6,t7,t8,t9
!***********************************************************************
CHARACTER (LEN=1)      :: restold(nYM+nXM+1)
integer                :: i1,i2,i3,i4,i5
integer                :: index1(nYM+nXM+1),index2(nYM+nXM+1),itmp

real (dp)              :: objold(nDMU+1),Aold(nYM+nXM+1,nDMU+1),bold(nYM+nXM+1)
!duals info
real (dp)              :: cold(nDMU+1+nDMU+1)
real (dp)              :: duals(nYM+nXM+1)
!***********************************************************************
!DEAnCm variables
integer                :: newseed 
integer                :: nvals(nDMU),nm,nmp1,im
integer                :: ipick,jm,jb,jbcount
integer                :: jpick(nDMU)

real (dp)              :: Am(:,:),cm(:) 
allocatable            :: Am,cm

! balance sampling variables
integer                :: jdmu,dmuj,nS1,nS2 
integer                :: jpick1(nDMU),jpick2(nDMU)
integer                :: Slist1(nDMU),Slist2(nDMU)
integer                :: m1,m2
!***********************************************************************
INTERFACE
  SUBROUTINE smplx (a,b0,c,ka,m,n0,ind,ibasis, x, z, iter, mxiter,   &
                    numle, numge, bi, rerr,MMLPV)
    USE constants_NSWC
    IMPLICIT NONE
    INTEGER, INTENT(IN)                    :: ka, m, n0, mxiter, numle, numge
    INTEGER, INTENT(IN OUT)                :: ind
    INTEGER, DIMENSION(:), INTENT(IN OUT)  :: ibasis
    INTEGER, INTENT(OUT)                   :: iter
    REAL (dp), DIMENSION(:,:), INTENT(IN)  :: a
    REAL (dp), DIMENSION(:), INTENT(IN)    :: b0, c
    REAL (dp), INTENT(OUT)                 :: z, rerr
    REAL (dp), DIMENSION(:), INTENT(OUT)   :: x
    REAL (dp), DIMENSION(:,:), INTENT(OUT) :: bi

    INTEGER, INTENT(IN)                    :: MMLPV 

  END SUBROUTINE smplx
END INTERFACE
!************************************************************************



!************************************************************************

boot=1.0d0
nr=nYM+nXM+1
nc=nDMU+1

ka=nr
m=nr
n=nc
mxiter=1000
ind=0
ibasis=0
iter=0
!**********************************

!**********************************
! construct LP data for DEA model
obj=0.0d0
A=0.0d0
rhs=0.0d0
cons='>='

obj(nc)=1.0d0
if(orient == 1) objtype='min'
if(orient == 2) objtype='max'

do i=1,nYM
 A(i,1:nDMU)=YM(:,i)
 if(orient == 1) A(i,nc)=0.0d0
 if(orient == 2) A(i,nc)=-YM(1,i)
 cons(i)='>='
 if(orient == 1) rhs(i)=YM(1,i)    ! input model
 if(orient == 2) rhs(i)=0.0d0      ! output model
end do

do i=1,nXM
 i1=nYM+i
 A(i1,1:nDMU)=XM(:,i)
 if(orient == 1) A(i1,nc)=-XM(1,i)  !input model
 if(orient == 2) A(i1,nc)=0.0d0     !output model
 cons(i1)='<='
 if(orient == 1) rhs(i1)=0.0d0      !input model
 if(orient == 2) rhs(i1)=XM(1,i)    !output model
end do

A(nr,1:nDMU)=1.0d0

!set CRS as default  RTS=3
 cons(nr)='>='   
 rhs(nr)=0.0d0


if(RTS==1) then  !VRS
 cons(nr)='=='  
 rhs(nr)=1.0d0
end if    


if(RTS==2) then  !DRS
 cons(nr)='<='  
 rhs(nr)=1.0d0
end if    


if(RTS==4) then  !IRS
 cons(nr)='>='  
 rhs(nr)=1.0d0
end if    


!set up smplx
c=obj
b=rhs
rest=cons
!*********************************************
!if(nDMU<=100) then
! open(10,file='DEA-MODEL.txt',status='unknown')
!  WRITE(10,*) 'c = ',c(1:n),'obj type',objtype
!  write(10,*)'  '
!  do i=1,m
!   WRITE(10,*) 'A = ',A(i,1:n),rest(i),b(i)
!  end do
! close(10)
!end if
!********************************************






!********************************************
do i=1,nr
 if(cons(i)=='<=') rest(i) = '<'
 if(cons(i)=='>=') rest(i) = '>'
 if(cons(i)=='==') rest(i) = '='
end do 

objold=obj
if(objtype=='min') then
 do i=1,nc
  if(abs(obj(i))>1E-6)  then
   obj(i)=obj(i)*(-1.0d0)
  end if 
 end do
end if

b=rhs
c=obj
Aold=A
bold=b
restold=rest
do i=1,nr
 index1(i)=i
 index2(i)=i
end do

!!*********************************************
!!*********************************************
!if(nDMU<=100) then
! open(10,file='DEA-MODEL-1-nCm.txt',status='unknown')
!  WRITE(10,*) 'c = ',c(1:n),'obj type',objtype
!  write(10,*)'  '
!  do i=1,m
!   WRITE(10,*) 'A = ',A(i,1:n),rest(i),b(i)
!  end do
! close(10)
!end if
!!********************************************

!*****************************************************
! sorting code borrowed from Alan Miller's lp example
!*****************************************************
z=0
rerr=0
x=0
bi=0

numle=0
numge=0


DO i = 1,m
  IF(rest(i) == '<') THEN
    constr_type(i) = 1
    numle = numle + 1
  ELSE IF (rest(i) == '>') THEN
    constr_type(i) = 2
    numge = numge + 1
  ELSE
    constr_type(i) = 3
  END IF
END DO


!     Re-order the constraints if necessary
DO i = 1, numle
  IF (constr_type(i) /= 1) THEN
    DO j = numle+1, m
      IF (constr_type(j) == 1) THEN
        temp(1:n) = a(i, 1:n)
        a(i, 1:n) = a(j, 1:n)
        a(j, 1:n) = temp(1:n)
        z = b(i)
        b(i) = b(j)
        b(j) = z
        itmp=index2(i)
        index2(i)=index2(j)
        index2(j)=itmp
        constr_type(j) = constr_type(i)
        constr_type(i) = 1
        EXIT
      END IF
    END DO
  END IF
END DO
DO i = numle+1, numle+numge
  IF (constr_type(i) /= 2) THEN
    DO j = numle+numge+1, m
      IF (constr_type(j) == 2) THEN
        temp(1:n) = a(i, 1:n)
        a(i, 1:n) = a(j, 1:n)
        a(j, 1:n) = temp(1:n)
        z = b(i)
        b(i) = b(j)
        b(j) = z
        itmp=index1(i)
        index2(i)=index2(j)
        index2(j)=itmp
        constr_type(j) = constr_type(i)
        constr_type(i) = 2
        EXIT
      END IF
    END DO
  END IF
END DO

nx=n+numle+numge
!*********************************************************************

!*************************************************************************
cold=0.0d0
cold(1:nc)=objold
!*************************************************************************
!estimate eff scores for each dmu in mlist
call cpu_time(t7) 
do i=1,nDMUboot
 i1=nYM+1
 i2=i1+nXM-1
 
 ipick=bootlist(i)
 
 if(orient == 1) A(index2(i1:i2),nc)=-1.0d0*XM(ipick,1:nXM) !input model
 if(orient == 2) A(index2(1:nYM),nc)=-1.0d0*YM(ipick,1:nYM) !output model

 if(orient == 1) b(index2(1:nYM))=YM(ipick,1:nYM)  ! input model
 if(orient == 2) b(index2(i1:i2))=XM(ipick,1:nXM)  ! output model
 
!!*********************************************
!if(nDMU<=100 .and. i==1) then
! open(10,file='DEA-MODEL-2-nCm.txt',status='unknown')
!  WRITE(10,*) 'c = ',c(1:n),'obj type',objtype
!  write(10,*)'  '
!  do i1=1,m
!   WRITE(10,*) 'A = ',A(i1,1:n),rest(i1),b(i1)
!  end do
! close(10)
!end if
!!********************************************

 CALL smplx(A, b, c, ka, m, n, ind, ibasis, x, z, iter, mxiter,   &
            numle, numge, bi, rerr,MMLPV)
 effstatus(i)=ind
 effvals(i)=z

! duals=0.0d0
! duals=cold(ibasis)
! duals=matmul(duals,bi)
! duals(index2)=duals
! duals=abs(duals)
end do
!*******************************************
if(objtype=='min') then
 effvals=-effvals
end if  
!call cpu_time(t8) 
!************************************************************************
!**************************************************************************
!t9=t8-t7
!write(*,'(a,f15.6)') 'DEA DMU runtime = ',t9
!**************************************************************************



!**************************************************************************
jbcount=0
if(nboot>0) then
!bootstrap section
!******************
do i=1,nDMU
 nvals(i)=i
end do   

!***************************************
 if(balance==0) then
 do 120 jb=1,nboot

 !  seedval=seedval+1
  seedval=newseed(seedval) 
  call nCm(nDMU,nDMU,nvals,jpick,replace,seedval)  
 
  do 110 jm=1,mNUM
  nm=mlist(jm)
  nmp1=nm+1
  allocate(Am(m,nmp1),cm(nmp1))
  AM=0.0d0
  cm=0.0d0
  cm(nmp1)=c(n)

  Am(1:m,1:nm)=A(1:m,jpick(1:nm))

call cpu_time(t7)
!!********************************************
  do 100 i=1,nDMUboot
   im=bootlist(i)   
   i1=nYM+1
   i2=i1+nXM-1
   if(orient == 1) b(index2(1:nYM))=YM(im,1:nYM)  ! input model
   if(orient == 2) b(index2(i1:i2))=XM(im,1:nXM)  ! output moel

   if(orient == 1) Am(index2(i1:i2),nmp1)=-1.0d0*XM(im,1:nXM) !input model
   if(orient == 2) Am(index2(1:nYM),nmp1)=-1.0d0*YM(im,1:nYM) !output model

   Am(index2(i1:i2),1)=1.0d0*XM(im,1:nXM) 
   Am(index2(1:nYM),1)=1.0d0*YM(im,1:nYM) 

!!*********************************************
!if(jb==152 .and. nm==5) then
! if((nDMU<=100)) then
!  open(10,file='DEA-BOOT-MODEL-3-nCm.txt',status='unknown')
!   WRITE(10,*) 'c = ',cm(1:nmp1),'obj type',objtype
!   write(10,*)'  '
!   do i1=1,m
!    WRITE(10,*) 'Am = ',Am(i1,1:nmp1),constr_type(i1),b(i1)
!   end do
!  close(10)
! end if   
!end if


!if((nDMU<=100) .and. (jm==1) .and. (jb==1) .and. (i==1)) then
! open(10,file='DEA-BOOT-MODEL-3-nCm.txt',status='unknown')
!  WRITE(10,*) 'c = ',cm(1:nmp1),'obj type',objtype
!  write(10,*)'  '
!  do i1=1,m
!   WRITE(10,*) 'Am = ',Am(i1,1:nmp1),constr_type(i1),b(i1)
!  end do
! close(10)
!end if
!!********************************************
!
   CALL smplx(Am, b, cm, ka, m, nmp1, ind, ibasis, x, z, iter, mxiter,   &
            numle, numge, bi, rerr,MMLPV)
!
   if(ind==0)  boot(i,jm,jb)=z
   bootstatus(i,jm,jb)=ind


   if(orient==1) then
    if(z<(-1.001d0)) then
     boot(i,jm,jb)=1000.0d0
     bootstatus(i,jm,jb)=10
!     write(*,*) z,i,jm,jb,boot(i,jm,jb)
    end if
    if(z>(0.001d0)) then
     boot(i,jm,jb)=1000.0d0
     bootstatus(i,jm,jb)=10
!     write(*,*) z,i,jm,jb,boot(i,jm,jb)
    end if
   end if

   if(orient==2) then
    if(z<(0.999d0)) then
     boot(i,jm,jb)=-1000.0d0
     bootstatus(i,jm,jb)=10
!     write(*,*) z,i,jm,jb,boot(i,jm,jb)
    end if
    if(z>(1e6)) then
     boot(i,jm,jb)=-1000.0d0
     bootstatus(i,jm,jb)=10
!     write(*,*) z,i,jm,jb,boot(i,jm,jb)
    end if
   end if

!
100 continue
!!*******************************************
!!*******************************************
!
!
!
!!********************************************
!!***************************************************
!call cpu_time(t8) 
! jbcount=jbcount+1
! if(jbcount>=1000) then
!  call cpu_time(t8) 
!  t9=t8-t7
!  write(*,'(a,2i6,f15.6)') 'm,jbboot,DEA DMUout runtime = ',nm,jb,t9
!  jbcount=0
! end if    
!!**************************************************
  deallocate(Am,cm)  

110  continue    
120 continue    
 end if ! end if(balance==0)
!***************************************************************



!***************************************************************
 if(balance>=1) then

 do 250 jdmu=1,nDMUboot
  dmuj=bootlist(jdmu)
  call Bsets(orient,dmuj,nDMU,nYM,nXM,YM,XM,nS1,nS2,Slist1,Slist2) 

  
 do 220 jb=1,nboot
  call Bsample(mlist(mNUM),nDMU,replace,nS1,nS2,Slist1,Slist2,jpick1,jpick2,seedval)

  do 210 jm=1,mNUM
  nm=mlist(jm)
  m2=floor(float(nm*nS2)/float(nDMU))
  if(m2==0) m2=1
  m1=nm-m2
  jpick(1:m1)=jpick1(1:m1)
  jpick((m1+1):(m1+m2))=jpick2(1:m2)

  nmp1=nm+1
  
  allocate(Am(m,nmp1),cm(nmp1))
  AM=0.0d0
  cm=0.0d0
  cm(nmp1)=c(n)

  Am(1:m,1:nm)=A(1:m,jpick(1:nm))

!!********************************************
   i1=nYM+1
   i2=i1+nXM-1
   if(orient == 1) b(index2(1:nYM))=YM(dmuj,1:nYM)  ! input model
   if(orient == 2) b(index2(i1:i2))=XM(dmuj,1:nXM)  ! output model

   if(orient == 1) Am(index2(i1:i2),nmp1)=-1.0d0*XM(dmuj,1:nXM) !input model
   if(orient == 2) Am(index2(1:nYM),nmp1)=-1.0d0*YM(dmuj,1:nYM) !output model

   Am(index2(i1:i2),1)=1.0d0*XM(dmuj,1:nXM) 
   Am(index2(1:nYM),1)=1.0d0*YM(dmuj,1:nYM) 

!!*********************************************
!if(jb==405 .and. nm==5) then
! if((nDMU<=100)) then
!  open(10,file='DEA-BOOT-MODEL-5-nCm.txt',status='unknown')
!   WRITE(10,*) 'c = ',cm(1:nmp1),'obj type',objtype
!   write(10,*)'  '
!   do i1=1,m
!    WRITE(10,*) 'Am = ',Am(i1,1:nmp1),constr_type(i1),b(i1)
!   end do
!  close(10)
! end if   
!end if

!if(jb==405 .and. nm==6) then
! if((nDMU<=100)) then
!  open(10,file='DEA-BOOT-MODEL-6-nCm.txt',status='unknown')
!   WRITE(10,*) 'c = ',cm(1:nmp1),'obj type',objtype
!   write(10,*)'  '
!   do i1=1,m
!    WRITE(10,*) 'Am = ',Am(i1,1:nmp1),constr_type(i1),b(i1)
!   end do
!  close(10)
! end if   
!end if
!!********************************************
!
   CALL smplx(Am, b, cm, ka, m, nmp1, ind, ibasis, x, z, iter, mxiter,   &
            numle, numge, bi, rerr,MMLPV)
!
   if(ind==0)  boot(jdmu,jm,jb)=z
   bootstatus(jdmu,jm,jb)=ind

   if(orient==1) then
    if(z<(-1.001d0)) then
     boot(i,jm,jb)=1000.0d0
     bootstatus(i,jm,jb)=10
!     write(*,*) z,i,jm,jb,boot(i,jm,jb)
    end if
    if(z>(0.001d0)) then
     boot(i,jm,jb)=1000.0d0
     bootstatus(i,jm,jb)=10
!     write(*,*) z,i,jm,jb,boot(i,jm,jb)
    end if
   end if

   if(orient==2) then
    if(z<(0.999d0)) then
     boot(i,jm,jb)=-1000.0d0
     bootstatus(i,jm,jb)=10
!     write(*,*) z,i,jm,jb,boot(i,jm,jb)
    end if
    if(z>(1e6)) then
     boot(i,jm,jb)=-1000.0d0
     bootstatus(i,jm,jb)=10
!     write(*,*) z,i,jm,jb,boot(i,jm,jb)
    end if
   end if



!
! jbcount=jbcount+1
! if(jbcount>=1000) then
!  write(*,'(a,3i6,f15.6)') 'jdmu,jm,jb,z ',jdmu,jm,jb,z
!  jbcount=0
! end if  
 
  deallocate(Am,cm)  

!  write(*,*) jdmu,jm,jb,z

210  continue    

220 continue     !
250 continue     !

 end if ! end if(balance==1)
!************************************************************************


end if ! end if(nboot>0) then
!!**************************************************************************
!
!!**************************************************************************
 if(objtype=='min') then
  boot=-boot
 end if  


!call cpu_time(t8) 
!

!open(10,file='effvals-DEAout-boot.out',status='unknown')
!do i=1,nDMU
! write(10,102) (boot(i,k),k=1,nboot)
!end do
!close(10)
!
!t9=t8-t7
!write(*,'(a,f15.6)') 'DEA DMUout runtime = ',t9
!write(*,*) ' '
!*************************************************************************
END subroutine DEAnCm
!*************************************************************************






!************************************************************************
subroutine DDEAnCm(RTS,nDMUboot,bootlist,mNUM,mlist,nboot,              &
replace,nDMU,nYM,nXM,YM,XM,DYM,DXM,effvals,effstatus,boot,bootstatus,seedval,MMLPV)
!************************************************************************
! written/modified by Joe Atwood
!************************************************************************
USE constants_NSWC
IMPLICIT NONE
!************************************************************************
!input variables
integer                :: RTS            ! 1=VRS, 2=DRS, 3=CRS, 4=IRS
integer                :: nDMUboot,bootlist(nDMUboot),mNUM,mlist(mnum),nboot
integer                :: replace  
integer                :: nDMU,nYM,nXM
integer                :: seedval 
integer                :: MMLPV
real (dp)              :: YM(nDMU,nYM),XM(nDMU,nXM)
real (dp)              :: DYM(nDMUboot,nYM),DXM(nDMUboot,nXM)
!************************************************************************
!output variables
integer                :: effstatus(nDMUboot),bootstatus(nDMUboot,mNUM,nboot)
real (dp)              :: effvals(nDMUboot),boot(nDMUboot,mNUM,nboot)
!************************************************************************
!internal variables
!to construct smplx subroutine variables
integer                :: nr,nc
real (dp)              :: obj(nDMU+1),rhs(nYM+nXM+1),a(nYM+nXM+1,nDMU+1)

CHARACTER (LEN=3)      :: objtype
CHARACTER (LEN=2)      :: cons(nYM+nXM+1)
CHARACTER (LEN=1)      :: rest(nYM+nXM+1)

INTEGER                :: i,j,k 
INTEGER                :: ka,m,n 
INTEGER                :: constr_type(nYM+nXM+1), ind, ibasis(nYM+nXM+1)
INTEGER                :: iter, mxiter, numle, numge
INTEGER                :: nx

REAL (dp)              :: b(nYM+nXM+1), c(nDMU+1), x(nYM+nXM+1+nDMU+1), z, temp(nDMU+1), rerr
REAL (dp)              :: bi(nYM+nXM+1,nYM+nXM+1)
REAL                   :: t1,t2,t3,t4,t5,t6,t7,t8,t9
!***********************************************************************
CHARACTER (LEN=1)      :: restold(nYM+nXM+1)
integer                :: i1,i2,i3,i4,i5
integer                :: index1(nYM+nXM+1),index2(nYM+nXM+1),itmp

real (dp)              :: objold(nDMU+1),Aold(nYM+nXM+1,nDMU+1),bold(nYM+nXM+1)
!duals info
real (dp)              :: cold(nDMU+1+nDMU+1)
real (dp)              :: duals(nYM+nXM+1)
!***********************************************************************
!DEAnCm variables
integer                :: newseed 
integer                :: nvals(nDMU),nm,nmp1,im
integer                :: ipick,jm,jb,jbcount
integer                :: jpick(nDMU)

real (dp)              :: Am(:,:),cm(:) 
allocatable            :: Am,cm

! balance sampling variables
integer                :: jdmu,dmuj,nS1,nS2 
integer                :: jpick1(nDMU),jpick2(nDMU)
integer                :: Slist1(nDMU),Slist2(nDMU)
integer                :: m1,m2
!***********************************************************************
INTERFACE
  SUBROUTINE smplx (a,b0,c,ka,m,n0,ind,ibasis, x, z, iter, mxiter,   &
                    numle, numge, bi, rerr,MMLPV)
    USE constants_NSWC
    IMPLICIT NONE
    INTEGER, INTENT(IN)                    :: ka, m, n0, mxiter, numle, numge
    INTEGER, INTENT(IN OUT)                :: ind
    INTEGER, DIMENSION(:), INTENT(IN OUT)  :: ibasis
    INTEGER, INTENT(OUT)                   :: iter
    REAL (dp), DIMENSION(:,:), INTENT(IN)  :: a
    REAL (dp), DIMENSION(:), INTENT(IN)    :: b0, c
    REAL (dp), INTENT(OUT)                 :: z, rerr
    REAL (dp), DIMENSION(:), INTENT(OUT)   :: x
    REAL (dp), DIMENSION(:,:), INTENT(OUT) :: bi

    INTEGER, INTENT(IN)                    :: MMLPV 

  END SUBROUTINE smplx
END INTERFACE
!************************************************************************



!************************************************************************

boot=1.0d0
nr=nYM+nXM+1
nc=nDMU+1

ka=nr
m=nr
n=nc
mxiter=1000
ind=0
ibasis=0
iter=0
!**********************************

!**********************************
! construct LP data for DEA model
obj=0.0d0
A=0.0d0
rhs=0.0d0
cons='>='

obj(nc)=1.0d0

objtype='max'

do i=1,nYM
 A(i,1:nDMU)=YM(:,i)
 A(i,nc)=-DYM(1,i)
 cons(i)='>='
 rhs(i)=YM(1,i)   
end do

do i=1,nXM
 i1=nYM+i
 A(i1,1:nDMU)=XM(:,i)
 A(i1,nc)=DXM(1,i)  
 cons(i1)='<='
 rhs(i1)=XM(1,i)    
end do

A(nr,1:nDMU)=1.0d0

!set CRS as default  RTS=3
 cons(nr)='>='   
 rhs(nr)=0.0d0


if(RTS==1) then  !VRS
 cons(nr)='=='  
 rhs(nr)=1.0d0
end if    


if(RTS==2) then  !DRS
 cons(nr)='<='  
 rhs(nr)=1.0d0
end if    


if(RTS==4) then  !IRS
 cons(nr)='>='  
 rhs(nr)=1.0d0
end if    


!set up smplx
c=obj
b=rhs
rest=cons
!*********************************************
!if(nDMU<=100) then
! open(10,file='DDEA-MODEL.txt',status='unknown')
!  WRITE(10,*) 'c = ',c(1:n),'obj type',objtype
!  write(10,*)'  '
!  do i=1,m
!   WRITE(10,*) 'A = ',A(i,1:n),rest(i),b(i)
!  end do
! close(10)
!end if
!********************************************






!********************************************
do i=1,nr
 if(cons(i)=='<=') rest(i) = '<'
 if(cons(i)=='>=') rest(i) = '>'
 if(cons(i)=='==') rest(i) = '='
end do 

objold=obj
if(objtype=='min') then
 do i=1,nc
  if(abs(obj(i))>1E-6)  then
   obj(i)=obj(i)*(-1.0d0)
  end if 
 end do
end if

b=rhs
c=obj
Aold=A
bold=b
restold=rest
do i=1,nr
 index1(i)=i
 index2(i)=i
end do

!!*********************************************
!!*********************************************
!if(nDMU<=100) then
! open(10,file='DDEA-MODEL-1-nCm.txt',status='unknown')
!  WRITE(10,*) 'c = ',c(1:n),'obj type',objtype
!  write(10,*)'  '
!  do i=1,m
!   WRITE(10,*) 'A = ',A(i,1:n),rest(i),b(i)
!  end do
! close(10)
!end if
!!********************************************

!*****************************************************
! sorting code borrowed from Alan Miller's lp example
!*****************************************************
z=0
rerr=0
x=0
bi=0

numle=0
numge=0


DO i = 1,m
  IF(rest(i) == '<') THEN
    constr_type(i) = 1
    numle = numle + 1
  ELSE IF (rest(i) == '>') THEN
    constr_type(i) = 2
    numge = numge + 1
  ELSE
    constr_type(i) = 3
  END IF
END DO


!     Re-order the constraints if necessary
DO i = 1, numle
  IF (constr_type(i) /= 1) THEN
    DO j = numle+1, m
      IF (constr_type(j) == 1) THEN
        temp(1:n) = a(i, 1:n)
        a(i, 1:n) = a(j, 1:n)
        a(j, 1:n) = temp(1:n)
        z = b(i)
        b(i) = b(j)
        b(j) = z
        itmp=index2(i)
        index2(i)=index2(j)
        index2(j)=itmp
        constr_type(j) = constr_type(i)
        constr_type(i) = 1
        EXIT
      END IF
    END DO
  END IF
END DO
DO i = numle+1, numle+numge
  IF (constr_type(i) /= 2) THEN
    DO j = numle+numge+1, m
      IF (constr_type(j) == 2) THEN
        temp(1:n) = a(i, 1:n)
        a(i, 1:n) = a(j, 1:n)
        a(j, 1:n) = temp(1:n)
        z = b(i)
        b(i) = b(j)
        b(j) = z
        itmp=index1(i)
        index2(i)=index2(j)
        index2(j)=itmp
        constr_type(j) = constr_type(i)
        constr_type(i) = 2
        EXIT
      END IF
    END DO
  END IF
END DO

nx=n+numle+numge
!*********************************************************************

!*************************************************************************
cold=0.0d0
cold(1:nc)=objold
!*************************************************************************
!estimate eff scores for each dmu in mlist
call cpu_time(t7) 
do i=1,nDMUboot
 i1=nYM+1
 i2=i1+nXM-1
 
 ipick=bootlist(i)
 
 A(index2(i1:i2),nc)=1.0d0*DXM(ipick,1:nXM) 
 A(index2(1:nYM),nc)=-1.0d0*DYM(ipick,1:nYM) 

 b(index2(1:nYM))=YM(ipick,1:nYM)  
 b(index2(i1:i2))=XM(ipick,1:nXM)  
 
!!*********************************************
!if(nDMU<=100 .and. i==1) then
! open(10,file='DEA-MODEL-2-nCm.txt',status='unknown')
!  WRITE(10,*) 'c = ',c(1:n),'obj type',objtype
!  write(10,*)'  '
!  do i1=1,m
!   WRITE(10,*) 'A = ',A(i1,1:n),rest(i1),b(i1)
!  end do
! close(10)
!end if
!!********************************************

 CALL smplx(A, b, c, ka, m, n, ind, ibasis, x, z, iter, mxiter,   &
            numle, numge, bi, rerr,MMLPV)
 effstatus(i)=ind
 effvals(i)=z

! duals=0.0d0
! duals=cold(ibasis)
! duals=matmul(duals,bi)
! duals(index2)=duals
! duals=abs(duals)
end do
!*******************************************
!if(objtype=='min') then
! effvals=-effvals
!end if  
!call cpu_time(t8) 
!************************************************************************
!**************************************************************************
!t9=t8-t7
!write(*,'(a,f15.6)') 'DEA DMU runtime = ',t9
!**************************************************************************


!**************************************************************************
jbcount=0
if(nboot>0) then
!bootstrap section
!******************
do i=1,nDMU
 nvals(i)=i
end do   

!***************************************
 do 120 jb=1,nboot

 !  seedval=seedval+1
  seedval=newseed(seedval) 
  call nCm(nDMU,nDMU,nvals,jpick,replace,seedval)  
 
  do 110 jm=1,mNUM
  nm=mlist(jm)
  nmp1=nm+1
  allocate(Am(m,nmp1),cm(nmp1))
  AM=0.0d0
  cm=0.0d0
  cm(nmp1)=c(n)

  Am(1:m,1:nm)=A(1:m,jpick(1:nm))

!call cpu_time(t7)
!!********************************************
  do 100 i=1,nDMUboot
   im=bootlist(i)   
   i1=nYM+1
   i2=i1+nXM-1
   b(index2(1:nYM))=YM(im,1:nYM)  
   b(index2(i1:i2))=XM(im,1:nXM)  

   Am(index2(i1:i2),nmp1)=1.0d0*DXM(im,1:nXM) 
   Am(index2(1:nYM),nmp1)=-1.0d0*DYM(im,1:nYM) 

   Am(index2(i1:i2),1)=1.0d0*XM(im,1:nXM) 
   Am(index2(1:nYM),1)=1.0d0*YM(im,1:nYM) 

!!*********************************************
!!if(jb==152 .and. nm==5) then
! if((nDMU<=100)) then
!  open(10,file='DDEA-BOOT-MODEL-3-nCm.txt',status='unknown')
!   WRITE(10,*) 'c = ',cm(1:nmp1),'obj type',objtype
!   write(10,*)'  '
!   do i1=1,m
!    WRITE(10,*) 'Am = ',Am(i1,1:nmp1),constr_type(i1),b(i1)
!   end do
!  close(10)
! end if   
!!end if
!!********************************************
!
   CALL smplx(Am, b, cm, ka, m, nmp1, ind, ibasis, x, z, iter, mxiter,   &
            numle, numge, bi, rerr,MMLPV)
!
   if(ind==0)  boot(i,jm,jb)=z
   bootstatus(i,jm,jb)=ind

    if(z<(0.0d0)) then
     boot(i,jm,jb)=-1000.0d0
     bootstatus(i,jm,jb)=10
    end if
    if(z>(1e9)) then
     boot(i,jm,jb)=-1000.0d0
     bootstatus(i,jm,jb)=10
    end if

!
100 continue
!!*******************************************
!!*******************************************
!
!
!
!!***************************************************
  deallocate(Am,cm)  

110  continue    
120 continue    
!***************************************************************



!***************************************************************

end if ! end if(nboot>0) then
!!**************************************************************************
!
!!**************************************************************************

!call cpu_time(t8) 
!

!open(10,file='effvals-DEAout-boot.out',status='unknown')
!do i=1,nDMU
! write(10,102) (boot(i,k),k=1,nboot)
!end do
!close(10)
!
!t9=t8-t7
!write(*,'(a,f15.6)') 'DEA DMUout runtime = ',t9
!write(*,*) ' '
!*************************************************************************
END subroutine DDEAnCm
!*************************************************************************
