+++
title = "Closest-pair problem/Fortran"
description = ""
date = 2010-02-06T12:42:12Z
aliases = []
[extra]
id = 5319
[taxonomies]
categories = []
tags = []
+++

{{collection|Closest pair problem}}

{{works with|Fortran|95 and later}}

This module defines the point type and implements only two operations on "vectors" ("difference" and "length")


```fortran
module Points_Module
  implicit none

  type point
     real :: x, y
  end type point

  interface operator (-)
     module procedure pt_sub
  end interface

  interface len
     module procedure pt_len
  end interface

  public :: point
  private :: pt_sub, pt_len

contains

  function pt_sub(a, b) result(c)
    type(point), intent(in) :: a, b
    type(point) :: c
    
    c = point(a%x - b%x, a%y - b%y)
  end function pt_sub

  function pt_len(a) result(l)
    type(point), intent(in) :: a
    real :: l

    l = sqrt((a%x)**2 + (a%y)**2)
  end function pt_len

end module Points_Module
```


Then we need a modified version of the [[Quicksort#Fortran|fortran quicksort]] able to handle "points" and that can use a custom comparator.


```fortran
module qsort_module
  use Points_Module
  implicit none

contains

  recursive subroutine qsort(a, comparator)
    type(point), intent(inout) :: a(:)
    interface
       integer function comparator(aa, bb)
         use Points_Module
         type(point), intent(in) :: aa, bb
       end function comparator
    end interface

    integer :: split

    if(size(a) > 1) then
       call partition(a, split, comparator)
       call qsort(a(:split-1), comparator)
       call qsort(a(split:), comparator)
    end if

  end subroutine qsort

  subroutine partition(a, marker, comparator)
    type(point), intent(inout) :: a(:)
    integer, intent(out) :: marker

    interface
       integer function comparator(aa, bb)
         use Points_Module
         type(point), intent(in) :: aa, bb
       end function comparator
    end interface

    type(point) :: pivot, temp
    integer :: left, right

    left = 0                       
    right = size(a) + 1
    pivot = a(size(a)/2)

    do while (left < right)
       right = right - 1
       do while (comparator(a(right), pivot) > 0)
          right = right - 1
       end do
       left = left + 1
       do while (comparator(a(left), pivot) < 0)
          left = left + 1
       end do
       if ( left < right ) then
          temp = a(left)
          a(left) = a(right)
          a(right) = temp
       end if
    end do

    if (left == right) then
       marker = left + 1
    else
       marker = left
    end if
  end subroutine partition

end module qsort_module
```


The module containing the custom comparators.


```fortran
module Order_By_XY
  use Points_Module
  implicit none
contains
  integer function order_by_x(aa, bb)
    type(point), intent(in) :: aa, bb

    if ( aa%x < bb%x ) then
       order_by_x = -1
    elseif (aa%x > bb%x) then
       order_by_x = 1
    else
       order_by_x = 0
    end if
  end function order_by_x

  integer function order_by_y(aa, bb)
    type(point), intent(in) :: aa, bb

    if ( aa%y < bb%y ) then
       order_by_y = -1
    elseif (aa%y > bb%y) then
       order_by_y = 1
    else
       order_by_y = 0
    end if    
  end function order_by_y
end module Order_By_XY
```


The '''closest pair''' functions' module.


```fortran
module ClosestPair
  use Points_Module
  use Order_By_XY
  use qsort_module
  implicit none

  private :: closest_pair_real

contains

  function closest_pair_simple(p, pair) result(distance)
    type(point), dimension(:), intent(in) :: p
    type(point), dimension(:), intent(out), optional :: pair
    real :: distance

    type(point), dimension(2) :: cp
    integer :: i, j
    real :: d

    if ( size(P) < 2 ) then
       distance = huge(0.0)
    else
       distance = len(p(1) - p(2))
       cp = (/ p(1), p(2) /)
       do i = 1, size(p) - 1
          do j = i+1, size(p)
             d = len(p(i) - p(j))
             if ( d < distance ) then
                distance = d
                cp = (/ p(i), p(j) /)
             end if
          end do
       end do
       if ( present(pair) ) pair = cp
    end if
  end function closest_pair_simple


  function closest_pair(p, pair) result(distance)
    type(point), dimension(:), intent(in) :: p
    type(point), dimension(:), intent(out), optional :: pair
    real :: distance

    type(point), dimension(2) :: dp
    type(point), dimension(size(p)) :: xp, yp
    integer :: i

    xp = p
    yp = p

    call qsort(xp, order_by_x)
    call qsort(yp, order_by_y)

    distance = closest_pair_real(xp, yp, dp)
    if ( present(pair) ) pair = dp
  end function closest_pair


  recursive function closest_pair_real(xp, yp, pair) result(distance)
    type(point), dimension(:), intent(in) :: xp, yp
    type(point), dimension(:), intent(out) :: pair
    real :: distance

    type(point), dimension(2) :: pairl, pairr
    type(point), dimension(:), allocatable :: ys
    type(point), dimension(:), allocatable :: pl, yl
    type(point), dimension(:), allocatable :: pr, yr
    real :: dl, dr, dmin, xm, d
    integer :: ns, i, k, j, midx

    if ( size(xp) <= 3 ) then
       distance = closest_pair_simple(xp, pair)
    else
       midx = ceiling(size(xp)/2.0)

       allocate(ys(size(xp)))
       allocate(pl(midx), yl(midx))
       allocate(pr(size(xp)-midx), yr(size(xp)-midx))

       pl = xp(1:midx)
       pr = xp((midx+1):size(xp))

       xm = xp(midx)%x

       k = 1; j = 1
       do i = 1, size(yp)
          if ( yp(i)%x > xm ) then
             ! guard ring; this is an "idiosyncrasy" that should be fixed in a
             ! smarter way
             if ( k <= size(yr) ) yr(k) = yp(i)
             k = k + 1
          else
             ! guard ring (see above)
             if ( j <= size(yl) ) yl(j) = yp(i)
             j = j + 1
          end if
       end do

       dl = closest_pair_real(pl, yl, pairl)
       dr = closest_pair_real(pr, yr, pairr)

       pair = pairr
       dmin = dr
       if ( dl < dr ) then
          pair = pairl
          dmin = dl
       end if

       ns = 0
       do i = 1, size(yp)
          if ( abs(yp(i)%x - xm) < dmin ) then
             ns = ns + 1
             ys(ns) = yp(i)
          end if
       end do

       distance = dmin
       do i = 1, ns-1
          k = i + 1
          do while ( k <= ns .and. abs(ys(k)%y - ys(i)%y) < dmin )
             d = len(ys(k) - ys(i))
             if ( d < distance ) then
                distance = d
                pair = (/ ys(k), ys(i) /)
             end if
             k = k + 1
          end do
       end do
       deallocate(ys)
       deallocate(pl, yl)
       deallocate(pr, yr)
    end if
  end function closest_pair_real

end module ClosestPair
```


Testing:


```fortran
program TestClosestPair
  use ClosestPair
  implicit none

  integer, parameter :: n = 10000

  integer :: i
  real :: x, y
  type(point), dimension(n) :: points

  type(point), dimension(2) :: p
  real :: dp, dr

  ! init the random generator here if needed

  do i = 1, n
     call random_number(x)
     call random_number(y)
     points(i) = point(x*20.0-10.0, y*20.0-10.0)
  end do

  dp = closest_pair_simple(points, p)
  print *, "sim ", dp
  dr = closest_pair(points, p)
  print *, "rec ", dr
end program TestClosestPair
```


<tt>Time</tt> gave 2.92user 0.00system 0:02.94elapsed for brute force, and 0.02user 0.00system 0:00.03elapsed for the other one.
