+++
title = "Bitmap/Fortran"
description = ""
date = 2011-03-17T01:25:21Z
aliases = []
[extra]
id = 5307
[taxonomies]
categories = []
tags = []
+++

{{collection|Basic bitmap storage}}

{{works with|Fortran|90 and later}}

```fortran
module RCImageBasic
  implicit none

  type rgbimage
     integer, dimension(:,:), pointer :: red, green, blue
     integer :: width, height
  end type rgbimage

  type rgb
     integer :: red, green, blue
  end type rgb

  interface operator (==)
     module procedure rgbequal
  end interface

  interface operator (.dist.)
     module procedure colordistance
  end interface

contains

  subroutine init_img(img)
    type(rgbimage), intent(out) :: img
    nullify(img%red)
    nullify(img%green)
    nullify(img%blue)
    img%width = 0
    img%height = 0
  end subroutine init_img

  subroutine set_color(color, red, green, blue)
    type(rgb), intent(out) :: color
    integer, intent(in) :: red, green, blue
    if ( red > 255 ) then
       color%red = 255
    elseif ( red < 0 ) then
       color%red = 0
    else
       color%red = red
    end if
    if ( green > 255 ) then
       color%green = 255
    elseif ( green < 0 ) then
       color%green = 0
    else
       color%green = green
    end if
    if ( blue > 255 ) then
       color%blue = 255
    elseif ( blue < 0 ) then
       color%blue = 0
    else
       color%blue = blue
    end if
  end subroutine set_color

  function colordistance(c1, c2) result(res)
    real :: res
    type(rgb), intent(in) :: c1, c2
    res = sqrt( real(c1%red - c2%red)**2 + real(c1%green - c2%green)**2 + &
                real(c1%blue - c2%blue)**2 ) / ( 256.0*sqrt(3.0) )
  end function colordistance

  function rgbequal(c1, c2)
    logical :: rgbequal
    type(rgb), intent(in) :: c1, c2
    rgbequal = .true.
    if ( (c1%red == c2%red) .and. (c1%green == c2%green) .and. &
         (c1%blue == c2%blue) ) return
    rgbequal = .false.
  end function rgbequal

  function inside_image(img, x, y) result(r)
    logical :: r
    type(rgbimage), intent(in) :: img
    integer, intent(in) :: x, y

    r = .false.
    if ( (x < img%width) .and. ( y < img%height ) .and. &
         (x >= 0 ) .and. ( y >= 0 ) ) then
       r = .true.
    end if
  end function inside_image

  function valid_image(img) result(r)
    logical :: r
    type(rgbimage) :: img

    r = .false.
    if ( img%width == 0 ) return
    if ( img%height == 0 ) return
    if ( .not. associated(img%red) .and. .not. associated(img%green) .and. &
         .not. associated(img%blue) ) return
    r = .true.
  end function valid_image

  subroutine normalize_img(img)
    type(rgbimage), intent(inout) :: img

    where ( img%red > 255 )
       img%red = 255
    elsewhere ( img%red < 0 )
       img%red = 0
    end where
    where ( img%green > 255 )
       img%green = 255
    elsewhere ( img%green < 0 )
       img%green = 0
    end where
    where ( img%blue > 255 )
       img%blue = 255
    elsewhere ( img%blue < 0 )
       img%blue = 0
    end where
  end subroutine normalize_img

  subroutine alloc_img(img, w, h)
    type(rgbimage) :: img
    integer, intent(in) :: w, h

    allocate(img%red(w, h))
    allocate(img%green(w, h))
    allocate(img%blue(w, h))
    img%width = w
    img%height = h
  end subroutine alloc_img

  subroutine free_img(img)
    type(rgbimage) :: img

    if ( associated(img%red) ) deallocate(img%red)
    if ( associated(img%green) ) deallocate(img%green)
    if ( associated(img%blue) ) deallocate(img%blue)
  end subroutine free_img

  subroutine fill_img(img, color)
    type(rgbimage), intent(inout) :: img
    type(rgb), intent(in) :: color

    if ( valid_image(img)  ) then
       img%red = mod(abs(color%red), 256)
       img%green = mod(abs(color%green), 256)
       img%blue = mod(abs(color%blue), 256)
    end if
  end subroutine fill_img
  
  subroutine put_pixel(img, x, y, color)
    type(rgbimage), intent(inout) :: img
    integer, intent(in) :: x, y
    type(rgb), intent(in) :: color

    if ( inside_image(img, x, y) .and. valid_image(img)) then
       img%red(x+1,y+1) = mod(abs(color%red), 256)
       img%green(x+1, y+1) = mod(abs(color%green), 256)
       img%blue(x+1, y+1) = mod(abs(color%blue), 256)
    end if
  end subroutine put_pixel

  subroutine get_pixel(img, x, y, color)
    type(rgbimage), intent(in) :: img
    integer, intent(in) :: x, y
    type(rgb), intent(out) :: color

    if ( inside_image(img, x, y) .and. valid_image(img)) then
       color%red = img%red(x+1, y+1)
       color%green = img%green(x+1, y+1)
       color%blue = img%blue(x+1, y+1)
    else
       color%red = 0
       color%green = 0
       color%blue = 0
    end if
  end subroutine get_pixel

end module RCImageBasic
```

