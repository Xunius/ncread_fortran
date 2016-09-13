! --------------------------------------------------------
! File:        nccreate
! Author:      Guangzhi XU
! Email:       xugzhi1987@gmail.com
! Description: Tools to faciliate creating dimensions, attributes and variables
!              
!              # Usage
!              1. Create a dimension obj from name and value:
!              ```
!              type(nc_att) :: dim
!              dim=createDim("time",value,.true.)
!              ```
!              where: `value` is a 1-d array of the dimension values.
!              Give the 3rd optional `.true.` argument if the dimension is unlimited length
!              
!              2. Add attribute to a nc_dim or nc_var obj:
!              ```
!              type(nc_att) :: dim
!              type(nc_var) :: var
!              call addAtt(dim,"units","meter")
!              call addAtt(var,"long_name","precipitation")
!              ```
!
!              3. Set a list of axis to a nc_var:
!              ```
!              type(nc_dim), dimension(2) :: axislist
!              type(nc_dim) :: dim1, dim2
!              type(nc_var) :: var
!              dim1=createDim("lon",lons)
!              dim2=createDim("lat",lats)
!              call setAxisList(var,(/dim1,dim2/))
!
!              4. Create a nc_var obj from name and data:
!              ```
!              type(nc_var) :: var
!              var=createVar("pre",data)
!              ```
!              Interface functions covers data ranks from 1 to 7 
!
!              5. Write dimension to netcdf file:
!              ```
!              type(nc_dim) :: dim1
!              dim1=createDim("lon",lons)
!              call check(nf90_create(FILE,nf90_clobber,ncid))
!              call writeDim(ncid,dim1)
!              ```
!
!              6. Write variable to netcdf file:
!              ```
!              type(nc_var) :: var
!              call check(nf90_create(FILE,nf90_clobber,ncid))
!              call writeVar(ncid,var)
!              ```
! --------------------------------------------------------

module nccreate
use ncread
use netcdf
implicit none

private
public :: createDim, addAtt, setAxisList, createVar, writeDim, writeVar


!-----Add attributes to nc_dim or nc_var objs-----
interface addAtt
    module procedure addAtt_var_str
    module procedure addAtt_var_int
    module procedure addAtt_var_real

    module procedure addAtt_dim_str
    module procedure addAtt_dim_int
    module procedure addAtt_dim_real
end interface addAtt

!--------Create variable of differen ranks--------
interface createVar
    module procedure createVar_1d
    module procedure createVar_2d
    module procedure createVar_3d
    module procedure createVar_4d
    module procedure createVar_5d
    module procedure createVar_6d
    module procedure createVar_7d
end interface

contains

    !---------------Create a nc_dim obj---------------
    function createDim(name,value,istime) result(dim)
    ! Create a nc_dim obj
        implicit none
        character(len=*), intent(in) :: name
        real, dimension(:), intent(in) :: value
        logical, intent(in), optional :: istime
        type(nc_dim) :: dim

        integer :: len

        !----------------Get dimension size----------------
        if (present(istime)) then
            if (istime .eqv. .true.) then
                len=nf90_unlimited
                dim%istime=.true.
            else
                len=size(value)
                dim%istime=.false.
            end if
        else
            len=size(value)
        end if

        !-------------Populate dim obj fields-------------
        dim%id=0           ! default
        dim%name=name
        dim%len=len
        dim%natts=0
        dim%data=value

        !--------------------Print info--------------------
        write(*,*) 
        write(*,*) " # <nccreate>: Create new dimension:"
        write(*,"(A,A,4x,A,i10)") "New dimension name = ", name, "Length = ", len
        if (dim%istime .eqv. .true.) then
            write(*,*) "Unlimited length dimension"
        end if
        write(*,*) "Values = "
        if (size(value)<=20) then
            write(*,*) value
        else
            write(*,*) value(1:5), " ... ", value(size(value)-5:size(value))
        end if

    end function createDim


    !------------Add new attribute to ncvar, str------------
    subroutine addAtt_var_str(var,name,value)
    ! Add new attribute to ncvar
        implicit none
        type(nc_var), intent(inout) :: var
        character(len=*), intent(in) :: name
        character(len=*), intent(in) :: value

        type(nc_att) :: att
        type(nc_att), allocatable, dimension(:) :: att_list   ! new extended att list
        integer :: i, len

        !------------Get attributes from nc_var------------
        if (allocated(var%atts)) then
            len=size(var%atts)
            allocate(att_list(len+1))
            do i=1,len
                att_list(i)=var%atts(i)
            end do
        else
            len=0
            allocate(att_list(len+1))
        end if

        !----------------Add new attribute----------------
        att%name=name
        call set_value(att,value)
        att_list(len+1)=att

        var%atts=att_list
        var%natts=var%natts+1

        write(*,*) 
        write(*,*) " # <nccreate>: Add new attribute to nc_var:"
        write(*,"(A,A,4x,A,A)") "New attribute name = ", name, "Value = ", att%value
        write(*,*) "New attribute list size = ", size(var%atts)

    end subroutine addAtt_var_str

    !------------Add new attribute to ncvar, int------------
    subroutine addAtt_var_int(var,name,value)
    ! Add new attribute to ncvar
        implicit none
        type(nc_var), intent(inout) :: var
        character(len=*), intent(in) :: name
        integer, intent(in) :: value

        type(nc_att) :: att
        type(nc_att), allocatable, dimension(:) :: att_list
        integer :: i, len

        !------------Get attributes from nc_var------------
        if (allocated(var%atts)) then
            len=size(var%atts)
            allocate(att_list(len+1))
            do i=1,len
                att_list(i)=var%atts(i)
            end do
        else
            len=0
            allocate(att_list(len+1))
        end if

        !----------------Add new attribute----------------
        att%name=name
        call set_value(att,value)
        att_list(len+1)=att

        var%atts=att_list
        var%natts=var%natts+1

        write(*,*) 
        write(*,*) " # <nccreate>: Add new attribute to nc_var:"
        write(*,"(A,A,4x,A,A)") "New attribute name = ", name, "Value = ", att%value
        write(*,*) "New attribute list size = ", size(var%atts)

    end subroutine addAtt_var_int

    !------------Add new attribute to ncvar, real------------
    subroutine addAtt_var_real(var,name,value)
    ! Add new attribute to ncvar
        implicit none
        type(nc_var), intent(inout) :: var
        character(len=*), intent(in) :: name
        real, intent(in) :: value

        type(nc_att) :: att
        type(nc_att), allocatable, dimension(:) :: att_list
        integer :: i, len

        !------------Get attributes from nc_var------------
        if (allocated(var%atts)) then
            len=size(var%atts)
            allocate(att_list(len+1))
            do i=1,len
                att_list(i)=var%atts(i)
            end do
        else
            len=0
            allocate(att_list(len+1))
        end if

        !----------------Add new attribute----------------
        att%name=name
        call set_value(att,value)
        att_list(len+1)=att

        var%atts=att_list
        var%natts=var%natts+1

        write(*,*) 
        write(*,*) " # <nccreate>: Add new attribute to nc_var:"
        write(*,"(A,A,4x,A,A)") "New attribute name = ", name, "Value = ", att%value
        write(*,*) "New attribute list size = ", size(var%atts)

    end subroutine addAtt_var_real
    

    !------------Add new attribute to ncdim, str------------
    subroutine addAtt_dim_str(dim,name,value)
    ! Add new attribute to ncdim
        implicit none
        type(nc_dim), intent(inout) :: dim
        character(len=*), intent(in) :: name
        character(len=*), intent(in) :: value

        type(nc_att) :: att
        type(nc_att), allocatable, dimension(:) :: att_list
        integer :: i, len

        !------------Get attributes from nc_var------------
        if (allocated(dim%atts)) then
            len=size(dim%atts)
            allocate(att_list(len+1))
            do i=1,len
                att_list(i)=dim%atts(i)
            end do
        else
            len=0
            allocate(att_list(len+1))
        end if

        !----------------Add new attribute----------------
        att%name=name
        call set_value(att,value)
        att_list(len+1)=att

        dim%atts=att_list
        dim%natts=dim%natts+1

        write(*,*) 
        write(*,*) " # <nccreate>: Add new attribute to nc_dim:"
        write(*,"(A,A,4x,A,A)") "New attribute name = ", name, "Value = ", att%value
        write(*,*) "New attribute list size = ", size(dim%atts)

    end subroutine addAtt_dim_str


    !------------Add new attribute to ncdim, int------------
    subroutine addAtt_dim_int(dim,name,value)
    ! Add new attribute to ncdim
        implicit none
        type(nc_dim), intent(inout) :: dim
        character(len=*), intent(in) :: name
        integer, intent(in) :: value

        type(nc_att) :: att
        type(nc_att), allocatable, dimension(:) :: att_list
        integer :: i, len

        !------------Get attributes from nc_var------------
        if (allocated(dim%atts)) then
            len=size(dim%atts)
            allocate(att_list(len+1))
            do i=1,len
                att_list(i)=dim%atts(i)
            end do
        else
            len=0
            allocate(att_list(len+1))
        end if

        !----------------Add new attribute----------------
        att%name=name
        call set_value(att,value)
        att_list(len+1)=att

        dim%atts=att_list
        dim%natts=dim%natts+1

        write(*,*) 
        write(*,*) " # <nccreate>: Add new attribute to nc_dim:"
        write(*,"(A,A,4x,A,A)") "New attribute name = ", name, "Value = ", att%value
        write(*,*) "New attribute list size = ", size(dim%atts)

    end subroutine addAtt_dim_int


    !------------Add new attribute to ncdim, real------------
    subroutine addAtt_dim_real(dim,name,value)
    ! Add new attribute to ncdim
        implicit none
        type(nc_dim), intent(inout) :: dim
        character(len=*), intent(in) :: name
        real, intent(in) :: value

        type(nc_att) :: att
        type(nc_att), allocatable, dimension(:) :: att_list
        integer :: i, len

        !------------Get attributes from nc_var------------
        if (allocated(dim%atts)) then
            len=size(dim%atts)
            allocate(att_list(len+1))
            do i=1,len
                att_list(i)=dim%atts(i)
            end do
        else
            len=0
            allocate(att_list(len+1))
        end if

        !----------------Add new attribute----------------
        att%name=name
        call set_value(att,value)
        att_list(len+1)=att

        dim%atts=att_list
        dim%natts=dim%natts+1

        write(*,*) 
        write(*,*) " # <nccreate>: Add new attribute to nc_dim:"
        write(*,"(A,A,4x,A,A)") "New attribute name = ", name, "Value = ", att%value
        write(*,*) "New attribute list size = ", size(dim%atts)

    end subroutine addAtt_dim_real


    !-----------Set axislist for a variable-----------
    subroutine setAxisList(var,axislist)
    ! Set axislist for a variable
        implicit none
        type(nc_var), intent(inout) :: var
        type(nc_dim), dimension(:), intent(inout) :: axislist

        integer :: i
        integer, dimension(:), allocatable :: varshape

        if (size(axislist) /= var%ndims) then
            write(*,*) " # <nccreate>: Variable rank not match, return."
            return
        else
            varshape=getShape(var)
            do i=1,var%ndims
                if (varshape(i)/=axislist(i)%len) then
                    write(*,*) " # <nccreate>: Variable shape not match, return."
                    return
                end if
                axislist(i)%id=i
            end do
            var%axislist=axislist
            write(*,*) " # <nccreate>: Set axislist to variable"
        end if

            

    end subroutine setAxisList

    !--------Create a new nc_var obj from data--------
    function createVar_1d(name,data,axislist) result(ncvar)
    ! Create a new nc_var obj from data
        implicit none
        character(len=*), intent(in) :: name
        real, dimension(:), intent(in) :: data
        type(nc_dim), allocatable, dimension(:), intent(inout), optional :: axislist

        type(nc_var) :: ncvar

        ncvar%id=0          ! default
        ncvar%name=trim(name)
        ncvar%data1=data
        ncvar%ndims=1
        ncvar%natts=0
        ncvar%shape=shape(data)

        if (present(axislist)) then
            call setAxisList(ncvar,axislist)
        end if
        
        write(*,*) 
        write(*,*) "# <nccreate>: Create new nc_var obj:"
        write(*,*) "Data is referred as 'ncvar%data1'."

    end function createVar_1d


    !--------Create a new nc_var obj from data--------
    function createVar_2d(name,data,axislist) result(ncvar)
    ! Create a new nc_var obj from data
        implicit none
        character(len=*), intent(in) :: name
        real, dimension(:,:), intent(in) :: data
        type(nc_dim), allocatable, dimension(:), intent(inout), optional :: axislist

        type(nc_var) :: ncvar

        ncvar%id=0          ! default
        ncvar%name=trim(name)
        ncvar%data2=data
        ncvar%ndims=2
        ncvar%natts=0
        ncvar%shape=shape(data)

        if (present(axislist)) then
            call setAxisList(ncvar,axislist)
        end if
        
        write(*,*) 
        write(*,*) "# <nccreate>: Create new nc_var obj:"
        write(*,*) "Data is referred as 'ncvar%data2'."

    end function createVar_2d

    !--------Create a new nc_var obj from data--------
    function createVar_3d(name,data,axislist) result(ncvar)
    ! Create a new nc_var obj from data
        implicit none
        character(len=*), intent(in) :: name
        real, dimension(:,:,:), intent(in) :: data
        type(nc_dim), allocatable, dimension(:), intent(inout), optional :: axislist

        type(nc_var) :: ncvar

        ncvar%id=0          ! default
        ncvar%name=trim(name)
        ncvar%data3=data
        ncvar%ndims=3
        ncvar%natts=0
        ncvar%shape=shape(data)

        if (present(axislist)) then
            call setAxisList(ncvar,axislist)
        end if
        
        write(*,*) 
        write(*,*) "# <nccreate>: Create new nc_var obj:"
        write(*,*) "Data is referred as 'ncvar%data3'."

    end function createVar_3d


    !--------Create a new nc_var obj from data--------
    function createVar_4d(name,data,axislist) result(ncvar)
    ! Create a new nc_var obj from data
        implicit none
        character(len=*), intent(in) :: name
        real, dimension(:,:,:,:), intent(in) :: data
        type(nc_dim), allocatable, dimension(:), intent(inout), optional :: axislist

        type(nc_var) :: ncvar

        ncvar%id=0          ! default
        ncvar%name=trim(name)
        ncvar%data4=data
        ncvar%ndims=4
        ncvar%natts=0
        ncvar%shape=shape(data)

        if (present(axislist)) then
            call setAxisList(ncvar,axislist)
        end if
        
        write(*,*) 
        write(*,*) "# <nccreate>: Create new nc_var obj:"
        write(*,*) "Data is referred as 'ncvar%data4'."

    end function createVar_4d


    !--------Create a new nc_var obj from data--------
    function createVar_5d(name,data,axislist) result(ncvar)
    ! Create a new nc_var obj from data
        implicit none
        character(len=*), intent(in) :: name
        real, dimension(:,:,:,:,:), intent(in) :: data
        type(nc_dim), allocatable, dimension(:), intent(inout), optional :: axislist

        type(nc_var) :: ncvar

        ncvar%id=0          ! default
        ncvar%name=trim(name)
        ncvar%data5=data
        ncvar%ndims=5
        ncvar%natts=0
        ncvar%shape=shape(data)

        if (present(axislist)) then
            call setAxisList(ncvar,axislist)
        end if
        
        write(*,*) 
        write(*,*) "# <nccreate>: Create new nc_var obj:"
        write(*,*) "Data is referred as 'ncvar%data5'."

    end function createVar_5d

    !--------Create a new nc_var obj from data--------
    function createVar_6d(name,data,axislist) result(ncvar)
    ! Create a new nc_var obj from data
        implicit none
        character(len=*), intent(in) :: name
        real, dimension(:,:,:,:,:,:), intent(in) :: data
        type(nc_dim), allocatable, dimension(:), intent(inout), optional :: axislist

        type(nc_var) :: ncvar

        ncvar%id=0          ! default
        ncvar%name=trim(name)
        ncvar%data6=data
        ncvar%ndims=6
        ncvar%natts=0
        ncvar%shape=shape(data)

        if (present(axislist)) then
            call setAxisList(ncvar,axislist)
        end if
        
        write(*,*) 
        write(*,*) "# <nccreate>: Create new nc_var obj:"
        write(*,*) "Data is referred as 'ncvar%data6'."

    end function createVar_6d


    !--------Create a new nc_var obj from data--------
    function createVar_7d(name,data,axislist) result(ncvar)
    ! Create a new nc_var obj from data
        implicit none
        character(len=*), intent(in) :: name
        real, dimension(:,:,:,:,:,:,:), intent(in) :: data
        type(nc_dim), allocatable, dimension(:), intent(inout), optional :: axislist

        type(nc_var) :: ncvar

        ncvar%id=0          ! default
        ncvar%name=trim(name)
        ncvar%data7=data
        ncvar%ndims=7
        ncvar%natts=0
        ncvar%shape=shape(data)

        if (present(axislist)) then
            call setAxisList(ncvar,axislist)
        end if
        
        write(*,*) 
        write(*,*) "# <nccreate>: Create new nc_var obj:"
        write(*,*) "Data is referred as 'ncvar%data7'."

    end function createVar_7d


    !------------Write dimension to netcdf------------
    subroutine writeDim(ncid,dim)
    ! Write dimension to netcdf
        implicit none
        integer, intent(in) :: ncid
        type(nc_dim), intent(inout) :: dim

        integer :: len, dimid, natts, i,varid

        write(*,*) 
        write(*,*) "# <nccreate>: Writing dimension '", trim(dim%name), "' into netcdf: ", ncid, "..."

        !------------------Create dim------------------
        if (dim%istime .eqv. .true.) then
            len=nf90_unlimited
        else
            len=size(dim%data)
        end if

        !--------------------Get dim id--------------------
        call check(nf90_def_dim(ncid,dim%name,len,dimid))
        dim%id=dimid

        !-----------------Define variable-----------------
        call check(nf90_def_var(ncid,dim%name,nf90_float,dimid,varid))

        !-----------------Write attributes-----------------
        if (allocated(dim%atts)) then
            natts=size(dim%atts)
            do i=1,natts
                call check(nf90_put_att(ncid,varid,dim%atts(i)%name,dim%atts(i)%value))
            end do
        end if

        !---------Exit define mode and write data---------
        call check(nf90_enddef(ncid))
        call check(nf90_put_var(ncid,varid,dim%data))
        call check(nf90_redef(ncid))

        write(*,*) 
        write(*,*) "# <nccreate>: Dimension wrote into netcdf: ", ncid

    end subroutine writeDim


    !-------------Write variable to netcdf-------------
    subroutine writeVar(ncid,var)
    ! Write variable to netcdf
        implicit none
        integer, intent(in) :: ncid
        type(nc_var), intent(inout) :: var

        integer :: i, natts, varid, ndims
        integer, allocatable, dimension(:) :: dimids

        varid=var%id
        ndims=var%ndims
        natts=var%natts

        write(*,*) 
        write(*,*) "# <nccreate>: Writing variable '", trim(var%name), "' into netcdf: ", ncid, "..."

        !--------------------Get dimids--------------------
        if (allocated(var%axislist) .eqv. .false.) then
            write(*,*) "# <nccreate>: Variable has no axis list. Return."
            return
        end if

        allocate(dimids(ndims))
        do i=1,ndims
            dimids(i)=var%axislist(i)%id
            call writeDim(ncid, var%axislist(i))
        end do

        !--------------------Write variable----------------
        call check(nf90_def_var(ncid,var%name,nf90_float,dimids,varid))

        !-----------------Write attributes-----------------
        write(*,*) 
        write(*,*) "# <nccreate>: Writing variable attributes into netcdf: ", ncid, "..."
        if (allocated(var%atts) .eqv. .true.) then
            do i=1,natts
                call check(nf90_put_att(ncid,varid,var%atts(i)%name,var%atts(i)%value))
            end do
        end if

        !--------------------Write data--------------------
        call check(nf90_enddef(ncid))

        write(*,*) 
        write(*,*) "# <nccreate>: Writing variable data into netcdf: ", ncid, "..."
        if (ndims==1) then
            call check(nf90_put_var(ncid,varid,var%data1))
        else if (ndims==2) then
            call check(nf90_put_var(ncid,varid,var%data2))
        else if (ndims==3) then
            call check(nf90_put_var(ncid,varid,var%data3))
        else if (ndims==4) then
            call check(nf90_put_var(ncid,varid,var%data4))
        else if (ndims==5) then
            call check(nf90_put_var(ncid,varid,var%data5))
        else if (ndims==6) then
            call check(nf90_put_var(ncid,varid,var%data6))
        else if (ndims==7) then
            call check(nf90_put_var(ncid,varid,var%data7))
        end if
        call check(nf90_redef(ncid))
        
        write(*,*) 
        write(*,*) "# <nccreate>: Variable wrote into netcdf: ", ncid

        

    end subroutine writeVar




end module nccreate






program main
use netcdf
use nccreate
use ncread
implicit none

character(len=*), parameter :: FILE_OUT="testout.nc"
integer :: ncid, i
real, allocatable, dimension(:) :: dimvalue
real, allocatable, dimension(:,:) :: dummy
type(nc_dim) :: dim1, dim2
type(nc_dim), dimension(2) :: axislist
type(nc_var) :: var


!----------------Create dimensions----------------
allocate(dimvalue(5))
dimvalue=(/1.0, 2.0, 3.0, 4.0, 5.0/)

dim1=createDim("dim1",(/10.,20./))
dim2=createDim("dim2",dimvalue)
dim1%id=1
dim2%id=2

call addAtt(dim1,"id",1)
call addAtt(dim2,"id",2)

call addAtt(dim1,"units","db")
call addAtt(dim2,"units","m")

!--------------Create dummy variable--------------
allocate(dummy(2,5))
dummy=reshape((/1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0/),(/2,5/))

var=createVar("dummy",dummy)
axislist=(/dim1,dim2/)
var%id=1

call setAxisList(var,axislist)

!------------------Add attributes------------------
call addAtt(var,"long_name", "a dummy variable")
call addAtt(var,"units", "stupidity")

!-------------Write to new netcdf file-------------
call check(nf90_create(FILE_OUT,nf90_clobber,ncid))
write(*,*) "ncid:", ncid

call writeVar(ncid,var)
call check(nf90_close(ncid))






end program main
