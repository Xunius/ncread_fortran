! --------------------------------------------------------
! File:        nccreate
! Author:      Guangzhi XU
! Email:       xugzhi1987@gmail.com
! Description: Tools to faciliate creating dimensions, attributes and variables
! --------------------------------------------------------

module nccreate
use ncread
use netcdf
implicit none


private
public :: createDim, addAtt, setAxisList, createVar


!---------------Set attribute values---------------
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
            else
                len=size(value)
            end if
        else
            len=size(value)
        end if

        !------------------Create dim------------------
        !call check(nf90_redef(ncid))
        !call check(nf90_def_dim(ncid,name,len,dimid))
        !call check(nf90_enddef(ncid))

        !-------------Populate dim obj fields-------------
        !dim%id=dimid
        dim%len=len
        dim%name=name
        dim%natts=0
        dim%data=value

        !--------------------Print info--------------------
        write(*,*) 
        write(*,*) " # <nccreate>: Create new dimension:"
        write(*,"(A,A,4x,A,i10)") "New dimension name = ", name, "Length = ", len
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
        type(nc_dim), dimension(:), intent(in) :: axislist

        if (size(axislist) /= var%ndims) then
            write(*,*) " # <nccreate>: Variable shape not match, return."
            return
        else
            var%axislist=axislist
            write(*,*) " # <nccreate>: Set axislist to variable"
        end if


    end subroutine setAxisList

    !--------Create a new nc_var obj from data--------
    function createVar_1d(name,data) result(ncvar)
    ! Create a new nc_var obj from data
        implicit none
        character(len=*), intent(in) :: name
        real, dimension(:), intent(in) :: data

        type(nc_var) :: ncvar

        ncvar%name=trim(name)
        ncvar%data1=data
        ncvar%ndims=1
        
        write(*,*) 
        write(*,*) "# <nccreate>: Create new nc_var obj:"
        write(*,*) "Data is referred as 'ncvar%data1'."

    end function createVar_1d


    !--------Create a new nc_var obj from data--------
    function createVar_2d(name,data) result(ncvar)
    ! Create a new nc_var obj from data
        implicit none
        character(len=*), intent(in) :: name
        real, dimension(:,:), intent(in) :: data

        type(nc_var) :: ncvar

        ncvar%name=trim(name)
        ncvar%data2=data
        ncvar%ndims=2
        
        write(*,*) 
        write(*,*) "# <nccreate>: Create new nc_var obj:"
        write(*,*) "Data is referred as 'ncvar%data2'."

    end function createVar_2d

    !--------Create a new nc_var obj from data--------
    function createVar_3d(name,data) result(ncvar)
    ! Create a new nc_var obj from data
        implicit none
        character(len=*), intent(in) :: name
        real, dimension(:,:,:), intent(in) :: data

        type(nc_var) :: ncvar

        ncvar%name=trim(name)
        ncvar%data3=data
        ncvar%ndims=3
        
        write(*,*) 
        write(*,*) "# <nccreate>: Create new nc_var obj:"
        write(*,*) "Data is referred as 'ncvar%data3'."

    end function createVar_3d


    !--------Create a new nc_var obj from data--------
    function createVar_4d(name,data) result(ncvar)
    ! Create a new nc_var obj from data
        implicit none
        character(len=*), intent(in) :: name
        real, dimension(:,:,:,:), intent(in) :: data

        type(nc_var) :: ncvar

        ncvar%name=trim(name)
        ncvar%data4=data
        ncvar%ndims=4
        
        write(*,*) 
        write(*,*) "# <nccreate>: Create new nc_var obj:"
        write(*,*) "Data is referred as 'ncvar%data4'."

    end function createVar_4d


    !--------Create a new nc_var obj from data--------
    function createVar_5d(name,data) result(ncvar)
    ! Create a new nc_var obj from data
        implicit none
        character(len=*), intent(in) :: name
        real, dimension(:,:,:,:,:), intent(in) :: data

        type(nc_var) :: ncvar

        ncvar%name=trim(name)
        ncvar%data5=data
        ncvar%ndims=5
        
        write(*,*) 
        write(*,*) "# <nccreate>: Create new nc_var obj:"
        write(*,*) "Data is referred as 'ncvar%data5'."

    end function createVar_5d

    !--------Create a new nc_var obj from data--------
    function createVar_6d(name,data) result(ncvar)
    ! Create a new nc_var obj from data
        implicit none
        character(len=*), intent(in) :: name
        real, dimension(:,:,:,:,:,:), intent(in) :: data

        type(nc_var) :: ncvar

        ncvar%name=trim(name)
        ncvar%data6=data
        ncvar%ndims=6
        
        write(*,*) 
        write(*,*) "# <nccreate>: Create new nc_var obj:"
        write(*,*) "Data is referred as 'ncvar%data6'."

    end function createVar_6d


    !--------Create a new nc_var obj from data--------
    function createVar_7d(name,data) result(ncvar)
    ! Create a new nc_var obj from data
        implicit none
        character(len=*), intent(in) :: name
        real, dimension(:,:,:,:,:,:,:), intent(in) :: data

        type(nc_var) :: ncvar

        ncvar%name=trim(name)
        ncvar%data7=data
        ncvar%ndims=7

        write(*,*) 
        write(*,*) "# <nccreate>: Create new nc_var obj:"
        write(*,*) "Data is referred as 'ncvar%data7'."
        
    end function createVar_7d

end module nccreate


program main
use netcdf
use nccreate
use ncread
implicit none

character(len=*), parameter :: FILE_NAME="pre_s_m_2000_ori-preprocessed.nc"
integer :: ncid, i
real, allocatable, dimension(:) :: dimvalue
real, allocatable, dimension(:,:) :: dummy
type(nc_dim) :: dim1, dim2
type(nc_dim), dimension(2) :: axislist
type(nc_var) :: var
type(nc_att) :: att

character(len=99), allocatable,dimension(:) :: varlist

!--------------------Open file--------------------
call check(nf90_open(FILE_NAME,nf90_write,ncid))
write(*,*) "ncid:", ncid

!write(*,*) att%name, att%value
!if (allocated(dim%atts)) then
    !write(*,*) dim%atts
!else
    !write(*,*) "NOT"
    
!end if
!varlist=listVariables(ncid)


allocate(dimvalue(5))
dimvalue=(/1.0, 2.0, 3.0, 4.0, 5.0/)

allocate(dummy(2,5))
dummy=reshape((/1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0/),(/2,5/))

write(*,*) "shape", shape(dummy)

var=createVar("dummy",dummy)

write(*,*) var%name, var%data2, var%natts

dim1=createDim("dim1",(/10.,20./))
dim2=createDim("dim2",dimvalue)

axislist=(/dim1,dim2/)

write(*,*) size(axislist), var%ndims

call setAxisList(var,axislist)

call addAtt(dim1,"id",1)
call addAtt(dim2,"id",2)

call addAtt(dim1,"units","db")
call addAtt(dim2,"units","m")

call addAtt(var,"long_name", "a dummy variable")
call addAtt(var,"units", "stupidity")


write(*,*) var%name, var%id, var%data2, var%natts

do i=1,var%natts
    write(*,*) var%atts(i)%name, var%atts(i)%value
end do


!write(*,*) dim%id, dim%name, dim%natts, dim%data, dim%atts

!var=getVar(ncid,"pre")

!call addAtt(var,"testatt","value!!!")
!call addAtt(var,"testatt2",100)
!call addAtt(var,"testatt2",200.5)

!do i=1,var%natts
    !write(*,"(A,4x,A)") trim(var%atts(i)%name), trim(var%atts(i)%value)
!end do

!write(*,*) 
!write(*,*) 
!dim=getAxis(ncid,5)

!call addAtt(dim,"testdimatt","value!!!!!!!")
!call addAtt(dim,"testdimatt",100)
!call addAtt(dim,"testdimatt",100.5)

!do i=1,dim%natts
    !write(*,"(A,4x,A)") trim(dim%atts(i)%name), trim(dim%atts(i)%value)
!end do



end program main
