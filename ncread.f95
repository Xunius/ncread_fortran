! --------------------------------------------------------
! File:        ncread
! Author:      Guangzhi XU
! Email:       xugzhi1987@gmail.com
! Description: Open and read data from netcdf file.
! --------------------------------------------------------
module ncread
use netcdf
implicit none


!------------------Attribute type------------------
type nc_att
    character(len=99) :: name
    character(len=99) :: value_s
    real :: value_r
    integer :: value_i
end type


!------------------dimension type------------------
type nc_dim
    integer :: id
    integer :: len
    integer :: natts
    character(len=30) :: name
    type(nc_att), allocatable, dimension(:) :: atts

    real, allocatable, dimension(:) :: data
end type


!-----------------nc variable type-----------------
type nc_var
    integer :: id
    integer :: ndims
    integer :: natts
    character(len=20) :: name
    integer, allocatable, dimension(:) :: shape
    type(nc_att), allocatable, dimension(:) :: atts
    type(nc_dim), allocatable, dimension(:) :: axislist

    real, allocatable, dimension(:) :: data1
    real, allocatable, dimension(:,:) :: data2
    real, allocatable, dimension(:,:,:) :: data3
    real, allocatable, dimension(:,:,:,:) :: data4
    real, allocatable, dimension(:,:,:,:,:) :: data5
    real, allocatable, dimension(:,:,:,:,:,:) :: data6
    real, allocatable, dimension(:,:,:,:,:,:,:) :: data7

end type


private

!-------------------General usage-------------------
integer :: status

!-------------------Public types-------------------
public :: nc_att, nc_dim, nc_var

!----------------Public procedures----------------
public :: check, listVariables, getAttributes, getAxis
public :: getAxisList, getShape, getVar


contains
    !-----------Check netcdf function calls-----------
    subroutine check(status)
        implicit none
        integer, intent (in) :: status
        if (status/=nf90_noerr) then
            write(*,*) trim(nf90_strerror(status))
            stop "# <ncread>: Stop at netCDF error."
        end if
    end subroutine check

    !----Convert int type nf90_ data type to string----
    function getDtype(xtype) result(dtype)
        ! Convert int type nf90_ data type to string
        implicit none
        integer, intent(in) :: xtype
        character(len=20) :: dtype
        if (xtype == 1) then
            dtype="NF90_BYTE"
        else if (xtype == 2) then
            dtype="NF90_CHAR"
        else if (xtype == 3) then
            dtype="NF90_SHORT"
        else if (xtype == 4) then
            dtype="NF90_INT"
        else if (xtype == 5) then
            dtype="NF90_FLOAT"
        else if (xtype == 6) then
            dtype="NF90_DOUBLE"
        end if
    end function getDtype



    !----------------List variable list----------------
    function listVariables(ncid) result(varlist)
    ! List variable list
        implicit none
        integer, intent(in) :: ncid
        character(len=99), allocatable, dimension(:) :: varlist  ! variable name list

        character(len=99) :: var_i        ! iterator variable name 
        integer :: ndims                  ! number of dimensions
        integer :: nvars                  ! number of variables
        integer :: i

        call check(nf90_inquire(ncid, ndims, nvars))
        allocate(varlist(nvars))

        write(*,*) 
        write(*,*) "# <ncread>: Get variable list in nc file:"
        do i=1,nvars
            call check(nf90_inquire_variable(ncid,i,var_i))
            varlist(i)=trim(var_i)
            write(*,"(A,A,i3,4x,A,A)") "Variable ", "ID = ", i, "Name = ", trim(varlist(i))
        end do

    end function listVariables


    !--------Get attributes of a variable (specified by name)--------
    function getAttributes(ncid,var) result(attlist)
    ! Get attributes of a variable
        implicit none
        integer, intent(in) :: ncid
        character(len=*), intent(in) :: var
        type(nc_att), allocatable, dimension(:) :: attlist  ! a list of nc_att objs

        type(nc_att) :: att_j                               ! iterator nc_att obj
        character(len=80) :: attname                        ! attribute name
        character(len=80) :: att_c                          ! attribute value, str form
        real :: att_r                                       ! attribute value, real form
        integer :: att_i                                    ! attribute value, int form

        integer :: varid, natts, i, xtype, attlen, maxlen
        character(len=30) :: format_str

        !---------------Get var id from name---------------
        call check(nf90_inq_varid(ncid,var,varid))

        !---------Get attribute number from var id---------
        call check(nf90_inquire_variable(ncid,varid,natts=natts))

        write(*,*) 
        write(*,*) "# <ncread>: Get attributes for variable '", var, "':"

        !-------------Loop through attributes-------------
        allocate(attlist(natts))

        !----------------Get format string----------------
        do i=1,natts
            call check(nf90_inq_attname(ncid,varid,i,attname))
            call check(nf90_inquire_attribute(ncid,varid,attname,xtype,attlen))
            if (i==1) then
                maxlen=len_trim(attname)
            else
                if (len_trim(attname)>maxlen) then
                    maxlen=len_trim(attname)
                end if
            end if
        end do

        !------------------Get attributes------------------
        do i=1,natts
            call check(nf90_inq_attname(ncid,varid,i,attname))
            call check(nf90_inquire_attribute(ncid,varid,attname,xtype,attlen))
            att_j%name=attname

            !-------------Store str form of value-------------
            if (xtype==nf90_char) then
                call check(nf90_get_att(ncid,varid,attname,att_c))
                att_j%value_s=att_c
                write (format_str, "('(A10,i3,2x,A',i2,',A,A)')") maxlen
                write(*,format_str) "Attribute", i, trim(attname), " = ", trim(att_c)
            !-------------Store int form of value-------------
            else if (xtype==nf90_int) then
                call check(nf90_get_att(ncid,varid,attname,att_i))
                att_j%value_i=att_i
                write (format_str, "('(A10,i3,2x,A',i3,',A,i20)')") maxlen
                write(*,format_str) "Attribute", i, trim(attname), " = ", att_i
            !-------------Store real form of value-------------
            else if (xtype==nf90_float) then
                call check(nf90_get_att(ncid,varid,attname,att_r))
                att_j%value_r=att_r
                write (format_str, "('(A10,i3,2x,A',i3,',A,f20.5)')") maxlen
                write(*,format_str) "Attribute", i, trim(attname), " = ", att_r
            end if

            attlist(i)=att_j
        end do

        
    end function getAttributes




    !---------------Get an axis by index---------------
    function getAxis(ncid,id) result(axis)
    ! Get an axis by index
    ! NOTE that the axis id is one in the array (dimids)
    ! returned by nf90_inquire_variable()

        implicit none
        integer, intent(in) :: ncid
        integer, intent(in) :: id
        type(nc_dim) :: axis                         ! nc_dim obj

        character(len=99) :: dimname                 ! axis name
        real, allocatable, dimension(:) :: values    ! axis values

        type(nc_att), allocatable, dimension(:) :: attlist  ! a list of nc_att objs
        integer :: i, dimlen, dimid 

        !---------------Get axis name by id---------------
        call check(nf90_inquire_dimension(ncid,id,dimname,dimlen))

        !-----------Get axis variable id by name-----------
        call check(nf90_inq_varid(ncid,dimname,dimid))

        !---------------Allocate value array---------------
        allocate(values(dimlen))
        call check(nf90_get_var(ncid,dimid,values))

        !--------------------Print info--------------------
        write(*,*) 
        write(*,"(A,i3,4x,A,A,A,i5)") " Axis ", id, "  Name = ", trim(dimname), "  Length = ", dimlen
        write(*,*) "    Axis values: "

        if (dimlen<=20) then
            write(*,*) values
        else
            write(*,*) values(1:5), " ... ", values(dimlen-5:dimlen)
        end if

        !---------------Get axis attributes---------------
        attlist=getAttributes(ncid,trim(dimname))

        !--------------Populate nc_dim fields--------------
        axis%id=id
        axis%len=dimlen
        axis%name=trim(dimname)
        axis%natts=size(attlist)
        axis%atts=attlist
        axis%data=values

    end function getAxis




    !----Get a list of axes of a variable by name----
    function getAxisList(ncid,var) result(axislist)
    ! Get a list of axes of a variable by name
        implicit none
        integer, intent(in) :: ncid
        character(len=*), intent(in) :: var                  ! variable name
        type(nc_dim), allocatable, dimension(:) :: axislist  ! a list of axis objs

        type(nc_dim) :: axis_i                               ! an axis obj
        character(len=99) :: dimname                         ! axis name
        integer, allocatable,dimension(:) :: dimids          ! axis ids
        integer :: i, ndims, varid

        !-------------Get variable id by name-------------
        call check(nf90_inq_varid(ncid,var,varid))

        !-------------Get number of dimensions-------------
        call check(nf90_inquire_variable(ncid,varid,ndims=ndims))
        allocate(dimids(ndims))
        allocate(axislist(ndims))

        !-----------------Get axis indices-----------------
        call check(nf90_inquire_variable(ncid,varid,dimids=dimids))

        write(*,*) 
        write(*,*) "# <ncread>: Get axislist for variable '", var, "':"
        do i=1,ndims
            axis_i=getAxis(ncid,dimids(i))
            axislist(i)=axis_i
        end do
        
    end function getAxisList


    !-----------Get the shape of a variable.-----------
    function getShape(ncid,var) result(shape_vec)
    ! Get the shape of a variable.
        implicit none
        integer, intent(in) :: ncid
        character(len=*), intent(in) :: var
        integer, allocatable, dimension(:) :: shape_vec

        integer :: i, ndims, varid, dimlen
        integer, allocatable, dimension(:) :: dimids

        !-------------Get variable id by name-------------
        call check(nf90_inq_varid(ncid,var,varid))

        !-----------------Get axis indices-----------------
        call check(nf90_inquire_variable(ncid,varid,ndims=ndims))
        allocate(shape_vec(ndims))
        allocate(dimids(ndims))
        call check(nf90_inquire_variable(ncid,varid,dimids=dimids))

        do i=1,ndims
            call check(nf90_inquire_dimension(ncid,dimids(i),len=dimlen))
            shape_vec(i)=dimlen
        end do

        write(*,*) 
        write(*,*) "# <ncread>: Get shape of variable '", var, "`:"
        write(*,*) "Shape = (", shape_vec, ")"

    end function getShape



    !----------Get a variable with meta data----------
    function getVar(ncid,var) result(ncvar)
    ! Get a variable with meta data
        implicit none
        integer, intent(in) :: ncid
        character(len=*), intent(in) :: var     ! variable name
        type(nc_var) :: ncvar                   ! nc_var obj

        integer :: i, varid, ndims, varsize
        integer, allocatable, dimension(:) :: varshape        ! shape array
        type(nc_dim), allocatable, dimension(:) :: axislist   ! a list of nc_dim objs
        type(nc_att), allocatable, dimension(:) :: attlist    ! a list of nc_att objs

        !---------------Array to store data---------------
        real, allocatable, dimension(:) :: dummy
        real, allocatable, dimension(:) :: data1
        real, allocatable, dimension(:,:) :: data2
        real, allocatable, dimension(:,:,:) :: data3
        real, allocatable, dimension(:,:,:,:) :: data4
        real, allocatable, dimension(:,:,:,:,:) :: data5
        real, allocatable, dimension(:,:,:,:,:,:) :: data6
        real, allocatable, dimension(:,:,:,:,:,:,:) :: data7

        write(*,*) " # <ncread>: Read in variable: ", var
        !-------------Get variable id by name-------------
        call check(nf90_inq_varid(ncid,var,varid))

        !---------------------Get rank---------------------
        call check(nf90_inquire_variable(ncid,varid,ndims=ndims))

        !------------------Get var shape------------------
        varshape=getShape(ncid,var)

        !------------------Get total size------------------
        varsize=product(varshape)

        !-------------------Get axislist-------------------
        axislist=getAxisList(ncid,var)

        !------------------Get attributes------------------
        attlist=getAttributes(ncid,var)

        !------------Get data into dummy vector------------

        !-------------Reshape to correct shape-------------
        if (ndims==1) then
            allocate(data1(varsize))
            call check(nf90_get_var(ncid,varid,data1))
            ncvar%data1=data1

        else if (ndims==2) then
            allocate(data2(varshape(1),varshape(2)))
            call check(nf90_get_var(ncid,varid,data2))
            ncvar%data2=data2

        else if (ndims==3) then
            allocate(data3(varshape(1),varshape(2),varshape(3)))
            call check(nf90_get_var(ncid,varid,data3))
            ncvar%data3=data3
        else if (ndims==4) then
            allocate(data4(varshape(1),varshape(2),varshape(3),varshape(4)))
            call check(nf90_get_var(ncid,varid,data4))
            ncvar%data4=data4
        else if (ndims==5) then
            allocate(data5(varshape(1),varshape(2),varshape(3), &
                & varshape(4),varshape(5)))
            call check(nf90_get_var(ncid,varid,data5))
            ncvar%data5=data5
        else if (ndims==6) then
            allocate(data6(varshape(1),varshape(2),varshape(3), &
                & varshape(4),varshape(5),varshape(6)))
            call check(nf90_get_var(ncid,varid,data6))
            ncvar%data6=data6
        else if (ndims==7) then
            allocate(data7(varshape(1),varshape(2),varshape(3), &
                & varshape(4),varshape(5),varshape(6),varshape(7)))
            call check(nf90_get_var(ncid,varid,data7))
            ncvar%data7=data7
        end if

        !------------------Populate nc_var------------------
        ncvar%id=varid
        ncvar%name=var
        ncvar%shape=varshape
        ncvar%ndims=ndims
        ncvar%atts=attlist
        ncvar%axislist=axislist

        !--------------------Print info--------------------
        write(*,*) 
        write(*,*) "# <ncread>: Variable data rank = ", ndims
        write(*,"(A,i1,A)") "# <ncread>: Variable data is referred as 'ncvar%data", ndims, "'."


    end function getVar



end module ncread




program main
use netcdf
use ncread
implicit none

character(len=*), parameter :: FILE_NAME="pre_s_m_2000_ori-preprocessed.nc"
integer :: ncid, i
character(len=99), allocatable,dimension(:) :: varlist
type(nc_att), allocatable, dimension(:) :: att_list
type(nc_dim), allocatable, dimension(:) :: axis_list
integer, allocatable, dimension(:) :: varshape
type(nc_var) :: ncvar
!type(ncvar) :: var

!--------------------Open file--------------------
call check(nf90_open(FILE_NAME,nf90_nowrite,ncid))
write(*,*) "ncid:", ncid

!var=newVar(ncid)
!varlist=listVariables(ncid)
!do i=1,size(varlist)
!    write(*,*) varlist(i)
!end do


!att_list=getAttributes(ncid, "time")
!do i=1,size(att_list)
!    write(*,*) att_list(i)%name, att_list(i)%str
!end do


!axis_list=getAxisList(ncid, "pre")
!write(*,*) size(axis_list)
!do i=1,size(axis_list)
     !write(*,*) axis_list(i)%id, axis_list(i)%name, axis_list(i)%data
!end do

!varshape=getShape(ncid,"pre")
!write(*,*) varshape


!write(*,*) 
!write(*,*) 

ncvar=getVar(ncid,"pre")
write(*,*) "ncvar.id", ncvar%id, "ncvar.name", ncvar%name
write(*,*) "ncvar.shape", ncvar%shape, shape(ncvar%data4)
do i=1,size(ncvar%atts)
    write(*,*) ncvar%atts(i)%name
end do






end program main
