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
    character(len=80) :: key
    character(len=80) :: str
    real :: value
end type

!------------------dimension type------------------
type nc_dim
    integer :: id
    integer :: len
    integer :: natts
    character(len=20) :: name
    type(nc_att), allocatable, dimension(:) :: atts

    real, allocatable, dimension(:,:) :: data
end type


!------------------Create nc type------------------
type nc_var
    integer :: id
    integer :: ndims
    integer :: natts
    character(len=20) :: name
    type(nc_att), allocatable, dimension(:) :: atts

    real, allocatable, dimension(:) :: data1
    real, allocatable, dimension(:,:) :: data2
    real, allocatable, dimension(:,:,:) :: data3
    real, allocatable, dimension(:,:,:,:) :: data4
    real, allocatable, dimension(:,:,:,:,:) :: data5

    type(nc_dim), allocatable, dimension(:) :: dims
end type


private

!--------------------Data info--------------------
integer :: ncid             ! id for ncfile
integer :: ndims            ! number of dimensions
integer :: nvars            ! number of variables
integer :: natts            ! number of global attributes
integer :: unlimdimid       ! id for unlimited dimension
integer :: xtype            ! data type code

!------------------Dimension info------------------
integer :: dimlen
character(len=20) :: dimname   

!------------------Variable info------------------
integer :: vartype, varndim, varnatts, varid
integer :: varsize
integer, dimension(7) :: vardimids
integer, dimension(7) :: varshape
character(len=12) :: dtype
character(len=20) :: varname

!------------------Attribute info------------------
integer :: attdtype
integer :: attlen
integer :: attidx
real :: attvalue_n
character(len=80) :: attvalue
character(len=40) :: attname

!------------------Variable array------------------
real, allocatable, dimension(:) :: dummy
real, allocatable, dimension(:,:) :: newdata2
real, allocatable, dimension(:,:,:) :: newdata3
real, allocatable, dimension(:,:,:,:) :: newdata4

!-------------------General usage-------------------
integer :: i, j, k, status

public :: readMeta, check, listVariables, nc_att, nc_dim, &
    & nc_var, getAttributes, getAxisList


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
        character(len=99), allocatable, dimension(:) :: varlist
        character(len=99) :: var_i

        call check(nf90_inquire(ncid, ndims, nvars))
        allocate(varlist(nvars))

        do i=1,nvars
            call check(nf90_inquire_variable(ncid,i,var_i))
            varlist(i)=trim(var_i)
            write(*,*) "var", i, "name:", varlist(i), "len", len_trim(varlist(i))
        end do

    end function listVariables


    !--------Get attributes of a variable name--------
    function getAttributes(ncid,var) result(attlist)
    ! Get attributes of a variable name
        implicit none
        integer, intent(in) :: ncid
        character(len=*), intent(in) :: var
        type(nc_att) :: att_i
        type(nc_att), allocatable, dimension(:) :: attlist
        character(len=80) :: attname
        character(len=80) :: att_c
        real :: att_n

        integer :: varid, natts, i, xtype, attlen

        call check(nf90_inq_varid(ncid,var,varid))
        write(*,*) "varid:", varid, "varname:", var

        call check(nf90_inquire_variable(ncid,varid,natts=natts))
        write(*,*) "natts", natts

        allocate(attlist(natts))

        do i=1,natts
            call check(nf90_inq_attname(ncid,varid,i,attname))
            call check(nf90_inquire_attribute(ncid,varid,attname,xtype,attlen))
            att_i%key=attname
            if (xtype==nf90_char) then
                call check(nf90_get_att(ncid,varid,attname,att_c))
                att_i%str=att_c
            else
                call check(nf90_get_att(ncid,varid,attname,att_n))
                att_i%value=att_n
            end if

            attlist(i)=att_i
        end do

        
    end function getAttributes


    !----Get a list of axes for a variable by name----
    function getAxisList(ncid,var) result(axislist)
    ! Get a list of axes for a variable by name
        implicit none
        integer, intent(in) :: ncid
        character(len=*), intent(in) :: var
        type(nc_dim), allocatable, dimension(:) :: axislist
        type(nc_dim) :: axis_i

        character(len=99) :: dimname,name
        integer :: i, status, ndims, varid, dimlen, dimid
        integer, allocatable,dimension(:) :: dimids
        real, allocatable, dimension(:,:) :: values
        integer :: maxlen

        call check(nf90_inq_varid(ncid,var,varid))
        call check(nf90_inquire_variable(ncid,varid,ndims=ndims))
        allocate(dimids(ndims))
        call check(nf90_inquire_variable(ncid,varid,dimids=dimids))
        write(*,*) "ndims:", ndims
        write(*,*) "dimids:", dimids

        do i=1,ndims
            call check(nf90_inquire_dimension(ncid,dimids(i),dimname,dimlen))
            call check(nf90_inq_varid(ncid,dimname,dimid))
            if (i==1) then
                maxlen=dimlen
            else
                if (dimlen>=maxlen) then
                    maxlen=dimlen
                end if
            end if
        end do
        allocate(values(ndims,maxlen))

        do i=1,ndims
            write(*,*) "id", dimids(i), "dimname", dimname, "dimvarid", dimid
            write(*,*) "dimlen:", dimlen
            call check(nf90_get_var(ncid,dimid,values(i,1:maxlen)))
            axis_i%id=dimids(i)
            axis_i%len=dimlen
            axis_i%name=dimname
            axis_i%data=values

            axislist(i)=axis_i
        end do
        
    end function getAxisList



    !---------------Read metadata of file---------------
    function readMeta(ncid) result(meta)
        use netcdf
        implicit none
        integer, intent(in) :: ncid
        !type(ncvar) :: var
        type(nc_att) :: meta

        !-------------------Get general-------------------
        call check(nf90_inquire(ncid, ndims, nvars, natts, unlimdimid, xtype))

        write(*,*) "General info:"
        write(*,*) "ndims:", ndims
        write(*,*) "nvars:", nvars
        write(*,*) "natts:", natts
        write(*,*) "unlimdimid:", unlimdimid
        write(*,*) "xtype:", xtype

        !------------------Get dimensions------------------
        write(*,*) 
        write(*,*) "Dimensions:"
        do i=1,ndims
            call check(nf90_inquire_dimension(ncid,i,dimname,dimlen))
            write(*,*) "dimname:", dimname, "dimlen:", dimlen
        end do

        !-------------------Get var info-------------------
        write(*,*)
        write(*,*) "Variables:"

        do i=1,nvars
            call check(nf90_inquire_variable(ncid,i,varname,vartype,varndim,vardimids,varnatts))
            call check(nf90_inq_varid(ncid,varname,varid))
            write(*,*) "varid:", varid, "varname:", varname
            write(*,*) "vartype:", vartype, getDtype(vartype)
            write(*,*) "varndim:", varndim
            write(*,*) "vardimids:", vardimids(1:varndim)
            write(*,*) "varnatts:", varnatts
        end do

        !------------------Get attributes------------------
        write(*,*) 
        write(*,*) "Get attributes:"

        do i=1,nvars
            call check(nf90_inquire_variable(ncid,i,name=varname,natts=varnatts))
            do j=1,varnatts
                call check(nf90_inq_attname(ncid,i,j,attname))
                call check(nf90_inquire_attribute(ncid,i,attname,attdtype,attlen,attidx))
                if (attdtype == 2) then
                    call check(nf90_get_att(ncid,i,attname,attvalue))
                else
                    call check(nf90_get_att(ncid,i,attname,attvalue_n))
                end if
                write(*,*) "varid", j, "varname:", varname
                write(*,*) "attid", attidx, "attname:", attname
                write(*,*) "attdtype:", attdtype, "attlen", attlen
                if (attdtype==2) then
                    write(*,*) "attvalue:", attvalue(1:attlen)
                else
                    write(*,*) "attvalue:", attvalue_n
                end if
            end do
        end do

        !------------------Allocate array------------------
        call check(nf90_inquire_variable(ncid,8,varname,vartype,varndim,vardimids,varnatts))
        write(*,*) 
        write(*,*) 
        write(*,*) "vardimids or pre:", vardimids(1:varndim)
        write(*,*) "varndim", varndim
        write(*,*) 

        !----------------Get varable shape----------------
        varsize=1
        do i=1,varndim
            call check(nf90_inquire_dimension(ncid,vardimids(i),dimname,dimlen))
            varsize=varsize*dimlen
            varshape(i)=dimlen

            write(*,*) "dimid:", vardimids(i), "dimlen:", dimlen, "varsize:", varsize
            write(*,*) "varshape:", varshape(1:varndim)
        end do

        allocate(dummy(varsize))

        write(*,*) "dummy shape:", shape(dummy)
        write(*,*) "varshape", varshape
        write(*,*) "varshape", varshape(1:varndim)

        write(*,*) "size", size(varshape(1:varndim))

        if (size(varshape(1:varndim))==2) then
            allocate(newdata2(varshape(1),varshape(2)))
            !newdata2=reshape(dummy,(/varshape(1),varshape(2)/))
            !call create_ncvar(reshape(dummy,(/varshape(1),varshape(2)/)))
        else if (size(varshape(1:varndim))==3) then
            allocate(newdata3(varshape(1),varshape(2),varshape(3)))
            !newdata3=reshape(dummy,(/varshape(1),varshape(2),varshape(3)/))
            !call create_ncvar(reshape(dummy,(/varshape(1),varshape(2),varshape(3)/)))
        else if (size(varshape(1:varndim))==4) then
            allocate(newdata4(varshape(1),varshape(2),varshape(3),varshape(4)))
            newdata4=reshape(dummy,(/varshape(1),varshape(2),varshape(3),varshape(4)/))
            !call create_ncvar(newdata4)
        end if

        write(*,*) "shape(dummy)", shape(dummy)
        write(*,*) "shape(newdata4)", shape(newdata4)



        !---------------------get var---------------------
        !call check(nf90_get_var(ncid,8,vardata))


        !--------------------Close file--------------------
        call check(nf90_close(ncid))
        write(*,*) "NC file closed."

        !--------------------Get return--------------------
        !var%id=8

    end function readMeta
        


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
!type(ncvar) :: var

!--------------------Open file--------------------
call check(nf90_open(FILE_NAME,nf90_nowrite,ncid))
write(*,*) "ncid:", ncid

!var=newVar(ncid)
varlist=listVariables(ncid)
do i=1,size(varlist)
    write(*,*) varlist(i)
end do


att_list=getAttributes(ncid, "pre")
do i=1,size(att_list)
    write(*,*) att_list(i)%key, att_list(i)%str
end do



axis_list=getAxisList(ncid, "pre")
do i=1,size(axis_list)
    write(*,*) axis_list(i)%id, axis_list(i)%name, axis_list(i)%data
end do



end program main
