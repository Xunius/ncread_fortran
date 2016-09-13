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

write(*,*) 
write(*,*) rank(ncvar%data4)

write(*,*) 
write(*,*) 
varshape=getShape(ncvar)





end program main
