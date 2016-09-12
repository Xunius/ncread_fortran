# ncread_fortran
Some tools to facilitate netcdf data reading using Fortran 95

Not working.
Dont like fortran........

Kind of work now.
Still dont like fortran .......

# Description: Open and read data from netcdf file.

Contains a number of procedures to facilitate reading of
netcdf files. All wrappers to the native Fortran-netcdf APIs.
Designed with the *CDAT*'s (a python module to manipulate netcdfs)
ways of doing things in mind, for instance some functions are
named after functions in *CDAT*.

Creates 3 dervied types:

1. `nc_att`: stores attribute name and its value (converted to string).
2. `nc_dim`: dimension object, contains the dimension id, name, length,
         attributes (as an array of `nc_att` objs) and value (1-d array).
3. `nc_var`: variable obj, contains the variable id, name, shape, attributes
         (as an array of `nc_att` objs), axis list (a list of `nc_dim` objs)
         and its data value (of some rank from 1-d upto 7-d).

I tried to use the same obj type for `nc_dim` and `nc_var`, as a dimension itself
can be treated as a variable. But it seems that derived type cannot be recursive:
you cannnot declare an array of `nc_vars` in the `nc_var` type definition. So
they are defined separately.

# Usages:

1. To list all variables in a file:
 ```
 integer :: ncid
 character(len=99), allocatable,dimension(:) :: varlist
 call check(nf90_open(FILE_NAME,nf90_nowrite,ncid))
 varlist=listVariables(ncid)
 ```
 
2. To get the attributes of a variable by variable name:
 ```
 type(nc_att), allocatable, dimension(:) :: att_list
 att_list=getAttributes(ncid, "pre")
 ```
 
3. To get the axis list of a variable:
 ```
 type(nc_dim), allocatable, dimension(:) :: axis_list
 axis_list=getAxisList(ncid, "pre")
 ```
 
4. To get the variable shape:
 ```
 integer, allocatable, dimension(:) :: varshape
 varshape=getShape(ncid,"pre")
 write(*,*) varshape
 ```
 
5. To get a variable by name:
 ```
 type(nc_var) :: ncvar
 ncvar=getVar(ncid,"pre")
 ```
