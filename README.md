# Some tools to facilitate netcdf data reading and manipulation using Fortran 95

# ncread.f95

## Description: Open and read data from netcdf file.

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

## Usages:

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


# nccreate.f95


## Description: Tools to faciliate creating dimensions, attributes and variables
            
## Usage

1. Create a dimension obj from name and value:
```
type(nc_att) :: dim
dim=createDim("time",value,.true.)
```
where: `value` is a 1-d array of the dimension values.
Give the 3rd optional `.true.` argument if the dimension is unlimited length

2. Add attribute to a nc_dim or nc_var obj:
```
type(nc_att) :: dim
type(nc_var) :: var
call addAtt(dim,"units","meter")
call addAtt(var,"long_name","precipitation")
```

3. Set a list of axis to a nc_var:
```
type(nc_dim), dimension(2) :: axislist
type(nc_dim) :: dim1, dim2
type(nc_var) :: var
dim1=createDim("lon",lons)
dim2=createDim("lat",lats)
call setAxisList(var,(/dim1,dim2/))

4. Create a nc_var obj from name and data:
```
type(nc_var) :: var
var=createVar("pre",data)
```
Interface functions covers data ranks from 1 to 7 

5. Write dimension to netcdf file:
```
type(nc_dim) :: dim1
dim1=createDim("lon",lons)
call check(nf90_create(FILE,nf90_clobber,ncid))
call writeDim(ncid,dim1)
```

6. Write variable to netcdf file:
```
type(nc_var) :: var
call check(nf90_create(FILE,nf90_clobber,ncid))
call writeVar(ncid,var)
```
