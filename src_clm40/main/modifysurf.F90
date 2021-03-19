program modifysurf
!subroutine to manipulate parameters in surfacedata file
!    M.Huang,Z.Hou, 08/24/2011
!
! !USES:
  !use modifyparamsMod, only : modifyparms 
  implicit none
  include 'netcdf.inc'

  character(len=32) :: subname = 'modifysurf'
!  character(len=100) surffile
  character :: surffile*100
  character(len=800) buffer
  character(len=15) parm_name
  integer ip, ncid 
  integer, parameter :: nlevsoi=10, np=10
  double precision pval(np), data1d, data2d(nlevsoi)
  character(len=15), parameter :: parm_names(np)=(/'FMAX', 'CS','FOVER', 'FDRAI',&
                     'QDM','SY','BCH','PSIS','KS','THETAS'/)

  !read in the filename and parameter values
  call getarg(1,buffer)
  read(buffer,*) surffile
  !print*,surffile
  do ip=1,np
     !print*, ip
     call getarg(ip+1,buffer)
     read(buffer,*) pval(ip)
     !print*, pval(ip) 
  enddo

  !open the surfdata file for writing
  call check_ret(nf_open(trim(surffile), nf_write, ncid), subname)
  !loop to change parameter values in surfdata file
  do ip=1,np
     parm_name=parm_names(ip)
     call modifyparms(ncid, ip, pval(ip),parm_name)
  enddo
  !close the netcdf file
   call check_ret(nf_close(ncid), subname)

end program modifysurf

subroutine modifyparms(ncid, pnum, pval,parm_name)
  !subroutine to manipulate parameters in surfacedata file file

! !USES:
  implicit none
  include 'netcdf.inc'

  character(len=32) :: subname = 'modifyparms'
  character(len=15) parm_name
  integer i, ncid, varid, pnum
  integer, parameter :: nlevsoi=10
  double precision pval, data1d, data2d(nlevsoi)

  ! parameters to be perturbed

  !print*, ncid, parm_name
  call check_ret(nf_inq_varid(ncid,trim(parm_name), varid), subname)
  if (pnum .ge. 7 .and. pnum .le. 10) then
     call check_ret(nf_get_var_double(ncid, varid, data2d), subname)
     data2d(:)  = pval
     call check_ret(nf_put_var_double(ncid, varid, data2d), subname)
  else
     call check_ret(nf_get_var_double(ncid, varid, data1d), subname)
     data1d  = pval
     call check_ret(nf_put_var_double(ncid, varid, data1d), subname)
  endif

end subroutine modifyparms

!=============================================================================

subroutine check_ret(ret, calling)

  ! Check return status from netcdf call

  implicit none
  include 'netcdf.inc'
  integer, intent(in) :: ret
  character(len=*) :: calling

  if (ret /= NF_NOERR) then
     write(6,*)'netcdf error from ',trim(calling),' error= ',ret
     write(6,*) NF_STRERROR(ret)
     stop
  end if

end subroutine check_ret

