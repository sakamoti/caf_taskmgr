module m_coarray_task_manager
  use, intrinsic :: iso_fortran_env
  implicit none
  private
  public t_co_task,set_msg,alloc_task,gc_task,task_broadcast,showobj
  ! real kind selection
  integer, parameter :: kp = real64
  !---
  ! type definition
  type :: t_co_task
    integer       :: idata(3)
    real(kind=kp) :: rdata(3)
    logical       :: ldata(3)
  end type t_co_task
  !---
  ! module variable definition
  type(lock_type), private                        :: task_lock[*]  !< lock variable to put or get task
  type(t_co_task), allocatable, public, protected :: co_task(:)[:] !< task allocatable
  integer, public, protected                      :: task_size[*] = 0 !< task size
  integer, public, protected                      :: allocstat = 0    !< allocate status indicator in each image
  character(len=100), private, protected  :: errmsg = "no error" !< error message in each image
  logical, public, protected              :: verbosemsg = .true. !< message showing flag
  ! information about compiler which compile this module
  character(len=*), public, parameter     :: compiled_by = "compiler_version()"
  character(len=*), public, parameter     :: compiled_with = "compiler_options()"
  contains
    subroutine show_err_msg(routinename)
      character(len=*),intent(in) :: routinename
      if(verbosemsg.and.allocstat/=0)then
        write(error_unit,'(A,I3,A)') "# WARNING("//trim(routinename)//")! image:",this_image(),":"//trim(errmsg)
      end if
    end subroutine
    subroutine set_msg(tf)
      logical, intent(in) :: tf
      verbosemsg=tf
    end subroutine
    subroutine alloc_task(num)
      integer ,intent(in) :: num
      allocate(co_task(num)[*],stat=allocstat,errmsg=errmsg)
      call show_err_msg("m_coarray_task_manager::alloc_task")
    end subroutine
    subroutine gc_task()
      deallocate(co_task,stat=allocstat,errmsg=errmsg)
      task_size=0
      call show_err_msg("m_coarray_task_manager::gc_task")
    end subroutine
    subroutine showobj(obj)
      type(t_co_task),intent(in) :: obj
      integer :: i,ii,ir,il
      ii=size(obj%idata)
      ir=size(obj%rdata)
      il=size(obj%ldata)
      critical
      write(output_unit,'(A,i4,A)',advance="no") "image=",this_image(),"("
      write(output_unit,'(100i5)',advance="no") [(obj%idata(i),i=1,ii)]
      write(output_unit,'(100f8.1)',advance="no") [(obj%rdata(i),i=1,ir)]
      write(output_unit,'(2X,100l1)',advance="no") [(obj%ldata(i),i=1,il)]
      write(output_unit,'(A)') ")"
      end critical
    end subroutine
    subroutine task_broadcast(obj)
      type(t_co_task),intent(in) :: obj(:)
      integer :: i,img
      do i=1,size(obj)
        img=mod(i,num_images())+1
        task_size[img]=task_size[img]+1
        co_task(task_size[img])[img]=obj(i)
      end do
    end subroutine
end module m_coarray_task_manager
