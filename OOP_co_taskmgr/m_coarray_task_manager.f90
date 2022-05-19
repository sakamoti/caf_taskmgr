!>@file m_coarray_task_manager.f90
!
module m_coarray_task_manager
  use, intrinsic :: iso_fortran_env
  implicit none
  private 
  ! public subroutines
  public :: set_msg, alloc_task,gc_task, clear_unused_task,&
    put_task, get_task, retrieve_task_from, showobj,&
    task_broadcast, task_maxloc, task_minloc
  !---
  ! type definition
  !>@brief task which hold user defined object
  type,private :: t_task
    class(*), allocatable :: obj !< object used by user
    contains
      private
      procedure :: t_eq_scl !< assignment routine
      generic :: assignment(=) => t_eq_scl
  end type
  !---
  ! module variable definition
  type(lock_type), private :: task_lock[*] !< lock variable to put or get task
  type(t_task), allocatable, private :: task(:)[:] !< task coarray
  integer, public, protected :: task_size[*] = 0 !<task size
  integer, public, protected :: allocstat = 0 !< status indicator in this module
  character(len = 100), public, protected :: errmsg = "no error" !< error message
  logical, public, protected :: verbosemsg = .true. !< message showing flag
  !---
  ! information about compiler
  !character(len = *), public, parameter :: compiled_by = compiler_version()
  !character(len = *), public, parameter :: compiled_with = compiler_options()
  contains
    subroutine show_err_msg(sroutine)
      character(len=*),intent(in) :: sroutine
      if(verbosemsg.and.allocstat/=0)then
        write(error_unit,*)"WARNING("//trim(sroutine)//")! image",this_image(),":",trim(errmsg)
      endif
    end subroutine
    subroutine t_eq_scl(t_out, t_in)
      class(t_task), intent(inout) :: t_out
      class(*), intent(in) :: t_in
      if(allocated(t_out%obj))then
        deallocate(t_out%obj, stat=allocstat, errmsg=errmsg)
        call show_err_msg("t_eq_scl:dealloc")
      end if
      select type(t_in)
        class is(t_task)
          allocate(t_out%obj, source = t_in%obj, stat = allocstat, errmsg = errmsg)
        class default
          allocate(t_out%obj, source = t_in, stat = allocstat, errmsg = errmsg)
      end select
      call show_err_msg("t_eq_scl:allocate")
    end subroutine
    subroutine set_msg(tf)
      logical, intent(in) :: tf
      verbosemsg=tf
    end subroutine
    subroutine alloc_task(num)
      integer,intent(in) :: num
      allocate(task(num)[*],stat=allocstat,errmsg=errmsg)
      call show_err_msg("alloc_task")
    end subroutine
    subroutine gc_task()
      !! clear all data "task"
      deallocate(task,stat=allocstat,errmsg=errmsg)
      task_size=0
      call show_err_msg("gc_task")
    end subroutine
    subroutine clear_unused_task()
      integer :: i, nt
      lock(task_lock)
      nt=size(task)
      do i=task_size+1,nt
        if(allocated(task(i)%obj))then
          deallocate(task(i)%obj,stat=allocstat,errmsg=errmsg)
          call show_err_msg("clear_unused_task")
        end if
      end do
      unlock(task_lock)
    end subroutine
    subroutine put_task(obj)
      class(*),intent(in) :: obj
      lock(task_lock)
      if(task_size>=size(task))then
        if(verbosemsg)then
          write(error_unit,'(1x,a,i5)') &
            &"ERROR: fail to put task since array size overflow. image=",&
            &this_image()
        end if
        unlock(task_lock)
        return
      end if
      task_size=task_size+1
      task(task_size)=obj
      unlock(task_lock)
    end subroutine
    subroutine get_task(obj,img)
      class(*),allocatable,intent(out) :: obj
      integer,intent(in) :: img
      lock(task_lock[img])
      if(task_size[img]<=0)then
        unlock(task_lock[img])
        return
      end if
      allocate(obj, source = task(task_size[img])[img]%obj,stat=allocstat,errmsg=errmsg)
      !print*,"fuga",allocated(obj),allocated(task(task_size[img])[img]%obj)
      !! Though ifort can compile this source, obj can't allocate.
      !! allocated(task(task_size[img])[img]%obj) is TRUE, but stat /= 0. (ifort 2021109)
      call show_err_msg("get_task")
      if(allocstat/=0)then
        !! if allocation is failed, task_size is not reduced
        unlock(task_lock[img])
        return
      end if
      task_size[img]=task_size[img]-1
      unlock(task_lock[img])
    end subroutine
    subroutine retrieve_task_from(img)
      class(*),allocatable :: obj
      integer,intent(in) :: img
      call get_task(obj,img)
      if(allocated(obj))then
        call put_task(obj)
      end if
    end subroutine
    subroutine showobj(obj)
      class(*),allocatable,intent(in) :: obj
      if(.not.allocated(obj))return
      select type(obj)
        type is(integer(kind=int8))
          print *,"img=",this_image(),", obj=",obj
        type is(integer(kind=int16))
          print *,"img=",this_image(),", obj=",obj
        type is(integer(kind=int32))
          print *,"img=",this_image(),", obj=",obj
        type is(integer(kind=int64))
          print *,"img=",this_image(),", obj=",obj
        type is(real(kind=real32))
          print *,"img=",this_image(),", obj=",obj
        type is(real(kind=real64))
          print *,"img=",this_image(),", obj=",obj
        type is(real(kind=real128))
          print *,"img=",this_image(),", obj=",obj
        type is(complex(kind=real32))
          print *,"img=",this_image(),", obj=",obj
        type is(complex(kind=real64))
          print *,"img=",this_image(),", obj=",obj
        type is(complex(kind=real128))
          print *,"img=",this_image(),", obj=",obj
        type is(logical)
          print *,"img=",this_image(),", obj=",obj
        type is(character(*))
          print *,"img=",this_image(),", obj=",obj
        class default
          print *,this_image(),"unknown class"
      end select
      contains
        subroutine showmsg_internal()
        end subroutine
    end subroutine
    subroutine task_broadcast(obj,img)
      !! img番目イメージにあるデータobjを全体のタスクとしてばらまく
      class(*),intent(in) :: obj(:)
      integer, intent(in) :: img
      integer, save :: ntask[*],nobj[*]
      integer :: i,nimg,thisimg,ict,ie
      thisimg=this_image()
      nimg=num_images()
      nobj=size(obj)
      ntask=nobj/nimg+1
      if(allocated(task))then
        call gc_task() !! deallocate existing all task
      endif
      sync all
      call alloc_task(nobj[img])
      if(img==thisimg)then
        do i=1,nobj
          call put_task(obj(i))
        end do
        sync images (*)
      else
        sync images (img)
      end if
      ict=mod(nobj[img],nimg)
      critical
        !! ie equal amount of task size at thisimg
        if(thisimg/=img)then
          if(thisimg>ict)then
            ie=ntask[img]-1
          else 
            ie=ntask[img]
          end if
          do i=1,ie
            if(task_size[img]>ntask[img])then
              call retrieve_task_from(img)
            endif
          end do
        end if
      end critical
      sync all
      call clear_unused_task()
      !--DEBUG WRITE--
     !if(this_image()>1) sync images (this_image()-1)
     !print*,"hoge",this_image(),task_size
     !if(this_image()<num_images())  sync images (this_image()+1)
      !--DEBUG WRITE--
    end subroutine
    subroutine task_maxloc(loc,ts,obj)
      integer, intent(out) :: loc
      integer, intent(out), optional :: ts !<task size
      class(*),allocatable,intent(out), optional :: obj
      integer :: i,nimg,tasksize
      nimg = num_images()
      tasksize = 0
      loc = 0
      critical
      do i = 1, nimg
        if(tasksize <= task_size[i])then
          tasksize = task_size[i]
          loc = i
        end if
      end do
      end critical
      if(present(ts))then
        ts=tasksize
      end if
      if(present(obj))then
        call get_task(obj,loc)
      end if
    end subroutine
    subroutine task_minloc(loc,ts,obj)
      integer, intent(out) :: loc
      integer, intent(out), optional :: ts !<task size
      class(*),allocatable,intent(out), optional :: obj
      integer :: i,nimg,tasksize
      nimg = num_images()
      tasksize = huge(0)
      loc = 0
      critical
      do i = 1, nimg
        if(tasksize >= task_size[i])then
          tasksize = task_size[i]
          loc = i
        end if
      end do
      end critical
      if(present(ts))then
        ts=tasksize
      end if
      if(present(obj))then
        call get_task(obj,loc)
      end if
    end subroutine
end module
