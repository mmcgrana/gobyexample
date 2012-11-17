!!$
!!$
!!$                    MD2P4
!!$    Multilevel Domain Decomposition Parallel Preconditioner Package for PSBLAS
!!$                      for
!!$              Parallel Sparse BLAS  v2.0
!!$    (C) Copyright 2006 Salvatore Filippone    University of Rome Tor Vergata
!!$                       Alfredo Buttari        University of Rome Tor Vergata
!!$                       Daniela Di Serafino    II University of Naples
!!$                       Pasqua D'Ambra         ICAR-CNR
!!$
!!$  Redistribution and use in source and binary forms, with or without
!!$  modification, are permitted provided that the following conditions
!!$  are met:
!!$    1. Redistributions of source code must retain the above copyright
!!$       notice, this list of conditions and the following disclaimer.
!!$    2. Redistributions in binary form must reproduce the above copyright
!!$       notice, this list of conditions, and the following disclaimer in the
!!$       documentation and/or other materials provided with the distribution.
!!$    3. The name of the MD2P4 group or the names of its contributors may
!!$       not be used to endorse or promote products derived from this
!!$       software without specific written permission.
!!$
!!$  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
!!$  ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
!!$  TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
!!$  PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE MD2P4 GROUP OR ITS CONTRIBUTORS
!!$  BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
!!$  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
!!$  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
!!$  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
!!$  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
!!$  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
!!$  POSSIBILITY OF SUCH DAMAGE.
!!$
!!$
subroutine psb_zmlprc_aply(alpha,baseprecv,x,beta,y,desc_data,trans,work,info)
  !
  !  Compute   Y <-  beta*Y + alpha*K^-1 X
  !  where K is a multilevel  preconditioner stored in baseprecv
  !
  !  cfr.: Smith, Biorstad & Gropp
  !        Domain Decomposition
  !        Cambridge Univ. Press
  !
  !  To each level I there corresponds a matrix A(I) and a preconditioner K(I)
  !
  !  A notational difference: in the DD reference above the preconditioner for
  !  a given level K(I) is written out as a sum over the subdomains
  !
  !  SUM_k(R_k^T A_k R_k)
  !
  !  whereas in this code the sum is implicit in the parallelization,
  !  i.e. each process takes care of one subdomain, and for each level we have
  !  as many subdomains as there are processes (except for the coarsest level where
  !  we might have a replicated index space). Thus the sum apparently disappears
  !  from our code, but only apparently, because it is implicit in the call
  !  to psb_baseprc_aply.
  !
  !  A bit of description of the baseprecv(:) data structure:
  !   1. Number of levels = NLEV = size(baseprecv(:))
  !   2. baseprecv(ilev)%av(:)    sparse matrices needed for the current level.
  !      Includes:
  !   2.1.:  baseprecv(ilev)%av(l_pr_)    L factor of ILU preconditioners
  !   2.2.:  baseprecv(ilev)%av(u_pr_)    U factor of ILU preconditioners
  !   2.3.:  baseprecv(ilev)%av(ap_nd_)   Off-diagonal part of A for Jacobi sweeps
  !   2.4.:  baseprecv(ilev)%av(ac_)      Aggregated matrix of level ILEV
  !   2.5.:  baseprecv(ilev)%av(sm_pr_t_) Smoother prolongator transpose; maps vectors
  !                                          (ilev-1) --->  (ilev)
  !   2.6.:  baseprecv(ilev)%av(sm_pr_)   Smoother prolongator; maps vectors
  !                                          (ilev)   --->  (ilev-1)
  !   Shouldn't we keep just one of them and handle transpose in the sparse BLAS? maybe
  !
  !   3.    baseprecv(ilev)%desc_data     comm descriptor for level ILEV
  !   4.    baseprecv(ilev)%base_a        Pointer (really a pointer!) to the base matrix
  !                                       of the current level, i.e.: if ILEV=1 then  A
  !                                       else the aggregated matrix av(ac_); so we have
  !                                       a unified treatment of residuals. Need this to
  !                                       avoid passing explicitly matrix A to the
  !                                       outer prec. routine
  !   5.    baseprecv(ilev)%mlia          The aggregation map from (ilev-1)-->(ilev)
  !                                       if no smoother, it is used instead of sm_pr_
  !   6.    baseprecv(ilev)%nlaggr        Number of aggregates on the various procs.
  !

  use psb_serial_mod
  use psb_descriptor_type
  use psb_prec_type
  use psb_psblas_mod
  use psb_penv_mod
  use psb_const_mod
  use psb_error_mod
  use psb_penv_mod
  implicit none

  type(psb_desc_type),intent(in)      :: desc_data
  type(psb_zbaseprc_type), intent(in) :: baseprecv(:)
  complex(kind(1.d0)),intent(in)      :: alpha,beta
  complex(kind(1.d0)),intent(inout)   :: x(:), y(:)
  character                           :: trans
  complex(kind(1.d0)),target          :: work(:)
  integer, intent(out)                :: info


  ! Local variables
  integer :: n_row,n_col
  complex(kind(1.d0)), allocatable :: tx(:),ty(:),t2l(:),w2l(:),&
       &   x2l(:),b2l(:),tz(:),tty(:)
  character     ::diagl, diagu
  integer :: ictxt,np,me,i, isz, nrg,nr2l,err_act, iptype, int_err(5)
  real(kind(1.d0)) :: omega
  real(kind(1.d0)) :: t1, t2, t3, t4, t5, t6, t7, mpi_wtime
  logical, parameter          :: debug=.false., debugprt=.false.
  integer      :: ismth, nlev, ilev
  external mpi_wtime
  character(len=20)   :: name, ch_err

  type psb_mlprec_wrk_type
    complex(kind(1.d0)), pointer :: tx(:)=>null(),ty(:)=>null(),&
         & x2l(:)=>null(),y2l(:)=>null(),&
         & b2l(:)=>null(),tty(:)=>null()
  end type psb_mlprec_wrk_type
  type(psb_mlprec_wrk_type), pointer :: mlprec_wrk(:)

  interface psb_baseprc_aply
    subroutine psb_zbaseprc_aply(alpha,prec,x,beta,y,desc_data,trans,work,info)
      use psb_descriptor_type
      use psb_prec_type
      type(psb_desc_type),intent(in)      :: desc_data
      type(psb_zbaseprc_type), intent(in) :: prec
      complex(kind(1.d0)),intent(inout)   :: x(:), y(:)
      complex(kind(1.d0)),intent(in)      :: alpha,beta
      character(len=1)                    :: trans
      complex(kind(1.d0)),target          :: work(:)
      integer, intent(out)                :: info
    end subroutine psb_zbaseprc_aply
  end interface

  name='psb_mlprc_aply'
  info = 0
  call psb_erractionsave(err_act)


  ictxt=desc_data%matrix_data(psb_ctxt_)
  call psb_info(ictxt, me, np)

  nlev = size(baseprecv)
  allocate(mlprec_wrk(nlev),stat=info)
  if (info /= 0) then
    call psb_errpush(4010,name,a_err='Allocate')
    goto 9999
  end if


  select case(baseprecv(2)%iprcparm(ml_type_))

  case(no_ml_)
    ! Should not really get here.
    call psb_errpush(4010,name,a_err='no_ml_ in mlprc_aply?')
    goto 9999


  case(add_ml_prec_)


    !
    !    Additive is very simple.
    !    1.  X(1) = Xext
    !    2.  DO ILEV=2,NLEV
    !           X(ILEV) = AV(PR_SM_T_)*X(ILEV-1)
    !    3.  Y(ILEV) = (K(ILEV)**(-1))*X(ILEV)
    !    4.  DO  ILEV=NLEV-1,1,-1
    !           Y(ILEV) = AV(PR_SM_)*Y(ILEV+1)
    !    5.  Yext    = beta*Yext + Y(1)
    !
    !    Note: level numbering reversed wrt ref. DD, i.e.
    !         1..NLEV <=>  (j) <-> 0


    call psb_baseprc_aply(alpha,baseprecv(1),x,beta,y,&
         & baseprecv(1)%base_desc,trans,work,info)
    if(info /=0) goto 9999
    allocate(mlprec_wrk(1)%x2l(size(x)),mlprec_wrk(1)%y2l(size(y)))
    mlprec_wrk(1)%x2l(:) = x(:)


    do ilev = 2, nlev
      n_row = baseprecv(ilev-1)%base_desc%matrix_data(psb_n_row_)
      n_col = baseprecv(ilev-1)%desc_data%matrix_data(psb_n_col_)
      nr2l  = baseprecv(ilev)%desc_data%matrix_data(psb_n_col_)
      nrg   = baseprecv(ilev)%desc_data%matrix_data(psb_n_row_)
      allocate(mlprec_wrk(ilev)%x2l(nr2l),mlprec_wrk(ilev)%y2l(nr2l),&
           & mlprec_wrk(ilev)%tx(max(n_row,n_col)),&
           & mlprec_wrk(ilev)%ty(max(n_row,n_col)), stat=info)
      if (info /= 0) then
        call psb_errpush(4010,name,a_err='Allocate')
        goto 9999
      end if

      mlprec_wrk(ilev)%x2l(:) = zzero
      mlprec_wrk(ilev)%y2l(:) = zzero
      mlprec_wrk(ilev)%tx(1:n_row) = mlprec_wrk(ilev-1)%x2l(1:n_row)
      mlprec_wrk(ilev)%tx(n_row+1:max(n_row,n_col)) = zzero
      mlprec_wrk(ilev)%ty(:) = zzero

      ismth=baseprecv(ilev)%iprcparm(smth_kind_)

      if (ismth  /= no_smth_) then
        !
        ! Smoothed aggregation
        !


        if (baseprecv(ilev)%iprcparm(glb_smth_) >0) then
          call psb_halo(mlprec_wrk(ilev-1)%x2l,baseprecv(ilev-1)%base_desc,&
               &  info,work=work)
          if(info /=0) goto 9999
        else
          mlprec_wrk(ilev-1)%x2l(n_row+1:max(n_row,n_col)) = zzero
        end if

        call psb_csmm(zone,baseprecv(ilev)%av(sm_pr_t_),mlprec_wrk(ilev-1)%x2l,&
             & zzero,mlprec_wrk(ilev)%x2l,info)
        if(info /=0) goto 9999

      else
        !
        ! Raw  aggregation, may take shortcut
        !
        do i=1,n_row
          mlprec_wrk(ilev)%x2l(baseprecv(ilev)%mlia(i)) = &
               &  mlprec_wrk(ilev)%x2l(baseprecv(ilev)%mlia(i)) + &
               &  mlprec_wrk(ilev-1)%x2l(i)
        end do

      end if

      if (baseprecv(ilev)%iprcparm(coarse_mat_)==mat_repl_) Then
        call psb_sum(ictxt,mlprec_wrk(ilev)%x2l(1:nrg))
      else if (baseprecv(ilev)%iprcparm(coarse_mat_) /= mat_distr_) Then
        write(0,*) 'Unknown value for baseprecv(2)%iprcparm(coarse_mat_) ',&
             & baseprecv(ilev)%iprcparm(coarse_mat_)
      endif

      call psb_baseprc_aply(zone,baseprecv(ilev),&
           & mlprec_wrk(ilev)%x2l,zzero,mlprec_wrk(ilev)%y2l,&
           & baseprecv(ilev)%desc_data, 'N',work,info)

    enddo

    do ilev =nlev,2,-1

      ismth=baseprecv(ilev)%iprcparm(smth_kind_)
      n_row = baseprecv(ilev-1)%base_desc%matrix_data(psb_n_row_)
      n_col = baseprecv(ilev-1)%desc_data%matrix_data(psb_n_col_)
      nr2l  = baseprecv(ilev)%desc_data%matrix_data(psb_n_col_)
      nrg   = baseprecv(ilev)%desc_data%matrix_data(psb_n_row_)

      if (ismth  /= no_smth_) then

        call psb_csmm(zone,baseprecv(ilev)%av(sm_pr_),mlprec_wrk(ilev)%y2l,&
             & zone,mlprec_wrk(ilev-1)%y2l,info)
        if(info /=0) goto 9999

      else

        do i=1, n_row
          mlprec_wrk(ilev-1)%y2l(i) = mlprec_wrk(ilev-1)%y2l(i) + &
               &   mlprec_wrk(ilev)%y2l(baseprecv(ilev)%mlia(i))
        enddo

      end if
    end do

    call psb_geaxpby(alpha,mlprec_wrk(1)%y2l,zone,y,baseprecv(1)%base_desc,info)
    if(info /=0) goto 9999


  case(mult_ml_prec_)

    !
    !  Multiplicative multilevel
    !  Pre/post smoothing versions.
    !

    select case(baseprecv(2)%iprcparm(smth_pos_))

    case(post_smooth_)


      !
      !    Post smoothing.
      !    1.   X(1) = Xext
      !    2.   DO ILEV=2, NLEV :: X(ILEV) = AV(PR_SM_T_,ILEV)*X(ILEV-1)
      !    3.   Y(NLEV) = (K(NLEV)**(-1))*X(NLEV)
      !    4.   DO  ILEV=NLEV-1,1,-1
      !          Y(ILEV) = AV(PR_SM_,ILEV+1)*Y(ILEV+1)
      !          Y(ILEV) = Y(ILEV) + (K(ILEV)**(-1))*(X(ILEV)-A(ILEV)*Y(ILEV))
      !
      !    5.  Yext    = beta*Yext + Y(1)
      !
      !    Note: level numbering reversed wrt ref. DD, i.e.
      !         1..NLEV <=>  (j) <-> 0
      !
      !    Also: post smoothing is not spelled out in detail in DD.
      !
      !


      n_col = desc_data%matrix_data(psb_n_col_)
      nr2l  = baseprecv(1)%desc_data%matrix_data(psb_n_col_)

      allocate(mlprec_wrk(1)%x2l(nr2l),mlprec_wrk(1)%y2l(nr2l), &
           & mlprec_wrk(1)%tx(nr2l), stat=info)
      mlprec_wrk(1)%x2l(:) = zzero
      mlprec_wrk(1)%y2l(:) = zzero
      mlprec_wrk(1)%tx(:) = zzero

      call psb_geaxpby(zone,x,zzero,mlprec_wrk(1)%tx,&
           & baseprecv(1)%base_desc,info)
      call psb_geaxpby(zone,x,zzero,mlprec_wrk(1)%x2l,&
           & baseprecv(1)%base_desc,info)

      do ilev=2, nlev
        n_row = baseprecv(ilev-1)%base_desc%matrix_data(psb_n_row_)
        n_col = baseprecv(ilev-1)%desc_data%matrix_data(psb_n_col_)
        nr2l  = baseprecv(ilev)%desc_data%matrix_data(psb_n_col_)
        nrg   = baseprecv(ilev)%desc_data%matrix_data(psb_n_row_)
        ismth = baseprecv(ilev)%iprcparm(smth_kind_)

        allocate(mlprec_wrk(ilev)%tx(nr2l),mlprec_wrk(ilev)%y2l(nr2l),&
             &   mlprec_wrk(ilev)%x2l(nr2l), stat=info)

        if (info /= 0) then
          call psb_errpush(4010,name,a_err='Allocate')
          goto 9999
        end if

        mlprec_wrk(ilev)%x2l(:) = zzero
        mlprec_wrk(ilev)%y2l(:) = zzero
        mlprec_wrk(ilev)%tx(:) = zzero
        if (ismth  /= no_smth_) then
          !
          ! Smoothed aggregation
          !
          if (baseprecv(ilev)%iprcparm(glb_smth_) >0) then
            call psb_halo(mlprec_wrk(ilev-1)%x2l,&
                 &  baseprecv(ilev-1)%base_desc,info,work=work)
            if(info /=0) goto 9999
          else
            mlprec_wrk(ilev-1)%x2l(n_row+1:max(n_row,n_col)) = zzero
          end if

          call psb_csmm(zone,baseprecv(ilev)%av(sm_pr_t_),mlprec_wrk(ilev-1)%x2l, &
               & zzero,mlprec_wrk(ilev)%x2l,info)
          if(info /=0) goto 9999

        else
          !
          ! Raw  aggregation, may take shortcut
          !
          do i=1,n_row
            mlprec_wrk(ilev)%x2l(baseprecv(ilev)%mlia(i)) = &
                 & mlprec_wrk(ilev)%x2l(baseprecv(ilev)%mlia(i)) + &
                 & mlprec_wrk(ilev-1)%x2l(i)
          end do
        end if

        if (baseprecv(ilev)%iprcparm(coarse_mat_)==mat_repl_) Then
          call psb_sum(ictxt,mlprec_wrk(ilev)%x2l(1:nrg))
        else if (baseprecv(ilev)%iprcparm(coarse_mat_) /= mat_distr_) Then
          write(0,*) 'Unknown value for baseprecv(2)%iprcparm(coarse_mat_) ',&
               & baseprecv(ilev)%iprcparm(coarse_mat_)
        endif
        call psb_geaxpby(zone,mlprec_wrk(ilev)%x2l,zzero,mlprec_wrk(ilev)%tx,&
             & baseprecv(ilev)%base_desc,info)
        if(info /=0) goto 9999

      enddo


      call psb_baseprc_aply(zone,baseprecv(nlev),mlprec_wrk(nlev)%x2l, &
           & zzero, mlprec_wrk(nlev)%y2l,baseprecv(nlev)%desc_data,'N',work,info)

      if(info /=0) goto 9999


      do ilev=nlev-1, 1, -1
        ismth = baseprecv(ilev+1)%iprcparm(smth_kind_)
        if (ismth  /= no_smth_) then
          if (ismth == smth_omg_) &
               & call psb_halo(mlprec_wrk(ilev+1)%y2l,baseprecv(ilev+1)%desc_data,&
               &  info,work=work)
          call psb_csmm(zone,baseprecv(ilev+1)%av(sm_pr_),mlprec_wrk(ilev+1)%y2l,&
               &  zzero,mlprec_wrk(ilev)%y2l,info)
          if(info /=0) goto 9999

        else
          n_row = baseprecv(ilev)%base_desc%matrix_data(psb_n_row_)
          mlprec_wrk(ilev)%y2l(:) = zzero
          do i=1, n_row
            mlprec_wrk(ilev)%y2l(i) = mlprec_wrk(ilev)%y2l(i) + &
                 & mlprec_wrk(ilev+1)%y2l(baseprecv(ilev+1)%mlia(i))
          enddo

        end if

        call psb_spmm(-zone,baseprecv(ilev)%base_a,mlprec_wrk(ilev)%y2l,&
             &   zone,mlprec_wrk(ilev)%tx,baseprecv(ilev)%base_desc,info,work=work)

        if(info /=0) goto 9999

        call psb_baseprc_aply(zone,baseprecv(ilev),mlprec_wrk(ilev)%tx,&
             & zone,mlprec_wrk(ilev)%y2l,baseprecv(ilev)%base_desc, trans, work,info)

        if(info /=0) goto 9999

      enddo

      call psb_geaxpby(alpha,mlprec_wrk(1)%y2l,beta,y,baseprecv(1)%base_desc,info)

      if(info /=0) goto 9999


    case(pre_smooth_)


      !
      !    Pre smoothing.
      !    1.   X(1)  = Xext
      !    2.   Y(1)  = (K(1)**(-1))*X(1)
      !    3.   TX(1) = X(1) - A(1)*Y(1)
      !    4.   DO ILEV=2, NLEV
      !          X(ILEV) = AV(PR_SM_T_,ILEV)*TX(ILEV-1)
      !          Y(ILEV) = (K(ILEV)**(-1))*X(ILEV)
      !          TX(ILEV) = (X(ILEV)-A(ILEV)*Y(ILEV))
      !    5.   DO  ILEV=NLEV-1,1,-1
      !          Y(ILEV) = Y(ILEV) + AV(PR_SM_,ILEV+1)*Y(ILEV+1)
      !    6.  Yext    = beta*Yext + Y(1)
      !
      !    Note: level numbering reversed wrt ref. DD, i.e.
      !         1..NLEV <=>  (j) <-> 0
      !
      !

      n_col = desc_data%matrix_data(psb_n_col_)
      nr2l  = baseprecv(1)%desc_data%matrix_data(psb_n_col_)

      allocate(mlprec_wrk(1)%x2l(nr2l),mlprec_wrk(1)%y2l(nr2l), &
           & mlprec_wrk(1)%tx(nr2l), stat=info)
      if (info /= 0) then
        call psb_errpush(4010,name,a_err='Allocate')
        goto 9999
      end if

      mlprec_wrk(1)%y2l(:) = zzero


      mlprec_wrk(1)%x2l(:) = x

      call psb_baseprc_aply(zone,baseprecv(1),mlprec_wrk(1)%x2l,&
           &  zzero,mlprec_wrk(1)%y2l,&
           &  baseprecv(1)%base_desc,&
           &  trans,work,info)

      if(info /=0) goto 9999

      mlprec_wrk(1)%tx = mlprec_wrk(1)%x2l

      call psb_spmm(-zone,baseprecv(1)%base_a,mlprec_wrk(1)%y2l,&
           & zone,mlprec_wrk(1)%tx,baseprecv(1)%base_desc,info,work=work)
      if(info /=0) goto 9999

      do ilev = 2, nlev
        n_row = baseprecv(ilev-1)%base_desc%matrix_data(psb_n_row_)
        n_col = baseprecv(ilev-1)%desc_data%matrix_data(psb_n_col_)
        nr2l  = baseprecv(ilev)%desc_data%matrix_data(psb_n_col_)
        nrg   = baseprecv(ilev)%desc_data%matrix_data(psb_n_row_)
        ismth = baseprecv(ilev)%iprcparm(smth_kind_)
        allocate(mlprec_wrk(ilev)%tx(nr2l),mlprec_wrk(ilev)%y2l(nr2l),&
             &   mlprec_wrk(ilev)%x2l(nr2l), stat=info)


        if (info /= 0) then
          call psb_errpush(4010,name,a_err='Allocate')
          goto 9999
        end if

        mlprec_wrk(ilev)%x2l(:) = zzero
        mlprec_wrk(ilev)%y2l(:) = zzero
        mlprec_wrk(ilev)%tx(:) = zzero


        if (ismth  /= no_smth_) then
          !
          !Smoothed Aggregation
          !
          if (baseprecv(ilev)%iprcparm(glb_smth_) >0) then

            call psb_halo(mlprec_wrk(ilev-1)%tx,baseprecv(ilev-1)%base_desc,&
                 & info,work=work)
            if(info /=0) goto 9999
          else
            mlprec_wrk(ilev-1)%tx(n_row+1:max(n_row,n_col)) = zzero
          end if

          call psb_csmm(zone,baseprecv(ilev)%av(sm_pr_t_),mlprec_wrk(ilev-1)%tx,zzero,&
               & mlprec_wrk(ilev)%x2l,info)
          if(info /=0) goto 9999

        else
          !
          ! Raw  aggregation, may take shortcuts
          !
          mlprec_wrk(ilev)%x2l = zzero
          do i=1,n_row
            mlprec_wrk(ilev)%x2l(baseprecv(ilev)%mlia(i)) = &
                 & mlprec_wrk(ilev)%x2l(baseprecv(ilev)%mlia(i)) + &
                 &  mlprec_wrk(ilev-1)%tx(i)
          end do
        end if

        if (baseprecv(ilev)%iprcparm(coarse_mat_)==mat_repl_) then
          call psb_sum(ictxt,mlprec_wrk(ilev)%x2l(1:nrg))
        else if (baseprecv(ilev)%iprcparm(coarse_mat_) /= mat_distr_) then
          write(0,*) 'Unknown value for baseprecv(2)%iprcparm(coarse_mat_) ',&
               & baseprecv(ilev)%iprcparm(coarse_mat_)
        endif


        call psb_baseprc_aply(zone,baseprecv(ilev),mlprec_wrk(ilev)%x2l,&
             & zzero,mlprec_wrk(ilev)%y2l,baseprecv(ilev)%desc_data, 'N',work,info)

        if(info /=0) goto 9999

        if(ilev < nlev) then
          mlprec_wrk(ilev)%tx = mlprec_wrk(ilev)%x2l
          call psb_spmm(-zone,baseprecv(ilev)%base_a,mlprec_wrk(ilev)%y2l,&
               & zone,mlprec_wrk(ilev)%tx,baseprecv(ilev)%base_desc,info,work=work)
          if(info /=0) goto 9999
        endif

      enddo

      do ilev = nlev-1, 1, -1

        ismth=baseprecv(ilev+1)%iprcparm(smth_kind_)

        if (ismth  /= no_smth_) then

          if (ismth == smth_omg_) &
               & call psb_halo(mlprec_wrk(ilev+1)%y2l,&
               & baseprecv(ilev+1)%desc_data,info,work=work)
          call psb_csmm(zone,baseprecv(ilev+1)%av(sm_pr_),mlprec_wrk(ilev+1)%y2l,&
               & zone,mlprec_wrk(ilev)%y2l,info)

          if(info /=0) goto 9999

        else

          n_row = baseprecv(ilev+1)%base_desc%matrix_data(psb_n_row_)
          do i=1, n_row
            mlprec_wrk(ilev)%y2l(i) = mlprec_wrk(ilev)%y2l(i) + &
                 & mlprec_wrk(ilev+1)%y2l(baseprecv(ilev+1)%mlia(i))
          enddo

        end if

      enddo

      call psb_geaxpby(alpha,mlprec_wrk(1)%y2l,beta,y,&
           &  baseprecv(1)%base_desc,info)

      if(info /=0) goto 9999



    case(smooth_both_)

      !
      !    Symmetrized  smoothing.
      !    1.   X(1)  = Xext
      !    2.   Y(1)  = (K(1)**(-1))*X(1)
      !    3.   TX(1) = X(1) - A(1)*Y(1)
      !    4.   DO ILEV=2, NLEV
      !          X(ILEV) = AV(PR_SM_T_,ILEV)*TX(ILEV-1)
      !          Y(ILEV) = (K(ILEV)**(-1))*X(ILEV)
      !          TX(ILEV) = (X(ILEV)-A(ILEV)*Y(ILEV))
      !    5.   DO  ILEV=NLEV-1,1,-1
      !          Y(ILEV) = Y(ILEV) + AV(PR_SM_,ILEV+1)*Y(ILEV+1)
      !          Y(ILEV) = Y(ILEV) + (K(ILEV)**(-1))*(X(ILEV)-A(ILEV)*Y(ILEV))
      !    6.  Yext    = beta*Yext + Y(1)
      !
      !    Note: level numbering reversed wrt ref. DD, i.e.
      !         1..NLEV <=>  (j) <-> 0
      !
      !
      n_col = desc_data%matrix_data(psb_n_col_)
      nr2l  = baseprecv(1)%desc_data%matrix_data(psb_n_col_)

      allocate(mlprec_wrk(1)%x2l(nr2l),mlprec_wrk(1)%y2l(nr2l), &
           & mlprec_wrk(1)%ty(nr2l), mlprec_wrk(1)%tx(nr2l), stat=info)

      mlprec_wrk(1)%x2l(:) = zzero
      mlprec_wrk(1)%y2l(:) = zzero
      mlprec_wrk(1)%tx(:) = zzero
      mlprec_wrk(1)%ty(:) = zzero


      if (info /= 0) then
        call psb_errpush(4010,name,a_err='Allocate')
        goto 9999
      end if

      call psb_geaxpby(zone,x,zzero,mlprec_wrk(1)%x2l,&
           & baseprecv(1)%base_desc,info)
      call psb_geaxpby(zone,x,zzero,mlprec_wrk(1)%tx,&
           & baseprecv(1)%base_desc,info)

      call psb_baseprc_aply(zone,baseprecv(1),mlprec_wrk(1)%x2l,&
           &  zzero,mlprec_wrk(1)%y2l,&
           &  baseprecv(1)%base_desc,&
           &  trans,work,info)

      if(info /=0) goto 9999

      mlprec_wrk(1)%ty = mlprec_wrk(1)%x2l

      call psb_spmm(-zone,baseprecv(1)%base_a,mlprec_wrk(1)%y2l,&
           & zone,mlprec_wrk(1)%ty,baseprecv(1)%base_desc,info,work=work)
      if(info /=0) goto 9999

      do ilev = 2, nlev
        n_row = baseprecv(ilev-1)%base_desc%matrix_data(psb_n_row_)
        n_col = baseprecv(ilev-1)%desc_data%matrix_data(psb_n_col_)
        nr2l  = baseprecv(ilev)%desc_data%matrix_data(psb_n_col_)
        nrg   = baseprecv(ilev)%desc_data%matrix_data(psb_n_row_)
        ismth=baseprecv(ilev)%iprcparm(smth_kind_)
        allocate(mlprec_wrk(ilev)%ty(nr2l),mlprec_wrk(ilev)%y2l(nr2l),&
             &   mlprec_wrk(ilev)%x2l(nr2l), stat=info)

        mlprec_wrk(ilev)%x2l(:) = zzero
        mlprec_wrk(ilev)%y2l(:) = zzero
        mlprec_wrk(ilev)%tx(:) = zzero
        mlprec_wrk(ilev)%ty(:) = zzero


        if (info /= 0) then
          call psb_errpush(4010,name,a_err='Allocate')
          goto 9999
        end if


        if (ismth  /= no_smth_) then
          !
          !Smoothed Aggregation
          !
          if (baseprecv(ilev)%iprcparm(glb_smth_) >0) then

            call psb_halo(mlprec_wrk(ilev-1)%ty,baseprecv(ilev-1)%base_desc,&
                 & info,work=work)
            if(info /=0) goto 9999
          else
            mlprec_wrk(ilev-1)%ty(n_row+1:max(n_row,n_col)) = zzero
          end if

          call psb_csmm(zone,baseprecv(ilev)%av(sm_pr_t_),mlprec_wrk(ilev-1)%ty,zzero,&
               & mlprec_wrk(ilev)%x2l,info)
          if(info /=0) goto 9999

        else
          !
          ! Raw  aggregation, may take shortcuts
          !
          mlprec_wrk(ilev)%x2l = zzero
          do i=1,n_row
            mlprec_wrk(ilev)%x2l(baseprecv(ilev)%mlia(i)) = &
                 & mlprec_wrk(ilev)%x2l(baseprecv(ilev)%mlia(i)) + &
                 &  mlprec_wrk(ilev-1)%ty(i)
          end do
        end if

        if (baseprecv(ilev)%iprcparm(coarse_mat_)==mat_repl_) then
          call psb_sum(ictxt,mlprec_wrk(ilev)%x2l(1:nrg))
        else if (baseprecv(ilev)%iprcparm(coarse_mat_) /= mat_distr_) then
          write(0,*) 'Unknown value for baseprecv(2)%iprcparm(coarse_mat_) ',&
               & baseprecv(ilev)%iprcparm(coarse_mat_)
        endif

        call psb_geaxpby(zone,mlprec_wrk(ilev)%x2l,zzero,mlprec_wrk(ilev)%tx,&
             & baseprecv(ilev)%base_desc,info)
        if(info /=0) goto 9999

        call psb_baseprc_aply(zone,baseprecv(ilev),mlprec_wrk(ilev)%x2l,&
             & zzero,mlprec_wrk(ilev)%y2l,baseprecv(ilev)%desc_data, 'N',work,info)

        if(info /=0) goto 9999

        if(ilev < nlev) then
          mlprec_wrk(ilev)%ty = mlprec_wrk(ilev)%x2l
          call psb_spmm(-zone,baseprecv(ilev)%base_a,mlprec_wrk(ilev)%y2l,&
               & zone,mlprec_wrk(ilev)%ty,baseprecv(ilev)%base_desc,info,work=work)
          if(info /=0) goto 9999
        endif

      enddo


      do ilev=nlev-1, 1, -1

        ismth=baseprecv(ilev+1)%iprcparm(smth_kind_)
        if (ismth  /= no_smth_) then
          if (ismth == smth_omg_) &
               & call psb_halo(mlprec_wrk(ilev+1)%y2l,baseprecv(ilev+1)%desc_data,&
               &  info,work=work)
          call psb_csmm(zone,baseprecv(ilev+1)%av(sm_pr_),mlprec_wrk(ilev+1)%y2l,&
               &  zone,mlprec_wrk(ilev)%y2l,info)
          if(info /=0) goto 9999

        else
          n_row = baseprecv(ilev)%base_desc%matrix_data(psb_n_row_)
          do i=1, n_row
            mlprec_wrk(ilev)%y2l(i) = mlprec_wrk(ilev)%y2l(i) + &
                 & mlprec_wrk(ilev+1)%y2l(baseprecv(ilev+1)%mlia(i))
          enddo

        end if

        call psb_spmm(-zone,baseprecv(ilev)%base_a,mlprec_wrk(ilev)%y2l,&
             &   zone,mlprec_wrk(ilev)%tx,baseprecv(ilev)%base_desc,info,work=work)

        if(info /=0) goto 9999

        call psb_baseprc_aply(zone,baseprecv(ilev),mlprec_wrk(ilev)%tx,&
             & zone,mlprec_wrk(ilev)%y2l,baseprecv(ilev)%base_desc, trans, work,info)

        if(info /=0) goto 9999

      enddo

      call psb_geaxpby(alpha,mlprec_wrk(1)%y2l,beta,y,&
           &   baseprecv(1)%base_desc,info)

      if(info /=0) goto 9999


    case default

      call psb_errpush(4013,name,a_err='wrong smooth_pos',&
           &  i_Err=(/baseprecv(2)%iprcparm(smth_pos_),0,0,0,0/))
      goto 9999

    end select

  case default
    call psb_errpush(4013,name,a_err='wrong mltype',&
         &  i_Err=(/baseprecv(2)%iprcparm(ml_type_),0,0,0,0/))
    goto 9999

  end select


  call mlprec_wrk_free(mlprec_wrk)
  deallocate(mlprec_wrk)

  call psb_erractionrestore(err_act)
  return

9999 continue
  call psb_errpush(info,name)
  call psb_erractionrestore(err_act)
  if (err_act.eq.act_abort) then
    call psb_error()
    return
  end if
  return

contains
  subroutine mlprec_wrk_free(wrk)
    type(psb_mlprec_wrk_type) :: wrk(:)
    ! This will not be needed when we have allocatables, as
    ! it is sufficient to deallocate the container, and
    ! the compiler is supposed to recursively deallocate the
    ! various components.
    integer i

    do i=1, size(wrk)
      if (associated(wrk(i)%tx))  deallocate(wrk(i)%tx)
      if (associated(wrk(i)%ty))  deallocate(wrk(i)%ty)
      if (associated(wrk(i)%x2l)) deallocate(wrk(i)%x2l)
      if (associated(wrk(i)%y2l)) deallocate(wrk(i)%y2l)
      if (associated(wrk(i)%b2l)) deallocate(wrk(i)%b2l)
      if (associated(wrk(i)%tty)) deallocate(wrk(i)%tty)
    end do
  end subroutine mlprec_wrk_free

end subroutine psb_zmlprc_aply

