program main
  integer, parameter :: mykind = selected_real_kind()
  print *, 1
  print *, 1_mykind
  print *, 1.
  print *, 1._mykind
  print *, (1., 1._mykind)
end program main
