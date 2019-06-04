Program test

  Use numbers_module
  Use RBtree_module

  Implicit None

  Type( RBtree ) :: T
  Type( RBtree_iterator ) :: V

  Real( wp ), Dimension( :, : ), Allocatable :: data

  Real( wp ) :: tmp

  Integer, Parameter :: maxkey = 1000000
  Integer, Parameter :: n_keys = 100000
  Integer, Dimension( 1:n_keys ) :: keys

  Integer :: n1, n2
  Integer :: i
  Integer :: error
  Integer :: tot_error

  Integer, Dimension( : ), Pointer :: k
  Real( wp ), Dimension( :, : ), Pointer :: d

  Call RBtree_init( T )
  
  Do i = 1, n_keys
     Call Random_number( tmp )
     keys( i ) = Int( tmp * maxkey ) + 1
  End Do

  Do i = 1, n_keys
     Call Random_number( tmp )
     n1 = 1 + Int( 5.0_wp * tmp )
     Call Random_number( tmp )     
     n2 = 1 + Int( 5.0_wp * tmp )
     Allocate( data( 1:n1, 1:n2 ) )
     Call Random_number( data )
     Call RBtree_insert( (/ keys( i ) /), data, T, error )
     Deallocate( data )
  End Do

  Write( *, * ) 'Size, depth, memory ', &
       RBtree_size( T ), RBtree_depth( T ), RBtree_memory( T )

!!$  Do i = 1, n_keys
!!$     Call Random_number( tmp )
!!$     keys( i ) = Int( tmp * maxkey ) + 1
!!$  End Do

  tot_error = 0
  Do i = 1, n_keys - 5
     Call RBtree_delete( (/ keys( i ) /), T, error )
     tot_error = tot_error + Merge( 1, 0, error /= 0 )
  End Do

  Call RBtree_print( T )
  Write( *, * ) 'Size, depth ', RBtree_size( T ), RBtree_depth( T )

  Call RBtree_init_iterate( T, RBtree_LEFT, V, k, d )
  i = 0
  Do While( Associated( k ) )
     i = i + 1
     Call RBtree_iterate( RBtree_RIGHT, V, k, d )
  End Do

  Write( *, * )
  Write( *, * ) maxkey, n_keys, tot_error, i

!!$  Do key = 2, 8, 3
!!$     Call Random_number( data )
!!$     Call RBtree_insert( (/ key /), data, T, error )
!!$  End Do
!!$  Do key = 3, 9, 3
!!$     Call Random_number( data )
!!$     Call RBtree_insert( (/ key /), data, T, error )
!!$  End Do
!!$  Do key = 1, 10, 3
!!$     Call Random_number( data )
!!$     Call RBtree_insert( (/ key /), data, T, error )
!!$  End Do
!!$  
!!$  Call RBtree_print( T )
!!$
!!$  Do key = 1, 9, 2
!!$     Write( *, * ) 'key = ', key
!!$     Call RBtree_delete( (/ key /), T )
!!$     Call RBtree_print( T )
!!$  End Do
!!$
!!$  Do key = 2, 10, 2
!!$     Write( *, * ) 'key = ', key
!!$     Call RBtree_delete( (/ key /), T )
!!$     Call RBtree_print( T )
!!$  End Do

  Call RBtree_free( T )
  

End Program test

