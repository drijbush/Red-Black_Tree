Module RBtree_module

  Use numbers_module, Only : wp

  Implicit None

  Public :: RBtree
  Public :: RBtree_iterator
  Public :: RBtree_LEFT
  Public :: RBtree_RIGHT

  Public :: RBtree_init
  Public :: RBtree_insert
  Public :: RBtree_delete
  Public :: RBtree_print
  Public :: RBtree_size
  Public :: RBtree_memory
  Public :: RBtree_depth
  Public :: RBtree_search
  Public :: RBtree_leftmost
  Public :: RBtree_rightmost
  Public :: RBtree_init_iterate
  Public :: RBtree_iterate
  Public :: RBtree_free
  Public :: RBtree_copy_structure

  Private

  Integer, Parameter :: RBtree_LEFT  = -1
  Integer, Parameter :: RBtree_RIGHT = +1

  Integer, Parameter :: COLOURLESS = -1000

  Integer, Parameter :: RED   = 0
  Integer, Parameter :: BLACK = 1

  Integer, Parameter :: RBtree_KEY_NOT_FOUND = -1

  Type node
     Private
     Integer   , Dimension( :    ), Pointer :: key
     Integer                                :: colour = COLOURLESS
     Type( node )                 , Pointer :: left   => Null()
     Type( node )                 , Pointer :: right  => Null()
     Type( node )                 , Pointer :: p      => Null()
     Real( wp ), Dimension( :, : ), Pointer :: data
  End Type node

  Type( node ), Save, Target :: NIL

  Type RBtree 
     Private
     Type( node ), Pointer :: root => Null()
  End Type RBtree

  Type RBtree_iterator
     Private
     Integer               :: over_edge
     Type( node ), Pointer :: root
     Type( node ), Pointer :: current
  End Type RBtree_iterator

  Interface RBtree_init_iterate
     Module Procedure init_iterate_end
     Module Procedure init_iterate_point
  End Interface

  Interface Operator( .keylessthan. )
     Module Procedure keylessthan
  End Interface

  Interface Operator( .keyequal. )
     Module Procedure keyequal
  End Interface

Contains

  Subroutine RBtree_init( T )

    Type( RBtree ), Intent( Out ) :: T

    NIL = node( Null(), BLACK, Null(), Null(), Null(), Null() )

    T%root => NIL

  End Subroutine RBtree_init

  Subroutine RBtree_insert( key, data, T, error )

    Integer   , Dimension( :    ), Intent( In    ) :: key
    Real( wp ), Dimension( :, : ), Intent( In    ) :: data
    Type( RBtree )               , Intent( InOut ) :: T
    Integer                      , Intent(   Out ) :: error

    Type( node ), Pointer :: x
    Type( node ), Pointer :: y
    Type( node ), Pointer :: z

    error = 0

    Call node_create( key, data, z, error )
    If( error /= 0 ) Then
       Return
    End If

    y => NIL
    x => T%root

    ! Insert node

    Do While( .Not. Associated( x, NIL ) )
       y => x
       If( z%key .keylessthan. x%key ) Then
          x => x%left
       Else
          x => x%right
       End If
    End Do

    z%p => y

    If( Associated( y, NIL ) ) Then
       t%root => z
    Else
       If( z%key .keylessthan. y%key ) Then
          y%left => z
       Else
          y%right => z
       End If
    End If

    z%left   => NIL
    z%right  => NIL
    z%colour =  RED

    ! Start fixup of tree properties
    Do While( z%p%colour == RED )
       If( Associated( z%p, z%p%p%left ) ) Then
          y => z%p%p%right
          If( y%colour == RED ) Then
             z%p%colour   = BLACK
             y%colour     = BLACK
             z%p%p%colour = RED
             z => z%p%p
          Else
             If( Associated( z, z%p%right ) ) Then
                z => z%p
                Call left_rotate( T, z )
             End If
             z%p%colour   = BLACK
             z%p%p%colour = RED
             Call right_rotate( T, z%p%p )
          End If
       Else
          y => z%p%p%left
          If( y%colour == RED ) Then
             z%p%colour   = BLACK
             y%colour     = BLACK
             z%p%p%colour = RED
             z => z%p%p
          Else
             If( Associated( z, z%p%left ) ) Then
                z => z%p
                Call right_rotate( T, z )
             End If
             z%p%colour   = BLACK
             z%p%p%colour = RED
             Call left_rotate( T, z%p%p )
          End If
       End If
    End Do

    T%root%colour = BLACK

  End Subroutine RBtree_insert

  Subroutine RBtree_delete( key, T, error )

    Integer   , Dimension( : ), Intent( In    ) :: key
    Type( RBtree )            , Intent( InOut ) :: T
    Integer                   , Intent(   Out ) :: error

    Type( node ), Pointer :: w
    Type( node ), Pointer :: x
    Type( node ), Pointer :: y
    Type( node ), Pointer :: z

    error = 0

    z => set_current_at_point( T%root, key )

    key_found: If( .Not. Associated( z, NIL ) ) Then

       If( Associated( z%left, NIL ) .Or. Associated( z%right, NIL ) ) Then
          y => z
       Else
          If( .Not. Associated( z%right, NIL ) ) Then
             y => z%right
             Call tree_minimum( y )
          Else
             y => z%p
             w => z
             Do While( .Not. Associated( y, NIL ) .And. Associated( w, y%right ) )
                w => y
                y => y%p
             End Do
          End If
       End If

       If( .Not. Associated( y%left, NIL ) ) Then
          x => y%left
       Else
          x => y%right
       End If

       x%p => y%p

       If( Associated( y%p, NIL ) ) Then
          T%root => x
       Else 
          If( Associated( y, y%p%left ) ) Then
             y%p%left  => x
          Else
             y%p%right => x
          End If
       End If

       If( .Not. Associated( y, z ) ) Then
          Deallocate( z%key )
          Allocate( z%key( 1:Size( y%key ) ), Stat = error )
          If( error /= 0 ) Then
             Return
          End If
          z%key = y%key
          Deallocate( z%data )
          Allocate( z%data( 1:Size( y%data, Dim = 1 ), 1:Size( y%data, Dim = 2 ) ), Stat = error )
          If( error /= 0 ) Then
             Return
          End If
          z%data = y%data
       End If

       If( y%colour == BLACK ) Then
          Do While( .Not. Associated( x, T%root ) .And. x%colour == BLACK )

             If( Associated( x, x%p%left ) ) Then

                w => x%p%right
                If( w%colour == RED ) Then
                   w%colour   = BLACK
                   x%p%colour = RED
                   Call left_rotate( T, x%p )
                   w => x%p%right
                End If
                If( w%left%colour == BLACK .And. w%right%colour == BLACK ) Then
                   w%colour =  RED
                   x        => x%p
                Else
                   If( w%right%colour == BLACK ) Then
                      w%left%colour = BLACK
                      w%colour      = RED
                      Call right_rotate( T, w )
                      w => x%p%right
                   End If
                   w%colour   = x%p%colour
                   x%p%colour = BLACK
                   w%right%colour = BLACK
                   Call left_rotate( T, x%p )
                   x => T%root
                End If

             Else

                w => x%p%left
                If( w%colour == RED ) Then
                   w%colour   = BLACK
                   x%p%colour = RED
                   Call right_rotate( T, x%p )
                   w => x%p%left
                End If
                If( w%right%colour == BLACK .And. w%left%colour == BLACK ) Then
                   w%colour =  RED
                   x        => x%p
                Else
                   If( w%left%colour == BLACK ) Then
                      w%right%colour = BLACK
                      w%colour      = RED
                      Call left_rotate( T, w )
                      w => x%p%left
                   End If
                   w%colour   = x%p%colour
                   x%p%colour = BLACK
                   w%left%colour = BLACK
                   Call right_rotate( T, x%p )
                   x => T%root
                End If

             End If

          End Do

          x%colour = BLACK

       End If

       Deallocate( y%key  )
       Deallocate( y%data )
       Deallocate( y )

    Else

       error = RBtree_KEY_NOT_FOUND

    End If key_found

  End Subroutine RBtree_delete

  Subroutine RBtree_print( T )

    Type( RBtree ), Intent( In ) :: T

    Integer :: depth, max_depth

    depth     =  0
    max_depth = -1

    Call tree_print( T%root )
    Write( *, * )
    Write( *, * ) 'Maximum depth: ', max_depth
    Write( *, * )

  Contains

    Recursive Subroutine tree_print( x )

      Type( node ), Pointer :: x

      Integer :: i

      If( .Not. Associated( x, NIL ) ) Then
         depth = depth + 1
         max_depth = Max( depth, max_depth )
         Call tree_print( x%left  )
         Write( *, '( 4( i0, 1x ) )' ) x%key
         Do i = 1, Size( x%data, Dim = 1 )
            Write( *, '( 100( es18.10, 1x ) )' ) x%data( i, : )
         End Do
         Call tree_print( x%right )
         depth = depth - 1
      End If

    End Subroutine tree_print

  End Subroutine RBtree_print

  Function RBtree_size( T ) Result( r )

    Integer :: r

    Type( RBtree ), Intent( In ) :: T

    r = 0

    Call tree_size( T%root )

  Contains

    Recursive Subroutine tree_size( x )

      Type( node ), Pointer :: x

      If( .Not. Associated( x, NIL ) ) Then
         Call tree_size( x%left  )
         r = r + 1
         Call tree_size( x%right )
      End If

    End Subroutine tree_size

  End Function RBtree_size

  Function RBtree_memory( T ) Result( r )

    Integer :: r

    Type( RBtree ), Intent( In ) :: T

    r = 0

    Call tree_memory( T%root )

  Contains

    Recursive Subroutine tree_memory( x )

      Type( node ), Pointer :: x

      If( .Not. Associated( x, NIL ) ) Then
         Call tree_memory( x%left  )
         r = r + Size( x%data )
         Call tree_memory( x%right )
      End If

    End Subroutine tree_memory

  End Function RBtree_memory

  Function RBtree_depth( T ) Result( r )

    Integer :: r

    Type( RBtree ), Intent( In ) :: T

    Integer :: depth, max_depth

    depth     =  0
    max_depth = -1

    Call tree_depth( T%root )

    r = max_depth

  Contains

    Recursive Subroutine tree_depth( x )

      Type( node ), Pointer :: x

      If( .Not. Associated( x, NIL ) ) Then
         depth = depth + 1
         max_depth = Max( depth, max_depth )
         Call tree_depth( x%left  )
         Call tree_depth( x%right )
         depth = depth - 1
      End If

    End Subroutine tree_depth

  End Function RBtree_depth

  Subroutine RBtree_free( T )

    Type( RBtree ), Intent( InOut ) :: T

    Call tree_free( T%root )

  Contains

    Recursive Subroutine tree_free( x )

      Type( node ), Pointer :: x

      Type( node ), Pointer :: y

      If( .Not. Associated( x, NIL ) ) Then
         Call tree_free( x%left  )
         Call tree_free( x%right )
      End If

      If( Associated( x%left , NIL ) .And. &
          Associated( x%right, NIL ) ) Then

         Deallocate( x%key  )
         Deallocate( x%data )
         
         y => x%p

         If( Associated( x%p%left, x ) ) Then
            Deallocate( x )
            y%left  => NIL
         Else
            Deallocate( x )
            y%right => NIL
         End If

      End If

    End Subroutine tree_free

  End Subroutine RBtree_free

  Function RBtree_search( T, k ) Result( data )

    Real( wp ), Dimension( :, : ), Pointer :: data

    Type( RBtree )            , Intent( In ) :: T
    Integer   , Dimension( : ), Intent( In ) :: k

    Type( node ), Pointer :: x

    x => search( T%root, k )

    If( .Not. Associated( x, NIL ) ) Then
       data => x%data
    Else
       data => Null()
    End If

  End Function RBtree_search

  Function RBtree_leftmost( T ) Result( data )

    Real( wp ), Dimension( :, : ), Pointer :: data

    Type( RBtree ), Intent( In ) :: T

    Type( node ), Pointer :: x

    x => T%root

    Call tree_minimum( x )

    data => x%data

  End Function RBtree_leftmost

  Function RBtree_rightmost( T ) Result( data )

    Real( wp ), Dimension( :, : ), Pointer :: data

    Type( RBtree ), Intent( In ) :: T

    Type( node ), Pointer :: x

    x => T%root

    Call tree_maximum( x )

    data => x%data

  End Function RBtree_rightmost

  Subroutine RBtree_iterate( direction, i, k, data ) 

    Integer                , Intent( In    ) :: direction
    Type( RBtree_iterator ), Intent( InOut ) :: i
    Integer   , Dimension( :    ), Pointer   :: k
    Real( wp ), Dimension( :, : ), Pointer   :: data

    Type( node ), Pointer :: y

    If( direction < 0 ) Then
       If( Associated( i%current, NIL ) ) Then
          If( i%over_edge == RBtree_RIGHT ) Then
             i%current => i%root
             Call tree_maximum( i%current )
          End If
       Else
          If( .Not. Associated( i%current%left, NIL ) ) Then
             i%current => i%current%left
             Call tree_maximum( i%current )
          Else
             y => i%current%p
             Do While( .Not. Associated( y, NIL ) .And. Associated( i%current, y%left ) )
                i%current => y
                y => y%p
             End Do
             i%current => y
          End If
       End If
       If( Associated( i%current, NIL ) ) Then
          i%over_edge = RBtree_LEFT
       Else
          i%over_edge = 0
       End If
    Else
       If( Associated( i%current, NIL ) ) Then
          If( i%over_edge == RBtree_LEFT ) Then
             i%current => i%root
             Call tree_minimum( i%current )
          End If
       Else
          If( .Not. Associated( i%current%right, NIL ) ) Then
             i%current => i%current%right
             Call tree_minimum( i%current )
          Else
             y => i%current%p
             Do While( .Not. Associated( y, NIL ) .And. Associated( i%current, y%right ) )
                i%current => y
                y => y%p
             End Do
             i%current => y
          End If
       End If
       If( Associated( i%current, NIL ) ) Then
          i%over_edge = RBtree_RIGHT
       Else
          i%over_edge = 0
       End If
    End If

    If( .Not. Associated( i%current, NIL ) ) Then
       k    => i%current%key
       data => i%current%data
    Else
       k    => Null()
       data => Null()
    End If

  End Subroutine RBtree_iterate

  Subroutine RBtree_copy_structure( Tin, Tout, error )

    Type( RBtree ), Intent( In    ) :: Tin
    Type( RBtree ), Intent(   Out ) :: Tout
    Integer       , Intent(   Out ) :: error

    Type( RBtree_iterator ) :: i

    Real( wp ), Dimension( :, : ), Pointer :: data

    Integer, Dimension( : ), Pointer :: key

    Call RBtree_init( Tout )

    Call RBtree_init_iterate( Tin, RBtree_LEFT, i, key, data )

    Do While( Associated( key ) )
       Call RBtree_insert( key, data, Tout, error )
       If( error /= 0 ) Then
          Return
       End If
       Call RBtree_iterate( RBtree_RIGHT, i, key, data )
    End Do

  End Subroutine RBtree_copy_structure

  Subroutine left_rotate( T, x )

    Type( RBtree ),         Intent( InOut ) :: T
    Type( node   ), Pointer                 :: x

    Type( node ), Pointer :: y
    Type( node ), Pointer :: z

    y       => x%right
    x%right => y%left

    If( .Not. Associated( y%left, NIL ) ) Then
       y%left%p => x
    End If

    z => x%p

    If( Associated( x%p, NIL ) ) Then
       T%root =>  y
    Else 
       If( Associated( x, x%p%left ) ) Then
          x%p%left =>  y
       Else
          x%p%right =>  y
       End If
    End If

    y%left => x
    x%p    => y
    y%p    => z

  End Subroutine left_rotate

  Subroutine right_rotate( T, x )

    Type( RBtree ),         Intent( InOut ) :: T
    Type( node   ), Pointer                 :: x

    Type( node ), Pointer :: y
    Type( node ), Pointer :: z

    y      => x%left
    x%left => y%right

    If( .Not. Associated( y%right, NIL ) ) Then
       y%right%p => x
    End If

    z => x%p

    If( Associated( x%p, NIL ) ) Then
       T%root =>  y
    Else 
       If( Associated( x, x%p%right ) ) Then
          x%p%right =>  y
       Else
          x%p%left =>  y
       End If
    End If

    y%right => x
    x%p     => y
    y%p     => z

  End Subroutine right_rotate

  Subroutine tree_minimum( x )

    Type( node ), Pointer :: x

    Do While( .Not. Associated( x%left, NIL ) )
       x => x%left
    End Do

  End Subroutine tree_minimum

  Subroutine tree_maximum( x ) 

    Type( node ), Pointer :: x

    Do While( .Not. Associated( x%right, NIL ) )
       x => x%right
    End Do

  End Subroutine tree_maximum

  Subroutine node_create( key, data, z, error )

    Integer     , Dimension( :    ), Intent( In    ) :: key
    Real( wp )  , Dimension( :, : ), Intent( In    ) :: data
    Type( node ), Pointer                            :: z
    Integer                        , Intent(   Out ) :: error

    Integer :: n_key
    Integer :: n1, n2

    Allocate( z, Stat = error )
    If( error /= 0 ) Then
       Return
    End If

    n_key = Size( key )
    Allocate( z%key( 1:n_key ), Stat = error )
    If( error /= 0 ) Then
       Return
    End If
    z%key = key

    n1 = Size( data, Dim = 1 )
    n2 = Size( data, Dim = 2 )
    Allocate( z%data( 1:n1, 1:n2 ), Stat = error )
    If( error /= 0 ) Then
       Return
    End If
    z%data = data

    z%left  => NIL
    z%right => NIL
    z%p     => NIL

  End Subroutine node_create

  Function search( n, k ) Result( x )

    Type( node ), Pointer :: x

    Type( node ), Target        , Intent( In ) :: n
    Integer     , Dimension( : ), Intent( In ) :: k

    x => n

    Do While( .Not. Associated( x, NIL ) )
       If( k .keyequal. x%key ) Then
          Exit
       End If
       If( k .keylessthan. x%key ) Then
          x => x%left
       Else
          x => x%right
       End If
    End Do

  End Function search

  Subroutine init_iterate_end( T, start, i, k, data ) 


    Type( RBtree          ), Intent( In    ) :: T
    Integer                , Intent( In    ) :: start
    Type( RBtree_iterator ), Intent( InOut ) :: i
    Integer   , Dimension( :    ), Pointer   :: k
    Real( wp ), Dimension( :, : ), Pointer   :: data

    If( .Not. Associated( T%root, NIL ) ) Then

       i%over_edge =  0
       i%current   => set_current_at_end( T%root, start )
       i%root      => T%root

       k    => i%current%key
       data => i%current%data

    Else

       i%over_edge =  start
       i%current   => NIL
       i%root      => T%root

       k    => Null()
       data => Null()

    End If

  End Subroutine init_iterate_end

  Function set_current_at_end( n, start ) Result( x )

    Type( node ), Pointer :: x

    Type( node ), Target, Intent( In ) :: n
    Integer             , Intent( In ) :: start

    x => n

    If( start < 0 ) Then
       Call tree_minimum( x )
    Else
       Call tree_maximum( x )
    End If

  End Function set_current_at_end

  Subroutine init_iterate_point( T, k, i, data )


    Type( RBtree          ), Intent( In    ) :: T
    Integer, Dimension( : ), Intent( In    ) :: k
    Type( RBtree_iterator ), Intent( InOut ) :: i
    Real( wp ), Dimension( :, : ), Pointer   :: data

    i%over_edge =  0
    i%current   => set_current_at_point( T%root, k )
    i%root      => T%root

    data => i%current%data

  End Subroutine init_iterate_point

  Function set_current_at_point( n, k ) Result( x )

    Type( node ), Pointer :: x

    Type( node )                , Intent( In ) :: n
    Integer     , Dimension( : ), Intent( In ) :: k

    x => search( n, k )

  End Function set_current_at_point

  Function keylessthan( k1, k2 ) Result( r )

    Logical :: r

    Integer, Dimension( : ), Intent( In ) :: k1
    Integer, Dimension( : ), Intent( In ) :: k2

    Integer :: s1, s2, s_min
    Integer :: i

    s1 = Size( k1 )
    s2 = Size( k2 )
    
    s_min = Min( s1, s2 )
    
    Do i = 1, s_min
       If( k1( i ) < k2( i ) ) Then
          r = .True.
          Return
       Else If(  k1( i ) > k2( i ) ) Then
          r = .False.
          Return
       End If
    End Do

    If( s1 >= s2 ) Then
       r = .False.
    Else
       r = .True.
    End If

  End Function keylessthan

  Function keyequal( k1, k2 ) Result( r )

    Logical :: r

    Integer, Dimension( : ), Intent( In ) :: k1
    Integer, Dimension( : ), Intent( In ) :: k2

    Integer :: s1, s2
    Integer :: i

    s1 = Size( k1 )
    s2 = Size( k2 )

    If( s1 /= s2 ) Then
       r = .False.
       Return
    End If
    
    Do i = 1, s1
       If( k1( i ) /= k2( i ) ) Then
          r = .False.
          Return
       End If
    End Do

    r = .True.

  End Function keyequal

End Module RBtree_module

