! Written by Rand Huso

MODULE MpiIocForBinaryTree
    USE :: MsgBase

    TYPE TreeContainerType
        TYPE( TreeContainerType ), ALLOCATABLE :: more
        TYPE( TreeContainerType ), ALLOCATABLE :: less
        CLASS( MsgBaseType ), POINTER :: item
        INTEGER :: tag
    CONTAINS
    END TYPE

    TYPE MpiIocForBinaryTreeType
        CLASS( TreeContainerType ), ALLOCATABLE :: head
    CONTAINS
        PROCEDURE :: dumpTree
        PROCEDURE :: addItem
        PROCEDURE :: getItem
        PROCEDURE, PRIVATE :: dumpTreeLine
        PROCEDURE, PRIVATE :: addItemLeaf
        PROCEDURE, PRIVATE :: getItemAt
    END TYPE

CONTAINS
    RECURSIVE SUBROUTINE getItemAt( self, item, node, tag )
        CLASS( MpiIocForBinaryTreeType ), INTENT( inout ) :: self
        CLASS( TreeContainerType ), INTENT( inout ) :: node
        INTEGER, INTENT( in ) :: tag
        CLASS( MsgBaseType ), INTENT( out ), POINTER :: item
        IF( tag == node%tag ) THEN
            item => node%item
        ELSE IF( tag < node%tag ) THEN
            IF( allocated( node%less )) THEN
                CALL self%getItemAt( item, node%less, tag )
            END IF
        ELSE IF( node%tag < tag ) THEN
            IF( allocated( node%more )) THEN
                CALL self%getItemAt( item, node%more, tag )
            END IF
        END IF
    END SUBROUTINE

    SUBROUTINE getItem( self, item, tag )
        CLASS( MpiIocForBinaryTreeType ), INTENT( inout ) :: self
        INTEGER, INTENT( in ) :: tag
        CLASS( MsgBaseType ), INTENT( out ), POINTER :: item
        IF( allocated( self%head )) THEN
            CALL self%getItemAt( item, self%head, tag )
        END IF
    END SUBROUTINE

    RECURSIVE SUBROUTINE addItemLeaf( self, node, item, tag )
        CLASS( MpiIocForBinaryTreeType ), INTENT( inout ) :: self
        CLASS( TreeContainerType ), INTENT( inout ) :: node
        CLASS( MsgBaseType ), INTENT( in ) :: item
        INTEGER, INTENT( in ) :: tag
        IF( tag < node%tag ) THEN
            IF( allocated( node%less )) THEN
                CALL self%addItemLeaf( node%less, item, tag )
            ELSE
                ALLOCATE( node%less )
                ALLOCATE( node%less%item, SOURCE=item )
                node%less%tag = tag
            END IF
        ELSE IF( node%tag < tag ) THEN
            IF( allocated( node%more )) THEN
                CALL self%addItemLeaf( node%more, item, tag )
            ELSE
                ALLOCATE( node%more )
                ALLOCATE( node%more%item, SOURCE=item )
                node%more%tag = tag
            END IF
        ELSE
            PRINT *, 'Duplication identified - ignore or replace?'
        END IF
    END SUBROUTINE

    SUBROUTINE addItem( self, item, tag )
        CLASS( MpiIocForBinaryTreeType ), INTENT( inout ) :: self
        CLASS( MsgBaseType ), INTENT( in ) :: item
        INTEGER, INTENT( in ) :: tag
        IF( .NOT. allocated( self%head )) THEN
            ALLOCATE( self%head )
            ALLOCATE( self%head%item, SOURCE=item )
            self%head%tag = tag
        ELSE
            CALL self%addItemLeaf( self%head, item, tag )
        END IF
    END SUBROUTINE

    RECURSIVE SUBROUTINE dumpTreeLine( self, item )
        CLASS( MpiIocForBinaryTreeType ), INTENT( in ) :: self
        CLASS( TreeContainerType ), INTENT( in ) :: item
        IF( allocated( item%more )) CALL self%dumpTreeLine( item%more )
        PRINT *, item%tag, item%item%str()
        IF( allocated( item%less )) CALL self%dumpTreeLine( item%less )
    END SUBROUTINE

    SUBROUTINE dumpTree( self )
        CLASS( MpiIocForBinaryTreeType ), INTENT( inout ) :: self
        PRINT *, 'MpiIocForBinaryTree::dumpTree'
        IF( allocated( self%head )) THEN
            CALL self%dumpTreeLine( self%head )
        END IF
    END SUBROUTINE
END MODULE
