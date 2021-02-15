(* LIFO Storage Library
 *
 *  @file LIFO.mod
 *  LIFO implementation
 *
 *  Universal Dynamic Stack
 *
 *  Author: Benjamin Kowarsch
 *
 *  Copyright (C) 2009 Benjamin Kowarsch. All rights reserved.
 *
 *  License:
 *
 *  Redistribution  and  use  in source  and  binary forms,  with  or  without
 *  modification, are permitted provided that the following conditions are met
 *
 *  1) NO FEES may be charged for the provision of the software.  The software
 *     may  NOT  be published  on websites  that contain  advertising,  unless
 *     specific  prior  written  permission has been obtained.
 *
 *  2) Redistributions  of source code must retain the above copyright notice,
 *     this list of conditions and the following disclaimer.
 *
 *  3) Redistributions  in binary form  must  reproduce  the  above  copyright
 *     notice,  this list of conditions  and  the following disclaimer  in the
 *     documentation and other materials provided with the distribution.
 *
 *  4) Neither the author's name nor the names of any contributors may be used
 *     to endorse  or  promote  products  derived  from this software  without
 *     specific prior written permission.
 *
 *  5) Where this list of conditions  or  the following disclaimer, in part or
 *     as a whole is overruled  or  nullified by applicable law, no permission
 *     is granted to use the software.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING,  BUT NOT LIMITED TO,  THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY  AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT  SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
 * LIABLE  FOR  ANY  DIRECT,  INDIRECT,  INCIDENTAL,  SPECIAL,  EXEMPLARY,  OR
 * CONSEQUENTIAL  DAMAGES  (INCLUDING,  BUT  NOT  LIMITED  TO,  PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES;  LOSS OF USE,  DATA,  OR PROFITS; OR BUSINESS
 * INTERRUPTION)  HOWEVER  CAUSED  AND ON ANY THEORY OF LIABILITY,  WHETHER IN
 * CONTRACT,  STRICT LIABILITY,  OR TORT  (INCLUDING NEGLIGENCE  OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,  EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 *  
 *)


IMPLEMENTATION (* OF *) MODULE LIFO;

FROM SYSTEM IMPORT ADDRESS, ADR, TSIZE;
FROM Storage IMPORT ALLOCATE, DEALLOCATE;


(* ---------------------------------------------------------------------------
// Private type : ListEntry
// ---------------------------------------------------------------------------
*)
TYPE ListPtr = POINTER TO ListEntry;

TYPE ListEntry = RECORD
    value : DataPtr;
    next  : ListPtr
END; (* ListEntry *)


(* ---------------------------------------------------------------------------
// Opaque type : LIFO.Stack
// ---------------------------------------------------------------------------
// CAUTION:  Modula-2 does not support the use of variable length array fields
// in records.  VLAs can  only  be implemented  using pointer arithmetic which
// means  there is  no type checking  and  no boundary checking  on the array.
// It also means that  array notation cannot be used on the array  which makes
// the code  difficult to read  and maintain.  As a result,  Modula-2  is less
// safe and less readable than C when it comes to using VLAs.  Great care must
// be taken to make sure that the code accessing VLA fields is safe.  Boundary
// checks must be inserted manually.  Size checks must be inserted manually to
// compensate for the absence of type checks. *)

TYPE Stack = POINTER TO StackDescriptor;

TYPE StackDescriptor = RECORD
    overflow   : ListPtr;
    entryCount : StackSize;
    arraySize  : StackSize;
    array      : ADDRESS (* ARRAY OF DataPtr *)
END; (* StackDescriptor *)


(* ---------------------------------------------------------------------------
// function:  LIFO.new( initial_size, status )
// ---------------------------------------------------------------------------
//
// Creates  and  returns  a new LIFO stack object  with an initial capacity of
// <initialSize>.  If  zero  is passed in  for <initialSize>,  then  the stack
// will be created  with an  initial capacity  of  LIFO.defaultStackSize.  The
// function fails  if a value greater than   LIFO.maximumStackSize  is  passed
// in  for <initialSize> or if memory could not be allocated.
//
// The initial capacity of a stack is the number of entries that can be stored
// in the stack without enlargement.
//
// The status of the operation  is passed back in <status>. *)

PROCEDURE new ( initialSize : StackSize; VAR status : Status ) : Stack;

VAR
    newStack : Stack;
    
BEGIN

    (* zero size means default *)
    IF initialSize = 0 THEN
        initialSize := defaultStackSize;
    END; (* IF *)
    
    (* bail out if initial size is too high *)
    IF initialSize > maximumStackSize THEN
        status := invalidSize;
        RETURN NIL;
    END; (* IF *)
    
    (* allocate new stack object *)
    ALLOCATE(newStack, TSIZE(Stack) + TSIZE(DataPtr) * (initialSize - 1));
    
    (* bail out if allocation failed *)
    IF newStack = NIL THEN
        status := allocationFailed;
        RETURN NIL;
    END; (* IF *)
        
    (* initialise meta data *)
    newStack^.arraySize := initialSize;
    newStack^.entryCount := 0;
    newStack^.overflow := NIL;
    
    (* pass status and new stack to caller *)
    status := success;
    RETURN newStack
    
END new;


(* ---------------------------------------------------------------------------
// function:  LIFO.push( stack, value, status )
// ---------------------------------------------------------------------------
//
// Adds a  new entry <value>  to the top of stack <stack>.  The  new entry  is
// added by reference,  no data is copied.  However,  no entry is added if the
// the stack is full,  that is  when the number of entries stored in the stack
// has reached LIFO.maximumStackSize.  The function fails  if NIL is passed in
// for <stack> or <value>,  or if memory could not be allocated.
//
// New entries are allocated dynamically  if the number of entries exceeds the
// initial capacity of the stack.
//
// The status of the operation is passed back in <status>. *)

PROCEDURE push ( VAR stack : Stack; value : DataPtr; VAR status : Status );
VAR
    newEntry : ListPtr;
    valuePtr : POINTER TO DataPtr;

BEGIN

    (* bail out if stack is NIL *)
    IF stack = NIL THEN
        status := invalidStack;
        RETURN;
    END; (* IF *)
    
    (* bail out if value is NIL *)
    IF value = NIL THEN
        status := invalidData;
        RETURN;
    END; (* IF *)

    (* bail out if stack is full *)
    IF stack^.entryCount >= maximumStackSize THEN
        status := stackFull;
        RETURN;
    END; (* IF *)

    (* check if index falls within array segment *)
    IF stack^.entryCount < stack^.arraySize THEN
    
        (* store value in array segment *)
        
        (* stack^.array^[stack^.entryCount] := value; *)
        valuePtr := ADR(stack^.array) + TSIZE(DataPtr) * stack^.entryCount;
        valuePtr^ := value;
        
    ELSE (* index falls within overflow segment *)
    
        (* allocate new entry slot *)
        NEW(newEntry);
        
        (* bail out if allocation failed *)
        IF newEntry = NIL THEN
            status := allocationFailed;
            RETURN;
        END; (* IF *)
        
        (* initialise new entry *)
        newEntry^.value := value;
        
        (* link new entry into overflow list *)
        newEntry^.next := stack^.overflow;
        stack^.overflow := newEntry;
    
    END; (* IF *)
    
    (* update entry counter *)
    INC(stack^.entryCount);
    
    (* pass status to caller *)
    status := success;
    RETURN

END push;


(* ---------------------------------------------------------------------------
// function:  LIFO.pop( stack, status )
// ---------------------------------------------------------------------------
//
// Removes the top most value from stack <stack> and returns it.  If the stack
// is empty,  that  is  when the  number  of  entries  stored in the stack has
// reached zero,  then NIL is returned.
//
// Entries which were allocated dynamically (above the initial capacity) are
// deallocated when their values are popped.
//
// The status of the operation is passed back in <status>. *)

PROCEDURE pop ( VAR stack : Stack; VAR status : Status ) : DataPtr;

VAR
    thisValue : DataPtr;
    thisEntry : ListPtr;
    valuePtr : POINTER TO DataPtr;

BEGIN

    (* bail out if stack is NIL *)
    IF stack = NIL THEN
        status := invalidStack;
        RETURN NIL;
    END; (* IF *)
    
    (* bail out if stack is empty *)
    IF stack^.entryCount = 0 THEN
        status := stackEmpty;
        RETURN NIL;
    END; (* IF *)

    DEC(stack^.entryCount);
    
    (* check if index falls within array segment *)
    IF stack^.entryCount < stack^.arraySize THEN
        
        (* obtain value at index entryCount in array segment *)
        
        (* thisValue := stack^.array^[stack^.entryCount]; *)
        valuePtr := ADR(stack^.array) + TSIZE(DataPtr) * stack^.entryCount;
        thisValue := valuePtr^;
        
    ELSE (* index falls within overflow segment *)
        
        (* obtain value of first entry in overflow list *)
        thisValue := stack^.overflow^.value;
        
        (* isolate first entry in overflow list *)
        thisEntry := stack^.overflow;
        stack^.overflow := stack^.overflow^.next;
        
        (* remove the entry from overflow list *)
        DISPOSE(thisEntry);
                
    END; (* IF *)

    (* return value and status to caller *)
    status := success;
    RETURN thisValue

END pop;


(* ---------------------------------------------------------------------------
// function:  LIFO.stackSize( stack )
// ---------------------------------------------------------------------------
//
// Returns the current capacity of <stack>.  The current capacity is the total
// number of allocated entries. Returns zero if NIL is passed in for <stack>.
*)
PROCEDURE stackSize( VAR stack : Stack ) : StackSize;

BEGIN

    (* bail out if stack is NIL *)
    IF stack = NIL THEN
        RETURN 0;
    END; (* IF *)

    IF stack^.entryCount < stack^.arraySize THEN
        RETURN stack^.arraySize;
    ELSE
        RETURN stack^.entryCount;
    END; (* IF *)
    
END stackSize;


(* ---------------------------------------------------------------------------
// function:  LIFO.stackEntries( stack )
// ---------------------------------------------------------------------------
//
// Returns  the  number of entries  stored in stack <stack>,  returns  zero if
// NIL is passed in for <stack>. *)

PROCEDURE stackEntries( VAR stack : Stack ) : StackSize;

BEGIN

    (* bail out if stack is NIL *)
    IF stack = NIL THEN
        RETURN 0;
    END; (* IF *)

    RETURN stack^.entryCount
    
END stackEntries;


(* ---------------------------------------------------------------------------
// function:  LIFO.dispose( stack )
// ---------------------------------------------------------------------------
//
// Disposes of LIFO stack object <stack>.  Returns NIL. *)

PROCEDURE dispose ( VAR stack : Stack ) : Stack;

VAR
    thisEntry : ListPtr;

BEGIN

    (* bail out if stack is NIL *)
    IF stack = NIL THEN
        RETURN NIL;
    END; (* IF *)
    
    (* deallocate any entries in stack's overflow list *)
    WHILE stack^.overflow # NIL DO
        
        (* isolate first entry in overflow list *)
        thisEntry := stack^.overflow;
        stack^.overflow := stack^.overflow^.next;
        
        (* deallocate the entry *)
        DISPOSE(thisEntry);
        
    END; (* WHILE *)
    
    (* deallocate stack object and pass NIL to caller *)
    DEALLOCATE(stack, TSIZE(Stack) + TSIZE(DataPtr) * (stack^.arraySize - 1));
    RETURN NIL

END dispose;


END LIFO.
