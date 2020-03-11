#include <stdio.h>
#include <stdlib.h>
#include "gc.h"
#include "types.h"

extern int* HEAP_END;

typedef struct Frame_ {
  int *sp;
  int *bp;
} Frame;

Frame caller(int* stack_bottom, Frame frame){
  Frame callerFrame;
  int *bptr = frame.bp;
  if (bptr == stack_bottom){
    return frame;
  } else {
    callerFrame.sp = bptr + 1;
    callerFrame.bp = (int *) *bptr;
    return callerFrame;
  }
}

void print_stack(int* stack_top, int* first_frame, int* stack_bottom){
  Frame frame = {stack_top, first_frame };
  if (DEBUG) fprintf(stderr, "***** STACK: START sp=%p, bp=%p,bottom=%p *****\n", stack_top, first_frame, stack_bottom);
  do {
    if (DEBUG) fprintf(stderr, "***** FRAME: START *****\n");
    for (int *p = frame.sp; p < frame.bp; p++){
      if (DEBUG) fprintf(stderr, "  %p: %p\n", p, (int*)*p);
    }
    if (DEBUG) fprintf(stderr, "***** FRAME: END *****\n");
    frame    = caller(stack_bottom, frame);
  } while (frame.sp != stack_bottom);
  if (DEBUG) fprintf(stderr, "***** STACK: END *****\n");
}

void print_heap(int* heap, int size) {
  fprintf(stderr, "\n");
  for(int i = 0; i < size; i += 1) {
    fprintf(stderr
          , "  %d/%p: %p (%d)\n"
          , i
          , (heap + i)
          , (int*)(heap[i])
          , *(heap + i));
  }
}


////////////////////////////////////////////////////////////////////////////////
// FILL THIS IN, see documentation in 'gc.h' ///////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
int* mark( int* stack_top
         , int* first_frame
         , int* stack_bottom
         , int* heap_start)
{
  return HEAP_END;
}

////////////////////////////////////////////////////////////////////////////////
// FILL THIS IN, see documentation in 'gc.h' ///////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
int* forward( int* heap_start
            , int* max_address)
{
  return HEAP_END;
}

////////////////////////////////////////////////////////////////////////////////
// FILL THIS IN, see documentation in 'gc.h' ///////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
void redirect( int* stack_bottom
             , int* stack_top
             , int* first_frame
             , int* heap_start
             , int* max_address )
{
  return; 
}

////////////////////////////////////////////////////////////////////////////////
// FILL THIS IN, see documentation in 'gc.h' ///////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
void compact( int* heap_start
            , int* max_address
            , int* heap_end )
{ 
  return;
}

////////////////////////////////////////////////////////////////////////////////
// Top-level GC function (you can leave this as is!) ///////////////////////////
////////////////////////////////////////////////////////////////////////////////

int* gc( int* stack_bottom
       , int* stack_top
       , int* first_frame
       , int* heap_start
       , int* heap_end )
{

  int* max_address = mark( stack_top
                         , first_frame
                         , stack_bottom
                         , heap_start );

  int* new_address = forward( heap_start
                            , max_address );

                     redirect( stack_bottom
                             , stack_top
                             , first_frame
                             , heap_start
                             , max_address );

                     compact( heap_start
                            , max_address
                            , heap_end );

  return new_address;
}
