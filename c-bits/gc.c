#include <stdio.h>
#include <stdlib.h>
#include "gc.h"
#include "types.h"

#define HEAPPRINT 30

#define OFF       1
#define VInit     0x00000000
#define VMark     0x00000001
#define VFwd(A)   (A | VMark)

////////////////////////////////////////////////////////////////////////////////
// Imported from types.c
////////////////////////////////////////////////////////////////////////////////

extern int  is_number(int);
extern int  is_tuple(int);
extern int  tuple_at(int* base, int i);
extern int  tuple_size(int* base);
extern int* int_addr(int);
extern int  addr_int(int*);

////////////////////////////////////////////////////////////////////////////////

extern int* HEAP_END;

typedef struct Frame_ {
  int *sp;
  int *bp;
} Frame;

typedef enum Tag_
  { VAddr
  , VStackAddr
  , VNumber
  , VBoolean
  } Tag;

union Data
  { int* addr;
    int  value;
    int  gcvalue;
  };

typedef struct Value_
  { Tag        tag;
    union Data data;
  } Value;

////////////////////////////////////////////////////////////////////////////////
// helper functions
////////////////////////////////////////////////////////////////////////////////

void  setGCWord(int* addr, int gv){
  if (DEBUG) fprintf(stderr, "\nsetGCWord: addr = %p, gv = %d\n", addr, gv);
  addr[1] = gv;
}

int*  forwardAddr(int* addr){
  return int_addr(addr[1]);
}

int round_to_even(int n){
  return (n % 2 == 0) ? n : n + 1;
}

int blockSize(int *addr){
  int n = tuple_size(addr);
  return (round_to_even(n+2));

}

int* searchTuple(int v, int* max){
  if(is_tuple(v)){
    int* base = int_addr(v);
    if(base > max){
      max = base;
    }
    setGCWord(base,1);
    for(int i = 0; i < tuple_size(base);i++){
      max = searchTuple(base[i+2],max);
    }
  }
  return max;
}

////////////////////////////////////////////////////////////////////////////////

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
int* mark( int* stack_top       // ESP
         , int* first_frame	// EBP
         , int* stack_bottom    
         , int* heap_start)
{ 
  int* curr = stack_top;
  int* ebp  = first_frame;
  int* max  = heap_start;
  while(curr != stack_bottom){
    curr++;
    if(curr == ebp){
    // if current is EBP, update EBP, then skip it and ret
      curr++;
      ebp = (int*)*ebp; // maybe curr
    }
    else{
      // if curr points to a tuple
      // update max,set GC-word,and go through each element of tuple
      max = searchTuple(*curr,max); 
    }
  }    
  return max;
}

////////////////////////////////////////////////////////////////////////////////
// FILL THIS IN, see documentation in 'gc.h' ///////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
int* forward( int* heap_start
            , int* max_address)
{
  int* curr_base = heap_start;
  int curr_empty = 0;
  while(curr_base <= max_address){
    int size = blockSize(curr_base);
    if(curr_base[1] == 1){
    // Tupe is live
      int new_base_addr = (int)curr_base- 4*curr_empty;
      curr_base[1] += new_base_addr; 
    }
    else{
      //not live increase size of empty positions
      curr_empty += size;
    }
    curr_base += size;
  }
  int new_start = (int)max_address + 4 * blockSize(max_address) - 4 * curr_empty;
  return (int*)new_start;
}


void redirectTuple(int* addr){
  int* base = int_addr(*addr);
  if(is_tuple(*addr) && (base[1]&1) == 1){
  // if it is marked tuple
    *addr = base[1];
    for(int i = 0; i < tuple_size(base);i++){
      redirectTuple(base+i+2);
    }
  }
  return ;
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
  int* curr = stack_top;
  int* ebp  = first_frame;
  int* max  = heap_start;
  while(curr != stack_bottom){
    curr++;
    if(curr == ebp){
    // if current is EBP, update EBP, then skip it and ret
      curr++;
      ebp = (int*)*ebp;
    }
    else{
      redirectTuple(curr);
    }
  }
  return; 
}

////////////////////////////////////////////////////////////////////////////////
// FILL THIS IN, see documentation in 'gc.h' ///////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
void compact( int* heap_start
            , int* max_address
            , int* heap_end )
{
  int* curr_base = heap_start;
  int* clear_start = max_address;
  //print_heap(curr_base, blockSize(curr_base));
  while(curr_base <= max_address){
    int size = blockSize(curr_base);
      if( (curr_base[1] & 1) == 1){
      // if tuple is marked
        int*  new_base = forwardAddr(curr_base); 
        for (int i = 0; i< size; i++){
          new_base[i] = curr_base[i];
          if(curr_base ==  max_address){
            clear_start = new_base + size;
          }
          curr_base[i] = 0;
        }
      } 
    setGCWord(curr_base, 0);
    curr_base += size;
  }
    while(clear_start < heap_end){
        *clear_start = 0x0cab005e;
        clear_start++;
    }
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
