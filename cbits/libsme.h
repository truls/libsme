#ifndef RUNNER_H
#define RUNNER_H

#include <stdbool.h>
#include <stdint.h>

// Opaque datatypes
typedef struct SmeCtx SmeCtx;
typedef struct Bus Bus;

typedef enum Type {
  SME_INT,
  SME_UINT,
  SME_FLOAT,
  SME_DOUBLE,
  SME_BOOL
} Type;

/* SME_NATIVE_INT, */
  /* SME_NATIVE_UINT, */

typedef struct SMEInt {
  int len;
  int alloc_size;
  // FIXME: This should really be a bool, but chs2hs guesses them to be the
  // wrong length (I think) of 4 instead of 1 byte.
  int negative;
  char* num;
} SMEInt;

typedef struct Value {
  Type type;
  union  {
    bool boolean;
    SMEInt* integer;
    intmax_t native_int;
    uintmax_t native_uint;
    double f64;
    float f32;
  } value;
} Value;

typedef struct ChannelVals {
  Value* read_ptr;
  Value* write_ptr;
} ChannelVals;

typedef struct ChannelRef {
  char* bus_name;
  char* chan_name;
  Type type;
  Value* read_ptr;
  Value* write_ptr;
} ChannelRef;

typedef struct BusMap {
  int len;
  ChannelRef** chans;
} BusMap;

/**
   Initializes and returns the SME library context.
**/
SmeCtx* sme_init();

/**
   Loads an SMEIL file, while applying the supplied arguments to libsme.
**/
bool sme_open_file(SmeCtx* ctx, const char* file, int argv, char** argc);

//void sme_set_options(SmeCtx* ctx, const char* options);

void sme_set_print_errors(SmeCtx* ctx, const char* file);

/**
   Returns true if an operation within the libsme library failed.
**/
bool sme_has_failed(SmeCtx* ctx);

/**
   Returns a string containing the error message emitted by libsme. The memory
   pointed to may not be freed except by calling the sme_free function.
**/
char* sme_get_error_buffer(SmeCtx* ctx);

/**
   Frees the SME library context and related resources.
**/
void sme_free(SmeCtx* ctx);

//
Bus* sme_add_bus(SmeCtx* ctx, char* name);
ChannelVals* sme_add_chan(Bus* bus, char* name, Type type);

/* SME Bus channel value accessors.

   These functions returns pointers to raw Value* pointers to the internal value
   representation used by this libsme API. They are the primary way of both
   reading to and writing from SME channels. No sanity checking is performed so
   it is the callers responsibility not to fuck up.
 */

/**
   Returns a pointer to the Value representing the read-end of an SME channel.
**/
Value* sme_get_write_val(SmeCtx* ctx, const char* bus, const char* chan);

/**
   Returns a pointer to the value representing the write-end of an SME channel.
**/
Value* sme_get_read_val(SmeCtx* ctx, const char* bus, const char* chan);

// Simulation management functions

/**
   Ticks the clock of an SME simulation synchronously. When this function
   returns, all processes defined within libsme will have run and written to
   their buses
**/
bool sme_tick(SmeCtx* ctx);

/**
   Finalizes a simulation and dumps the recorded trace file (if any) to the file
   system. This function should always be called following the final call to
   sme_tick.
**/
bool sme_finalize(SmeCtx* ctx);

bool sme_gen_code(SmeCtx* ctx, const char* file);

/**
   Same as sme_tick except that this function will start a simulation for one
   clock tick and return immediately. Use sme_tick_await to wait for the cycle
   to complete.
**/
void sme_tick_async(SmeCtx* ctx);

/**
   This function will block until a tick previously started using sme_tick_async
   has completed. Returns immediately if no simulation is currently running.
**/
void sme_tick_await(SmeCtx* ctx);

/**
   Propagates the values of both internal and external facing buses defined in
   libsme. Run this function before the clock is advanced (by calling sme_tick)
   in the simulation loop and it should be run together with any bus
   propagations that need to be performed by the calling code. When this
   function returns, the values of all buses defined within libsme have been
   propagated.
**/
bool sme_propagate(SmeCtx* ctx);

// Integer representation helper functions


/**
   When manipulating values of type SMEInt (arbitrary-size integers) the
   sme_resize_integer function will make sure that the memory pointed to by
   Value.value is large enough to hold the number that you intend to store. The
   function takes a pointer to the SMEInt structure and a parameter len which is
   the size of the number to be stored in base 256. This function must be called
   before every direct manipulation of SMEInt.num. For a safer interface, see
   sme_store_integer and sme_i

 **/
void sme_integer_resize(SMEInt* num, int len);

/**
   Stores the base-256 representation of an integer in an SMEInt.
 **/
void sme_integer_store(SMEInt* num, int len, const char val[]);

/**
   Sets the sign of an SMEInt. Possible values for sign are 0 meaning the number
   is positive and 1 for a negative value.
 **/
void sme_set_sign(SMEInt* num, int sign);


void sme_integer_store_native_signed(SMEInt* num, int len, intmax_t val);
void sme_integer_store_native_unsigned(SMEInt* num, int len, intmax_t val);
intmax_t sme_integer_read_native_singed(SMEInt* num);
uintmax_t sme_integer_read_native_unsigned_integer(SMEInt* num);


/**
   Returns a pointer to a BusMap structure containing the exposed buses of the
   SME network. This function is intended to be used by implementers of libsme
   to generate internal representations of their SME buses. It is the caller
   responsibility to free the memory returned by the function by calling
   sme_free_busmap.
**/
BusMap* sme_get_busmap(SmeCtx* ctx);

/**
   Frees a BusMap structure allocated by sme_get_busmap
**/
void sme_free_busmap(BusMap* bm);

Value* sme_bus_get_value(SmeCtx* ctx, char* busname, char* channame);

#endif // RUNNER_H
