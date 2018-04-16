// Wrapper around the Haskell interface to libsme

#include <pthread.h>
#include <stdlib.h>
#include <stdio.h>
#include <inttypes.h>
#include <string.h>
#include <assert.h>
#include <stdbool.h>
#include <gmp.h>

#include <HsFFI.h>
//#include <API_stub.h>

extern HsBool hs_propagate_buses(HsStablePtr a1);
extern HsBool hs_run_procs(HsStablePtr a1);
extern HsBool hs_sme_load_file(HsPtr a1, HsPtr a2);
extern HsBool hs_finalize(HsStablePtr a1);
extern HsBool hs_gen_code(HsStablePtr a1, HsPtr a2);


#include "uthash.h"
#include "libsme.h"

typedef struct Channel {
  char* name;
  Type type;
  Value read;
  Value write;
  UT_hash_handle hh;
} Channel;

struct Bus {
  char* name;
  int chan_count;
  Channel* channel;
  UT_hash_handle hh;
};


pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;

struct SmeCtx {
  bool has_failed;
  char* err_msg;
  HsStablePtr sim_state;
  Bus* buses;
  int (*wait_var)();
  void (*done_var)();
};

//static void sme_library_init(void) __attribute__((constructor));
static void
sme_library_init(void)
{
  int argc = 2;
  //char* argv[] = { "+RTS", "-A32m", NULL };
  char* argv[] = { "+RTS", "-A256m", NULL };
  char **pargv = argv;
  hs_init(&argc, &pargv);
  //sme_init();
}

//static void sme_library_end(void) __attribute__((destructor));
static void
sme_library_end(void)
{
  hs_exit();
}

// Bus constructor API (This is not exposed through the header file)

void sme_set_sim_state(SmeCtx* ctx, HsStablePtr sim_state) {
  ctx->sim_state = sim_state;
}

/* Creates a new named bus */
Bus* sme_add_bus(SmeCtx* ctx, char* name) {
  Bus* bus = malloc(sizeof(Bus));
  *bus = (Bus) {
    .name = strdup(name),
    .chan_count = 0,
    .channel = NULL
  };
  HASH_ADD_KEYPTR(hh, ctx->buses, name, strlen(name), bus);
  return bus;
}

ChannelVals*
sme_add_chan(Bus* bus, char* name, Type type)
{
  Channel* chan = malloc(sizeof(Channel));
  *chan = (Channel) {
    .name = strdup(name),
    .type = type,
    .read = { 0 },
    .write = { 0 },
  };

  SMEInt* write;
  SMEInt* read;
  switch (type) {
  case SME_INT:
  case SME_UINT:
    write = malloc(sizeof(SMEInt));
    read = malloc(sizeof(SMEInt));
    *read = (SMEInt) {
      .num = calloc(1, 1),
      .len = 1,
      .alloc_size = 1,
      .negative = 0 //false
    };
    *write = (SMEInt) {
      .num = calloc(1, 1),
      .len = 1,
      .alloc_size = 1,
      .negative = 0 //false
    };

    read->num = calloc(1, 1);
    chan->read.value.integer = read;
    chan->write.value.integer = write;

    // Fallthrough
  default:
    chan->read.type = type;
    chan->write.type = type;
  }

  HASH_ADD_KEYPTR(hh, bus->channel, name, strlen(name), chan);
  bus->chan_count += 1;

  ChannelVals* ret = malloc(sizeof(ChannelRef));
  *ret = (ChannelVals) {
    .read_ptr = &(chan->read),
    .write_ptr = &(chan->write)
  };

  return ret;
}

void
free_chan_vals(ChannelVals* v)
{
  free(v);
}

static Channel*
sme_get_chan(SmeCtx* ctx, const char* bus, const char* chan)
{
  Bus* bus_ptr = 0;
  HASH_FIND_STR (ctx->buses, bus, bus_ptr);

  if (! bus_ptr) {
    return NULL;
  }

  Channel* chan_ptr = 0;
  HASH_FIND_STR (bus_ptr->channel, chan, chan_ptr);
  if (! chan_ptr) {
    return NULL;
  }

  return chan_ptr;
}

Value*
sme_get_chan_write(Channel* chan)
{
  return &(chan->write);
}

Value*
sme_get_chan_read(Channel* chan)
{
  return &(chan->read);
}

Value*
sme_get_write_val(SmeCtx* ctx, const char* bus, const char* chan)
{
  Channel* chan_ptr = sme_get_chan(ctx, bus, chan);
  return &(chan_ptr->write);
}

Value*
sme_get_read_val(SmeCtx* ctx, const char* bus, const char* chan)
{
  Channel* chan_ptr = sme_get_chan(ctx, bus, chan);
  return &(chan_ptr->read);
}

BusMap*
sme_get_busmap(SmeCtx* ctx)
{
  pthread_mutex_lock(&mutex);
  Bus* bus;
  Channel* chan;
  int size = 0;

  for (bus = ctx->buses; bus != NULL; bus = bus->hh.next) {
    size = size + HASH_COUNT(bus->channel);
  }

  BusMap * bm = malloc (sizeof(BusMap));
  *bm = (BusMap) {
    .len = size,
    .chans = malloc(sizeof(ChannelRef*) * size)
  };

  int idx = 0;
  for (bus = ctx->buses; bus != NULL; bus = bus->hh.next) {
    for (chan = bus->channel; chan != NULL; chan = chan->hh.next) {
      ChannelRef* cr = malloc(sizeof(ChannelRef));
      *cr = (ChannelRef) {
        .type = chan->type,
        .bus_name = strdup(bus->name),
        .chan_name = strdup(chan->name),
        .read_ptr = &(chan->read),
        .write_ptr = &(chan->write)
      };
      bm->chans[idx++] = cr;
    }
  }
  pthread_mutex_unlock(&mutex);
  return bm;
}

/* void */
/* sme_free_chanref(ChannelRef* v */

void
sme_free_busmap(BusMap* bm)
{
  if (! bm) {
    return;
  }

  for (int i = 0; i < bm->len; i++) {
    ChannelRef* c = bm->chans[i];
    free(c->bus_name);
    free(c->chan_name);
    free(c);
  }

  free(bm->chans);
  free(bm);
}

SmeCtx*
sme_init()
{
  sme_library_init();
  SmeCtx* ctx = calloc(sizeof(SmeCtx), 1);
  return ctx;
}

bool
sme_open_file(SmeCtx* ctx, const char* file)
{
  return hs_sme_load_file(ctx, (HsPtr) file);
}

intmax_t
sme_read_native_signed_integer(SMEInt* num)
{
  mpz_t n;
  mpz_init(n);
  mpz_import(n, num->len, -1, 1, 0, 0, num->num);
  if (num->negative) {
    // FIXME: We leak memory here?
    mpz_neg(n, n);
  }
  intmax_t res = mpz_get_ui(n);
  mpz_clear(n);
  return res;
}

uintmax_t
sme_read_native_unsigned_integer(SMEInt* num)
{
  mpz_t n;
  mpz_init(n);
  mpz_import(n, num->len, -1, 1, 0, 0, num->num);
  uintmax_t res = mpz_get_si(n);
  mpz_clear(n);
  return res;
}

bool
sme_tick(SmeCtx* ctx)
{
  //printf("Called tick\n");
  return hs_run_procs(ctx->sim_state);
}

bool
sme_finalize(SmeCtx* ctx)
{
  //printf("Called tick\n");
  return hs_finalize(ctx->sim_state);
}

bool
sme_gen_code(SmeCtx* ctx, const char* file)
{
  //printf("Called tick\n");
  return hs_gen_code(ctx->sim_state, (HsPtr) file);
}

void
sme_tick_async(SmeCtx* ctx)
{
}

void
sme_tick_await(SmeCtx* ctx)
{
}

void
sme_set_error(SmeCtx* ctx, char* msg)
{
  printf("Called set_error\n");
  if (ctx->err_msg) {
    free(ctx->err_msg);
    ctx->err_msg = 0;
  }
  ctx->err_msg = strdup(msg);
  ctx->has_failed = true;
}

char*
sme_get_error_buffer(SmeCtx* ctx)
{
  return ctx->err_msg;
}

bool
sme_has_failed(SmeCtx* ctx)
{
  return ctx->has_failed;
}

bool
sme_propagate(SmeCtx* ctx)
{
  Bus* bus;
  Channel* chan;

  //printf("Called propagate\n");
  // Propagate local buses
  for (bus = ctx->buses; bus != NULL; bus = bus->hh.next) {
    for (chan = bus->channel; chan != NULL; chan = chan->hh.next) {
      switch (chan->type) {
      case SME_INT:
      case SME_UINT: {
        SMEInt* rv = chan->read.value.integer;
        SMEInt* wv = chan->write.value.integer;
        sme_integer_resize(rv, wv->len);
        memcpy(rv->num, wv->num, wv->len);
        rv->len = wv->len;
        rv->negative = wv->negative;
        break;
      }
      default:
        chan->read = chan->write;
        //chan->read.value.native_int = chan->write.value.native_int;
        chan->write = (Value) {
          .type = chan->write.type,
          .value = { 0 }
        }; //(Value) { 0 };
        /* chan->write.value.native_int = 0; */
      }
    assert(bus->name);
    assert(chan);
    assert(chan->name);
    //assert(chan->read.type == SME_NATIVE_INT);
    /* switch (chan->read.type) { */
    /* case SME_INT: */
    /*   printf("Trace %s.%s %li\n", bus->name, chan->name, sme_read_native_signed_integer(chan->read.value.integer)); */
    /*   break; */
    /* /\* case SME_NATIVE_INT: *\/ */
    /* /\*   printf("Trace %s.%s %li\n", bus->name, chan->name, chan->read.value.native_int); *\/ */
    /* /\*   break; *\/ */
    /*   //sme_read_native_signed_integer(chan->read.value.integer));e */
    /* default: */
    /*   break; */
    /* } */
    }
  }

  // Propagate buses in simulator
  return hs_propagate_buses(ctx->sim_state);
}

// Integer representation functions

void
sme_integer_store(SMEInt* num, int len, const char val[len])
{
  sme_integer_resize(num, len);
  memcpy(num->num, num, len);
  num->len = len;
}

void
sme_integer_resize(SMEInt* num, int len)
{
  if (len > num->alloc_size) {
    num->alloc_size *= 2;
    if (len > num->alloc_size) {
      num->alloc_size = len;
    }
    num->num = realloc(num->num, num->alloc_size);
  }
  num->len = len;
}

static void
free_value(Value* val)
{
  switch (val->type) {
  case SME_INT:
  case SME_UINT:
    free(val->value.integer->num);
    free(val->value.integer);
    break;
  default:
    break;
  }
}

void
sme_free(SmeCtx* ctx)
{
  //printf("Called free\n");

  if (! ctx) {
    return;
  }

  if (ctx->buses) {
    Bus* bus, *bus_;
    Channel* chan, *chan_;
    HASH_ITER(hh, ctx->buses, bus, bus_) {
      HASH_ITER(hh, bus->channel, chan, chan_) {
        HASH_DEL(bus->channel, chan);
        free_value(&(chan->read));
        free_value(&(chan->write));
        free((char*) chan->name);
        free(chan);
      }
      HASH_DEL(ctx->buses, bus);
      free((char*) bus->name);
      free(bus);
    }
  }
  if (ctx->sim_state) {
    hs_free_stable_ptr(ctx->sim_state);
  }
  free(ctx->err_msg);
  hs_thread_done();
  free(ctx);
  sme_library_end();
}
