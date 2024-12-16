//
//  Copyright (C) 2011-2024  Nick Gasson
//
//  This program is free software: you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation, either version 3 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program.  If not, see <http://www.gnu.org/licenses/>.
//

#ifndef _UTIL_H
#define _UTIL_H

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif

#include <stddef.h>
#include <stdio.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdarg.h>
#include <time.h>

#include "prim.h"

#ifdef HAVE_FUNC_ATTRIBUTE_RETURNS_NONNULL
#define RETURNS_NONNULL __attribute__((returns_nonnull))
#else
#define RETURNS_NONNULL
#endif

#ifdef __GNUC__
#define GCC_VERSION (__GNUC__ * 10000       \
                     + __GNUC_MINOR__ * 100 \
                     + __GNUC_PATCHLEVEL__)
#else
#define GCC_VERSION 0
#endif

#if GCC_VERSION > 40600 && !defined __cplusplus
#define STATIC_ASSERT(x) _Static_assert((x), "Static assertion failed");
#else
#define STATIC_ASSERT(x)
#endif

#if __SANITIZE_ADDRESS__
#include <sanitizer/asan_interface.h>

#define ASAN_POISON(addr, size) ASAN_POISON_MEMORY_REGION(addr, size)
#define ASAN_UNPOISON(addr, size) ASAN_UNPOISON_MEMORY_REGION(addr, size)
#else
#define ASAN_POISON(addr, size)
#define ASAN_UNPOISON(addr, size)
#endif

#if defined __x86_64__
#define ARCH_X86_64 1
#elif defined __i386__
#define ARCH_I386 1
#elif defined __aarch64__
#define ARCH_ARM64 1
#endif

#ifdef __MINGW32__
#define realpath(N, R) _fullpath((R), (N), _MAX_PATH)
#define setenv(x, y, z) _putenv_s((x), (y))
#define sigjmp_buf jmp_buf
#define sigsetjmp(x, y) setjmp((x))
#define siglongjmp(x, y) longjmp((x), (y))
#endif

#define is_power_of_2(x) (((x) & (x - 1)) == 0)

#define ARRAY_LEN(a) (sizeof(a) / sizeof(a[0]))
#define ALIGN_UP(p, a) (typeof(p))({                  \
   assert(is_power_of_2(a));                          \
   const typeof(a) __a = (a);                         \
   (((uintptr_t)(p) + (__a) - 1) & ~((__a) - 1)); })

#define MAX(x, y)                               \
  ({ typeof(x) __x = (x); typeof(y) __y = (y);  \
    ((__x) > (__y) ? (__x) : (__y)); })
#define MIN(x, y)                               \
  ({ typeof(x) __x = (x); typeof(y) __y = (y);  \
    ((__x) < (__y) ? (__x) : (__y)); })

#define FLOAT_BITS(f) \
   (((union { double __d; int64_t __i; }){.__d = (f)}).__i)

#ifdef NDEBUG
#undef DEBUG
#define DEBUG_ONLY(x)
#else
#define DEBUG 1
#define DEBUG_ONLY(x) x
#endif

#ifdef ENABLE_LLVM
#define LLVM_ONLY(x) x
#define NOT_LLVM_ONLY(x)
#else
#define LLVM_ONLY(x)
#define NOT_LLVM_ONLY(x) x
#endif

#define UNUSED __attribute__((unused))

#define PASTE(x, y) x ## y
#define JOIN(x, y) PASTE(x, y)
#define UNIQUE(name) JOIN(name, __COUNTER__)

#define LCOV_EXCL_LINE
#define LCOV_EXCL_START
#define LCOV_EXCL_STOP
#define LCOV_EXCL_BR_LINE
#define LCOV_EXCL_BR_START
#define LCOV_EXCL_BR_STOP

#ifndef HAVE_MEMMEM
void *memmem(const void *haystack, size_t haystacklen,
             const void *needle, size_t needlelen);
#endif

#ifndef HAVE_STRCASESTR
char *strcasestr(const char *haystack_start, const char *needle_start);
#endif

#ifndef HAVE_GETLINE
ssize_t getline(char **lineptr, size_t *n, FILE *stream);
#endif

#ifndef HAVE_FTELLO
#define ftello(f) (off_t)ftell(f)
#endif

#ifndef HAVE_FSEEKO
#define fseeko(f, o, w) (off_t)fseek(f, o, w)
#endif

#ifndef HAVE_STRCHRNUL
char *strchrnul(const char *s, int c_in);
#endif

#ifndef HAVE_STRNDUP
char *strndup(char const *s, size_t n);
#endif

#define container_of(ptr, type, member) ({               \
   const typeof(((type *)0)->member) * __mptr = (ptr);   \
   (type *)((char *)__mptr - offsetof(type, member)); })

#define tag_pointer(p, tag) ({                          \
         typeof((p)) __p = (p);                         \
         assert(((uintptr_t)__p & 7) == 0);             \
         assert((unsigned)(tag) < 8u);                  \
         (void *)((uintptr_t)__p | (uintptr_t)(tag));   \
      })

#define untag_pointer(p, type) (type *)((uintptr_t)(p) & ~7)
#define pointer_tag(p) ((uintptr_t)(p) & 7)

#define prefetch_read(ptr) __builtin_prefetch(ptr, 0)
#define prefetch_write(ptr) __builtin_prefetch(ptr, 1, 1)

// Scrambling functions from MurmurHash3
#define mix_bits_32(n) ({                       \
         uint32_t __n = (uint32_t)(n);          \
         __n *= 0xcc9e2d51;                     \
         __n = (__n << 15) | (__n >> 17);       \
         __n *= 0x1b873593;                     \
      })
#define mix_bits_64(n) ({                       \
         uint64_t __n = (uint64_t)(n);          \
         __n ^= (__n >> 33);                    \
         __n *= UINT64_C(0xff51afd7ed558ccd);   \
         __n ^= (__n >> 33);                    \
         __n *= UINT64_C(0xc4ceb9fe1a85ec53);   \
         __n ^= (__n >> 33);                    \
      })

void *xmalloc(size_t size) RETURNS_NONNULL;
void *xmalloc_array(size_t nelems, size_t size) RETURNS_NONNULL;
void *xmalloc_flex(size_t fixed, size_t nelems, size_t size) RETURNS_NONNULL;
void *xcalloc(size_t size) RETURNS_NONNULL;
void *xcalloc_array(size_t nelems, size_t size) RETURNS_NONNULL;
void *xcalloc_flex(size_t fixed, size_t nelems, size_t size) RETURNS_NONNULL;
void *xrealloc(void *ptr, size_t size) RETURNS_NONNULL;
void *xrealloc_array(void *ptr, size_t nelems, size_t size) RETURNS_NONNULL;
void *xrealloc_flex(void *ptr, size_t fixed, size_t nelems, size_t size)
   RETURNS_NONNULL;
char *xstrdup(const char *str) RETURNS_NONNULL;
char *xstrndup(const char *str, size_t n) RETURNS_NONNULL;

char *xvasprintf(const char *fmt, va_list ap) RETURNS_NONNULL;
char *xasprintf(const char *fmt, ...)
   __attribute__((format(printf, 1, 2))) RETURNS_NONNULL;

int color_printf(const char *fmt, ...)
   __attribute__((format(printf, 1, 2)));
int color_fprintf(FILE *file, const char *fmt, ...)
   __attribute__((format(printf, 2, 3)));
int color_vprintf(const char *fmt, va_list ap);
char *color_vasprintf(const char *fmt, va_list ap);
char *color_asprintf(const char *fmt, ...)
   __attribute__((format(printf, 1, 2)));
char *strip_color(const char *fmt, va_list ap);

void print_centred(const char *text);

void errorf(const char *fmt, ...)
   __attribute__((format(printf, 1, 2)));
void warnf(const char *fmt, ...)
   __attribute__((format(printf, 1, 2)));
void notef(const char *fmt, ...)
   __attribute__((format(printf, 1, 2)));
void debugf(const char *fmt, ...)
   __attribute__((format(printf, 1, 2)));
void fatal(const char *fmt, ...)
   __attribute__((format(printf, 1, 2), noreturn, cold, noinline));
void fatal_trace(const char *fmt, ...)
   __attribute__((format(printf, 1, 2), noreturn, cold, noinline));
void fatal_errno(const char *fmt, ...)
   __attribute__((format(printf, 1, 2), noreturn, cold, noinline));

const char *last_os_error(void);

#define likely(x) __builtin_expect(x, 1)
#define unlikely(x) __builtin_expect(x, 0)

#ifdef DEBUG
void should_not_reach_here(void) __attribute__((noreturn, cold));
#else
#define should_not_reach_here() __builtin_unreachable()
#endif

void error_at(const loc_t *loc, const char *fmt, ...)
   __attribute__((format(printf, 2, 3)));
void warn_at(const loc_t *loc, const char *fmt, ...)
   __attribute__((format(printf, 2, 3)));
void note_at(const loc_t *loc, const char *fmt, ...)
   __attribute__((format(printf, 2, 3)));
void fatal_at(const loc_t *loc, const char *fmt, ...)
   __attribute__((format(printf, 2, 3), noreturn));

void fatal_exit(int status) __attribute__((noreturn));
void show_stacktrace(void);
void register_signal_handlers(void);
void set_ctrl_c_handler(void (*fn)(void *), void *arg);

void term_init(void);
bool color_terminal(void);
bool utf8_terminal(void);
int terminal_width(void);

const char *ordinal_str(int n);
char *null_terminate(const uint8_t *data, size_t len);

char toupper_iso88591(unsigned char ch);
char tolower_iso88591(unsigned char ch);
bool isprint_iso88591(unsigned char ch);
bool isspace_iso88591(unsigned char ch);
bool isdigit_iso88591(unsigned char ch);
bool isupper_iso88591(unsigned char ch);
bool isalnum_iso88591(unsigned char ch);

int checked_sprintf(char *buf, int len, const char *fmt, ...)
   __attribute__((format(printf, 3, 4)));

int next_power_of_2(int n) __attribute__((pure));
int ilog2(int64_t n) __attribute__((pure));
int64_t ipow(int64_t x, int64_t y)  __attribute__((pure));
bool ipow_safe(int64_t x, int64_t y, int64_t *result);

typedef enum {
   MEM_NONE, MEM_RO, MEM_RW, MEM_RX, MEM_RWX
} mem_access_t;

void *nvc_memalign(size_t align, size_t sz);
void nvc_munmap(void *ptr, size_t length);
void nvc_memprotect(void *ptr, size_t length, mem_access_t prot);
void nvc_decommit(void *ptr, size_t length);
void *map_huge_pages(size_t align, size_t sz);
void *map_jit_pages(size_t align, size_t sz);

void run_program(const char *const *args);
char *nvc_temp_file(void);

text_buf_t *safe_symbol(ident_t id);

#define LOCAL_TEXT_BUF __attribute__((cleanup(_tb_cleanup))) text_buf_t *

text_buf_t *tb_new(void);
void tb_free(text_buf_t *tb);
void _tb_cleanup(text_buf_t **tb);
void tb_printf(text_buf_t *tb, const char *fmt, ...)
   __attribute__((format(printf, 2, 3)));
void tb_vprintf(text_buf_t *tb, const char *fmt, va_list ap);
void tb_append(text_buf_t *tb, char ch);
void tb_istr(text_buf_t *tb, ident_t ident);
void tb_cat(text_buf_t *tb, const char *str);
void tb_catn(text_buf_t *tb, const char *str, size_t nchars);
void tb_repeat(text_buf_t *tb, char ch, size_t count);
const char *tb_get(text_buf_t *tb);
char *tb_claim(text_buf_t *tb);
char *tb_reserve(text_buf_t *tb, size_t size);
void tb_rewind(text_buf_t *tb);
void tb_trim(text_buf_t *tb, size_t newlen);
void tb_strip(text_buf_t *tb);
size_t tb_len(text_buf_t *tb);
void tb_downcase(text_buf_t *tb);
void tb_upcase(text_buf_t *tb);
void tb_replace(text_buf_t *tb, char old, char rep);
void tb_strftime(text_buf_t *tb, const char *fmt, time_t time);

#define LOCAL __attribute__((cleanup(_local_free)))

void _local_free(void *ptr);

#ifdef __MINGW32__
#define DLLEXPORT __declspec(dllexport)
#else
#define DLLEXPORT __attribute__((used))
#endif

typedef enum {
   MESSAGE_FULL,
   MESSAGE_COMPACT
} message_style_t;

void set_message_style(message_style_t style);
message_style_t get_message_style(void);

typedef struct {
   unsigned rss;
   unsigned ms;
   unsigned user;
   unsigned sys;
} nvc_rusage_t;

void nvc_rusage(nvc_rusage_t *ru);

typedef uint64_t timestamp_t;   // Nanoseconds

uint64_t get_timestamp_ns(void);
uint64_t get_timestamp_us(void);
timestamp_t get_real_time(void);
unsigned nvc_nprocs(void);

typedef enum {
   FILE_REGULAR,
   FILE_DIR,
   FILE_FIFO,
} file_type_t;

typedef struct {
   file_type_t type;
   size_t      size;
   timestamp_t mtime;
} file_info_t;

typedef enum {
   FILE_READ,
   FILE_WRITE,
   FILE_APPEND,
   FILE_READ_WRITE,
} file_mode_t;

bool get_file_info(const char *path, file_info_t *info);
bool get_handle_info(int fd, file_info_t *info);
bool get_handle_path(int fd, text_buf_t *tb);
bool get_handle_mode(int fd, file_mode_t *mode);

void progress(const char *fmt, ...)
   __attribute__((format(printf, 1, 2)));

void file_read_lock(int fd);
void file_write_lock(int fd);
void file_unlock(int fd);

void *map_file(int fd, size_t size);
void unmap_file(void *ptr, size_t size);
void make_dir(const char *path);
char *search_path(const char *name);
void get_libexec_dir(text_buf_t *tb);
void get_lib_dir(text_buf_t *tb);
void get_data_dir(text_buf_t *tb);
bool get_exe_path(text_buf_t *tb);
void open_pipe(int *rfd, int *wfd);
bool is_absolute_path(const char *path);
void check_cpu_features(void);

struct cpu_state;
typedef void (*fault_fn_t)(int, void *, struct cpu_state *, void *);

void add_fault_handler(fault_fn_t fn, void *context);
void remove_fault_handler(fault_fn_t fn, void *context);

struct cpu_state;
void capture_registers(struct cpu_state *cpu);

mem_pool_t *pool_new(void);
void pool_free(mem_pool_t *mp);
void *pool_malloc(mem_pool_t *mp, size_t size);
void *pool_calloc(mem_pool_t *mp, size_t size);
void *pool_malloc_array(mem_pool_t *mp, size_t nelems, size_t size);
void pool_stats(mem_pool_t *mp, size_t *alloc, size_t *npages);

typedef struct _ptr_list {
   unsigned  count;
   unsigned  max;
   void     *items[0];
} *ptr_list_t;

#define LOCAL_LIST __attribute__((cleanup(list_free))) ptr_list_t

typedef int (*list_cmp_fn_t)(const void *, const void *);

void list_add(ptr_list_t *l, void *item);
void list_free(ptr_list_t *l);
void list_sort(ptr_list_t *l, list_cmp_fn_t cmp);
void list_clear(ptr_list_t *l);

#define list_size(l) ((l) == NULL ? 0 : (l)->count)

#define list_get(l, nth) ({                  \
         typeof (nth) __nth = (nth);         \
         typeof (l) __l = (l);               \
         assert(__nth < list_size(__l));     \
         (__l)->items[(__nth)];              \
      })

#define list_start(l) ((l) == NULL ? NULL : (l)->items)
#define list_end(l) ((l) == NULL ? NULL : (l)->items + (l)->count)

#define list_iter(type, it, list)                      \
   typeof(type) it,                                   \
      *__p = (typeof(__p))list_start(list),            \
      *__end = (typeof(__end))list_end(list);          \
   __p != __end && (it = *__p, 1); __p++               \

#define list_foreach(type, it, list) \
   for (list_iter(type, it, list))

#define INIT_ONCE(body) do {                    \
      static volatile int __done = 0;           \
      if (!load_acquire(&__done)) {             \
         static nvc_lock_t __lock;              \
         SCOPED_LOCK(__lock);                   \
         body;                                  \
         store_release(&__done, 1);             \
      }                                         \
   } while (0)

#define FOR_ALL_SIZES(size, macro) do {                 \
      switch (size) {                                   \
      case 1:                                           \
         macro(uint8_t); break;                         \
      case 2:                                           \
         macro(uint16_t); break;                        \
      case 4:                                           \
         macro(uint32_t); break;                        \
      case 8:                                           \
         macro(uint64_t); break;                        \
      }                                                 \
   } while (0)

#define UNPACK_BE16(b) ((b)[0] << 8 | (b)[1])
#define UNPACK_BE32(b) \
   ((uint32_t)UNPACK_BE16(b) << 16 | UNPACK_BE16(b + 2))
#define UNPACK_BE64(b) \
   ((uint64_t)UNPACK_BE32(b) << 32 | UNPACK_BE32(b + 4))

#define PACK_BE16(u) \
   ((u) >> 8) & 0xff, (u) & 0xff
#define PACK_BE32(u) \
   PACK_BE16(u >> 16), PACK_BE16(u)
#define PACK_BE64(u) \
   PACK_BE32(u >> 32), PACK_BE32(u)

#define unaligned_load(ptr, type) ({                                    \
         const void *__ptr = (ptr);                                     \
         const struct { type value; }                                   \
            __attribute__((packed)) *__s = __ptr;                       \
         __s->value;                                                    \
      })

#define TYPE_MAX(x)                             \
   _Generic((x),                                \
            uint64_t: UINT64_MAX,               \
            int64_t: INT64_MAX,                 \
            uint32_t: UINT32_MAX,               \
            int32_t: INT32_MAX,                 \
            uint16_t: UINT16_MAX,               \
            int16_t: INT16_MAX,                 \
            uint8_t: UINT8_MAX,                 \
            int8_t: INT8_MAX)

#define saturate_add(a, b) ({                           \
         typeof((a)) __tmp;                             \
         if (__builtin_add_overflow((a), (b), &__tmp))  \
            __tmp = TYPE_MAX(__tmp);                    \
         __tmp;                                         \
      })

#define HASH_INIT 5381;
typedef uint32_t hash_state_t;

static inline int hash_update(hash_state_t *state, const char *key, int nchars)
{
   // DJB2 hash function from here:
   //   http://www.cse.yorku.ca/~oz/hash.html

   hash_state_t hash = *state;
   const char *p = key;
   for (; p < key + nchars && *p; p++)
      hash = ((hash << 5) + hash) + *p;

   *state = hash;
   return p - key;
}

#endif // _UTIL_H
