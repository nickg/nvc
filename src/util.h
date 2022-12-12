//
//  Copyright (C) 2011-2022  Nick Gasson
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

#ifdef __MINGW32__
#define realpath(N, R) _fullpath((R), (N), _MAX_PATH)
#define setenv(x, y, z) _putenv_s((x), (y))
#define sigjmp_buf jmp_buf
#define sigsetjmp(x, y) setjmp((x))
#define siglongjmp(x, y) longjmp((x), (y))
#endif

#define is_power_of_2(x) (((x) & ~(x)) == 0)

#define ARRAY_LEN(a) (sizeof(a) / sizeof(a[0]))
#define ALIGN_UP(p, a) (typeof(p))({                  \
   assert(is_power_of_2(a));                          \
   const typeof(a) __a = (a);                         \
   (((uintptr_t)(p) + (__a) - 1) & ~((__a) - 1)); })

#define MAX(x, y) ((x) > (y) ? (x) : (y))
#define MIN(x, y) ((x) < (y) ? (x) : (y))

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

#ifdef ENABLE_JIT
#define JIT_ONLY(x) x
#else
#define JIT_ONLY(x)
#endif

#if defined ENABLE_LLVM && !defined ENABLE_JIT
#define AOT_ONLY(x) x
#define NOT_AOT_ONLY(x)
#else
#define AOT_ONLY(x)
#define NOT_AOT_ONLY(x) x
#endif

#define UNUSED __attribute__((unused))

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

#define container_of(ptr, type, member) ({               \
   const typeof(((type *)0)->member) * __mptr = (ptr);   \
   (type *)((char *)__mptr - offsetof(type, member)); })

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

void errorf(const char *fmt, ...)
   __attribute__((format(printf, 1, 2)));
void warnf(const char *fmt, ...)
   __attribute__((format(printf, 1, 2)));
void notef(const char *fmt, ...)
   __attribute__((format(printf, 1, 2)));
void debugf(const char *fmt, ...)
   __attribute__((format(printf, 1, 2)));
void fatal(const char *fmt, ...)
   __attribute__((format(printf, 1, 2), noreturn));
void fatal_trace(const char *fmt, ...)
   __attribute__((format(printf, 1, 2), noreturn));
void fatal_errno(const char *fmt, ...)
   __attribute__((format(printf, 1, 2), noreturn));

const char *last_os_error(void);

#define likely(x) __builtin_expect(x, 1)
#define unlikely(x) __builtin_expect(x, 0)

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
int terminal_width(void);

const char *ordinal_str(int n);

int checked_sprintf(char *buf, int len, const char *fmt, ...)
   __attribute__((format(printf, 3, 4)));

int next_power_of_2(int n) __attribute__((pure));
int ilog2(int64_t n) __attribute__((pure));
int64_t ipow(int64_t x, int64_t y)  __attribute__((pure));

typedef enum {
   MEM_NONE, MEM_RO, MEM_RW
} mem_access_t;

typedef void (*guard_fault_fn_t)(void *, void *);

void *nvc_memalign(size_t align, size_t sz);
void nvc_munmap(void *ptr, size_t length);
void nvc_memprotect(void *ptr, size_t length, mem_access_t prot);
void *mmap_guarded(size_t sz, guard_fault_fn_t fn, void *ctx);
void *map_huge_pages(size_t align, size_t sz);

void run_program(const char *const *args);

text_buf_t *safe_symbol(ident_t id);
text_buf_t *unsafe_symbol(const char *text);

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
size_t tb_len(text_buf_t *tb);
void tb_downcase(text_buf_t *tb);
void tb_replace(text_buf_t *tb, char old, char rep);

#define LOCAL __attribute__((cleanup(_local_free)))

void _local_free(void *ptr);

#ifdef __MINGW32__
#define DLLEXPORT __declspec(dllexport)
#else
#define DLLEXPORT
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
} nvc_rusage_t;

void nvc_rusage(nvc_rusage_t *ru);

uint64_t get_timestamp_us();
unsigned nvc_nprocs(void);

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
bool get_exe_path(text_buf_t *tb);
void open_pipe(int *rfd, int *wfd);

struct cpu_state;
void capture_registers(struct cpu_state *cpu);

#endif // _UTIL_H
