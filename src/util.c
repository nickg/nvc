//
//  Copyright (C) 2011-2023  Nick Gasson
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

#if defined(__MINGW32__)
#define WINVER 0x0A00
#define _WIN32_WINNT 0x0A00
#include <windows.h>
#include <fileapi.h>
#include <psapi.h>
#include <io.h>
#endif

#include "util.h"
#include "array.h"
#include "cpustate.h"
#include "debug.h"
#include "diag.h"
#include "option.h"
#include "thread.h"

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <errno.h>
#include <string.h>
#include <stdbool.h>
#include <signal.h>
#include <stdint.h>
#include <inttypes.h>
#include <math.h>
#include <unistd.h>
#include <ctype.h>
#include <assert.h>
#include <limits.h>
#include <time.h>
#include <libgen.h>
#include <fcntl.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/time.h>
#ifdef HAVE_SYS_PTRACE_H
#include <sys/ptrace.h>
#endif
#ifdef __APPLE__
#include <sys/sysctl.h>
#include <libproc.h>
#endif
#ifdef __FreeBSD__
#include <sys/sysctl.h>
#endif
#ifndef __MINGW32__
#include <sys/mman.h>
#include <sys/wait.h>
#include <sys/resource.h>
#include <sys/file.h>
#include <sys/ioctl.h>
#include <termios.h>
#endif

#ifdef HAVE_PTHREAD
#include <pthread.h>
#endif

#ifdef HAVE_SYS_PRCTL_H
#include <sys/prctl.h>
#endif

#ifdef __CYGWIN__
#include <process.h>
#endif

#if defined(HAVE_UCONTEXT_H)
#include <ucontext.h>
#elif defined(HAVE_SYS_UCONTEXT_H)
#include <sys/ucontext.h>
#endif

#define N_TRACE_DEPTH   16
#define ERROR_SZ        1024
#define PAGINATE_RIGHT  72
#define TRACE_MAX_LINE  256

#define ANSI_RESET      0
#define ANSI_BOLD       1
#define ANSI_FG_BLACK   30
#define ANSI_FG_RED     31
#define ANSI_FG_GREEN   32
#define ANSI_FG_YELLOW  33
#define ANSI_FG_BLUE    34
#define ANSI_FG_MAGENTA 35
#define ANSI_FG_CYAN    36
#define ANSI_FG_WHITE   37

#define MAX_FMT_BUFS    32
#define MAX_PRINTF_BUFS 8

#define HUGE_PAGE_SIZE  0x200000

typedef void (*print_fn_t)(const char *fmt, ...);

static char *ansi_vasprintf(const char *fmt, va_list ap, bool force_plain);

typedef struct _fault_handler fault_handler_t;

struct color_escape {
   const char *name;
   int         value;
};

struct text_buf {
   char  *buf;
   size_t alloc;
   size_t len;
};

struct _fault_handler {
   fault_handler_t *next;
   fault_fn_t       fn;
   void            *context;
};

static bool             want_color = false;
static bool             want_links = false;
static bool             want_utf8 = false;
static message_style_t  message_style = MESSAGE_FULL;
static sig_atomic_t     crashing = SIG_ATOMIC_MAX;
static int              term_width = 0;
static void            *ctrl_c_arg = NULL;
static fault_handler_t *fault_handlers = NULL;

#ifdef __MINGW32__
static UINT win32_codepage = 0;
#endif

static void (*ctrl_c_fn)(void *) = NULL;

static const struct color_escape escapes[] = {
   { "",        ANSI_RESET },
   { "bold",    ANSI_BOLD },
   { "black",   ANSI_FG_BLACK },
   { "red",     ANSI_FG_RED },
   { "green",   ANSI_FG_GREEN },
   { "yellow",  ANSI_FG_YELLOW },
   { "blue",    ANSI_FG_BLUE },
   { "magenta", ANSI_FG_MAGENTA },
   { "cyan",    ANSI_FG_CYAN },
   { "white",   ANSI_FG_WHITE },
};

void *xmalloc(size_t size)
{
   void *p = malloc(size);
   if (p == NULL)
      fatal("memory exhausted (malloc %lu)", (long unsigned)size);
   return p;
}

void *xmalloc_flex(size_t fixed, size_t nelems, size_t size)
{
   size_t bytes;
   if (__builtin_mul_overflow(nelems, size, &bytes))
      fatal_trace("array size overflow: requested %zd * %zd bytes",
                  nelems, size);

   return xmalloc(fixed + bytes);
}

void *xmalloc_array(size_t nelems, size_t size)
{
   return xmalloc_flex(0, nelems, size);
}

void *xcalloc(size_t size)
{
   void *p = calloc(1, size);
   if (p == NULL)
      fatal("memory exhausted (calloc %lu)", (long unsigned)size);
   return p;
}

void *xcalloc_flex(size_t fixed, size_t nelems, size_t size)
{
   size_t bytes;
   if (__builtin_mul_overflow(nelems, size, &bytes))
      fatal_trace("array size overflow: requested %zd * %zd bytes",
                  nelems, size);

   return xcalloc(fixed + bytes);
}

void *xcalloc_array(size_t nelems, size_t size)
{
   return xcalloc_flex(0, nelems, size);
}

void *xrealloc(void *ptr, size_t size)
{
   ptr = realloc(ptr, size);
   if (ptr == NULL)
      fatal("memory exhausted (realloc %lu)", (long unsigned)size);
   return ptr;
}

void *xrealloc_array(void *ptr, size_t nelems, size_t size)
{
   size_t bytes;
   if (__builtin_mul_overflow(nelems, size, &bytes))
      fatal_trace("array size overflow: requested %zd * %zd bytes",
                  nelems, size);

   return xrealloc(ptr, bytes);
}

void *xrealloc_flex(void *ptr, size_t fixed, size_t nelems, size_t size)
{
   size_t bytes;
   if (__builtin_mul_overflow(nelems, size, &bytes))
      fatal_trace("array size overflow: requested %zd * %zd bytes",
                  nelems, size);

   return xrealloc(ptr, bytes + fixed);
}

char *xstrdup(const char *str)
{
   char *copy = strdup(str);
   if (copy == NULL)
      fatal("memory exhausted (strdup)");
   return copy;
}

char *xstrndup(const char *str, size_t n)
{
   char *copy = strndup(str, n);
   if (copy == NULL)
      fatal("memory exhausted (strndup)");
   return copy;
}

char *xvasprintf(const char *fmt, va_list ap)
{
   char *strp = NULL;
   if (vasprintf(&strp, fmt, ap) < 0)
      fatal("memory exhausted (vasprintf)");
   return strp;
}

char *xasprintf(const char *fmt, ...)
{
   va_list ap;
   va_start(ap, fmt);
   char *strp = xvasprintf(fmt, ap);
   va_end(ap);
   return strp;
}

void errorf(const char *fmt, ...)
{
   diag_t *d = diag_new(DIAG_ERROR, NULL);
   va_list ap;
   va_start(ap, fmt);
   diag_vprintf(d, fmt, ap);
   va_end(ap);
   diag_emit(d);
}

void warnf(const char *fmt, ...)
{
   diag_t *d = diag_new(DIAG_WARN, NULL);
   va_list ap;
   va_start(ap, fmt);
   diag_vprintf(d, fmt, ap);
   va_end(ap);
   diag_emit(d);
}

void notef(const char *fmt, ...)
{
   diag_t *d = diag_new(DIAG_NOTE, NULL);
   va_list ap;
   va_start(ap, fmt);
   diag_vprintf(d, fmt, ap);
   va_end(ap);
   diag_emit(d);
}

void debugf(const char *fmt, ...)
{
   diag_t *d = diag_new(DIAG_DEBUG, NULL);
   va_list ap;
   va_start(ap, fmt);
   diag_vprintf(d, fmt, ap);
   va_end(ap);
   diag_emit(d);
}

static char *ansi_vasprintf(const char *fmt, va_list ap, bool force_plain)
{
   // Replace color strings like "$red$foo$$bar" with ANSI escaped
   // strings like "\033[31mfoo\033[0mbar"

   static int override = 0;

   if (strchr(fmt, '$') == NULL)
      return xvasprintf(fmt, ap);

   LOCAL_TEXT_BUF tb = tb_new();
   const char *escape_start = NULL;

   while (*fmt != '\0') {
      if (*fmt == '$') {
         if (escape_start == NULL)
            escape_start = fmt;
         else {
            const char *e = escape_start + 1;
            size_t len = fmt - e;

            bool bold;
            if ((bold = (*e == '!')))
               ++e, --len;

            bool bright;
            if ((bright = (*e == '+')))
               ++e, --len;

            if ((*e == '<' || *e == '>') && *(e + 1) == '$') {
               override += *e == '<' ? -1 : 1;
               escape_start = NULL;
            }
            else if (strncmp(e, "link:", 5) == 0) {
               if (want_links && !force_plain) {
                  tb_cat(tb, "\033]8;;");
                  tb_catn(tb, e + 5, len - 5);
                  tb_cat(tb, "\033]8;;\07");
               }
               else {
                  const char *bel = strchr(e, '\07');
                  if (bel && bel < e + len)
                     tb_catn(tb, bel + 1, e + len - bel - 1);
               }
               escape_start = NULL;
            }
            else if (want_color && !force_plain && override >= 0) {
               bool found = false;

               if (*e == '#') {
                  char *eptr;
                  int code = strtoul(e + 1, &eptr, 10);
                  if (eptr == e + len) {
                     if (bold)
                        tb_printf(tb, "\033[1;38;5;%dm", code);
                     else
                        tb_printf(tb, "\033[38;5;%dm", code);
                     found = true;
                  }
               }
               else if (strncmp(e, "link:", 5) == 0) {
                  tb_cat(tb, "\033]8;;");
                  tb_catn(tb, e + 5, len - 5);
                  tb_cat(tb, "\033]8;;\07");
                  found = true;
               }

               for (int i = 0; !found && i < ARRAY_LEN(escapes); i++) {
                  if (strncmp(e, escapes[i].name, len) == 0) {
                     int code = escapes[i].value + (bright ? 60 : 0);
                     if (bold)
                        tb_printf(tb, "\033[1;%dm", code);
                     else
                        tb_printf(tb, "\033[%dm", code);
                     found = true;
                     break;
                  }
               }

               if (!found) {
                  tb_catn(tb, escape_start, len + 1 + bold);
                  escape_start = fmt;
               }
               else
                  escape_start = NULL;
            }
            else
               escape_start = NULL;
         }
      }
      else if (escape_start == NULL)
         tb_append(tb, *fmt);

      ++fmt;
   }

   if (escape_start != NULL)
      tb_cat(tb, escape_start);

   return xvasprintf(tb_get(tb), ap);
}

static int color_vfprintf(FILE *f, const char *fmt, va_list ap)
{
   char *strp LOCAL = ansi_vasprintf(fmt, ap, false);

   bool escape = false;
   int len = 0;
   for (const char *p = strp; *p != '\0'; p++) {
      if (*p == '\033')
         escape = true;
      if (escape)
         escape = (*p != 'm');
      else
         len += 1;
   }

   fputs(strp, f);
   return len;
}

char *color_vasprintf(const char *fmt, va_list ap)
{
   return ansi_vasprintf(fmt, ap, false);
}

char *strip_color(const char *fmt, va_list ap)
{
   return ansi_vasprintf(fmt, ap, true);
}

int color_fprintf(FILE *f, const char *fmt, ...)
{
   va_list ap;
   va_start(ap, fmt);
   const int len = color_vfprintf(f, fmt, ap);
   va_end(ap);
   return len;
}

int color_printf(const char *fmt, ...)
{
   va_list ap;
   va_start(ap, fmt);
   int rc = color_vprintf(fmt, ap);
   va_end(ap);
   return rc;
}

int color_vprintf(const char *fmt, va_list ap)
{
   return color_vfprintf(stdout, fmt, ap);
}

char *color_asprintf(const char *fmt, ...)
{
   va_list ap;
   va_start(ap, fmt);
   char *str = ansi_vasprintf(fmt, ap, false);
   va_end(ap);
   return str;
}

bool color_terminal(void)
{
   return want_color;
}

bool utf8_terminal(void)
{
   return want_utf8;
}

void print_centred(const char *text)
{
   if (term_width == 0)
      fputs(text, stdout);
   else {
      const int pad = (term_width - strlen(text)) / 2;
      printf("%*s%s", pad, "", text);
   }
}

void fatal_exit(int status)
{
   async_barrier();

   if (atomic_load(&crashing) != SIG_ATOMIC_MAX)
      _exit(status);   // Exit during crash
   else if (!thread_attached() || thread_id() != 0)
      _exit(status);
   else
      exit(status);
}

void error_at(const loc_t *loc, const char *fmt, ...)
{
   diag_t *d = diag_new(DIAG_ERROR, loc);

   va_list ap;
   va_start(ap, fmt);
   diag_vprintf(d, fmt, ap);
   va_end(ap);

   diag_emit(d);
}

void warn_at(const loc_t *loc, const char *fmt, ...)
{
   diag_t *d = diag_new(DIAG_WARN, loc);

   va_list ap;
   va_start(ap, fmt);
   diag_vprintf(d, fmt, ap);
   va_end(ap);

   diag_emit(d);
}

void note_at(const loc_t *loc, const char *fmt, ...)
{
   diag_t *d = diag_new(DIAG_NOTE, loc);

   va_list ap;
   va_start(ap, fmt);
   diag_vprintf(d, fmt, ap);
   va_end(ap);

   diag_emit(d);
}

void fatal_at(const loc_t *loc, const char *fmt, ...)
{
   diag_t *d = diag_new(DIAG_FATAL, loc);
   diag_suppress(d, false);

   va_list ap;
   va_start(ap, fmt);
   diag_vprintf(d, fmt, ap);
   va_end(ap);

   diag_emit(d);
   fatal_exit(EXIT_FAILURE);
}

void fatal(const char *fmt, ...)
{
   diag_t *d = diag_new(DIAG_FATAL, NULL);
   diag_suppress(d, false);

   va_list ap;
   va_start(ap, fmt);
   diag_vprintf(d, fmt, ap);
   va_end(ap);

   diag_emit(d);
   fatal_exit(EXIT_FAILURE);
}

void fatal_trace(const char *fmt, ...)
{
   diag_t *d = diag_new(DIAG_FATAL, NULL);

   va_list ap;
   va_start(ap, fmt);
   diag_vprintf(d, fmt, ap);
   va_end(ap);

   diag_set_consumer(NULL, NULL);
   diag_suppress(d, false);
   diag_stacktrace(d, true);
   diag_emit(d);
   fatal_exit(EXIT_FAILURE);
}

void fatal_errno(const char *fmt, ...)
{
   diag_t *d = diag_new(DIAG_FATAL, NULL);
   diag_suppress(d, false);

   va_list ap;
   va_start(ap, fmt);
   diag_vprintf(d, fmt, ap);
   diag_printf(d, ": %s", last_os_error());
   va_end(ap);

   diag_emit(d);
   fatal_exit(EXIT_FAILURE);
}

const char *last_os_error(void)
{
#ifdef __MINGW32__
   static __thread LPSTR mbuf = NULL;

   if (mbuf != NULL)
      LocalFree(mbuf);

   FormatMessage(
      FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM
      | FORMAT_MESSAGE_IGNORE_INSERTS,
      NULL,
      GetLastError(),
      MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
      (LPSTR)&mbuf, 0, NULL);

   return mbuf;
#else
   return strerror(errno);
#endif
}

static void trace_one_frame(uintptr_t pc, const char *module,
                            const char *srcfile, const char *symbol,
                            unsigned lineno, unsigned colno,
                            ptrdiff_t disp, frame_kind_t kind)
{
   color_fprintf(stderr, "[$green$%p$$] ", (void *)pc);
   if (kind == FRAME_LIB)
      color_fprintf(stderr, "($red$%s$$) ", module);
   if (srcfile != NULL)
      color_fprintf(stderr, "%s:%d ", srcfile, lineno);
   if (symbol != NULL) {
      color_fprintf(stderr, "$yellow$%s$$", symbol);
      if (srcfile == NULL && disp != 0)
         color_fprintf(stderr, "$yellow$+0x%"PRIxPTR"$$", disp);
   }
   if (kind == FRAME_VHDL)
      color_fprintf(stderr, " $magenta$[VHDL]$$");
   fprintf(stderr, "\n");

#ifndef __MINGW32__
   if (srcfile != NULL) {
      FILE *f = fopen(srcfile, "r");
      if (f != NULL) {
         char *line LOCAL = NULL;
         size_t linesz = 0;
         for (int i = 0, len; i < lineno + 1
                 && (len = getline(&line, &linesz, f)) != -1; i++) {
            if (i < lineno - 2)
               continue;

            if (len <= 1)
               continue;
            else if (line[len - 1] == '\n')
               line[len - 1] = '\0';

            if (i == lineno - 1)
               color_fprintf(stderr, "$cyan$$bold$-->$$ $cyan$%s$$\n", line);
            else
               color_fprintf(stderr, "    $cyan$%s$$\n", line);
         }
         fclose(f);
      }
   }
#endif
}

__attribute__((noinline))
void show_stacktrace(void)
{
   debug_info_t *di = debug_capture();

   const int nframes = debug_count_frames(di);
   for (int n = 1; n < nframes; n++) {
      const debug_frame_t *f = debug_get_frame(di, n);

      for (debug_inline_t *inl = f->inlined; inl != NULL; inl = inl->next)
         trace_one_frame(f->pc, f->module, inl->srcfile, inl->symbol,
                         inl->lineno, inl->colno, f->disp, f->kind);

      trace_one_frame(f->pc, f->module, f->srcfile, f->symbol, f->lineno,
                      f->colno, f->disp, f->kind);

   }

   debug_free(di);

#if defined __linux__ && !defined HAVE_LIBDW && !defined HAVE_LIBDWARF
   color_fprintf(stderr, "\n$cyan$Hint: you can get better stack traces by "
                 "installing the libdw-dev package and reconfiguring$$\n");
#endif

   fflush(stderr);
}

#ifdef __MINGW32__

static const char *exception_name(DWORD code)
{
   switch (code) {
   case EXCEPTION_ACCESS_VIOLATION:
      return "EXCEPTION_ACCESS_VIOLATION";
   case EXCEPTION_ARRAY_BOUNDS_EXCEEDED:
      return "EXCEPTION_ARRAY_BOUNDS_EXCEEDED";
   case EXCEPTION_BREAKPOINT:
      return "EXCEPTION_BREAKPOINT";
   case EXCEPTION_DATATYPE_MISALIGNMENT:
      return "EXCEPTION_DATATYPE_MISALIGNMENT";
   case EXCEPTION_ILLEGAL_INSTRUCTION:
      return "EXCEPTION_ILLEGAL_INSTRUCTION";
   case EXCEPTION_IN_PAGE_ERROR:
      return "EXCEPTION_IN_PAGE_ERROR";
   case EXCEPTION_INT_DIVIDE_BY_ZERO:
      return "EXCEPTION_INT_DIVIDE_BY_ZERO";
   case EXCEPTION_INT_OVERFLOW:
      return "EXCEPTION_INT_OVERFLOW";
   case EXCEPTION_PRIV_INSTRUCTION:
      return "EXCEPTION_PRIV_INSTRUCTION";
   case EXCEPTION_STACK_OVERFLOW:
      return "EXCEPTION_STACK_OVERFLOW";
   }

   return "???";
}

WINAPI
static LONG win32_exception_handler(EXCEPTION_POINTERS *ExceptionInfo)
{
   DWORD code = ExceptionInfo->ExceptionRecord->ExceptionCode;
   PVOID addr = ExceptionInfo->ExceptionRecord->ExceptionAddress;

#ifdef __WIN64
   DWORD64 ip = ExceptionInfo->ContextRecord->Rip;
#else
   DWORD ip = ExceptionInfo->ContextRecord->Eip;
#endif

   if (code == EXCEPTION_ACCESS_VIOLATION)
      addr = (PVOID)ExceptionInfo->ExceptionRecord->ExceptionInformation[1];

   color_fprintf(stderr, "\n$red$$bold$*** Caught exception %x (%s)",
                 (int)code, exception_name(code));

   switch (code) {
   case EXCEPTION_ACCESS_VIOLATION:
   case EXCEPTION_ILLEGAL_INSTRUCTION:
      fprintf(stderr, " [address=%p, ip=%p]", (void *)addr, (void*)ip);
      break;
   }

   color_fprintf(stderr, " ***$$\n\n");
   fflush(stderr);

#ifdef __WIN64
   if (code != EXCEPTION_STACK_OVERFLOW)
      show_stacktrace();
#endif

  return EXCEPTION_EXECUTE_HANDLER;
}

#else

#ifndef __SANITIZE_THREAD__
static const char *signame(int sig, siginfo_t *info)
{
   switch (sig) {
   case SIGSEGV:
#ifdef __linux__
      switch (info->si_code) {
      case SEGV_MAPERR: return "SEGV_MAPERR";
      case SEGV_ACCERR: return "SEGV_ACCERR";
      default: return "SIGSEGV";
      }
#else
      return "SIGSEGV";
#endif
   case SIGABRT: return "SIGABRT";
   case SIGILL: return "SIGILL";
   case SIGFPE: return "SIGFPE";
   case SIGUSR1: return "SIGUSR1";
   case SIGUSR2: return "SIGUSR2";
   case SIGBUS: return "SIGBUS";
   case SIGINT: return "SIGINT";
   case SIGTRAP: return "SIGTRAP";
   default: return "???";
   }
}

static void print_fatal_signal(int sig, siginfo_t *info, struct cpu_state *cpu)
{
   static volatile __thread sig_atomic_t recurse = 0;

   if (recurse++ > 1) {
      signal(SIGABRT, SIG_DFL);
      abort();
   }

   char buf[512], *p = buf, *s = buf, *end = buf + ARRAY_LEN(buf);
   p += checked_sprintf(p, end - p, "\n%s*** Caught signal %d (%s)%s",
                        want_color ? "\033[31m\033[1m" : "",
                        sig, signame(sig, info),
                        recurse > 1 ? " inside signal handler" : "");

   switch (sig) {
   case SIGSEGV:
   case SIGILL:
   case SIGFPE:
   case SIGBUS:
      p += checked_sprintf(p, end - p, " [address=%p, ip=%p]",
                           info->si_addr, (void*)cpu->pc);
      break;
   }

   p += checked_sprintf(p, end - p, " ***%s\n\n", want_color ? "\033[0m" : "");

   for (int n; s < p && (n = write(STDERR_FILENO, s, p - s)) > 0; s += n);

   if (sig != SIGUSR1 && !atomic_cas(&crashing, SIG_ATOMIC_MAX, thread_id())) {
      sleep(60);
      _exit(EXIT_FAILURE);
   }
}
#endif  // !__SANITIZE_THREAD__

static __thread struct cpu_state *thread_regs = NULL;

static void signal_handler(int sig, siginfo_t *info, void *context)
{
   ucontext_t *uc = (ucontext_t*)context;
   struct cpu_state cpu;
   fill_cpu_state(&cpu, uc);

   struct cpu_state *req;
   if (sig == SIGUSR2 && (req = atomic_load(&thread_regs)) != NULL) {
      // Fill in registers for capture_registers
      *req = cpu;
      atomic_store(&thread_regs, NULL);
      return;
   }
   else if (sig == SIGINT) {
      void (*fn)(void *) = atomic_load(&ctrl_c_fn);
      if (fn != NULL) {
         (*fn)(ctrl_c_arg);
         return;
      }
   }

#ifdef __SANITIZE_THREAD__
   abort();
#else

   for (fault_handler_t *f = fault_handlers; f; f = f->next)
      (*f->fn)(sig, info->si_addr, &cpu, f->context);

   print_fatal_signal(sig, info, &cpu);

   show_stacktrace();

   if (sig != SIGUSR1)
      _exit(2);
#endif  // !__SANITIZE_THREAD__
}
#endif  // ! __MINGW32__

void register_signal_handlers(void)
{
#ifdef __MINGW32__
   SetUnhandledExceptionFilter(win32_exception_handler);
#else

   struct sigaction sa = {
      .sa_sigaction = signal_handler,
      .sa_flags = SA_RESTART | SA_SIGINFO
   };
   sigemptyset(&sa.sa_mask);

#ifndef __SANITIZE_THREAD__
   sigaction(SIGSEGV, &sa, NULL);
   sigaction(SIGUSR1, &sa, NULL);
   sigaction(SIGFPE, &sa, NULL);
   sigaction(SIGBUS, &sa, NULL);
   sigaction(SIGILL, &sa, NULL);
   sigaction(SIGABRT, &sa, NULL);
   sigaction(SIGTRAP, &sa, NULL);
#endif  // !__SANITIZE_THREAD__
   sigaction(SIGUSR2, &sa, NULL);
#endif  // !__MINGW32__
}

#ifdef __MINGW32__
static BOOL win32_ctrl_c_handler(DWORD fdwCtrlType)
{
   switch (fdwCtrlType) {
   case CTRL_C_EVENT:
      {
         void (*fn)(void *) = atomic_load(&ctrl_c_fn);
         if (fn != NULL)
            (*fn)(ctrl_c_arg);
         return TRUE;
      }

   default:
      return FALSE;
   }
}
#endif

void set_ctrl_c_handler(void (*fn)(void *), void *arg)
{
   ctrl_c_arg = arg;
   atomic_store(&ctrl_c_fn, fn);

   if (fn != NULL) {
#ifndef __MINGW32__
      struct sigaction sa = {};
      sa.sa_sigaction = signal_handler;
      sigemptyset(&sa.sa_mask);
      sa.sa_flags = SA_RESTART | SA_SIGINFO;

      sigaction(SIGINT, &sa, NULL);
#else
      if (!SetConsoleCtrlHandler(win32_ctrl_c_handler, TRUE))
         fatal_trace("SetConsoleCtrlHandler");
#endif
   }
   else {
#ifndef __MINGW32__
      struct sigaction sa = {};
      sa.sa_handler = SIG_DFL;
      sigaction(SIGINT, &sa, NULL);
#else
      if (!SetConsoleCtrlHandler(win32_ctrl_c_handler, FALSE))
         fatal_trace("SetConsoleCtrlHandler");
#endif
   }
}

#ifdef __MINGW32__
static void restore_win32_codepage(void)
{
   assert(win32_codepage != 0);
   SetConsoleOutputCP(win32_codepage);
}
#endif

void term_init(void)
{
   const char *nvc_colors = getenv("NVC_COLORS");
   const char *term = getenv("TERM") ?: "";

   static const char *term_blacklist[] = {
      "dumb"
   };

   spin_wait();  // Dummy, to force linking thread.c

   bool is_tty = isatty(STDERR_FILENO);

#ifdef __MINGW32__
   if (!is_tty) {
      // Handle running under MinTty
      HANDLE hStdOut = GetStdHandle(STD_OUTPUT_HANDLE);
      const size_t size = sizeof(FILE_NAME_INFO) + sizeof(WCHAR) * MAX_PATH;
      FILE_NAME_INFO *nameinfo = malloc(size);
      if (!GetFileInformationByHandleEx(hStdOut, FileNameInfo, nameinfo, size))
         fatal_errno("GetFileInformationByHandle");

      if ((wcsncmp(nameinfo->FileName, L"\\msys-", 6) == 0
           || wcsncmp(nameinfo->FileName, L"\\cygwin-", 8) == 0)
          && wcsstr(nameinfo->FileName, L"pty") != NULL)
         is_tty = true;

      free(nameinfo);
   }
#endif

   if (nvc_colors && strcmp(nvc_colors, "always") == 0)
      want_color = true;
   else if (nvc_colors && strcmp(nvc_colors, "never") == 0)
      want_color = false;
   else {
      want_color = is_tty;

      if (want_color && (term != NULL)) {
         for (size_t i = 0; i < ARRAY_LEN(term_blacklist); i++) {
            if (strcmp(term, term_blacklist[i]) == 0) {
               want_color = false;
               break;
            }
         }
      }
   }

#ifdef __MINGW32__
   HANDLE hConsole = GetStdHandle(STD_ERROR_HANDLE);
   DWORD mode;
   if (GetConsoleMode(hConsole, &mode)) {
      mode |= ENABLE_VIRTUAL_TERMINAL_PROCESSING | ENABLE_PROCESSED_OUTPUT;
      if (!SetConsoleMode(hConsole, mode))
         want_color = false;

      CONSOLE_SCREEN_BUFFER_INFO info;
      if (GetConsoleScreenBufferInfo(hConsole, &info))
         term_width = info.dwSize.X;
      else
         term_width = 80;
   }
#else
   if (is_tty) {
      // Try to find the terminal size with tcgetwinsize or TIOCGWINSZ
      term_width = 80;
#if defined HAVE_TCGETWINSIZE
      struct winsize ws;
      if (tcgetwinsize(STDIN_FILENO, &ws) == 0)
         term_width = ws.ws_col;
#elif defined TIOCGWINSZ
      struct winsize ws;
      if (ioctl(STDIN_FILENO, TIOCGWINSZ, &ws) == 0)
         term_width = ws.ws_col;
#endif
   }
#endif

#ifndef __MINGW32__
   // Only print link escape codes if this is really a terminal
   want_links = want_color && is_tty;
#else
   want_links = false;    // Winpty doesn't recognise these
#endif

#ifndef __MINGW32__
   // Assume the terminal is expecting UTF-8 by default
   want_utf8 = true;

   const char *lang = getenv("LANG");
   if (lang != NULL && *lang != '\0' && strcasestr(lang, "utf-8") == NULL)
      want_utf8 = false;
#else
   win32_codepage = GetConsoleOutputCP();
   if (win32_codepage == 65001)
      want_utf8 = true;
   else if (win32_codepage != 28591) {
      SetConsoleOutputCP(28591);
      atexit(restore_win32_codepage);
   }
#endif

   // Diagnostics are printed to stderr and explicitly flushed
   setvbuf(stderr, NULL, _IOLBF, BUFSIZ);
}

int terminal_width(void)
{
   return term_width;
}

const char *ordinal_str(int n)
{
   switch (n) {
   case 1: return "first";
   case 2: return "second";
   case 3: return "third";
   default:
      {
         static char buf[16];
         if (n > 20 && n % 10 == 1)
            checked_sprintf(buf, sizeof(buf), "%dst", n);
         else if (n > 20 && n % 10 == 2)
            checked_sprintf(buf, sizeof(buf), "%dnd", n);
         else if (n > 20 && n % 10 == 2)
            checked_sprintf(buf, sizeof(buf), "%drd", n);
         else
            checked_sprintf(buf, sizeof(buf), "%dth", n);
         return buf;
      }
   }
}

char *null_terminate(const uint8_t *data, size_t len)
{
   char *cstr = xmalloc(len + 1);
   if (data != NULL)
      memcpy(cstr, data, len);
   else
      assert(len == 0);
   cstr[len] = '\0';
   return cstr;
}

char toupper_iso88591(unsigned char ch)
{
   if (ch >= 'a' && ch <= 'z')
      return ch - 'a' + 'A';
   else if ((ch >= 0xe0 && ch <= 0xf6) || (ch >= 0xf8 && ch <= 0xfe))
      return ch - 0x20;
   else
      return ch;
}

char tolower_iso88591(unsigned char ch)
{
   if (ch >= 'A' && ch <= 'Z')
      return ch + 'a' - 'A';
   else if ((ch >= 0xc0 && ch <= 0xd6) || (ch >= 0xd8 && ch <= 0xde))
      return ch + 0x20;
   else
      return ch;
}

bool isprint_iso88591(unsigned char ch)
{
   return (ch >= 0x20 && ch <= 0x7e) || (ch >= 0xa0 && ch <= 0xff);
}

bool isspace_iso88591(unsigned char ch)
{
   return ch == ' ' || ch == '\t' || ch == '\n' || ch == '\v' || ch == '\f'
      || ch == '\r' || ch == 0xa0;
}

bool isdigit_iso88591(unsigned char ch)
{
   return ch >= '0' && ch <= '9';
}

bool isupper_iso88591(unsigned char ch)
{
   return (ch >= 'A' && ch <= 'Z')
      || (ch >= 0xc0 && ch <= 0xd6)
      || (ch >= 0xd8 && ch <= 0xde);
}

bool isalnum_iso88591(unsigned char ch)
{
   return (ch >= 'A' && ch <= 'Z')
      || (ch >= 'a' && ch <= 'z')
      || (ch >= '0' && ch <= '9')
      || (ch >= 0xc0 && ch <= 0xd6)
      || (ch >= 0xd8 && ch <= 0xde)
      || (ch >= 0xe0 && ch <= 0xf6)
      || (ch >= 0xf8 && ch <= 0xfe);
}

int next_power_of_2(int n)
{
   n--;
   n |= n >> 1;
   n |= n >> 2;
   n |= n >> 4;
   n |= n >> 8;
   n |= n >> 16;
   n++;
   return n;
}

int ilog2(int64_t n)
{
   if (n <= 1)
      return 0;
   else {
      int r = 0;
      int64_t c = 1;
      while (c < n) {
         r += 1;
         c <<= 1;
      }
      return r;
   }
}

bool ipow_safe(int64_t x, int64_t y, int64_t *result)
{
   assert(y >= 0);
   int overflow = 0, xo = 0;
   int64_t r = 1;
   while (y) {
      if (y & 1)
         overflow |= xo || __builtin_mul_overflow(r, x, &r);
      y >>= 1;
      xo |= __builtin_mul_overflow(x, x, &x);
   }
   *result = r;
   return !overflow;
}

int64_t ipow(int64_t x, int64_t y)
{
   int64_t result;
   if (!ipow_safe(x, y, &result))
      DEBUG_ONLY(fatal_trace("integer overflow in ipow"));

   return result;
}

#ifndef __SANITIZE_ADDRESS__
static long nvc_page_size(void)
{
#ifdef __MINGW32__
   SYSTEM_INFO si;
   GetSystemInfo(&si);
   return si.dwPageSize;
#else
   return sysconf(_SC_PAGESIZE);
#endif
}
#endif

void nvc_munmap(void *ptr, size_t length)
{
#if __SANITIZE_ADDRESS__
   free(ptr);
#elif !defined __MINGW32__
   if (munmap(ptr, length) != 0)
      fatal_errno("munmap");
#else
   if (!VirtualFree(ptr, length, MEM_DECOMMIT))
      fatal_errno("VirtualFree");
#endif
}

void *nvc_memalign(size_t align, size_t sz)
{
#if __SANITIZE_ADDRESS__
   void *ptr;
   if (posix_memalign(&ptr, align, sz) != 0)
      fatal_errno("posix_memalign");

   memset(ptr, '\0', sz);
   return ptr;
#else
   assert(is_power_of_2(align));
   const size_t mapalign = MAX(align, nvc_page_size());
   const size_t mapsz = ALIGN_UP(sz + mapalign - 1, mapalign);

#if defined __MINGW32__
   void *ptr = VirtualAlloc(NULL, mapsz, MEM_COMMIT | MEM_RESERVE,
                            PAGE_READWRITE);
   if (ptr == NULL)
      fatal_errno("VirtualAlloc");
#else
   void *ptr = mmap(NULL, mapsz, PROT_READ | PROT_WRITE,
                    MAP_PRIVATE | MAP_ANON, -1, 0);
   if (ptr == MAP_FAILED)
      fatal_errno("mmap");
#endif

   void *aligned = ALIGN_UP(ptr, align);
   void *limit = aligned + sz;

   if (align > nvc_page_size()) {
      const size_t low_waste = aligned - ptr;
      const size_t high_waste = ptr + mapsz - limit;
      assert(low_waste + high_waste == align);

      if (low_waste > 0) nvc_munmap(ptr, low_waste);
      if (high_waste > 0) nvc_munmap(limit, high_waste);
   }

   return aligned;
#endif
}

void nvc_memprotect(void *ptr, size_t length, mem_access_t prot)
{
#if defined __MINGW32__
   static const int map[] = {
      PAGE_NOACCESS, PAGE_READONLY, PAGE_READWRITE, PAGE_EXECUTE_READ,
      PAGE_EXECUTE_READWRITE
   };
   DWORD old_prot;
   if (!VirtualProtect(ptr, length, map[prot], &old_prot))
      fatal_errno("VirtualProtect");
#else
#if __SANITIZE_ADDRESS__
   // LeakSanitizer will not detect leaks in regions mapped read-only
   if (prot == MEM_RO || prot == MEM_NONE)
      return;
#endif
   static const int map[] = {
      PROT_NONE, PROT_READ, PROT_READ | PROT_WRITE, PROT_READ | PROT_EXEC,
      PROT_READ | PROT_WRITE | PROT_EXEC
   };
   if (mprotect(ptr, length, map[prot]) < 0)
      fatal_errno("mprotect");
#endif
}

void *map_huge_pages(size_t align, size_t sz)
{
#ifdef __linux__
   if (sz >= HUGE_PAGE_SIZE) {
      const size_t mapsz = ALIGN_UP(sz, HUGE_PAGE_SIZE);
      void *mem = nvc_memalign(MAX(HUGE_PAGE_SIZE, align), mapsz);

      if (madvise(mem, mapsz, MADV_HUGEPAGE) < 0)
         warnf("madvise: MADV_HUGEPAGE: %s", last_os_error());

      return mem;
   }
#endif

   return nvc_memalign(align, sz);
}

void *map_jit_pages(size_t align, size_t sz)
{
#ifdef __APPLE__
   void *ptr = mmap(NULL, sz, PROT_READ | PROT_WRITE | PROT_EXEC,
                    MAP_PRIVATE | MAP_ANON | MAP_JIT, -1, 0);
   if (ptr == MAP_FAILED)
      fatal_errno("mmap");
#else
   void *ptr = map_huge_pages(align, sz);
   nvc_memprotect(ptr, sz, MEM_RWX);
#endif

   return ptr;
}

int checked_sprintf(char *buf, int len, const char *fmt, ...)
{
   assert(len > 0);

   va_list ap;
   va_start(ap, fmt);

   const int nbytes = vsnprintf(buf, len, fmt, ap);
   if (nbytes >= len)
      fatal_trace("checked_sprintf requires %d bytes but have %d",
                  nbytes + 1, len);

   va_end(ap);

   return nbytes;
}

text_buf_t *tb_new(void)
{
   text_buf_t *tb = xmalloc(sizeof(text_buf_t));
   tb->alloc = 256;
   tb->len   = 0;
   tb->buf   = xmalloc(tb->alloc);

   tb->buf[0] = '\0';

   return tb;
}

void tb_free(text_buf_t *tb)
{
   free(tb->buf);
   free(tb);
}

void _tb_cleanup(text_buf_t **tb)
{
   if (*tb != NULL)
      tb_free(*tb);
}

void tb_vprintf(text_buf_t *tb, const char *fmt, va_list ap)
{
   int nchars, avail;
   for (;;) {
      va_list aq;
      va_copy(aq, ap);

      avail  = tb->alloc - tb->len;
      nchars = vsnprintf(tb->buf + tb->len, avail, fmt, aq);

      va_end(aq);

      if (nchars + 1 < avail)
         break;

      tb->alloc *= 2;
      tb->buf = xrealloc(tb->buf, tb->alloc);
   }

   tb->len += nchars;
}

void tb_printf(text_buf_t *tb, const char *fmt, ...)
{
   va_list ap;
   va_start(ap, fmt);
   tb_vprintf(tb, fmt, ap);
   va_end(ap);
}

void tb_istr(text_buf_t *tb, ident_t ident)
{
   // TODO: this function seems useless now
   tb_cat(tb, istr(ident));
}

void tb_append(text_buf_t *tb, char ch)
{
   if (tb->len + 2 >= tb->alloc) {
      tb->alloc *= 2;
      tb->buf = xrealloc(tb->buf, tb->alloc);
   }

   tb->buf[(tb->len)++] = ch;
   tb->buf[tb->len] = '\0';
}

void tb_catn(text_buf_t *tb, const char *str, size_t nchars)
{
   if (tb->len + nchars + 1 >= tb->alloc) {
      tb->alloc = next_power_of_2(tb->alloc + nchars);
      tb->buf = xrealloc(tb->buf, tb->alloc);
   }

   memcpy(tb->buf + tb->len, str, nchars);
   tb->len += nchars;
   tb->buf[tb->len] = '\0';
}

void tb_cat(text_buf_t *tb, const char *str)
{
   tb_catn(tb, str, strlen(str));
}

void tb_repeat(text_buf_t *tb, char ch, size_t count)
{
   if (tb->len + count + 1 >= tb->alloc) {
      tb->alloc = next_power_of_2(tb->alloc + count + 1);
      tb->buf = xrealloc(tb->buf, tb->alloc);
   }

   memset(tb->buf + tb->len, ch, count);
   tb->len += count;
   tb->buf[tb->len] = '\0';
}

char *tb_reserve(text_buf_t *tb, size_t size)
{
   if (tb->len + size + 1 >= tb->alloc) {
      tb->alloc = next_power_of_2(tb->alloc + size + 1);
      tb->buf = xrealloc(tb->buf, tb->alloc);
   }

   char *start = tb->buf + tb->len;

   tb->len += size;
   tb->buf[tb->len] = '\0';

   return start;
}

size_t tb_len(text_buf_t *tb)
{
   return tb->len;
}

char *tb_claim(text_buf_t *tb)
{
   char *buf = tb->buf;
   tb->buf = NULL;
   return buf;
}

void tb_move(text_buf_t *to, text_buf_t *from)
{
   free(to->buf);

   to->buf = from->buf;
   to->len = from->len;
   to->alloc = from->alloc;

   from->alloc = from->len = 0;
   from->buf = NULL;
}

const char *tb_get(text_buf_t *tb)
{
   return tb->buf;
}

void tb_rewind(text_buf_t *tb)
{
   tb->len = 0;
   tb->buf[0] = '\0';
}

void tb_trim(text_buf_t *tb, size_t newlen)
{
   assert(newlen <= tb->len);
   tb->len = newlen;
   tb->buf[tb->len] = '\0';
}

void tb_strip(text_buf_t *tb)
{
   while (tb->len > 0 && isspace_iso88591(tb->buf[tb->len - 1]))
      tb->buf[--(tb->len)] = '\0';
}

void tb_downcase(text_buf_t *tb)
{
   for (size_t i = 0; i < tb->len; i++)
      tb->buf[i] = tolower_iso88591(tb->buf[i]);
}

void tb_upcase(text_buf_t *tb)
{
   for (size_t i = 0; i < tb->len; i++)
      tb->buf[i] = toupper_iso88591(tb->buf[i]);
}

void tb_replace(text_buf_t *tb, char old, char rep)
{
   for (size_t i = 0; i < tb->len; i++) {
      if (tb->buf[i] == old)
         tb->buf[i] = rep;
   }
}

void _local_free(void *ptr)
{
   free(*(void **)ptr);
}

void set_message_style(message_style_t style)
{
   message_style = style;

   if (style == MESSAGE_COMPACT)
      want_color = false;
}

message_style_t get_message_style(void)
{
   return message_style;
}

#ifndef __MINGW32__
static uint64_t timeval_us(struct timeval *tv)
{
   return (tv->tv_sec * UINT64_C(1000000)) + tv->tv_usec;
}
#endif

void nvc_rusage(nvc_rusage_t *ru)
{
#ifndef __MINGW32__
   static uint64_t last_user, last_sys;

   struct rusage buf;
   if (getrusage(RUSAGE_SELF, &buf) < 0)
      fatal_errno("getrusage");

   const uint64_t user = timeval_us(&(buf.ru_utime));
   const uint64_t sys = timeval_us(&(buf.ru_stime));

   ru->user = (user - last_user) / 1000;
   ru->sys = (sys - last_sys) / 1000;

   last_sys = sys;
   last_user = user;

#ifdef __APPLE__
   const int rss_units = 1024;
#else
   const int rss_units = 1;
#endif

   ru->rss = buf.ru_maxrss / rss_units;
#else
   static ULARGE_INTEGER last_kernel, last_user;
   ULARGE_INTEGER lv_Tkernel, lv_Tuser;
   HANDLE hProcess = GetCurrentProcess();

   FILETIME ftCreation, ftExit, ftKernel, ftUser;
   if (!GetProcessTimes(hProcess, &ftCreation, &ftExit, &ftKernel, &ftUser))
      fatal_errno("GetProcessTimes");

   lv_Tkernel.LowPart = ftKernel.dwLowDateTime;
   lv_Tkernel.HighPart = ftKernel.dwHighDateTime;
   lv_Tuser.LowPart = ftUser.dwLowDateTime;
   lv_Tuser.HighPart = ftUser.dwHighDateTime;

   ru->user = (lv_Tuser.QuadPart - last_user.QuadPart) / 10000;
   ru->sys = (lv_Tkernel.QuadPart - last_kernel.QuadPart) / 10000;

   last_user = lv_Tuser;
   last_kernel = lv_Tkernel;

   PROCESS_MEMORY_COUNTERS counters;
   if (!GetProcessMemoryInfo(GetCurrentProcess(), &counters, sizeof(counters)))
      fatal_errno("GetProcessMemoryInfo");

   ru->rss = counters.PeakWorkingSetSize / 1024;
#endif

   static uint64_t last_ts;
   const uint64_t ts = get_timestamp_us();
   ru->ms = last_ts == 0 ? ru->user + ru->sys : (ts - last_ts) / 1000;
   last_ts = ts;
}

#ifdef __MINGW32__
static uint64_t file_time_to_nanos(LPFILETIME ft)
{
   uint64_t nanos = (uint64_t)ft->dwHighDateTime << 32;
   nanos |= ft->dwLowDateTime;

   // Windows file timestamps are in units of 100 nanoseconds since
   // 1601-01-01T00:00:00Z: convert that to nanoseconds since the Unix
   // epoch 1970-01-01T00:00:00Z
   nanos -= UINT64_C(116444736000000000);
   nanos *= 100;

   return nanos;
}

static bool fill_file_info(file_info_t *info, HANDLE handle)
{
   memset(info, '\0', sizeof(file_info_t));

   BY_HANDLE_FILE_INFORMATION hinfo;
   if (!GetFileInformationByHandle(handle, &hinfo))
      fatal_errno("GetFileInformationByHandle");

   info->size = (uint64_t)hinfo.nFileSizeHigh << 32;
   info->size |= hinfo.nFileSizeLow;

   if (hinfo.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY)
      info->type = FILE_DIR;
   else
      info->type = FILE_REGULAR;

   info->mtime = file_time_to_nanos(&(hinfo.ftLastWriteTime));

   return true;
}
#else  // !__MINGW32__
static void fill_file_info(file_info_t *info, const struct stat *st)
{
   memset(info, '\0', sizeof(file_info_t));

   info->size = st->st_size;

   if (S_ISDIR(st->st_mode))
      info->type = FILE_DIR;
   else if (!S_ISREG(st->st_mode))
      info->type = FILE_FIFO;
   else
      info->type = FILE_REGULAR;

   info->mtime = st->st_mtime * UINT64_C(1000000000);

#if defined HAVE_STRUCT_STAT_ST_MTIMESPEC_TV_NSEC
   info->mtime += st->st_mtimespec.tv_nsec;
#elif defined HAVE_STRUCT_STAT_ST_MTIM_TV_NSEC
   info->mtime += st->st_mtim.tv_nsec;
#endif
}
#endif  // !__MINGW32__

bool get_file_info(const char *path, file_info_t *info)
{
#ifdef __MINGW32__
   HANDLE handle = CreateFile(
        path, FILE_READ_ATTRIBUTES,
        FILE_SHARE_DELETE | FILE_SHARE_READ | FILE_SHARE_WRITE,
        NULL, OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS, NULL);

   if (handle == INVALID_HANDLE_VALUE)
      return false;

   fill_file_info(info, handle);

   if (!CloseHandle(handle))
      fatal_errno("CloseHandle");

   return true;
#else
   struct stat st;
   if (stat(path, &st) == 0) {
      fill_file_info(info, &st);
      return true;
   }
   else
      return false;
#endif
}

bool get_handle_info(int fd, file_info_t *info)
{
#ifdef __MINGW32__
   HANDLE handle = (HANDLE)_get_osfhandle(fd);
   fill_file_info(info, handle);
   return true;
#else
   struct stat st;
   if (fstat(fd, &st) == 0) {
      fill_file_info(info, &st);
      return true;
   }
   else
      return false;
#endif
}

bool get_handle_path(int fd, text_buf_t *tb)
{
#ifdef __MINGW32__
   HANDLE handle = (HANDLE)_get_osfhandle(fd);
   char buf[PATH_MAX];

   DWORD nchars = GetFinalPathNameByHandle(handle, buf, PATH_MAX, 0);
   if (nchars == 0)
      return false;

   tb_catn(tb, buf, nchars);
   return true;
#elif defined __linux__
   char path[64], buf[PATH_MAX];
   checked_sprintf(path, sizeof(path), "/proc/self/fd/%d", fd);

   size_t nchars = readlink(path, buf, PATH_MAX);
   if (nchars == -1)
      return false;

   tb_catn(tb, buf, nchars);
   return true;
#elif defined F_GETPATH
   char buf[PATH_MAX];
   if (fcntl(fd, F_GETPATH, buf) == -1)
      return false;

   tb_cat(tb, buf);
   return true;
#else
   return false;
#endif
}

void run_program(const char *const *args)
{
#if defined __CYGWIN__ || defined __MINGW32__
   int status = spawnvp(_P_WAIT, args[0], (char *const *)args);
#else  // __CYGWIN__
   pid_t pid = fork();
   int status = 0;
   if (pid == 0) {
      execvp(args[0], (char *const *)args);
      fatal_errno("execv");
   }
   else if (pid > 0) {
      if (waitpid(pid, &status, 0) != pid)
         fatal_errno("waitpid");

      status = WEXITSTATUS(status);
   }
   else
      fatal_errno("fork");
#endif  // __CYGWIN__

   if (status != 0) {
      LOCAL_TEXT_BUF tb = tb_new();
      for (size_t i = 0; args[i] != NULL; i++)
         tb_printf(tb, "%s%s", i > 0 ? " " : "", args[i]);
      fatal("$bold$%s$$ failed with status %d", tb_get(tb), status);
   }
}

char *nvc_temp_file(void)
{
   static const char *try[] = { "TMPDIR", "TEMP", "TMP" };
   const char *tmpdir = NULL;
   for (int i = 0; tmpdir == NULL && i < ARRAY_LEN(try); i++)
      tmpdir = getenv(try[i]);

#ifdef __MINGW32__
   char *buf = xasprintf("%s\\nvc-XXXXXX", tmpdir ?: ".");
   return _mktemp(buf);
#else
   char *buf = xasprintf("%s/nvc-XXXXXX", tmpdir ?: "/tmp");
   int fd = mkstemp(buf);
   if (fd < 0)
      fatal_errno("mkstemp");
   close(fd);
   return buf;
#endif
}

void file_read_lock(int fd)
{
#ifdef __MINGW32__
   HANDLE hf = (HANDLE)_get_osfhandle(fd);

   LARGE_INTEGER li;
   li.QuadPart = _filelengthi64(fd);

   OVERLAPPED ovlp;
   memset(&ovlp, 0, sizeof ovlp);

   if (!LockFileEx(hf, 0, 0, li.LowPart, li.HighPart, &ovlp))
      fatal_errno("LockFileEx");
#else
   if (flock(fd, LOCK_SH) < 0)
      fatal_errno("flock");
#endif
}

void file_write_lock(int fd)
{
#ifdef __MINGW32__
   HANDLE hf = (HANDLE)_get_osfhandle(fd);

   LARGE_INTEGER li;
   li.QuadPart = _filelengthi64(fd);

   OVERLAPPED ovlp;
   memset(&ovlp, 0, sizeof ovlp);

   if (!LockFileEx(hf, LOCKFILE_EXCLUSIVE_LOCK, 0,
                   li.LowPart, li.HighPart, &ovlp))
      fatal_errno("LockFileEx");
#else
   if (flock(fd, LOCK_EX) < 0)
      fatal_errno("flock");
#endif
}

void file_unlock(int fd)
{
#ifdef __MINGW32__
   HANDLE hf = (HANDLE)_get_osfhandle(fd);

   LARGE_INTEGER li;
   li.QuadPart = _filelengthi64 (fd);

   UnlockFile(hf, 0, 0, li.LowPart, li.HighPart);
#else
   if (flock(fd, LOCK_UN) < 0)
      fatal_errno("flock");
#endif
}

void *map_file(int fd, size_t size)
{
#ifdef __MINGW32__
   HANDLE handle = CreateFileMapping((HANDLE) _get_osfhandle(fd), NULL,
                                     PAGE_READONLY, 0, size, NULL);
   if (!handle)
      fatal_errno("CreateFileMapping");

   void *ptr = MapViewOfFileEx(handle, FILE_MAP_READ, 0,
                               0, (SIZE_T) size, (LPVOID) NULL);
   CloseHandle(handle);
   if (ptr == NULL)
      fatal_errno("MapViewOfFileEx");
#else
   void *ptr = mmap(NULL, size, PROT_READ, MAP_PRIVATE, fd, 0);
   if (ptr == MAP_FAILED)
      fatal_errno("mmap");
#endif
   return ptr;
}

void unmap_file(void *ptr, size_t size)
{
#ifdef __MINGW32__
   if (!UnmapViewOfFile((LPCVOID) ptr))
      fatal_errno("UnmapViewOfFile");
#else
   munmap(ptr, size);
#endif
}

void make_dir(const char *path)
{
#ifdef __MINGW32__
   if (!CreateDirectory(path, NULL) && (GetLastError() != ERROR_ALREADY_EXISTS))
      fatal_errno("mkdir: %s", path);
#else
   if (mkdir(path, 0777) != 0 && errno != EEXIST)
      fatal_errno("mkdir: %s", path);
#endif
}

uint64_t get_timestamp_ns(void)
{
#if defined __MINGW32__
   static volatile uint64_t freq;
   if (load_acquire(&freq) == 0) {
      LARGE_INTEGER tmp;
      if (!QueryPerformanceFrequency(&tmp))
         fatal_errno("QueryPerformanceFrequency");
      store_release(&freq, tmp.QuadPart);
   }

   LARGE_INTEGER ticks;
   if (!QueryPerformanceCounter(&ticks))
      fatal_errno("QueryPerformanceCounter");
   return (double)ticks.QuadPart * (1e9 / (double)freq);
#else
   struct timespec ts;
   if (clock_gettime(CLOCK_MONOTONIC, &ts) != 0)
      fatal_errno("clock_gettime");
   return ts.tv_nsec + (ts.tv_sec * UINT64_C(1000000000));
#endif
}

uint64_t get_timestamp_us(void)
{
   return get_timestamp_ns() / 1000;
}

timestamp_t get_real_time(void)
{
#if defined __MINGW32__
   FILETIME ft;
   GetSystemTimeAsFileTime(&ft);

   return file_time_to_nanos(&ft);
#else
   struct timespec ts;
   if (clock_gettime(CLOCK_REALTIME, &ts) != 0)
      fatal_errno("clock_gettime");

   return (uint64_t)ts.tv_nsec + ((uint64_t)ts.tv_sec * UINT64_C(1000000000));
#endif
}

void open_pipe(int *rfd, int *wfd)
{
   int fds[2];
#ifdef __MINGW32__
   const int rc = _pipe(fds, 4096, _O_BINARY);
#else
   const int rc = pipe(fds) < 0;
#endif
   if (rc < 0)
      fatal_errno("failed to create pipe");

   *rfd = fds[0];
   *wfd = fds[1];
}

#if defined _WIN32 || defined __CYGWIN__
static struct {
   char illegal;
   const char *rep;
} symbol_replacements[] = {
   { '(', "_lp_"   },
   { ')', "_rp_"   },
   { '"', "_q_"    },
   { '[', "_ls_"   },
   { ']', "_rs_"   },
   { '*', "_mult_" },
   { '+', "_plus_" },
   { '=', "_eq_"   },
   { '\\', "_bs_"  },
};

static text_buf_t *safe_symbol_win32(const char *text)
{
   text_buf_t *tb = tb_new();

   for (const char *p = text; *p != '\0'; p++) {
      bool replaced = false;
      for (size_t j = 0; j < ARRAY_LEN(symbol_replacements); j++) {
         if (*p == symbol_replacements[j].illegal) {
            tb_cat(tb, symbol_replacements[j].rep);
            replaced = true;
            break;
         }
      }

      if (!replaced)
         tb_append(tb, *p);
   }

   return tb;
}

#endif

text_buf_t *safe_symbol(ident_t id)
{
   // Return a string that is safe to use as a symbol name on this platform

   text_buf_t *tb = tb_new();
   tb_istr(tb, id);

#if defined _WIN32 || defined __CYGWIN__
   if (strpbrk(tb_get(tb), "()\"[]*+=\\") == NULL)
      return tb;
   else {
      text_buf_t *new = safe_symbol_win32(tb_get(tb));
      tb_free(tb);
      return new;
   }
#else
   return tb;
#endif
}

text_buf_t *unsafe_symbol(const char *text)
{
   // Restore original symbol from safe_symbol

   text_buf_t *tb = tb_new();

#if defined _WIN32 || defined __CYGWIN__
   const char *p = text;
   while (*p) {
      bool replaced = false;
      for (size_t j = 0; j < ARRAY_LEN(symbol_replacements); j++) {
         size_t len = strlen(symbol_replacements[j].rep);
         if (strncmp(p, symbol_replacements[j].rep, len) == 0) {
            tb_append(tb, symbol_replacements[j].illegal);
            p += len;
            replaced = true;
            break;
         }
      }

      if (!replaced)
         tb_append(tb, *p++);
   }

   return tb;
#else
   tb_cat(tb, text);
#endif

   return tb;
}

void __cleanup_array(void *ptr)
{
   A(void *) *a = ptr;
   ACLEAR(*a);
}

void __array_resize_slow(void **ptr, uint32_t *limit, uint32_t count,
                         size_t size)
{
   if (count == 0) {
      free(*ptr);
      *ptr = NULL;
      *limit = 0;
   }
   else {
      if (*limit == 0)
         *limit = count;  // Setting the initial size of the array
      else
         *limit = next_power_of_2(count);
      *ptr = xrealloc_array(*ptr, *limit, size);
   }
}

char *search_path(const char *name)
{
   const char *path = getenv("PATH");
   if (path == NULL)
      return xstrdup(name);

   char LOCAL *tmp = xstrdup(path);
   for (char *p = strtok(tmp, ":"); p; p = strtok(NULL, ":")) {
      char *full = xasprintf("%s"DIR_SEP"%s", p, name);

      struct stat sb;
      if (stat(full, &sb) == 0)
         return full;

      free(full);
   }

   return xstrdup(name);
}

bool get_exe_path(text_buf_t *tb)
{
#if defined __linux__
   char buf[PATH_MAX];
   ssize_t nchars = readlink("/proc/self/exe", buf, sizeof(buf));
   if (nchars > 0) {   // Does not append '\0'
      tb_catn(tb, buf, nchars);
      return true;
   }
#elif defined __APPLE__
   char buf[PATH_MAX];
   if (proc_pidpath(getpid(), buf, sizeof(buf)) > 0) {
      tb_cat(tb, buf);
      return true;
   }
#elif defined __FreeBSD__
   char buf[PATH_MAX];
   size_t size = sizeof(buf);
   const int name[] = { CTL_KERN, KERN_PROC, KERN_PROC_PATHNAME, -1 };
   if (sysctl(name, ARRAY_LEN(name), buf, &size, NULL, 0) == 0) {
      tb_catn(tb, buf, size);
      return true;
   }
#elif defined __MINGW32__
   HANDLE hProc = GetCurrentProcess();
   char buf[PATH_MAX];
   DWORD size = sizeof(buf);
   if (QueryFullProcessImageNameA(hProc, 0, buf, &size)) {
      tb_cat(tb, buf);
      return true;
   }
#endif
   return false;
}

#if defined __MINGW32__
static void get_relative_prefix(text_buf_t *tb)
{
   if (get_exe_path(tb)) {
      int len = tb_len(tb);
      const char *str = tb_get(tb);
      for (int i = 0; i < 2; i++) {
         do {
            len--;
         } while (str[len] != DIR_SEP[0]);
      }
      tb_trim(tb, len);
   }
   else
      fatal("failed to read executable path");
}
#endif

void get_libexec_dir(text_buf_t *tb)
{
#if defined __MINGW32__
   get_relative_prefix(tb);
   tb_cat(tb, DIR_SEP "libexec" DIR_SEP "nvc");
#else
   tb_cat(tb, LIBEXECDIR);
#endif
}

void get_lib_dir(text_buf_t *tb)
{
#if defined __MINGW32__
   get_relative_prefix(tb);
   tb_cat(tb, DIR_SEP "lib" DIR_SEP "nvc");
#else
   tb_cat(tb, LIBDIR);
#endif
}

void get_data_dir(text_buf_t *tb)
{
#if defined __MINGW32__
   get_relative_prefix(tb);
   tb_cat(tb, DIR_SEP "share" DIR_SEP "nvc");
#else
   tb_cat(tb, DATADIR);
#endif
}

bool is_absolute_path(const char *path)
{
   if (path[0] == DIR_SEP[0] || path[0] == '/')
      return true;

#ifdef __MINGW32__
   if (isalpha((int)path[0]) && path[1] == ':')
      return true;
#endif

   return false;
}

void progress(const char *fmt, ...)
{
   if (opt_get_int(OPT_VERBOSE)) {
      va_list ap;
      va_start(ap, fmt);
      char *msg LOCAL = xvasprintf(fmt, ap);
      va_end(ap);

      static nvc_rusage_t last_ru;

      nvc_rusage_t ru;
      nvc_rusage(&ru);

      const double conc = (double)(ru.user + ru.sys) / ru.ms;

      if (!isinf(conc) && conc > 1.1)
         notef("%s [%ums %.1fx %+dkB]", msg, ru.ms, conc,
               ru.rss - last_ru.rss);
      else
         notef("%s [%ums %+dkB]", msg, ru.ms, ru.rss - last_ru.rss);

      last_ru = ru;
   }
}

unsigned nvc_nprocs(void)
{
#if defined _WIN32
   SYSTEM_INFO sysinfo;
   GetSystemInfo(&sysinfo);

   return sysinfo.dwNumberOfProcessors;
#elif defined _SC_NPROCESSORS_ONLN
   long count = sysconf(_SC_NPROCESSORS_ONLN);
   if (count == -1)
      fatal_errno("sysconf(_SC_NPROCESSORS_ONLN)");

#if defined __linux__ && defined HAVE_GETTID
   // Restrict to the number of CPUs we are allowed to run on
   cpu_set_t s;
   if (sched_getaffinity(gettid(), sizeof(cpu_set_t), &s) == 0)
      return MAX(1, MIN(count, CPU_COUNT(&s)));
#endif

   return count;
#else
#warning Cannot detect number of processors on this platform
   return 1;
#endif
}

void capture_registers(struct cpu_state *cpu)
{
#if defined HAVE_GETCONTEXT
   ucontext_t uc;
   if (getcontext(&uc) != 0)
      fatal_errno("getcontext");

   fill_cpu_state(cpu, &uc);
#elif defined __MINGW32__
   CONTEXT context;
   RtlCaptureContext(&context);
   fill_cpu_state(cpu, &context);
#elif defined HAVE_PTHREAD
   assert(atomic_load(&thread_regs) == NULL);
   atomic_store(&thread_regs, cpu);

   if (pthread_kill(pthread_self(), SIGUSR2) != 0)
      fatal_errno("pthread_kill");

   // Registers filled in by signal_handler
   if (atomic_load(&thread_regs) != NULL)
      fatal_trace("signal handler did not capture thread registers");
#else
#error cannot capture registers on this platform
#endif
}

void add_fault_handler(fault_fn_t fn, void *context)
{
   fault_handler_t *h = xmalloc(sizeof(fault_handler_t));
   h->next    = fault_handlers;
   h->fn      = fn;
   h->context = context;

   fault_handlers = h;
}

void remove_fault_handler(fault_fn_t fn, void *context)
{
   for (fault_handler_t **p = &fault_handlers; *p; p = &((*p)->next)) {
      if ((*p)->fn == fn && (*p)->context == context) {
         fault_handler_t *tmp = (*p)->next;
         free(*p);
         *p = tmp;
         return;
      }
   }

   fatal_trace("no fault handler for %p with context %p", fn, context);
}

void check_cpu_features(void)
{
#ifdef HAVE_POPCNT
   if (!__builtin_cpu_supports("popcnt"))
      fatal("CPU is missing support for POPCNT instruction, reconfigure "
            "with $bold$--disable-popcnt$$");
#endif
}

void list_add(ptr_list_t *l, void *item)
{
   if (*l == NULL) {
      *l = xmalloc_flex(sizeof(struct _ptr_list), 16, sizeof(void *));
      (*l)->count = 0;
      (*l)->max   = 16;
   }
   else if ((*l)->count == (*l)->max) {
      (*l)->max *= 2;
      *l = xrealloc_flex(*l, sizeof(struct _ptr_list),
                         (*l)->max, sizeof(void *));
   }

   (*l)->items[(*l)->count++] = item;
}

void list_free(ptr_list_t *l)
{
   if (*l != NULL)
      free(*l);
   *l = NULL;
}

void list_sort(ptr_list_t *l, list_cmp_fn_t cmp)
{
   if (*l != NULL)
      qsort((*l)->items, (*l)->count, sizeof(void *), cmp);
}

void list_clear(ptr_list_t *l)
{
   if (*l != NULL)
      (*l)->count = 0;
}
