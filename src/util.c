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

#if defined(__MINGW32__)
#define WINVER 0x0A00
#define _WIN32_WINNT 0x0A00
#include <windows.h>
#include <DbgHelp.h>
#include <fileapi.h>
#include <psapi.h>
#endif

#include "util.h"
#include "array.h"
#include "cpustate.h"
#include "debug.h"
#include "diag.h"
#include "opt.h"
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
#include <unistd.h>
#include <ctype.h>
#include <assert.h>
#include <limits.h>
#include <time.h>
#include <libgen.h>

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
#ifndef __MINGW32__
#include <sys/mman.h>
#include <sys/wait.h>
#include <sys/resource.h>
#include <sys/file.h>
#include <sys/ioctl.h>
#include <fcntl.h>
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

typedef void (*print_fn_t)(const char *fmt, ...);

static char *ansi_vasprintf(const char *fmt, va_list ap, bool force_plain);

typedef struct guard guard_t;

struct color_escape {
   const char *name;
   int         value;
};

struct guard {
   guard_fault_fn_t  fn;
   void             *context;
   uintptr_t         base;
   uintptr_t         limit;
   guard_t          *next;
};

struct text_buf {
   char  *buf;
   size_t alloc;
   size_t len;
};

static bool            want_color = false;
static bool            want_links = false;
static guard_t        *guards;
static message_style_t message_style = MESSAGE_FULL;
static sig_atomic_t    crashing = 0;
static int             term_width = 0;
static void           *ctrl_c_arg = NULL;

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
      fatal("memory exhausted (strdup %p)", str);
   return copy;
}

char *xvasprintf(const char *fmt, va_list ap)
{
   char *strp = NULL;
   if (vasprintf(&strp, fmt, ap) < 0)
      abort();
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
                     if (bold)
                        tb_printf(tb, "\033[1;%dm", escapes[i].value);
                     else
                        tb_printf(tb, "\033[%dm", escapes[i].value);
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

void fatal_exit(int status)
{
   if (atomic_load(&crashing))
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

   diag_set_consumer(NULL);
   diag_suppress(d, false);
   diag_emit(d);
   show_stacktrace();
   fatal_exit(EXIT_FAILURE);
}

void fatal_errno(const char *fmt, ...)
{
   diag_t *d = diag_new(DIAG_FATAL, NULL);

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
}

#ifndef __SANITIZE_THREAD__
static bool check_guard_page(uintptr_t addr)
{
   for (guard_t *it = guards; it != NULL; it = it->next) {
      if ((addr >= it->base) && (addr < it->limit)) {
         (*it->fn)((void *)addr, it->context);
         return true;
      }
   }

   return false;
}
#endif

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

   if (code == EXCEPTION_ACCESS_VIOLATION) {
      addr = (PVOID)ExceptionInfo->ExceptionRecord->ExceptionInformation[1];
      check_guard_page((uintptr_t)addr);
   }

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

static const char *signame(int sig)
{
   switch (sig) {
   case SIGSEGV: return "SIGSEGV";
   case SIGABRT: return "SIGABRT";
   case SIGILL: return "SIGILL";
   case SIGFPE: return "SIGFPE";
   case SIGUSR1: return "SIGUSR1";
   case SIGUSR2: return "SIGUSR2";
   case SIGBUS: return "SIGBUS";
   case SIGINT: return "SIGINT";
   default: return "???";
   }
}

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
   if (sig != SIGUSR1) {
      while (!atomic_cas(&crashing, 0, 1))
         sleep(1);
   }

   extern void check_frozen_object_fault(void *addr);

   if (sig == SIGSEGV) {
      signal(SIGSEGV, SIG_DFL);
      check_guard_page((uintptr_t)info->si_addr);
      check_frozen_object_fault(info->si_addr);
   }

   color_fprintf(stderr, "\n$red$$bold$*** Caught signal %d (%s)",
                 sig, signame(sig));

   switch (sig) {
   case SIGSEGV:
   case SIGILL:
   case SIGFPE:
   case SIGBUS:
      fprintf(stderr, " [address=%p, ip=%p]", info->si_addr, (void*)cpu.pc);
      break;
   }

   color_fprintf(stderr, " ***$$\n\n");
   fflush(stderr);

   show_stacktrace();

   if (sig != SIGUSR1)
      _exit(2);
#endif
}
#endif  // !__SANITIZE_THREAD__

void register_signal_handlers(void)
{
#ifdef __MINGW32__
   SetUnhandledExceptionFilter(win32_exception_handler);
#else

   struct sigaction sa;
   sa.sa_sigaction = signal_handler;
   sigemptyset(&sa.sa_mask);
   sa.sa_flags = SA_RESTART | SA_SIGINFO;

#ifndef __SANITIZE_THREAD__
   sigaction(SIGSEGV, &sa, NULL);
   sigaction(SIGUSR1, &sa, NULL);
   sigaction(SIGFPE, &sa, NULL);
   sigaction(SIGBUS, &sa, NULL);
   sigaction(SIGILL, &sa, NULL);
   sigaction(SIGABRT, &sa, NULL);
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
      mode |= 0x04; // ENABLE_VIRTUAL_TERMINAL_PROCESSING
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

   // Only print link escape codes if this is really a terminal
   want_links = want_color && is_tty;

   // Diagnostics are printed to stderr and explicitly flushed
   setvbuf(stderr, NULL, _IOLBF, 0);
}

int terminal_width(void)
{
   return term_width;
}

char *get_fmt_buf(size_t len)
{
   // This is a bit of a kludge but keeping a sufficient number
   // of static buffers allows us to use format functions multiple
   // times in printf
   static char   *buf_set[MAX_FMT_BUFS];
   static size_t  buflen[MAX_FMT_BUFS];
   static int     next_buf = 0;

   char **bufp = &buf_set[next_buf];
   size_t *blenp = &buflen[next_buf];
   next_buf = (next_buf + 1) % MAX_FMT_BUFS;

   if (*bufp == NULL) {
      *bufp = xmalloc(len);
      *blenp = len;
   }

   while (len > *blenp) {
      *blenp *= 2;
      *bufp = xrealloc(*bufp, *blenp);
   }

   return *bufp;
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
      return 1;
   else {
      int r = 0;
      int64_t c = 1;
      while (c < n) {
         r += 1;
         c *= 2;
      }
      return r;
   }
}

int64_t ipow(int64_t x, int64_t y)
{
   assert(y >= 0);
   int64_t r = 1;
   while (y) {
      if (y & 1)
         r *= x;
      y >>= 1;
      x *= x;
   }
   return r;
}

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

static void *nvc_mmap(size_t sz)
{
   sz = ALIGN_UP(sz, nvc_page_size());

#if __SANITIZE_ADDRESS__
   void *ptr;
   if (posix_memalign(&ptr, nvc_page_size(), sz) != 0)
      fatal_errno("posix_memalign");

   return ptr;
#elif !defined __MINGW32__
   void *ptr = mmap(NULL, sz, PROT_READ | PROT_WRITE,
                    MAP_PRIVATE | MAP_ANON, -1, 0);
   if (ptr == MAP_FAILED)
      fatal_errno("mmap");
#else
   void *ptr = VirtualAlloc(NULL, sz, MEM_COMMIT | MEM_RESERVE, PAGE_READWRITE);
   if (ptr == NULL)
      fatal_errno("VirtualAlloc");
#endif

   return ptr;
}

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
   assert((align & (align - 1)) == 0);
   const size_t mapsz = ALIGN_UP(sz + align - 1, align);
   void *ptr = nvc_mmap(mapsz);

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
#if __SANITIZE_ADDRESS__
   // Ignore it
#elif !defined __MINGW32__
   static const int map[] = {
      PROT_NONE, PROT_READ, PROT_READ | PROT_WRITE
   };
   if (mprotect(ptr, length, map[prot]) < 0)
      fatal_errno("mprotect");
#else
   static const int map[] = {
      PAGE_NOACCESS, PAGE_READONLY, PAGE_READWRITE
   };
   DWORD old_prot;
   if (!VirtualProtect(ptr, length, map[prot], &old_prot))
      fatal_errno("VirtualProtect");
#endif
}

void *mmap_guarded(size_t sz, guard_fault_fn_t fn, void *ctx)
{
   const long pagesz = nvc_page_size();
   sz = ALIGN_UP(sz, pagesz);

   void *ptr = nvc_mmap(sz + pagesz);

   uint8_t *guard_ptr = (uint8_t *)ptr + sz;
   nvc_memprotect(guard_ptr, pagesz, MEM_NONE);

   guard_t *guard = xmalloc(sizeof(guard_t));
   guard->next    = guards;
   guard->fn      = fn;
   guard->context = ctx;
   guard->base    = (uintptr_t)guard_ptr;
   guard->limit   = guard->base + pagesz;

   guards = guard;

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
   const size_t len = ident_len(ident);
   char *p = tb_reserve(tb, len);   // Adds one byte for terminating null
   istr_r(ident, p, len + 1);
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

void tb_downcase(text_buf_t *tb)
{
   for (size_t i = 0; i < tb->len; i++)
      tb->buf[i] = tolower((int)tb->buf[i]);
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
static unsigned tv2ms(struct timeval *tv)
{
   return (tv->tv_sec * 1000) + (tv->tv_usec / 1000);
}
#endif

void nvc_rusage(nvc_rusage_t *ru)
{
#ifndef __MINGW32__
   static struct rusage last;

   struct rusage sys;
   if (getrusage(RUSAGE_SELF, &sys) < 0)
      fatal_errno("getrusage");

   const unsigned utime = tv2ms(&(sys.ru_utime)) - tv2ms(&(last.ru_utime));
   const unsigned stime = tv2ms(&(sys.ru_stime)) - tv2ms(&(last.ru_stime));

   ru->ms = utime + stime;

#ifdef __APPLE__
   const int rss_units = 1024;
#else
   const int rss_units = 1;
#endif

   ru->rss = sys.ru_maxrss / rss_units;

   last = sys;
#else
   static long long last;
   ULARGE_INTEGER lv_Tkernel, lv_Tuser;
   HANDLE hProcess = GetCurrentProcess();

   FILETIME ftCreation, ftExit, ftKernel, ftUser;
   if (!GetProcessTimes(hProcess, &ftCreation, &ftExit, &ftKernel, &ftUser))
      fatal_errno("GetProcessTimes");

   lv_Tkernel.LowPart = ftKernel.dwLowDateTime;
   lv_Tkernel.HighPart = ftKernel.dwHighDateTime;
   lv_Tuser.LowPart = ftUser.dwLowDateTime;
   lv_Tuser.HighPart = ftUser.dwHighDateTime;

   ru->ms = (lv_Tkernel.QuadPart + lv_Tuser.QuadPart) / 10000 - last;
   last = ru->ms;

   PROCESS_MEMORY_COUNTERS counters;
   if (!GetProcessMemoryInfo(GetCurrentProcess(), &counters, sizeof(counters)))
      fatal_errno("GetProcessMemoryInfo");

   ru->rss = counters.PeakWorkingSetSize / 1024;
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
      execv(args[0], (char *const *)args);
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

   void *ptr = MapViewOfFileEx(handle, FILE_MAP_COPY, 0,
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

uint64_t get_timestamp_us()
{
#if defined __MINGW32__
   return 0;  // TODO
#else
   struct timespec ts;
   if (clock_gettime(CLOCK_MONOTONIC, &ts) != 0)
      fatal_errno("clock_gettime");
   return (ts.tv_nsec / 1000) + (ts.tv_sec * 1000 * 1000);
#endif
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
   { '=', "_eq_"   }
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
   if (strpbrk(tb_get(tb), "()\"[]*+=") == NULL)
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
