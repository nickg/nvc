//
//  Copyright (C) 2011  Nick Gasson
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

#include "util.h"

// Get REG_EIP from ucontext.h
#define __USE_GNU
#include <sys/ucontext.h>

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <errno.h>
#include <string.h>
#include <stdbool.h>
#include <execinfo.h>
#include <signal.h>
#include <stdint.h>
#include <unistd.h>

#include <sys/types.h>
#include <sys/wait.h>
#include <sys/ptrace.h>
#include <sys/sysctl.h>

// The IP register is different depending on the CPU arch
// Try x86-64 first then regular x86: nothing else is supported
#if defined REG_RIP
#define ARCH_IP_REG REG_RIP
#elif defined REG_EIP
#define ARCH_IP_REG REG_EIP
#elif defined __APPLE__
#ifdef __LP64__
#define ARCH_IP_REG __rip
#else
#define ARCH_IP_REG __eip
#endif
#else
#warning "Don't know the IP register name for your architecture!"
#define NO_STACK_TRACE
#endif

#define N_TRACE_DEPTH  16
#define ERROR_SZ  1024

static void def_error_fn(const char *msg, const loc_t *loc);

static error_fn_t  error_fn = def_error_fn;

void *xmalloc(size_t size)
{
   void *p = malloc(size);
   if (p == NULL)
      abort();
   return p;
}

void *xrealloc(void *ptr, size_t size)
{
   ptr = realloc(ptr, size);
   if (ptr == NULL)
      abort();
   return ptr;
}

void errorf(const char *fmt, ...)
{
   va_list ap;
   va_start(ap, fmt);

   fprintf(stderr, "error: ");
   vfprintf(stderr, fmt, ap);
   fprintf(stderr, "\n");

   va_end(ap);
}

static void def_error_fn(const char *msg, const loc_t *loc)
{
   if (loc->first_line != (unsigned short)-1) {
      fprintf(stderr, "%s:%d: %s\n", loc->file, loc->first_line, msg);
      fmt_loc(stderr, loc);
   }
   else
      fprintf(stderr, "(none): %s\n", msg);
}

void error_at(const loc_t *loc, const char *fmt, ...)
{
   va_list ap;
   va_start(ap, fmt);

   char buf[ERROR_SZ];
   vsnprintf(buf, ERROR_SZ, fmt, ap);
   error_fn(buf, loc != NULL ? loc : &LOC_INVALID);
   va_end(ap);
}

error_fn_t set_error_fn(error_fn_t fn)
{
   error_fn_t old = error_fn;
   error_fn = fn;
   return old;
}

void fatal(const char *fmt, ...)
{
   va_list ap;
   va_start(ap, fmt);

   fprintf(stderr, "fatal: ");
   vfprintf(stderr, fmt, ap);
   fprintf(stderr, "\n");

   va_end(ap);

   exit(EXIT_FAILURE);
}

void fatal_errno(const char *fmt, ...)
{
   va_list ap;
   va_start(ap, fmt);

   fprintf(stderr, "fatal: ");
   vfprintf(stderr, fmt, ap);
   fprintf(stderr, ": %s\n", strerror(errno));

   va_end(ap);

   exit(EXIT_FAILURE);
}

void fmt_loc(FILE *f, const struct loc *loc)
{
   if (loc->first_line == (unsigned short)-1 || loc->linebuf == NULL)
      return;

   const char *lb = loc->linebuf;
   char buf[80];
   size_t i = 0;
   while (i < sizeof(buf) - 1 && *lb != '\0' && *lb != '\n') {
      if (*lb == '\t')
         buf[i++] = ' ';
      else
         buf[i++] = *lb;
      ++lb;
   }
   buf[i] = '\0';

   // Print ... if error location spans multiple lines
   bool many_lines = (loc->first_line != loc->last_line)
      || (i == sizeof(buf) - 1);
   int last_col = many_lines ? strlen(buf) + 3 : loc->last_column;

   fprintf(stderr, "    %s%s\n", buf, many_lines ? " ..." : "");
   for (int j = 0; j < loc->first_column + 4; j++)
      fprintf(stderr, " ");
   for (int j = 0; j < last_col - loc->first_column + 1; j++)
      fprintf(stderr, "^");
   fprintf(stderr, "\n");
}

#ifndef NO_STACK_TRACE

static void print_trace(char **messages, int trace_size)
{
   int i;

   fputs("\n-------- STACK TRACE --------\n", stderr);
   for (i = 0; i < trace_size; i++) {
      fprintf(stderr, "%s\n", messages[i]);
   }
   fputs("-----------------------------\n", stderr);
}

void show_stacktrace(void)
{
   void *trace[N_TRACE_DEPTH];
   char **messages = NULL;
   int trace_size = 0;

   trace_size = backtrace(trace, N_TRACE_DEPTH);
   messages = backtrace_symbols(trace, trace_size);

   print_trace(messages, trace_size);

   free(messages);
}

static const char *signame(int sig)
{
   switch (sig) {
   case SIGSEGV: return "SIGSEGV";
   case SIGABRT: return "SIGABRT";
   case SIGILL: return "SIGILL";
   case SIGFPE: return "SIGFPE";
   case SIGUSR1: return "SIGUSR1";
   case SIGBUS: return "SIGBUS";
   default: return "???";
   }
}

static void bt_sighandler(int sig, siginfo_t *info, void *secret)
{
   void *trace[N_TRACE_DEPTH];
   char **messages = NULL;
   int trace_size = 0;
   ucontext_t *uc = (ucontext_t*)secret;

#ifdef __APPLE__
   uintptr_t ip = uc->uc_mcontext->__ss.ARCH_IP_REG;
#else
   uintptr_t ip = uc->uc_mcontext.gregs[ARCH_IP_REG];
#endif

   fprintf(stderr, "\n*** Caught signal %d (%s)", sig, signame(sig));

   switch (sig) {
   case SIGSEGV:
   case SIGILL:
   case SIGFPE:
   case SIGBUS:
      fprintf(stderr, " [address=%p, ip=%p]", info->si_addr, (void*)ip);
      break;
   }

   fputs(" ***\n", stderr);

   trace_size = backtrace(trace, N_TRACE_DEPTH);

   // Overwrite sigaction with caller's address
   trace[1] = (void*)ip;

   messages = backtrace_symbols(trace, trace_size);

   // Skip first stack frame (points here)
   print_trace(messages + 1, trace_size - 1);

   free(messages);

   if (sig != SIGUSR1)
      exit(EXIT_FAILURE);
}

static bool is_debugger_running(void)
{
#ifdef __APPLE__
   struct kinfo_proc info;
   info.kp_proc.p_flag = 0;

   int mib[4];
   mib[0] = CTL_KERN;
   mib[1] = KERN_PROC;
   mib[2] = KERN_PROC_PID;
   mib[3] = getpid();

   size_t size = sizeof(info);
   int rc = sysctl(mib, sizeof(mib) / sizeof(*mib), &info, &size, NULL, 0);
   if (rc != 0)
      fatal_errno("sysctl");

   return (info.kp_proc.p_flag & P_TRACED) != 0;
#else  // __APPLE__

#ifdef __linux
   // Hack to detect if Valgrind is running
   FILE *f = fopen("/proc/self/maps", "r");
   if (f != NULL) {
      char buf[256];
      bool valgrind = false;
      while (!valgrind && fgets(buf, sizeof(buf), f)) {
         if (strstr(buf, "vgpreload"))
            valgrind = true;
      }
      fclose(f);
      if (valgrind)
         return true;
   }
#endif  //__linux

   pid_t pid = fork();

   if (pid == -1)
      fatal_errno("fork");
   else if (pid == 0) {
      int ppid = getppid();

      // Try to trace the parent: if we can then GDB is not running
      if (ptrace(PTRACE_ATTACH, ppid, NULL, NULL) == 0) {
         // Wait for the parent to stop and continue it
         waitpid(ppid, NULL, 0);
         ptrace(PTRACE_CONT, NULL, NULL);

         // Detach
         ptrace(PTRACE_DETACH, getppid(), NULL, NULL);

         // Able to trace so debugger not present
         exit(0);
      }
      else {
         // Trace failed so debugger is present
         exit(1);
      }
   }
   else {
      int status;
      waitpid(pid, &status, 0);
      return WEXITSTATUS(status);
   }
#endif  // __APPLE__
}

#endif  // NO_STACK_TRACE

void register_trace_signal_handlers(void)
{
#ifndef NO_STACK_TRACE
   if (is_debugger_running())
      return;

   struct sigaction sa;
   sa.sa_sigaction = (void*)bt_sighandler;
   sigemptyset(&sa.sa_mask);
   sa.sa_flags = SA_RESTART | SA_SIGINFO;

   sigaction(SIGSEGV, &sa, NULL);
   sigaction(SIGUSR1, &sa, NULL);
   sigaction(SIGFPE, &sa, NULL);
   sigaction(SIGBUS, &sa, NULL);
   sigaction(SIGILL, &sa, NULL);
   sigaction(SIGABRT, &sa, NULL);
#endif  // NO_STACK_TRACE
}

void write_u32(uint32_t u, FILE *f)
{
   fwrite(&u, sizeof(uint32_t), 1, f);
}

void write_i32(int32_t i, FILE *f)
{
   fwrite(&i, sizeof(int32_t), 1, f);
}

void write_i64(int64_t i, FILE *f)
{
   fwrite(&i, sizeof(int64_t), 1, f);
}

void write_u16(uint16_t s, FILE *f)
{
   fwrite(&s, sizeof(uint16_t), 1, f);
}

bool write_b(bool b, FILE *f)
{
   uint8_t c = b;
   fwrite(&c, 1, 1, f);
   return b;
}

uint32_t read_u32(FILE *f)
{
   uint32_t u;
   if (fread(&u, sizeof(uint32_t), 1, f) != 1)
      fatal("premature end of file");
   return u;
}

uint16_t read_u16(FILE *f)
{
   uint16_t u;
   if (fread(&u, sizeof(uint16_t), 1, f) != 1)
      fatal("premature end of file");
   return u;
}

bool read_b(FILE *f)
{
   uint8_t u;
   if (fread(&u, sizeof(uint8_t), 1, f) != 1)
      fatal("premature end of file");
   return u;
}

int32_t read_i32(FILE *f)
{
   int32_t i;
   if (fread(&i, sizeof(int32_t), 1, f) != 1)
      fatal("premature end of file");
   return i;
}

int64_t read_i64(FILE *f)
{
   int64_t i;
   if (fread(&i, sizeof(int64_t), 1, f) != 1)
      fatal("premature end of file");
   return i;
}
