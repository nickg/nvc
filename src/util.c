#include "util.h"

// Get REG_EIP from ucontext.h
#define __USE_GNU
#include <ucontext.h>

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <errno.h>
#include <string.h>
#include <stdbool.h>
#include <execinfo.h>
#include <signal.h>

// The IP register is different depending on the CPU arch
// Try x86-64 first then regular x86: nothing else is supported 
#if defined REG_RIP
#define ARCH_IP_REG REG_RIP
#elif defined REG_EIP
#define ARCH_IP_REG REG_EIP
#else
#warning "Don't know the IP register name for your architecture!"
#define NO_STACK_TRACE
#endif

#define N_TRACE_DEPTH  16

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

void fmt_loc(FILE *f, const struct loc *loc)
{
   if (loc->first_line == -1)
      return;
   
   const char *lb = loc->linebuf;
   char buf[80];
   size_t i = 0;
   while (i < sizeof(buf) - 4 && *lb != '\0' && *lb != '\n') {
      // TODO: expand tabs?
      buf[i++] = *lb++;
   }

   if (i == sizeof(buf) - 4) {
      buf[i++] = '.';
      buf[i++] = '.';
      buf[i++] = '.';
   }

   buf[i] = '\0';

   // Print ... if error location spans multiple lines
   bool many_lines = (loc->first_line != loc->last_line);
   int last_col = many_lines ? strlen(buf) + 4 : loc->last_column;

   fprintf(stderr, "    %s%s\n", buf, many_lines ? " ..." : "");
   for (int i = 0; i < loc->first_column + 4; i++)
      fprintf(stderr, " ");
   for (int i = 0; i < last_col - loc->first_column + 1; i++)
      fprintf(stderr, "^");
   fprintf(stderr, "\n");
}


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

   fprintf(stderr, "\n*** Caught signal %d (%s)", sig, signame(sig));

   switch (sig) {
   case SIGSEGV:
   case SIGILL:
   case SIGFPE:
   case SIGBUS:
      fprintf(stderr, " [address=%p, ip=%p]",
              info->si_addr, (void*)uc->uc_mcontext.gregs[ARCH_IP_REG]);
      break;
   }

   fputs(" ***\n", stderr);

   trace_size = backtrace(trace, N_TRACE_DEPTH);

   // Overwrite sigaction with caller's address
   trace[1] = (void*)uc->uc_mcontext.gregs[ARCH_IP_REG];

   messages = backtrace_symbols(trace, trace_size);

   // Skip first stack frame (points here)
   print_trace(messages + 1, trace_size - 1);

   free(messages);

   if (sig != SIGUSR1)
      exit(EXIT_FAILURE);
}

void register_trace_signal_handlers(void)
{
#ifndef NO_STACK_TRACE
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
