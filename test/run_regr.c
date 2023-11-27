//
//  Copyright (C) 2016-2023  Nick Gasson
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

#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif

#ifdef __MINGW32__
#define WINVER 0x0A00
#define _WIN32_WINNT 0x0A00
#endif

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <errno.h>
#include <string.h>
#include <stdarg.h>
#include <unistd.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <limits.h>
#include <libgen.h>
#include <fcntl.h>
#include <assert.h>
#include <signal.h>
#include <dirent.h>
#include <ctype.h>

#ifdef __CYGWIN__
#include <process.h>
#endif

#ifdef __MINGW32__
#include <windows.h>
#include <fileapi.h>
#define setenv(x, y, z) _putenv_s((x), (y))
#define realpath(N, R) _fullpath((R), (N), _MAX_PATH)
#else
#include <sys/wait.h>
#endif
#include "config.h"

#define WHITESPACE " \t\r\n"
#define TIMEOUT    10
#define OUTFILE    ".test.out"

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

#define F_GOLD    (1 << 0)
#define F_FAIL    (1 << 1)
#define F_STOP    (1 << 2)
#define F_VHPI    (1 << 3)
#define F_2008    (1 << 4)
#define F_2000    (1 << 5)
#define F_NOTWIN  (1 << 6)
#define F_COVER   (1 << 7)
#define F_2019    (1 << 8)
#define F_RELAX   (1 << 9)
#define F_RELAXED (1 << 10)
#define F_WORKLIB (1 << 11)
#define F_SHELL   (1 << 12)
#define F_2002    (1 << 13)
#define F_SLOW    (1 << 14)
#define F_VERILOG (1 << 15)
#define F_MIXED   (1 << 16)
#define F_WAVE    (1 << 17)
#define F_PSL     (1 << 18)
#define F_DEFINE  (1 << 19)
#define F_TCL     (1 << 20)
#define F_GTKW    (1 << 21)
#define F_NOCOLL  (1 << 22)
#define F_EXPORT  (1 << 23)
#define F_SHUFFLE (1 << 24)
#define F_NOTBSD  (1 << 25)

typedef struct test test_t;
typedef struct param param_t;
typedef struct arglist arglist_t;

typedef enum {
   P_GENERIC,
   P_ENVVAR
} param_kind_t;

struct param {
   param_kind_t  kind;
   char         *name;
   char         *value;
   param_t      *next;
};

struct test {
   char      *name;
   test_t    *next;
   int        flags;
   char      *stop;
   param_t   *params;
   char      *relax;
   char      *work;
   unsigned   olevel;
   char      *heapsz;
   char      *cover;
   char      *define;
   char      *export;
   char      *plusarg;
};

struct arglist {
   char      *data;
   arglist_t *next;
   unsigned   count;
};

typedef enum {
   RUN_OK,
   RUN_FAILED,
   RUN_SIGNALLED,
} run_status_t;

static test_t *test_list = NULL;
static char test_dir[PATH_MAX];
static char bin_dir[PATH_MAX];
static bool is_tty = false;
static bool force_jit = false;
static bool broken_libc = false;

#ifdef __MINGW32__
static char *strndup(const char *s, size_t n)
{
   size_t len = strlen(s);
   if (n < len)
      len = n;
   char *result = malloc(len + 1);
   result[len] = '\0';
   return memcpy(result, s, len);
}

static void win32_error(const char *msg)
{
   LPSTR mbuf = NULL;
   FormatMessage(
      FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM
      | FORMAT_MESSAGE_IGNORE_INSERTS,
      NULL,
      GetLastError(),
      MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
      (LPSTR)&mbuf, 0, NULL);

   fprintf(stderr, "%s: %s", msg, mbuf);
   fflush(stderr);

   LocalFree(mbuf);
   exit(EXIT_FAILURE);
}

static DWORD win32_timeout_thread(LPVOID lpParam)
{
   HANDLE hProcess = (HANDLE)lpParam;

   if (WaitForSingleObject(hProcess, TIMEOUT * 1000) == WAIT_TIMEOUT) {
      if (!TerminateProcess(hProcess, 0x500))
         win32_error("TerminateProcess");
   }

   return 0;
}

static run_status_t win32_run_cmd(FILE *log, arglist_t **args)
{
   SECURITY_ATTRIBUTES saAttr;
   saAttr.nLength = sizeof(SECURITY_ATTRIBUTES);
   saAttr.bInheritHandle = TRUE;
   saAttr.lpSecurityDescriptor = NULL;

   HANDLE hChildStdOutRd, hChildStdOutWr;
   if (!CreatePipe(&hChildStdOutRd, &hChildStdOutWr, &saAttr, 0))
      win32_error("StdoutRd CreatePipe");

   if (!SetHandleInformation(hChildStdOutRd, HANDLE_FLAG_INHERIT, 0))
      win32_error("Stdout SetHandleInformation");

   char cmdline[8192], *p = cmdline;
   for (arglist_t *it = *args; it != NULL; it = it->next)
      p += snprintf(p, cmdline + sizeof(cmdline) - p, "%s%s",
                    it != *args ? " " : "", it->data);

   fprintf(log, "%s\n", cmdline);
   fflush(log);

   PROCESS_INFORMATION piProcInfo;
   ZeroMemory(&piProcInfo, sizeof(PROCESS_INFORMATION));

   STARTUPINFO siStartInfo;
   ZeroMemory(&siStartInfo, sizeof(STARTUPINFO));
   siStartInfo.cb = sizeof(STARTUPINFO);
   siStartInfo.hStdError = hChildStdOutWr;
   siStartInfo.hStdOutput = hChildStdOutWr;
   siStartInfo.dwFlags |= STARTF_USESTDHANDLES;

   if (!CreateProcess(NULL, cmdline, NULL, NULL, TRUE, 0,
                      NULL, NULL, &siStartInfo, &piProcInfo))
      win32_error("CreateProcess");

   CloseHandle(hChildStdOutWr);

   HANDLE hTimeoutThread =
      CreateThread(NULL, 0, (LPTHREAD_START_ROUTINE)win32_timeout_thread,
                   piProcInfo.hProcess, 0, NULL);
   if (hTimeoutThread == NULL)
      win32_error("CreateThread");

   for (;;) {
      DWORD dwRead;
      CHAR chBuf[1024];
      if (!ReadFile(hChildStdOutRd, chBuf, sizeof(chBuf), &dwRead, NULL))
         break;
      else if (dwRead == 0)
         break;

      fwrite(chBuf, dwRead, 1, log);
      fflush(log);
   }

   if (WaitForSingleObject(piProcInfo.hProcess, 1000) != WAIT_OBJECT_0)
      win32_error("WaitForSingleObject");

   DWORD status;
   if (!GetExitCodeProcess(piProcInfo.hProcess, &status))
      win32_error("GetExitCodeProcess");

   if (status == 0x500)
      fprintf(log, "Timeout!\n");
   else if (status == STILL_ACTIVE)
      fprintf(log, "Still running!\n");

   if (WaitForSingleObject(hTimeoutThread, 1000) != WAIT_OBJECT_0)
      win32_error("WaitForSingleObject");

   CloseHandle(hTimeoutThread);
   CloseHandle(piProcInfo.hProcess);
   CloseHandle(piProcInfo.hThread);

   while (*args) {
      arglist_t *tmp = (*args)->next;
      free((*args)->data);
      free(*args);
      *args = tmp;
   }

   return status == 0 ? RUN_OK : RUN_FAILED;
}
#endif

static char *xvasprintf(const char *fmt, va_list ap)
{
   char *strp = NULL;
   if (vasprintf(&strp, fmt, ap) < 0)
      abort();
   return strp;
}

__attribute__((format(printf, 1, 2)))
static char *xasprintf(const char *fmt, ...)
{
   va_list ap;
   va_start(ap, fmt);
   char *strp = xvasprintf(fmt, ap);
   va_end(ap);
   return strp;
}

static void set_attr(int escape)
{
   if (is_tty)
      printf("\033[%dm", escape);
}

static bool is_comment(const char *str)
{
   return str[0] == '#';
}

static bool parse_test_list(int argc, char **argv)
{
   char testlist[PATH_MAX + 22];
   snprintf(testlist, sizeof(testlist), "%s/regress/testlist.txt", test_dir);

   FILE *f = fopen(testlist, "r");
   if (f == NULL) {
      fprintf(stderr, "%s: %s\n", testlist, strerror(errno));
      return false;
   }

   bool result = false;
   int lineno = 0;
   test_t *last = NULL;
   while (lineno++, !feof(f)) {
      char line[256];
      if (fgets(line, sizeof(line), f) == NULL)
          break;

      char *name = strtok(line, WHITESPACE);
      if (name == NULL || is_comment(name))
         continue;

      char *options = strtok(NULL, WHITESPACE);
      if (options == NULL || is_comment(options)) {
         fprintf(stderr, "Error on testlist line %d: missing options for "
                 "test %s\n", lineno, name);
         goto out_close;
      }

      char *remain = strtok(NULL, WHITESPACE);
      if (remain != NULL && !is_comment(remain)) {
         fprintf(stderr, "Error on testlist line %d: extra tokens after "
                 "test %s options list\n", lineno, name);
         goto out_close;
      }

      if (argc > 0) {
         bool found = false;
         for (int i = 0; i < argc && !found; i++)
            found = strstr(name, argv[i]) != NULL;
         if (!found)
            continue;
      }

      test_t *test = calloc(sizeof(test_t), 1);
      test->name = strdup(name);
      test->olevel = 0;

      if (last == NULL)
         test_list = test;
      else
         last->next = test;

      char *opt = strtok(options, ",");
      while (opt != NULL) {
         if (strcmp(opt, "normal") == 0)
            ;
         else if (strcmp(opt, "gold") == 0)
            test->flags |= F_GOLD;
         else if (strcmp(opt, "fail") == 0)
            test->flags |= F_FAIL;
         else if (strncmp(opt, "stop", 4) == 0) {
            char *stop = strchr(opt, '=');
            if (stop == NULL) {
               fprintf(stderr, "Error on testlist line %d: missing argument to "
                       "stop option in test %s\n", lineno, name);
               goto out_close;
            }

            test->flags |= F_STOP;
            test->stop = strdup(stop + 1);
         }
         else if (strcmp(opt, "2008") == 0)
            test->flags |= F_2008;
         else if (strcmp(opt, "2000") == 0)
            test->flags |= F_2000;
         else if (strcmp(opt, "2002") == 0)
            test->flags |= F_2002;
         else if (strcmp(opt, "2019") == 0)
            test->flags |= F_2019;
         else if (strcmp(opt, "vhpi") == 0)
            test->flags |= F_VHPI;
         else if (strcmp(opt, "shell") == 0)
            test->flags |= F_SHELL | F_NOTWIN;
         else if (strcmp(opt, "slow") == 0)
            test->flags |= F_SLOW;
         else if (strcmp(opt, "!windows") == 0)
            test->flags |= F_NOTWIN;
         else if (strcmp(opt, "!freebsd") == 0)
            test->flags |= F_NOTBSD;
         else if (strcmp(opt, "mixed") == 0)
            test->flags |= F_MIXED;
         else if (strcmp(opt, "verilog") == 0)
            test->flags |= F_VERILOG;
         else if (strcmp(opt, "wave") == 0)
            test->flags |= F_WAVE;
         else if (strcmp(opt, "gtkw") == 0)
            test->flags |= F_GTKW;
         else if (strcmp(opt, "psl") == 0)
            test->flags |= F_PSL;
         else if (strcmp(opt, "tcl") == 0)
            test->flags |= F_TCL;
         else if (strcmp(opt, "shuffle") == 0)
            test->flags |= F_SHUFFLE;
         else if (strcmp(opt, "no-collapse") == 0)
            test->flags |= F_NOCOLL;
         else if (strncmp(opt, "O", 1) == 0) {
            if (sscanf(opt + 1, "%u", &(test->olevel)) != 1) {
               fprintf(stderr, "Error on testlist line %d: invalid "
                       "optimisation level %s\n", lineno, opt);
               goto out_close;
            }
         }
         else if (strncmp(opt, "H=", 2) == 0)
            test->heapsz = strdup(opt + 2);
         else if (strncmp(opt, "cover", 5) == 0) {
            test->flags |= F_COVER;
            if (opt[5] == '=') {
               test->cover = strdup(opt + 6);
               for (char *p = test->cover; *p; p++) {
                  if (*p == '+') *p = ',';  // Allow + instead of , as separator
               }
            }
         }
         else if (opt[0] == 'g' || opt[0] == '$') {
            char *value = strchr(opt, '=');
            if (value == NULL) {
               fprintf(stderr, "Error on testlist line %d: missing argument to "
                       "parameter option in test %s\n", lineno, name);
               goto out_close;
            }

            param_t *p = calloc(sizeof(param_t), 1);
            p->name = strndup(opt + 1, value - opt - 1);
            p->value = strdup(value + 1);
            p->next = test->params;
            p->kind = opt[0] == 'g' ? P_GENERIC : P_ENVVAR;

            test->params = p;
         }
         else if (strcmp(opt, "relaxed") == 0)
            test->flags |= F_RELAXED;
         else if (strncmp(opt, "relax", 5) == 0) {
            char *value = strchr(opt, '=');
            if (value == NULL) {
               fprintf(stderr, "Error on testlist line %d: missing argument to "
                       "relax option in test %s\n", lineno, name);
               goto out_close;
            }

            test->flags |= F_RELAX;
            test->relax = strdup(value + 1);
         }
         else if (strncmp(opt, "work", 4) == 0) {
            char *value = strchr(opt, '=');
            if (value == NULL) {
               fprintf(stderr, "Error on testlist line %d: missing argument to "
                       "work option in test %s\n", lineno, name);
               goto out_close;
            }

            test->flags |= F_WORKLIB;
            test->work = strdup(value + 1);
         }
         else if (strncmp(opt, "define", 6) == 0) {
            char *value = strchr(opt, '=');
            if (value == NULL) {
               fprintf(stderr, "Error on testlist line %d: missing argument to "
                       "define option in test %s\n", lineno, name);
               goto out_close;
            }

            test->flags |= F_DEFINE;
            test->define = strdup(value + 1);
         }
         else if (strncmp(opt, "export", 6) == 0) {
            char *value = strchr(opt, '=');
            if (value == NULL) {
               fprintf(stderr, "Error on testlist line %d: missing argument to "
                       "export option in test %s\n", lineno, name);
               goto out_close;
            }

            test->flags |= F_EXPORT;
            test->export = strdup(value + 1);
         }
         else if (opt[0] == '+')
            test->plusarg = strdup(opt + 1);
         else {
            fprintf(stderr, "Error on testlist line %d: invalid option %s in "
                 "test %s\n", lineno, opt, name);
            goto out_close;
         }

         opt = strtok(NULL, ",");
      }

      last = test;
   }

   result = true;

 out_close:
   fclose(f);
   return result;
}

__attribute__((format(printf, 2, 3)))
static void push_arg(arglist_t **args, const char *fmt, ...)
{
   va_list ap;
   va_start(ap, fmt);
   char *cmd = xvasprintf(fmt, ap);
   va_end(ap);

   arglist_t *n = calloc(1, sizeof(arglist_t));
   n->data = cmd;
   n->next = NULL;
   n->count = 1;

   arglist_t *it = *args;
   if (it == NULL)
      *args = n;
   else {
      for (; it->next != NULL; it = it->next)
         (it->count)++;
      it->next = n;
      (it->count)++;
   }
}

#ifndef __MINGW32__
static void signal_handler(int sig)
{
}
#endif

static run_status_t run_cmd(FILE *log, arglist_t **args)
{
   fflush(stdout);
   fflush(stderr);

#if defined __MINGW32__
   return win32_run_cmd(log, args);
#else
   pid_t pid = fork();
   if (pid < 0) {
      fprintf(stderr, "Fork failed: %s", strerror(errno));
      return RUN_SIGNALLED;
   }
   else if (pid == 0) {
      dup2(fileno(log), STDOUT_FILENO);
      dup2(fileno(log), STDERR_FILENO);

      int nullfd = open("/dev/null", O_WRONLY);
      if (nullfd != -1)
         dup2(nullfd, STDIN_FILENO);

      char **argv = calloc((*args)->count + 1, sizeof(char *));
      arglist_t *it = *args;
      for (int i = 0; i < (*args)->count; i++, it = it->next) {
         printf("%s%s", i > 0 ? " " : "", it->data);
         argv[i] = it->data;
      }
      printf("\n");

      execv((*args)->data, argv);
      fprintf(stderr, "Exec failed: %s\n", strerror(errno));
      _exit(EXIT_FAILURE);
   }
   else {
      while (*args) {
         arglist_t *tmp = (*args)->next;
         free((*args)->data);
         free(*args);
         *args = tmp;
      }

      signal(SIGALRM, signal_handler);
      signal(SIGCHLD, signal_handler);
      alarm(TIMEOUT);
      pause();
      signal(SIGALRM, SIG_DFL);
      signal(SIGCHLD, SIG_DFL);

      int status = 0, nready = waitpid(pid, &status, WNOHANG);
      if (nready < 0) {
         fprintf(stderr, "Waiting for child failed: %s\n", strerror(errno));
         return RUN_SIGNALLED;
      }
      else if (nready != 0 && WIFSIGNALED(status)) {
         fprintf(log, "Caught signal %d\n", WTERMSIG(status));
         return RUN_SIGNALLED;
      }
      else if (nready == 0 || !WIFEXITED(status)) {
         fprintf(log, "Timeout!\n");

         if (kill(pid, SIGTERM) != 0)
            fprintf(stderr, "Sending SIGTERM to child failed: %s\n",
                    strerror(errno));
         else
            wait(&status);

         return RUN_SIGNALLED;
      }
      else
         return WEXITSTATUS(status) == 0 ? RUN_OK : RUN_FAILED;
   }
#endif  // __CYGWIN__ || __MINGW32__
}

static void push_std(test_t *test, arglist_t **args)
{
   if (test->flags & F_2000)
      push_arg(args, "--std=2000");
   else if (test->flags & F_2002)
      push_arg(args, "--std=2002");
   else if (test->flags & F_2008)
      push_arg(args, "--std=2008");
   else if (test->flags & F_2019)
      push_arg(args, "--std=2019");
   else
      push_arg(args, "--std=1993");
}

static void chomp(char *str)
{
   const size_t len = strlen(str);
   if (len > 0 && str[len - 1] == '\n')
      str[len - 1] = '\0';
}

#ifndef __MINGW32__
static bool remove_dir(const char *path)
{
   DIR *d = opendir(path);
   if (d == NULL)
      return false;

   bool result = false;
   char buf[PATH_MAX];
   struct dirent *e;
   while ((e = readdir(d))) {
      if (strcmp(e->d_name, ".") == 0 || strcmp(e->d_name, "..") == 0)
         continue;

      snprintf(buf, sizeof(buf), "%s" DIR_SEP "%s", path, e->d_name);
      if (e->d_type == DT_DIR && !remove_dir(buf))
         goto out_close;
      else if (e->d_type == DT_REG && unlink(buf) < 0)
         goto out_close;
   }

   if (rmdir(path) < 0)
      goto out_close;

   result = true;

 out_close:
   closedir(d);
   return result;
}
#endif

__attribute__((unused))
static bool make_dir(const char *name)
{
#ifdef __MINGW32__
   return mkdir(name) == 0;
#else
   return mkdir(name, 0777) == 0;
#endif
}

__attribute__((unused))
static bool dir_exists(const char *path)
{
   struct stat st;
   return stat(path, &st) == 0 && S_ISDIR(st.st_mode);
}

static bool file_exists(const char *path)
{
   struct stat st;
   return stat(path, &st) == 0 && S_ISREG(st.st_mode);
}

static bool enter_test_directory(test_t *test, char *dir)
{
#ifdef __MINGW32__
   if (!dir_exists("logs") && !make_dir("logs")) {
      printf("make_dir %s failed\n", "logs");
      return false;
   }

   snprintf(dir, PATH_MAX, "logs/%s", test->name);

   if (!dir_exists(dir) && !make_dir(dir)) {
      printf("make_dir %s failed\n", dir);
      return false;
   }
#else
   snprintf(dir, PATH_MAX, "%s/nvcXXXXXX", getenv("TEMP") ?: "/tmp");

   if (mkdtemp(dir) == NULL)
      return false;
#endif

   if (chdir(dir) != 0)
      return false;

   return true;
}

static void explain(int attr, const char *prefix, const char *fmt, va_list ap)
{
   char *reason = NULL;
   if (fmt != NULL)
      reason = xvasprintf(fmt, ap);

   set_attr(attr);
   if (reason != NULL)
      printf("%s (%s)", prefix, reason);
   else
      printf("%s", prefix);
   set_attr(ANSI_RESET);
   printf("\n");
   fflush(stdout);

   free(reason);
}

__attribute__((format(printf, 1, 2)))
static void failed(const char *fmt, ...)
{
   va_list ap;
   va_start(ap, fmt);
   explain(ANSI_FG_RED, "failed", fmt, ap);
   va_end(ap);
}

__attribute__((format(printf, 1, 2)))
static void skipped(const char *fmt, ...)
{
   va_list ap;
   va_start(ap, fmt);
   explain(ANSI_FG_CYAN, "skipped", fmt, ap);
   va_end(ap);
}

static bool run_test(test_t *test)
{
   printf("%15s : ", test->name);

   int skip = 0;
#if defined __MINGW32__ || defined __CYGWIN__
   skip |= (test->flags & F_NOTWIN);
   skip |= (test->flags & F_WAVE);   // XXX: needs debugging
#endif
#ifdef __FreeBSD__
   skip |= (test->flags & F_NOTBSD);
#endif
#ifndef HAVE_LLVM
   skip |= (test->flags & F_SLOW);
#else
   if (force_jit) skip |= (test->flags & F_SLOW);
#endif
#ifndef ENABLE_VERILOG
   skip |= (test->flags & F_VERILOG) | (test->flags & F_MIXED);
#endif
#ifndef ENABLE_TCL
   skip |= (test->flags & F_TCL);
#endif

   if (skip) {
      if (skip & F_SLOW)
         skipped("slow with interpreter");
      else if (skip & (F_VERILOG | F_MIXED))
         skipped("verilog not enabled");
      else if (skip & F_TCL)
         skipped("tcl not enabled");
      else if (skip & (F_NOTWIN | F_WAVE))
         skipped("disabled on windows");
      else if (skip & F_NOTBSD)
         skipped("disabled on freebsd");
      else
         skipped(NULL);

      return true;
   }

   bool result = false;
   char dir[PATH_MAX], cwd[PATH_MAX];

   if (getcwd(cwd, PATH_MAX) == NULL) {
      failed("error getting working directory: %s", strerror(errno));
      return false;
   }

   if (!enter_test_directory(test, dir)) {
      failed("error creating test directory: %s", strerror(errno));
      return false;
   }

   FILE *outf = fopen(OUTFILE, "w");
   if (outf == NULL) {
      failed("error creating %s/out log file: %s", dir, strerror(errno));
      result = false;
      goto out_chdir;
   }

   setenv("TEST_NAME", test->name, 1);

   arglist_t *args = NULL;

   if (test->flags & F_SHELL) {
      push_arg(&args, SH_PATH);
      push_arg(&args, "%s" DIR_SEP "regress" DIR_SEP "%s.sh",
               test_dir, test->name);
   }
   else if (test->flags & F_TCL) {
      push_arg(&args, "%s" DIR_SEP "nvc%s", bin_dir, EXEEXT);
      push_std(test, &args);

      if (test->flags & F_WORKLIB)
         push_arg(&args, "--work=%s", test->work);

      if (test->heapsz != NULL)
         push_arg(&args, "-H%s", test->heapsz);

      push_arg(&args, "--do");
      push_arg(&args, "%s" DIR_SEP "regress" DIR_SEP "%s.tcl",
               test_dir, test->name);
   }
   else {
      push_arg(&args, "%s" DIR_SEP "nvc%s", bin_dir, EXEEXT);
      push_std(test, &args);

      if (test->flags & F_WORKLIB)
         push_arg(&args, "--work=%s", test->work);

      if (test->heapsz != NULL)
         push_arg(&args, "-H%s", test->heapsz);

      push_arg(&args, "-a");

      if (!(test->flags & F_VERILOG))
         push_arg(&args, "%s" DIR_SEP "regress" DIR_SEP "%s.vhd",
                  test_dir, test->name);

      if (test->flags & (F_MIXED | F_VERILOG))
         push_arg(&args, "%s" DIR_SEP "regress" DIR_SEP "%s.v",
                  test_dir, test->name);

      if (test->flags & F_RELAX)
         push_arg(&args, "--relax=%s", test->relax);

      if (test->flags & F_RELAXED)
         push_arg(&args, "--relaxed");

      if (test->flags & F_PSL)
         push_arg(&args, "--psl");

      if (test->flags & F_DEFINE)
         push_arg(&args, "--define=%s", test->define);

      push_arg(&args, "-e");
      push_arg(&args, "%s", test->name);
      push_arg(&args, "-O%u", test->olevel);

      if (test->flags & F_NOCOLL)
         push_arg(&args, "--no-collapse");

      if (test->flags & F_COVER) {
         if (test->cover)
            push_arg(&args, "--cover=%s", test->cover);
         else
            push_arg(&args, "--cover");
      }

      for (param_t *p = test->params; p != NULL; p = p->next) {
         switch (p->kind) {
         case P_GENERIC:
            push_arg(&args, "-g%s=%s", p->name, p->value);
            break;
         case P_ENVVAR:
            setenv(p->name, p->value, 1);
            break;
         }
      }

      if ((test->flags & F_FAIL) || broken_libc) {
         if (run_cmd(outf, &args) != RUN_OK) {
            failed(NULL);
            result = false;
            goto out_print;
         }

         push_arg(&args, "%s/nvc%s", bin_dir, EXEEXT);
         push_std(test, &args);

         if (test->heapsz != NULL)
            push_arg(&args, "-H%s", test->heapsz);
      }
      else if (force_jit) {
         push_arg(&args, "--no-save");
         push_arg(&args, "--jit");
      }

      push_arg(&args, "-r");

      if (test->flags & F_STOP)
         push_arg(&args, "--stop-time=%s", test->stop);

      if (test->flags & F_VHPI)
         push_arg(&args, "--load=%s/../lib/vhpi_test.so%s", bin_dir, EXEEXT);

      if (test->flags & F_WAVE)
         push_arg(&args, "-w");

      if (test->flags & F_GTKW)
         push_arg(&args, "-g");

      if (test->flags & F_SHUFFLE)
         push_arg(&args, "--shuffle");

      if (test->plusarg != NULL)
         push_arg(&args, "+%s", test->plusarg);

      push_arg(&args, "%s", test->name);
   }

   run_status_t status = run_cmd(outf, &args);

   if (test->flags & F_FAIL) {
      if (!(result = (status == RUN_FAILED))) {
         failed("expected error");
         goto out_print;
      }
   }
   else if (!(result = (status == RUN_OK))) {
      failed(NULL);
      goto out_print;
   }

   if ((test->flags & F_COVER) && (test->flags & F_EXPORT)) {
      // Generate and check XML report
      push_arg(&args, "%s/nvc%s", bin_dir, EXEEXT);
      push_arg(&args, "--cover-export");
      push_arg(&args, "--relative=%s", test_dir);
      push_arg(&args, "--out=export.xml");
      push_arg(&args, "--format=%s", test->export);
      push_arg(&args, "%s", test->name);

      if (run_cmd(outf, &args) != RUN_OK) {
         failed("coverage report");
         result = false;
         goto out_print;
      }
      else if (!file_exists("export.xml")) {
         failed("missing exported coverage report");
         result = false;
         goto out_print;
      }

#ifdef XMLLINT_PATH
      push_arg(&args, "%s", XMLLINT_PATH);
      push_arg(&args, "--noout");
      push_arg(&args, "--nonet");
      push_arg(&args, "--dtdvalid");
      push_arg(&args, "%s/%s.dtd", test_dir, test->export);
      push_arg(&args, "export.xml");

      if (run_cmd(outf, &args) != RUN_OK) {
         failed("XML lint failed");
         result = false;
         goto out_print;
      }
#else
      skipped("missing xmllint");
      goto out_close;
#endif

#ifndef __MINGW32__  // Directory separator different on Windows
      push_arg(&args, "%s", DIFF_PATH);
      push_arg(&args, "-u");
      push_arg(&args, "%s/regress/gold/%s.xml", test_dir, test->name);
      push_arg(&args, "export.xml");

      if (run_cmd(outf, &args) != RUN_OK) {
         failed("XML mismatch");
         result = false;
         goto out_print;
      }
#endif
   }
   else if ((test->flags & F_COVER) && !(test->flags & F_SHELL)) {
      // Generate coverage report
      push_arg(&args, "%s/nvc%s", bin_dir, EXEEXT);
      push_arg(&args, "-c");
      push_arg(&args, "--report");
      push_arg(&args, "html");

      char *unit = strdup(test->name);
      for (char *p = unit; *p; p++)
         *p = toupper((int)*p);

      push_arg(&args, "work/_WORK.%s.elab.covdb", unit);
      free(unit);

      if (run_cmd(outf, &args) != RUN_OK) {
         failed("coverage report");
         result = false;
         goto out_print;
      }
      else if (!file_exists("html/index.html")) {
         failed("missing coverage report index.html");
         result = false;
         goto out_print;
      }
   }

   if (test->flags & F_WAVE) {
      push_arg(&args, "%s/fstdump%s", bin_dir, EXEEXT);
      push_arg(&args, "-o");
      push_arg(&args, "%s.dump", test->name);
      push_arg(&args, "%s.fst", test->name);

      if (run_cmd(outf, &args) != RUN_OK) {
         failed("fstdump");
         result = false;
         goto out_print;
      }

      push_arg(&args, "%s", DIFF_PATH);
#if defined __MINGW32__ || defined __CYGWIN__
      push_arg(&args, "--strip-trailing-cr");
#endif
      push_arg(&args, "-u");
      push_arg(&args, "%s/regress/gold/%s.dump", test_dir, test->name);
      push_arg(&args, "%s.dump", test->name);

      if (run_cmd(outf, &args) != RUN_OK) {
         failed("waveform mismatch");
         result = false;
         goto out_print;
      }
   }

   if (test->flags & F_GTKW) {
      push_arg(&args, "%s", DIFF_PATH);
#if defined __MINGW32__ || defined __CYGWIN__
      push_arg(&args, "--strip-trailing-cr");
#endif
      push_arg(&args, "-u");
      push_arg(&args, "%s/regress/gold/%s.gtkw", test_dir, test->name);
      push_arg(&args, "%s.gtkw", test->name);

      if (run_cmd(outf, &args) != RUN_OK) {
         failed("gtkw file mismatch");
         result = false;
         goto out_print;
      }
   }

   if (test->flags & F_GOLD) {
      char goldname[PATH_MAX + 19];
      snprintf(goldname, sizeof(goldname), "%s/regress/gold/%s.txt",
               test_dir, test->name);

      FILE *goldf = fopen(goldname, "r");
      if (goldf == NULL) {
         failed("missing gold file");
         result = false;
         goto out_close;
      }

      outf = freopen(OUTFILE, "r", outf);
      assert(outf != NULL);

      int lineno = 0;
      char gold_line[256];
      char out_line[256];
      while (fgets(gold_line, sizeof(gold_line), goldf)) {
         chomp(gold_line);
         lineno++;

         bool match = false;
         while (!match && fgets(out_line, sizeof(out_line), outf)) {
            chomp(out_line);
            match = strstr(out_line, gold_line) != NULL;
         }

         if (!match) {
            failed("no match line %d", lineno);
            set_attr(ANSI_FG_CYAN);
            printf("%s\n", gold_line);
            set_attr(ANSI_RESET);
            result = false;
            break;
         }
      }

      fclose(goldf);

      while (fgets(out_line, sizeof(out_line), outf)) {
         if (strstr(out_line, "*** Caught signal") != NULL
             || strstr(out_line, "fatal_trace") != NULL) {
            failed("crashed!");
            result = false;
            break;
         }
      }
   }

 out_print:
   if (result) {
      set_attr(ANSI_FG_GREEN);
      printf("ok\n");
      set_attr(ANSI_RESET);
   }
   else {
      char line[256];
      outf = freopen(OUTFILE, "r", outf);
      assert(outf != NULL);
      while (fgets(line, sizeof(line), outf))
         fputs(line, stdout);
   }

 out_close:
   fclose(outf);

 out_chdir:
   if (chdir(cwd) != 0) {
      set_attr(ANSI_FG_RED);
      printf("Failed to switch to %s: %s\n", cwd, strerror(errno));
      set_attr(ANSI_RESET);
      return false;
   }

#ifndef __MINGW32__
   if (!remove_dir(dir)) {
      set_attr(ANSI_FG_RED);
      printf("Failed to remove directory %s: %s\n", dir, strerror(errno));
      set_attr(ANSI_RESET);
      return false;
   }
#endif

   return result;
}

static void checked_realpath(const char *path, char *output)
{
   if (realpath(path, output) == NULL) {
      fprintf(stderr, "Error: failed to get real path for %s: %s\n",
              path, strerror(errno));
      exit(EXIT_FAILURE);
   }
}

int main(int argc, char **argv)
{
   checked_realpath(TESTDIR, test_dir);

   char argv0_path[PATH_MAX];
   checked_realpath(argv[0], argv0_path);
   strncpy(bin_dir, dirname(argv0_path), sizeof(bin_dir) - 1);

   is_tty = isatty(STDOUT_FILENO);

#ifdef __MINGW32__
   if (!is_tty) {
      // Handle running under MinTty
      HANDLE hStdOut = GetStdHandle(STD_OUTPUT_HANDLE);
      const size_t size = sizeof(FILE_NAME_INFO) + sizeof(WCHAR) * MAX_PATH;
      FILE_NAME_INFO *nameinfo = malloc(size);
      if (!GetFileInformationByHandleEx(hStdOut, FileNameInfo, nameinfo, size))
         win32_error("GetFileInformationByHandle");

      if ((wcsncmp(nameinfo->FileName, L"\\msys-", 6) == 0
           || wcsncmp(nameinfo->FileName, L"\\cygwin-", 8) == 0)
          && wcsstr(nameinfo->FileName, L"pty") != NULL)
         is_tty = true;

      free(nameinfo);
   }
#endif

   setenv("NVC_LIBPATH", "", 1);

   char lib_dir[PATH_MAX + 7];
   snprintf(lib_dir, sizeof(lib_dir), "%s/../lib", bin_dir);

   setenv("NVC_IMP_LIB", lib_dir, 1);
   setenv("NVC_LIBPATH", lib_dir, 1);

#ifdef __MINGW32__
   SetConsoleOutputCP(65001);
#else
   setenv("LANG", "en_US.UTF-8", 1);
#endif

   setenv("NVC_COVER_TIMESTAMP", "0", 1);

   if (getenv("QUICK"))
      return 0;

   if (!parse_test_list(argc - 1, argv + 1))
      return EXIT_FAILURE;

   force_jit = getenv("FORCE_JIT") != NULL;
   broken_libc = getenv("BROKEN_LIBC") != NULL;

   char *newpath = xasprintf("%s:%s", bin_dir, getenv("PATH"));

#ifndef __MINGW32__
   setenv("PATH", newpath, 1);
   setenv("TESTDIR", test_dir, 1);
#endif

   free(newpath);

   int fails = 0;
   for (test_t *it = test_list; it != NULL; it = it->next) {
      if (!run_test(it))
         fails++;
   }

   if (fails > 0) {
      set_attr(ANSI_FG_RED);
      set_attr(ANSI_BOLD);
      printf("%d failures!\n", fails);
      set_attr(ANSI_RESET);
      return EXIT_FAILURE;
   }

   return 0;
}
