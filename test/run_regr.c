#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <errno.h>
#include <string.h>
#include <stdarg.h>
#include <unistd.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <limits.h>
#include <libgen.h>
#include <fcntl.h>
#include <assert.h>

#include "config.h"

#define WHITESPACE " \t\r\n"
#define TIMEOUT    10

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
#define F_OPT     (1 << 6)
#define F_COVER   (1 << 7)
#define F_GENERIC (1 << 8)
#define F_RELAX   (1 << 9)

typedef struct test test_t;
typedef struct generic generic_t;
typedef struct arglist arglist_t;

struct generic {
   char      *name;
   char      *value;
   generic_t *next;
};

struct test {
   char      *name;
   test_t    *next;
   int        flags;
   char      *stop;
   generic_t *generics;
   char      *relax;
};

struct arglist {
   char      *data;
   arglist_t *next;
   unsigned   count;
};

static test_t *test_list = NULL;
static char test_dir[PATH_MAX];
static char bin_dir[PATH_MAX];
static bool is_tty = false;

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
   char testlist[PATH_MAX];
   snprintf(testlist, PATH_MAX, "%s/regress/testlist.txt", test_dir);

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
         else if (strcmp(opt, "vhpi") == 0)
            test->flags |= F_VHPI;
         else if (strcmp(opt, "opt") == 0)
            test->flags |= F_OPT;
         else if (strcmp(opt, "cover") == 0)
            test->flags |= F_COVER;
         else if (strncmp(opt, "g", 1) == 0) {
            char *value = strchr(opt, '=');
            if (value == NULL) {
               fprintf(stderr, "Error on testlist line %d: missing argument to "
                       "stop option in test %s\n", lineno, name);
               goto out_close;
            }

            generic_t *g = calloc(sizeof(generic_t), 1);
            g->name = strndup(opt + 1, value - opt - 1);
            g->value = strdup(value + 1);
            g->next = test->generics;

            test->generics = g;
            test->flags |= F_GENERIC;
         }
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
   char *cmd = NULL;
   va_list ap;
   va_start(ap, fmt);
   vasprintf(&cmd, fmt, ap);
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

static void signal_handler(int sig)
{
}

static bool run_cmd(FILE *log, arglist_t **args)
{
   fflush(stdout);
   fflush(stderr);

   pid_t pid = fork();
   if (pid < 0) {
      fprintf(stderr, "Fork failed: %s", strerror(errno));
      return false;
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

      int status;
      if (waitpid(pid, &status, WNOHANG) < 0) {
         fprintf(stderr, "Waiting for child failed: %s\n", strerror(errno));
         return false;
      }
      else if (!WIFEXITED(status)) {
         fprintf(log, "Timeout!\n");

         if (kill(pid, SIGTERM) != 0)
            fprintf(stderr, "Sending SIGTERM to child failed: %s\n",
                    strerror(errno));
         else
            wait(&status);

         return false;
      }
      else
         return WEXITSTATUS(status) == 0;
   }
}

static void push_std(test_t *test, arglist_t **args)
{
   if (test->flags & F_2000)
      push_arg(args, "--std=2000");
   else if (test->flags & F_2008)
      push_arg(args, "--std=2008");
}

static void chomp(char *str)
{
   const size_t len = strlen(str);
   if (str[len - 1] == '\n')
      str[len - 1] = '\0';
}

static bool run_test(test_t *test)
{
   bool result = false;

   if (mkdir(test->name, 0777) != 0 && errno != EEXIST) {
      fprintf(stderr, "Failed to make logs/%s directory: %s\n",
              strerror(errno), test->name);
      return false;
   }

   if (chdir(test->name) != 0) {
      fprintf(stderr, "Failed to switch to logs/%s directory: %s\n",
              strerror(errno), test->name);
      return false;
   }

   printf("%15s : ", test->name);

#ifndef ENABLE_VHPI
   if (test->flags & F_VHPI) {
      set_attr(ANSI_FG_CYAN);
      printf("skipped\n");
      set_attr(ANSI_RESET);
      return true;
   }
#endif

   FILE *outf = fopen("out", "w");
   if (outf == NULL) {
      fprintf(stderr, "Failed to create logs/%s/out log file: %s\n",
              strerror(errno), test->name);
      goto out_chdir;
   }

   arglist_t *args = NULL;
   push_arg(&args, "%s/nvc%s", bin_dir, EXEEXT);
   push_std(test, &args);

   push_arg(&args, "-a");
   push_arg(&args, "%s/regress/%s.vhd", test_dir, test->name);

   if (test->flags & F_RELAX)
      push_arg(&args, "--relax=%s", test->relax);

   push_arg(&args, "-e");
   push_arg(&args, "%s", test->name);

   if (!(test->flags & F_OPT))
      push_arg(&args, "--disable-opt");

   if (test->flags & F_COVER)
      push_arg(&args, "--cover");

   for (generic_t *g = test->generics; g != NULL; g = g->next)
      push_arg(&args, "-g%s=%s", g->name, g->value);

   if (test->flags & F_FAIL) {
      if (!run_cmd(outf, &args))
         goto out_print;

      push_arg(&args, "%s/nvc%s", bin_dir, EXEEXT);
      push_std(test, &args);
   }

   push_arg(&args, "-r");

   if (test->flags & F_STOP)
      push_arg(&args, "--stop-time=%s", test->stop);

   if (test->flags & F_VHPI)
      push_arg(&args, "--load=%s/../lib/%s.so%s", bin_dir, test->name, EXEEXT);

   push_arg(&args, "%s", test->name);

   result = run_cmd(outf, &args);

   if (test->flags & F_FAIL)
      result = !result;

   if (result && test->flags & F_GOLD) {
      char goldname[PATH_MAX];
      snprintf(goldname, PATH_MAX, "%s/regress/gold/%s.txt",
               test_dir, test->name);

      FILE *goldf = fopen(goldname, "r");
      if (goldf == NULL) {
         set_attr(ANSI_FG_RED);
         printf("failed (missing gold file)\n");
         set_attr(ANSI_RESET);
         result = false;
         goto out_close;
      }

      outf = freopen("out", "r", outf);
      assert(outf != NULL);

      char gold_line[256];
      char out_line[256];
      while (fgets(gold_line, sizeof(gold_line), goldf)) {
         chomp(gold_line);

         bool match = false;
         while (!match && fgets(out_line, sizeof(out_line), outf)) {
            chomp(out_line);
            match = strstr(out_line, gold_line) != NULL;
         }

         if (!match) {
            set_attr(ANSI_FG_RED);
            printf("failed (no match)\n");
            set_attr(ANSI_FG_CYAN);
            printf("%s\n", gold_line);
            set_attr(ANSI_RESET);
            result = false;
            break;
         }
      }

      fclose(goldf);
   }

 out_print:
   if (result) {
      set_attr(ANSI_FG_GREEN);
      printf("ok\n");
      set_attr(ANSI_RESET);
   }
   else {
      set_attr(ANSI_FG_RED);
      printf("failed\n");
      set_attr(ANSI_RESET);

      char line[256];
      outf = freopen("out", "r", outf);
      assert(outf != NULL);
      while (fgets(line, sizeof(line), outf))
         fputs(line, stdout);
   }

 out_close:
   fclose(outf);

 out_chdir:
   chdir("..");
   return result;
}

int main(int argc, char **argv)
{
   realpath(TESTDIR, test_dir);

   realpath(argv[0], bin_dir);
   dirname(bin_dir);

   is_tty = isatty(STDOUT_FILENO);

   setenv("NVC_LIBPATH", "", 1);

   char lib_dir[PATH_MAX];
   snprintf(lib_dir, PATH_MAX, "%s/../lib", bin_dir);

   setenv("NVC_CYG_LIB", lib_dir, 1);
   setenv("NVC_LIBPATH", lib_dir, 1);

   if (getenv("QUICK"))
      return 0;

   if (!parse_test_list(argc - 1, argv + 1))
      return EXIT_FAILURE;

   if (mkdir("logs", 0777) != 0 && errno != EEXIST) {
      fprintf(stderr, "Failed to make logs directory: %s\n", strerror(errno));
      return EXIT_FAILURE;
   }

   if (chdir("logs") != 0) {
      fprintf(stderr, "Failed to change to logs directory: %s\n",
              strerror(errno));
      return EXIT_FAILURE;
   }

   bool pass = true;
   for (test_t *it = test_list; it != NULL; it = it->next) {
      if (!run_test(it))
         pass = false;
   }

   return pass ? 0 : EXIT_FAILURE;
}
