When submitting patches please make sure your code is formatted using
the same style as existing code.  Namely:

* Indent with three spaces *never* with tabs.

* Lines should never be longer than 80 characters, including comments.

* Do not break lines before opening braces *except* at the start of a
  function.  For example:

  ```c
  void my_func(int x)
  {
     if (x > 5) {
        for (int i = 0; i < 100; i++) {
           // ....
        }
     }
  }
  ```

* When an `if` statement branch has a single statement do not wrap it
  with braces, unless that statement is particularly large and split
  over multiple lines.

  ```c
  if (x < 10)
     do_something();
  ```

* Put a line break after a closing brace and before an `else`.  This is
  sometimes called the "Stroustrup" style in editors.

  ```c
  if (x == 7) {
     // ....
  }
  else {
     // ....
  }
  ```

* Insert a space around operators and between keywords and opening
  parenthesis, as in the examples above.

* Remove all trailing whitespace and do not insert more than one blank
  line at a time.

* Use C++ style `//` comments instead of traditional C `/*` comments.

* All identifiers are `snake_case`, never `camelCase` or `PascalCase`.

* When in doubt, refer to how existing code formats the construct you
  are using.

Some more general coding guidelines:

* Avoid the standard C string functions where possible as they are
  difficult to use correctly.  Instead use the `text_buf_t` functions
  declared in `util.h` (e.g. `tb_printf` and friends).

* If you must call `sprintf` then use `checked_sprintf` instead which
  avoids common mistakes.

* Generally, try to use the wrapper functions from `util.h` where
  possible as these are portable and handle errors for you.  For example
  `make_dir()` instead of `mkdir()` (which takes different arguments on
  Windows vs other systems).

* Do not call `malloc`, `calloc`, etc. directly. Instead use the wrapper
  functions `xmalloc`, `xcalloc` and so on which check for allocation
  failure.

* To allocate an array dynamically use `xmalloc_array(count, size)`
  instead of `xmalloc(count * size)` as the former checks for integer
  overflow.

* Typedefs are suffixed with `_t`.  Yes, I know this is reserved by
  POSIX.

* Most of the code is written in pseudo object-oriented style.  Prefer
  opaque structs in header files with accessor functions over exposing
  the struct fields.  For a type `foo_t` there should be a constructor
  `foo_new` and, if applicable, a cleanup function `foo_free`.  Methods
  on `foo_t` are usually prefixed `foo_` unless this makes the name
  awkward.

* Use `LOCAL` in a pointer declaration to automatically free storage at
  the end of its scope (this is a macro that expands to
  `__attribute__((cleanup(..)))`).  This is much less error-prone than
  manually calling `free`.  `LOCAL_TEXT_BUF` is an instance of
  `text_buf_t` that is freed at the end of its scope.

* Avoid writing excessive amounts of comments unless it is particularly
  difficult to understand the code without them.
