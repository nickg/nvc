# Exercise the +package / -package coverage spec directives.

# Include every package whose name ends in _pkg ...
+package *_pkg

# ... except those compiled into other_lib.
-hierarchy other_lib.*

# Always cover the top-level entity itself.
+block cover30
