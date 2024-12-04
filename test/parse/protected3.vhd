package fred4 is
  type fred4_t is protected
    impure function is_empty return boolean;
    impure function hi_there return is_empty; -- error
  end protected;
end package fred4;

--------------------------------------------------------------------------------

package pkg is
  type SharedCounter is protected body -- first body, but incomplete
  type SharedCounter is protected body -- duplicated body
  end protected body;
end package;
