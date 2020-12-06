package assert_after_missing_type is
end package;

package body assert_after_missing_type is
  procedure proc(var : type_t) is
  begin
  end;

  procedure calling_proc is
  begin
    proc(1); -- Error (used to cause SIGABRT)
  end;

end package body;
