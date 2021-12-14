entity file3 is
end entity;

architecture a of file3 is
begin
  main : process
    constant file_name : string := "output.raw";
    type binary_file is file of character;
    file fptr : binary_file;
    variable fstatus : file_open_status;
    variable tmp, tmp2 : character;
  begin
    assert character'pos(character'low) = 0;
    assert character'pos(character'high) = 255;

    file_open(fstatus, fptr, file_name, write_mode);
    assert fstatus = open_ok;
    for i in 0 to 255 loop
      write(fptr, character'val(i));
    end loop;
    file_close(fptr);

    file_open(fstatus, fptr, file_name, read_mode);
    assert fstatus = open_ok;

    for i in 0 to 255 loop
      read(fptr, tmp);
      assert character'pos(tmp) = i;
    end loop;
    assert endfile(fptr);
    file_close(fptr);

    report "Success";
    wait;
  end process;
end;
