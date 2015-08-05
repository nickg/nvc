package fred3 is
  function my_favorite_letter return character;
end package fred3;

package body fred3 is

  type other is ('X', 'Y', 'Z');

  function my_favorite_letter return character is
  begin
    for ch in 'A' to 'Z' loop           -- Was error
      if ch = 'R' then
        return ch;
      end if;
    end loop;
    for ch in 'Z' downto 'A' loop           -- Was error
      if ch = 'R' then
        return ch;
      end if;
    end loop;
  end function my_favorite_letter;

end package body fred3;
