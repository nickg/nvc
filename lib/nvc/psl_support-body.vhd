-------------------------------------------------------------------------------
--  Copyright (C) 2021  Nick Gasson
--
--  Licensed under the Apache License, Version 2.0 (the "License");
--  you may not use this file except in compliance with the License.
--  You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0
--
--  Unless required by applicable law or agreed to in writing, software
--  distributed under the License is distributed on an "AS IS" BASIS,
--  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--  See the License for the specific language governing permissions and
--  limitations under the License.
-------------------------------------------------------------------------------

package body psl_support is

`if VHDL_VERSION < "2008" then

    function isunknown(
        val : in std_logic_vector
    ) return boolean is
    begin
        for i in val'low to val'high loop
            case val(i) is
            when 'U' => return true;
            when 'X' => return true;
            when 'Z' => return true;
            when 'W' => return true;
            when '-' => return true;
            when others => null;
            end case;
        end loop;
        return false;
    end;

`end if

    function isunknown(
        val : in std_ulogic_vector
    ) return boolean is
    begin
        for i in val'low to val'high loop
            case val(i) is
                when 'U' => return true;
                when 'X' => return true;
                when 'Z' => return true;
                when 'W' => return true;
                when '-' => return true;
                when others => null;
            end case;
        end loop;
        return false;
    end;

`if VHDL_VERSION < "2008" then

    function countones(
        val : in std_logic_vector
    ) return integer is
        variable n : integer := 0;
    begin
        for i in val'low to val'high loop
            if (val(i) = '1' or val(i) = 'H') then
                n := n + 1;
            end if;
        end loop;
        return n;
    end;

`end if

    function countones(
        val : in std_ulogic_vector
    ) return integer is
        variable n : integer := 0;
    begin
        for i in val'low to val'high loop
            if (val(i) = '1' or val(i) = 'H') then
                n := n + 1;
            end if;
        end loop;
        return n;
    end;

    function countones(
        val : in bit_vector
    ) return integer is
        variable n : integer := 0;
    begin
        for i in val'low to val'high loop
            if (val(i) = '1') then
                n := n + 1;
            end if;
        end loop;
        return n;
    end;

`if VHDL_VERSION < "2008" then

    function onehot(
        val : in std_logic_vector
    ) return boolean is
        variable flag : boolean := false;
    begin
        for i in val'low to val'high loop
            if (val(i) = '1' or val(i) = 'H') then
                if (flag) then
                    return false;
                else
                    flag := true;
                end if;
            end if;
        end loop;
        return flag;
    end;

`end if

    function onehot(
        val : in std_ulogic_vector
    ) return boolean is
        variable flag : boolean := false;
    begin
        for i in val'low to val'high loop
            if (val(i) = '1' or val(i) = 'H') then
                if (flag) then
                    return false;
                else
                    flag := true;
                end if;
            end if;
        end loop;
        return flag;
    end;

    function onehot(
        val : in bit_vector
    ) return boolean is
        variable flag : boolean := false;
    begin
        for i in val'low to val'high loop
            if (val(i) = '1') then
                if (flag) then
                    return false;
                else
                    flag := true;
                end if;
            end if;
        end loop;
        return flag;
    end;

`if VHDL_VERSION < "2008" then

    function onehot0(
        val : in std_logic_vector
    ) return boolean is
        variable flag : boolean := false;
    begin
        for i in val'low to val'high loop
            if (val(i) = '1' or val(i) = 'H') then
                if (flag) then
                    return false;
                else
                    flag := true;
                end if;
            end if;
        end loop;
        return true;
    end;

`end if

    function onehot0(
        val : in std_ulogic_vector
    ) return boolean is
        variable flag : boolean := false;
    begin
        for i in val'low to val'high loop
            if (val(i) = '1' or val(i) = 'H') then
                if (flag) then
                    return false;
                else
                    flag := true;
                end if;
            end if;
        end loop;
        return true;
    end;

    function onehot0(
        val : in bit_vector
    ) return boolean is
        variable flag : boolean := false;
    begin
        for i in val'low to val'high loop
            if (val(i) = '1') then
                if (flag) then
                    return false;
                else
                    flag := true;
                end if;
            end if;
        end loop;
        return true;
    end;

end package body;
