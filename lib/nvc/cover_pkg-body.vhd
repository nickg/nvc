-------------------------------------------------------------------------------
--  Copyright (C) 2024  Nick Gasson
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

package body cover_pkg is

    procedure create_cover_scope (scope : out t_scope_handle;
                                  name  : in string) is
    begin
        -- Foreign subprogram
    end procedure;

    procedure add_cover_item (scope     : inout t_scope_handle;
                              item      : out t_item_handle;
                              name      : in string;
                              atleast   : in natural;
                              ranges    : in t_item_range_array) is
    begin
        -- Foreign subprogram
    end procedure;

    procedure increment_cover_item (scope : inout t_scope_handle;
                                    item  : in t_item_handle) is
    begin
        -- Foreign subprogram
    end procedure;

    procedure set_cover_scope_name(scope : inout t_scope_handle;
                                   name  : in string) is
    begin
        -- Foreign subprogram
    end procedure;

end package body;
