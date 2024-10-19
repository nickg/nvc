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

-------------------------------------------------------------------------------
-- This package provides an interface to create functional coverage bins
-- at runtime.
-------------------------------------------------------------------------------

package cover_pkg is

    type t_opaque_scope;              -- Internal data type

    type t_scope_handle is access t_opaque_scope;

    type t_item_handle is range -1 to 2147483647;

    type t_item_range is record
        min: integer;
        max: integer;
    end record;

    type t_item_range_array is array ( integer range <> ) of t_item_range;

    constant INVALID_ITEM : t_item_handle := t_item_handle'left;

    procedure create_cover_scope (scope : out t_scope_handle;
                                  name  : in string);

    procedure set_cover_scope_name(scope : inout t_scope_handle;
                                   name  : in string);

    procedure add_cover_item (scope     : inout t_scope_handle;
                              item      : out t_item_handle;
                              name      : in string;
                              source    : in integer;
                              atleast   : in integer;
                              flags     : in integer;
                              n_ranges  : in integer;
                              ranges    : in t_item_range_array
                              );

    procedure increment_cover_item (scope : inout t_scope_handle;
                                    item  : in t_item_handle);

    attribute foreign of create_cover_scope : procedure
        is "INTERNAL _nvc_create_cover_scope";

    attribute foreign of set_cover_scope_name : procedure
        is "INTERNAL _nvc_set_cover_scope_name";

    attribute foreign of add_cover_item : procedure
        is "INTERNAL _nvc_add_cover_item";

    attribute foreign of increment_cover_item : procedure
        is "INTERNAL _nvc_increment_cover_item";

end package;
