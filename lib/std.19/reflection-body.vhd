-------------------------------------------------------------------------------
--  Copyright (C) 2023  Nick Gasson
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

package body reflection is

    type string_ptr is access string;

    ---------------------------------------------------------------------------

    type enumeration_value_mirror_pt is protected body
        impure function get_subtype_mirror return enumeration_subtype_mirror is
        begin
            report "unimplemented" severity failure;
        end function;

        impure function to_value_mirror return value_mirror is
        begin
            report "unimplemented" severity failure;
        end function;

        impure function pos return integer is
        begin
            report "unimplemented" severity failure;
        end function;

        impure function image return string is
        begin
            report "unimplemented" severity failure;
        end function;
    end protected body;

    ---------------------------------------------------------------------------

    type enumeration_subtype_mirror_pt is protected body
        impure function to_subtype_mirror return subtype_mirror is
        begin
            report "unimplemented" severity failure;
        end function;

        impure function enumeration_literal (literal_idx : natural_index)
            return enumeration_value_mirror is
        begin
            report "unimplemented" severity failure;
        end function;

        impure function enumeration_literal (literal_name : string)
            return enumeration_value_mirror is
        begin
            report "unimplemented" severity failure;
        end function;

        impure function simple_name return string is
        begin
            report "unimplemented" severity failure;
        end function;

        impure function left return enumeration_value_mirror is
        begin
            report "unimplemented" severity failure;
        end function;

        impure function right return enumeration_value_mirror is
        begin
            report "unimplemented" severity failure;
        end function;

        impure function low return enumeration_value_mirror is
        begin
            report "unimplemented" severity failure;
        end function;

        impure function high return enumeration_value_mirror is
        begin
            report "unimplemented" severity failure;
        end function;

        impure function length return positive_index is
        begin
            report "unimplemented" severity failure;
        end function;

        impure function ascending return boolean is
        begin
            report "unimplemented" severity failure;
        end function;
    end protected body;

    ---------------------------------------------------------------------------

    type integer_value_mirror_pt is protected body
        variable f_owner   : value_mirror;
        variable f_subtype : integer_subtype_mirror;
        variable f_value   : integer;

        impure function get_subtype_mirror return integer_subtype_mirror is
        begin
            report "unimplemented" severity failure;
        end function;

        impure function to_value_mirror return value_mirror is
        begin
            return f_owner;
        end function;

        impure function value return integer is
        begin
            return f_value;
        end function;

        impure function image return string is
        begin
            return integer'image(f_value);
        end function;
    end protected body;

    ---------------------------------------------------------------------------

    type integer_subtype_mirror_pt is protected body
        variable f_owner : subtype_mirror;

        impure function to_subtype_mirror return subtype_mirror is
        begin
            return f_owner;
        end function;

        impure function simple_name return string is
        begin
            return f_owner.simple_name;
        end function;

        impure function left return integer_value_mirror is
        begin
            report "unimplemented" severity failure;
        end function;

        impure function right return integer_value_mirror is
        begin
            report "unimplemented" severity failure;
        end function;

        impure function low return integer_value_mirror is
        begin
            report "unimplemented" severity failure;
        end function;

        impure function high return integer_value_mirror is
        begin
            report "unimplemented" severity failure;
        end function;

        impure function length return index is
        begin
            report "unimplemented" severity failure;
        end function;

        impure function ascending return boolean is
        begin
            report "unimplemented" severity failure;
        end function;
    end protected body;

    ---------------------------------------------------------------------------

    type floating_value_mirror_pt is protected body
        impure function get_subtype_mirror return floating_subtype_mirror is
        begin
            report "unimplemented" severity failure;
        end function;

        impure function to_value_mirror return value_mirror is
        begin
            report "unimplemented" severity failure;
        end function;

        impure function value return real is
        begin
            report "unimplemented" severity failure;
        end function;

        impure function image return string is
        begin
            report "unimplemented" severity failure;
        end function;
    end protected body;

    ---------------------------------------------------------------------------

    type floating_subtype_mirror_pt is protected body
        impure function to_subtype_mirror return subtype_mirror is
        begin
            report "unimplemented" severity failure;
        end function;

        impure function simple_name return string is
        begin
            report "unimplemented" severity failure;
        end function;

        impure function left return floating_value_mirror is
        begin
            report "unimplemented" severity failure;
        end function;

        impure function right return floating_value_mirror is
        begin
            report "unimplemented" severity failure;
        end function;

        impure function low return floating_value_mirror is
        begin
            report "unimplemented" severity failure;
        end function;

        impure function high return floating_value_mirror is
        begin
            report "unimplemented" severity failure;
        end function;

        impure function ascending return boolean is
        begin
            report "unimplemented" severity failure;
        end function;
    end protected body;

    ---------------------------------------------------------------------------

    type physical_value_mirror_pt is protected body
        impure function get_subtype_mirror return physical_subtype_mirror is
        begin
            report "unimplemented" severity failure;
        end function;

        impure function to_value_mirror return value_mirror is
        begin
            report "unimplemented" severity failure;
        end function;

        impure function unit_index return index is
        begin
            report "unimplemented" severity failure;
        end function;

        impure function value return integer is
        begin
            report "unimplemented" severity failure;
        end function;

        impure function image return string is
        begin
            report "unimplemented" severity failure;
        end function;
    end protected body;

    ---------------------------------------------------------------------------

    type physical_subtype_mirror_pt is protected body
        impure function to_subtype_mirror return subtype_mirror is
        begin
            report "unimplemented" severity failure;
        end function;

        impure function units_length return index is
        begin
            report "unimplemented" severity failure;
        end function;

        impure function unit_name (unit_idx: index) return string is
        begin
            report "unimplemented" severity failure;
        end function;

        impure function unit_index (unit_name : string) return index is
        begin
            report "unimplemented" severity failure;
        end function;

        impure function scale (unit_idx: index) return natural is
        begin
            report "unimplemented" severity failure;
        end function;

        impure function scale (unit_name: string) return natural is
        begin
            report "unimplemented" severity failure;
        end function;

        impure function simple_name return string is
        begin
            report "unimplemented" severity failure;
        end function;

        impure function left return physical_value_mirror is
        begin
            report "unimplemented" severity failure;
        end function;

        impure function right return physical_value_mirror is
        begin
            report "unimplemented" severity failure;
        end function;

        impure function low return physical_value_mirror is
        begin
            report "unimplemented" severity failure;
        end function;

        impure function high return physical_value_mirror is
        begin
            report "unimplemented" severity failure;
        end function;

        impure function length return index is
        begin
            report "unimplemented" severity failure;
        end function;

        impure function ascending return boolean is
        begin
            report "unimplemented" severity failure;
        end function;
    end protected body;

    ---------------------------------------------------------------------------

    type record_value_mirror_pt is protected body
        impure function get_subtype_mirror return record_subtype_mirror is
        begin
            report "unimplemented" severity failure;
        end function;

        impure function to_value_mirror return value_mirror is
        begin
            report "unimplemented" severity failure;
        end function;

        impure function get (element_idx : index) return value_mirror is
        begin
            report "unimplemented" severity failure;
        end function;

        impure function get (element_name : string) return value_mirror is
        begin
            report "unimplemented" severity failure;
        end function;
    end protected body;

    ---------------------------------------------------------------------------

    type record_subtype_mirror_pt is protected body
        impure function to_subtype_mirror return subtype_mirror is
        begin
            report "unimplemented" severity failure;
        end function;

        impure function length return index is
        begin
            report "unimplemented" severity failure;
        end function;

        impure function element_name (element_idx : index) return string is
        begin
            report "unimplemented" severity failure;
        end function;

        impure function element_index (element_name : string) return index is
        begin
            report "unimplemented" severity failure;
        end function;

        impure function element_subtype (element_idx : index) return subtype_mirror is
        begin
            report "unimplemented" severity failure;
        end function;

        impure function element_subtype (element_name : string) return subtype_mirror is
        begin
            report "unimplemented" severity failure;
        end function;

        impure function simple_name return string is
        begin
            report "unimplemented" severity failure;
        end function;
    end protected body;

    ---------------------------------------------------------------------------

    type array_value_mirror_pt is protected body
        impure function get_subtype_mirror return array_subtype_mirror is
        begin
            report "unimplemented" severity failure;
        end function;

        impure function to_value_mirror return value_mirror is
        begin
            report "unimplemented" severity failure;
        end function;

        impure function get (idx : index) return value_mirror is
        begin
            report "unimplemented" severity failure;
        end function;

        impure function get (idx1, idx2 : index) return value_mirror is
        begin
            report "unimplemented" severity failure;
        end function;

        impure function get (idx1, idx2, idx3 : index) return value_mirror is
        begin
            report "unimplemented" severity failure;
        end function;

        impure function get (idx : index_vector) return value_mirror is
        begin
            report "unimplemented" severity failure;
        end function;
    end protected body;

    ---------------------------------------------------------------------------

    type array_subtype_mirror_pt is protected body
        impure function to_subtype_mirror return subtype_mirror is
        begin
            report "unimplemented" severity failure;
        end function;

        impure function dimensions return dimension is
        begin
            report "unimplemented" severity failure;
        end function;

        impure function index_subtype(idx : dimension := 1) return subtype_mirror is
        begin
            report "unimplemented" severity failure;
        end function;

        impure function element_subtype return subtype_mirror is
        begin
            report "unimplemented" severity failure;
        end function;

        impure function simple_name return string is
        begin
            report "unimplemented" severity failure;
        end function;

        impure function left (idx : dimension := 1) return index is
        begin
            report "unimplemented" severity failure;
        end function;

        impure function right (idx : dimension := 1) return index is
        begin
            report "unimplemented" severity failure;
        end function;

        impure function low (idx : dimension := 1) return index is
        begin
            report "unimplemented" severity failure;
        end function;

        impure function high (idx : dimension := 1) return index is
        begin
            report "unimplemented" severity failure;
        end function;

        impure function length (idx : dimension := 1) return index is
        begin
            report "unimplemented" severity failure;
        end function;

        impure function ascending (idx : dimension := 1) return boolean is
        begin
            report "unimplemented" severity failure;
        end function;
    end protected body;

    ---------------------------------------------------------------------------

    type access_value_mirror_pt is protected body
        impure function get_subtype_mirror return access_subtype_mirror is
        begin
            report "unimplemented" severity failure;
        end function;

        impure function to_value_mirror return value_mirror is
        begin
            report "unimplemented" severity failure;
        end function;

        impure function get return value_mirror is
        begin
            report "unimplemented" severity failure;
        end function;

        impure function is_null return boolean is
        begin
            report "unimplemented" severity failure;
        end function;
    end protected body;

    ---------------------------------------------------------------------------

    type access_subtype_mirror_pt is protected body
        impure function to_subtype_mirror return subtype_mirror is
        begin
            report "unimplemented" severity failure;
        end function;

        impure function simple_name return string is
        begin
            report "unimplemented" severity failure;
        end function;

        impure function designated_subtype return subtype_mirror is
        begin
            report "unimplemented" severity failure;
        end function;
    end protected body;

    ---------------------------------------------------------------------------

    type file_value_mirror_pt is protected body
        impure function get_subtype_mirror return file_subtype_mirror is
        begin
            report "unimplemented" severity failure;
        end function;

        impure function to_value_mirror return value_mirror is
        begin
            report "unimplemented" severity failure;
        end function;

        impure function get_file_logical_name return string is
        begin
            report "unimplemented" severity failure;
        end function;

        impure function get_file_open_kind return file_open_kind is
        begin
            report "unimplemented" severity failure;
        end function;
    end protected body;

    ---------------------------------------------------------------------------

    type file_subtype_mirror_pt is protected body
        impure function to_subtype_mirror return subtype_mirror is
        begin
            report "unimplemented" severity failure;
        end function;

        impure function simple_name return string is
        begin
            report "unimplemented" severity failure;
        end function;

        impure function designated_subtype return subtype_mirror is
        begin
            report "unimplemented" severity failure;
        end function;
    end protected body;

    ---------------------------------------------------------------------------

    type protected_value_mirror_pt is protected body
        impure function get_subtype_mirror return protected_subtype_mirror is
        begin
            report "unimplemented" severity failure;
        end function;

        impure function to_value_mirror return value_mirror is
        begin
            report "unimplemented" severity failure;
        end function;
    end protected body;

    ---------------------------------------------------------------------------

    type protected_subtype_mirror_pt is protected body
        impure function to_subtype_mirror return subtype_mirror is
        begin
            report "unimplemented" severity failure;
        end function;

        impure function simple_name return string is
        begin
            report "unimplemented" severity failure;
        end function;
    end protected body;

    ---------------------------------------------------------------------------

    type subtype_mirror_pt is protected body
        variable f_class   : type_class;
        variable f_name    : string_ptr;
        variable f_integer : integer_subtype_mirror;

        impure function get_type_class return type_class is
        begin
            return f_class;
        end function;

        impure function to_enumeration return enumeration_subtype_mirror is
        begin
            report "unimplemented" severity failure;
        end function;

        impure function to_integer return integer_subtype_mirror is
        begin
            assert f_class = CLASS_INTEGER;
            return f_integer;
        end function;

        impure function to_floating return floating_subtype_mirror is
        begin
            report "unimplemented" severity failure;
        end function;

        impure function to_physical return physical_subtype_mirror is
        begin
            report "unimplemented" severity failure;
        end function;

        impure function to_record return record_subtype_mirror is
        begin
            report "unimplemented" severity failure;
        end function;

        impure function to_array return array_subtype_mirror is
        begin
            report "unimplemented" severity failure;
        end function;

        impure function to_access return access_subtype_mirror is
        begin
            report "unimplemented" severity failure;
        end function;

        impure function to_file return file_subtype_mirror is
        begin
            report "unimplemented" severity failure;
        end function;

        impure function to_protected return protected_subtype_mirror is
        begin
            report "unimplemented" severity failure;
        end function;

        impure function simple_name return string is
        begin
            return f_name.all;
        end function;
    end protected body;

    ---------------------------------------------------------------------------

    type value_mirror_pt is protected body
        variable f_class   : value_class;
        variable f_subtype : subtype_mirror;
        variable f_integer : integer_value_mirror;

        impure function get_value_class return value_class is
        begin
            return f_class;
        end function;

        impure function get_subtype_mirror return subtype_mirror is
        begin
            return f_subtype;
        end function;

        impure function to_enumeration return enumeration_value_mirror is
        begin
            report "unimplemented" severity failure;
        end function;

        impure function to_integer return integer_value_mirror is
        begin
            assert f_class = CLASS_INTEGER;
            return f_integer;
        end function;

        impure function to_floating return floating_value_mirror is
        begin
            report "unimplemented" severity failure;
        end function;

        impure function to_physical return physical_value_mirror is
        begin
            report "unimplemented" severity failure;
        end function;

        impure function to_record return record_value_mirror is
        begin
            report "unimplemented" severity failure;
        end function;

        impure function to_array return array_value_mirror is
        begin
            report "unimplemented" severity failure;
        end function;

        impure function to_access return access_value_mirror is
        begin
            report "unimplemented" severity failure;
        end function;

        impure function to_file return file_value_mirror is
        begin
            report "unimplemented" severity failure;
        end function;

        impure function to_protected return protected_value_mirror is
        begin
            report "unimplemented" severity failure;
        end function;
    end protected body;

end package body;
