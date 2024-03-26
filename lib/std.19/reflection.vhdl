-- -----------------------------------------------------------------
--
-- Copyright 2019 IEEE P1076 WG Authors
--
-- See the LICENSE file distributed with this work for copyright and
-- licensing information and the AUTHORS file.
--
-- This file to you under the Apache License, Version 2.0 (the "License").
-- You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
-- implied.  See the License for the specific language governing
-- permissions and limitations under the License.
--

package REFLECTION is
  type    INDEX           is       range INTEGER'low to INTEGER'high;
  subtype NATURAL_INDEX   is INDEX range 0 to INDEX'high;
  subtype POSITIVE_INDEX  is INDEX range 1 to INDEX'high;
  subtype DIMENSION       is INDEX range 1 to INDEX'high;
  type    INDEX_VECTOR    is array(DIMENSION range <>) of INDEX;

  -- Incomplete type declarations
  type VALUE_MIRROR_PT;
  type VALUE_MIRROR is access VALUE_MIRROR_PT;
  type SUBTYPE_MIRROR_PT;
  type SUBTYPE_MIRROR is access SUBTYPE_MIRROR_PT;

  -- Enumeration subtype/value mirror
  type ENUMERATION_SUBTYPE_MIRROR_PT;
  type ENUMERATION_SUBTYPE_MIRROR is access ENUMERATION_SUBTYPE_MIRROR_PT;

  type ENUMERATION_VALUE_MIRROR_PT is protected
    impure function get_subtype_mirror return ENUMERATION_SUBTYPE_MIRROR;
    impure function to_value_mirror    return VALUE_MIRROR;
    impure function pos   return INTEGER;
    impure function image return STRING;
  end protected;
  type ENUMERATION_VALUE_MIRROR is access ENUMERATION_VALUE_MIRROR_PT;

  type ENUMERATION_SUBTYPE_MIRROR_PT is protected
    impure function to_subtype_mirror return SUBTYPE_MIRROR;

    impure function enumeration_literal(literal_idx : NATURAL_INDEX) return ENUMERATION_VALUE_MIRROR;
    impure function enumeration_literal(literal_name : STRING)       return ENUMERATION_VALUE_MIRROR;

    impure function simple_name return STRING;
    impure function left        return ENUMERATION_VALUE_MIRROR;
    impure function right       return ENUMERATION_VALUE_MIRROR;
    impure function low         return ENUMERATION_VALUE_MIRROR;
    impure function high        return ENUMERATION_VALUE_MIRROR;
    impure function length      return POSITIVE_INDEX;
    impure function ascending   return BOOLEAN;
  end protected;


  -- Integer subtype/value mirror
  type INTEGER_SUBTYPE_MIRROR_PT;
  type INTEGER_SUBTYPE_MIRROR is access INTEGER_SUBTYPE_MIRROR_PT;

  type INTEGER_VALUE_MIRROR_PT is protected
    impure function get_subtype_mirror return INTEGER_SUBTYPE_MIRROR;
    impure function to_value_mirror    return VALUE_MIRROR;

    impure function value return INTEGER;
    impure function image return STRING;
  end protected;
  type INTEGER_VALUE_MIRROR is access INTEGER_VALUE_MIRROR_PT;

  type INTEGER_SUBTYPE_MIRROR_PT is protected
    impure function to_subtype_mirror return SUBTYPE_MIRROR;

    impure function simple_name return STRING;
    impure function left        return INTEGER_VALUE_MIRROR;
    impure function right       return INTEGER_VALUE_MIRROR;
    impure function low         return INTEGER_VALUE_MIRROR;
    impure function high        return INTEGER_VALUE_MIRROR;
    impure function length      return INDEX;
    impure function ascending   return BOOLEAN;
  end protected;


  -- Floating-point subtype/value mirror
  type FLOATING_SUBTYPE_MIRROR_PT;
  type FLOATING_SUBTYPE_MIRROR is access FLOATING_SUBTYPE_MIRROR_PT;

  type FLOATING_VALUE_MIRROR_PT is protected
    impure function get_subtype_mirror return FLOATING_SUBTYPE_MIRROR;
    impure function to_value_mirror    return VALUE_MIRROR;

    impure function value return REAL;
    impure function image return STRING;
  end protected;
  type FLOATING_VALUE_MIRROR is access FLOATING_VALUE_MIRROR_PT;

  type FLOATING_SUBTYPE_MIRROR_PT is protected
    impure function to_subtype_mirror return SUBTYPE_MIRROR;

    impure function simple_name return STRING;
    impure function left        return FLOATING_VALUE_MIRROR;
    impure function right       return FLOATING_VALUE_MIRROR;
    impure function low         return FLOATING_VALUE_MIRROR;
    impure function high        return FLOATING_VALUE_MIRROR;
    impure function ascending   return BOOLEAN;
  end protected;


  -- Physical subtype/value mirror
  type PHYSICAL_SUBTYPE_MIRROR_PT;
  type PHYSICAL_SUBTYPE_MIRROR is access PHYSICAL_SUBTYPE_MIRROR_PT;

  type PHYSICAL_VALUE_MIRROR_PT is protected
    impure function get_subtype_mirror return PHYSICAL_SUBTYPE_MIRROR;
    impure function to_value_mirror    return VALUE_MIRROR;

    impure function unit_index return INDEX;
    impure function value      return INTEGER;
    impure function image      return STRING;
  end protected;
  type PHYSICAL_VALUE_MIRROR is access PHYSICAL_VALUE_MIRROR_PT;

  type PHYSICAL_SUBTYPE_MIRROR_PT is protected
    impure function to_subtype_mirror return SUBTYPE_MIRROR;

    impure function units_length                   return INDEX;
    impure function unit_name (unit_idx: INDEX)    return STRING;
    impure function unit_index(unit_name : STRING) return INDEX;
    impure function scale(unit_idx: INDEX)         return NATURAL;
    impure function scale(unit_name: STRING)       return NATURAL;

    impure function simple_name return STRING;
    impure function left        return PHYSICAL_VALUE_MIRROR;
    impure function right       return PHYSICAL_VALUE_MIRROR;
    impure function low         return PHYSICAL_VALUE_MIRROR;
    impure function high        return PHYSICAL_VALUE_MIRROR;
    impure function length      return INDEX;
    impure function ascending   return BOOLEAN;
  end protected;


  -- Record subtype/value mirror
  type RECORD_SUBTYPE_MIRROR_PT;
  type RECORD_SUBTYPE_MIRROR is access RECORD_SUBTYPE_MIRROR_PT;

  type RECORD_VALUE_MIRROR_PT is protected
    impure function get_subtype_mirror return RECORD_SUBTYPE_MIRROR;
    impure function to_value_mirror    return VALUE_MIRROR;

    impure function get(element_idx : INDEX)   return VALUE_MIRROR;
    impure function get(element_name : STRING) return VALUE_MIRROR;
  end protected;
  type RECORD_VALUE_MIRROR is access RECORD_VALUE_MIRROR_PT;

  type RECORD_SUBTYPE_MIRROR_PT is protected
    impure function to_subtype_mirror return SUBTYPE_MIRROR;

    impure function length                                 return INDEX;
    impure function element_name(element_idx : INDEX)      return STRING;
    impure function element_index(element_name : STRING)   return INDEX;
    impure function element_subtype(element_idx : INDEX)   return SUBTYPE_MIRROR;
    impure function element_subtype(element_name : STRING) return SUBTYPE_MIRROR;

    impure function simple_name return STRING;
  end protected;


  -- Array subtype/value mirror
  type ARRAY_SUBTYPE_MIRROR_PT;
  type ARRAY_SUBTYPE_MIRROR is access ARRAY_SUBTYPE_MIRROR_PT;

  type ARRAY_VALUE_MIRROR_PT is protected
    impure function get_subtype_mirror return ARRAY_SUBTYPE_MIRROR;
    impure function to_value_mirror    return VALUE_MIRROR;

    impure function get(idx : INDEX)              return VALUE_MIRROR;
    impure function get(idx1, idx2 : INDEX)       return VALUE_MIRROR;
    impure function get(idx1, idx2, idx3 : INDEX) return VALUE_MIRROR;
    impure function get(idx : INDEX_VECTOR)       return VALUE_MIRROR;
  end protected;
  type ARRAY_VALUE_MIRROR is access ARRAY_VALUE_MIRROR_PT;

  type ARRAY_SUBTYPE_MIRROR_PT is protected
    impure function to_subtype_mirror return SUBTYPE_MIRROR;

    impure function dimensions                          return DIMENSION;
    impure function index_subtype(idx : DIMENSION := 1) return SUBTYPE_MIRROR;
    impure function element_subtype                     return SUBTYPE_MIRROR;

    impure function simple_name                     return STRING;
    impure function left(idx : DIMENSION := 1)      return INDEX;
    impure function right(idx : DIMENSION := 1)     return INDEX;
    impure function low(idx : DIMENSION := 1)       return INDEX;
    impure function high(idx : DIMENSION := 1)      return INDEX;
    impure function length(idx : DIMENSION := 1)    return INDEX;
    impure function ascending(idx : DIMENSION := 1) return BOOLEAN;
  end protected;


  -- Access subtype/value mirror
  type ACCESS_SUBTYPE_MIRROR_PT;
  type ACCESS_SUBTYPE_MIRROR is access ACCESS_SUBTYPE_MIRROR_PT;

  type ACCESS_VALUE_MIRROR_PT is protected
    impure function get_subtype_mirror return ACCESS_SUBTYPE_MIRROR;
    impure function to_value_mirror    return VALUE_MIRROR;

    --
    impure function get return VALUE_MIRROR;
    impure function is_null return BOOLEAN;
  end protected;
  type ACCESS_VALUE_MIRROR is access ACCESS_VALUE_MIRROR_PT;

  type ACCESS_SUBTYPE_MIRROR_PT is protected
    impure function to_subtype_mirror return SUBTYPE_MIRROR;

    impure function simple_name        return STRING;
    impure function designated_subtype return SUBTYPE_MIRROR;
  end protected;


  -- File subtype/value mirror
  type FILE_SUBTYPE_MIRROR_PT;
  type FILE_SUBTYPE_MIRROR is access FILE_SUBTYPE_MIRROR_PT;

  type FILE_VALUE_MIRROR_PT is protected
    impure function get_subtype_mirror return FILE_SUBTYPE_MIRROR;
    impure function to_value_mirror    return VALUE_MIRROR;

    impure function get_file_logical_name return STRING;
    impure function get_file_open_kind    return FILE_OPEN_KIND;
  end protected;
  type FILE_VALUE_MIRROR is access FILE_VALUE_MIRROR_PT;

  type FILE_SUBTYPE_MIRROR_PT is protected
    impure function to_subtype_mirror return SUBTYPE_MIRROR;

    impure function simple_name        return STRING;
    impure function designated_subtype return SUBTYPE_MIRROR;
  end protected;


  -- Protected subtype/value mirror
  type PROTECTED_SUBTYPE_MIRROR_PT;
  type PROTECTED_SUBTYPE_MIRROR is access PROTECTED_SUBTYPE_MIRROR_PT;

  type PROTECTED_VALUE_MIRROR_PT is protected
    impure function get_subtype_mirror return PROTECTED_SUBTYPE_MIRROR;
    impure function to_value_mirror    return VALUE_MIRROR;
  end protected;
  type PROTECTED_VALUE_MIRROR is access PROTECTED_VALUE_MIRROR_PT;

  type PROTECTED_SUBTYPE_MIRROR_PT is protected
    impure function to_subtype_mirror return SUBTYPE_MIRROR;

    impure function simple_name return STRING;
  end protected;


  -- Type classes and sub-classes
  type TYPE_CLASS is (
    CLASS_ENUMERATION,
    CLASS_INTEGER,
    CLASS_FLOATING,
    CLASS_PHYSICAL,
    CLASS_RECORD,
    CLASS_ARRAY,
    CLASS_ACCESS,
    CLASS_FILE,
    CLASS_PROTECTED
  );
  alias VALUE_CLASS is TYPE_CLASS;


  -- Subtype/value mirror
  type SUBTYPE_MIRROR_PT is protected
    impure function get_type_class return TYPE_CLASS;

    -- Get the corresponding representation
    impure function to_enumeration return ENUMERATION_SUBTYPE_MIRROR;
    impure function to_integer     return INTEGER_SUBTYPE_MIRROR;
    impure function to_floating    return FLOATING_SUBTYPE_MIRROR;
    impure function to_physical    return PHYSICAL_SUBTYPE_MIRROR;
    impure function to_record      return RECORD_SUBTYPE_MIRROR;
    impure function to_array       return ARRAY_SUBTYPE_MIRROR;
    impure function to_access      return ACCESS_SUBTYPE_MIRROR;
    impure function to_file        return FILE_SUBTYPE_MIRROR;
    impure function to_protected   return PROTECTED_SUBTYPE_MIRROR;

    impure function simple_name return STRING;
  end protected;

  type VALUE_MIRROR_PT is protected
    impure function get_value_class    return VALUE_CLASS;
    impure function get_subtype_mirror return SUBTYPE_MIRROR;

    -- Get the corresponding representation
    impure function to_enumeration return ENUMERATION_VALUE_MIRROR;
    impure function to_integer     return INTEGER_VALUE_MIRROR;
    impure function to_floating    return FLOATING_VALUE_MIRROR;
    impure function to_physical    return PHYSICAL_VALUE_MIRROR;
    impure function to_record      return RECORD_VALUE_MIRROR;
    impure function to_array       return ARRAY_VALUE_MIRROR;
    impure function to_access      return ACCESS_VALUE_MIRROR;
    impure function to_file        return FILE_VALUE_MIRROR;
    impure function to_protected   return PROTECTED_VALUE_MIRROR;
  end protected;
end package REFLECTION;
