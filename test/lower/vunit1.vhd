package string_ptr_pkg is
  subtype index_t is integer range -1 to integer'high;
  subtype byte_t is integer range 0 to 255;
  type storage_mode_t is (internal, extfnc, extacc);

  type string_access_t is access string;
  type string_access_vector_t is array (natural range <>) of string_access_t;
  type string_access_vector_access_t is access string_access_vector_t;

  type extstring_access_t is access string(1 to integer'high);
  type extstring_access_vector_t is array (natural range <>) of extstring_access_t;
  type extstring_access_vector_access_t is access extstring_access_vector_t;

  type string_ptr_t is record
    ref : index_t;
  end record;
  constant null_string_ptr : string_ptr_t := (ref => -1);

  alias  ptr_t  is string_ptr_t;
  alias  val_t  is character;
  alias  vec_t  is string;
  alias  vav_t  is string_access_vector_t;
  alias evav_t  is extstring_access_vector_t;
  alias  vava_t is string_access_vector_access_t;
  alias evava_t is extstring_access_vector_access_t;


  procedure set (
    ptr   : ptr_t;
    index : positive;
    value : val_t
  );

end package;

package body string_ptr_pkg is
  type prot_storage_t is protected
    procedure set (
      ref   : natural;
      index : positive;
      value : val_t
    );
  end protected;

  type prot_storage_t is protected body
    type storage_t is record
      id     : integer;
      mode   : storage_mode_t;
      length : integer;
    end record;
    constant null_storage : storage_t := (integer'low, internal, integer'low);

    type storage_vector_t is array (natural range <>) of storage_t;
    type storage_vector_access_t is access storage_vector_t;

    type ptr_storage is record
      idx   : natural;
      ptr   : natural;
      eptr  : natural;
      idxs  : storage_vector_access_t;
      ptrs  : vava_t;
      eptrs : evava_t;
    end record;

    variable st : ptr_storage := (0, 0, 0, null, null, null);

    procedure set (
      ref   : natural;
      index : positive;
      value : val_t
    ) is
      variable s : storage_t := st.idxs(ref);
    begin
      case s.mode is
        when extfnc   => null;--write_char(s.id, index-1, value);
        when extacc   => st.eptrs(s.id)(index) := value;
        when internal => st.ptrs(s.id)(index)  := value;
      end case;
    end;


  end protected body;

  shared variable vec_ptr_storage : prot_storage_t;

  procedure set (
    ptr   : ptr_t;
    index : positive;
    value : val_t
  ) is begin
    vec_ptr_storage.set(ptr.ref, index, value);
  end;

end package body;
