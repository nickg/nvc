#include "vhpi_test.h"

#include <stdio.h>

static void check_vector(vhpiHandleT payload, const char *label,
                         int size, int left, int right, const char *expected)
{
   check_handle(payload);
   fail_unless(vhpi_get(vhpiKindP, payload) == vhpiSelectedNameK);
   fail_unless(vhpi_get(vhpiSizeP, payload) == size);

   vhpiHandleT payload_type = VHPI_CHECK(vhpi_handle(vhpiType, payload));
   check_handle(payload_type);
   fail_unless(vhpi_get(vhpiKindP, payload_type) == vhpiSubtypeDeclK);

   vhpiHandleT constraints =
      VHPI_CHECK(vhpi_iterator(vhpiConstraints, payload_type));
   check_handle(constraints);

   vhpiHandleT range = VHPI_CHECK(vhpi_scan(constraints));
   check_handle(range);
   fail_unless(vhpi_get(vhpiLeftBoundP, range) == left);
   fail_unless(vhpi_get(vhpiRightBoundP, range) == right);
   fail_if(vhpi_get(vhpiIsUpP, range));
   fail_if(vhpi_get(vhpiIsNullP, range));
   fail_unless(vhpi_get(vhpiIsDiscreteP, range));
   fail_unless(vhpi_scan(constraints) == NULL);
   check_error();

   vhpiValueT value = {
      .format = vhpiBinStrVal,
   };
   vhpiCharT buf[16];
   value.bufSize = sizeof(buf);
   value.value.str = buf;
   VHPI_CHECK(vhpi_get_value(payload, &value));
   vhpi_printf("%s = '%s'", label, (char *)buf);
   check_string(buf, expected);

   vhpi_release_handle(range);
   vhpi_release_handle(constraints);
   vhpi_release_handle(payload_type);
}

static void check_payload(vhpiHandleT payload, const char *label)
{
   check_vector(payload, label, 8, 7, 0, "00000000");
}

static void check_field(vhpiHandleT record, const char *prefix,
                        const char *field, int size, int left, int right,
                        const char *expected)
{
   char label[128];
   snprintf(label, sizeof(label), "%s.%s", prefix, field);

   vhpiHandleT handle = VHPI_CHECK(vhpi_handle_by_name(field, record));
   check_vector(handle, label, size, left, right, expected);
   vhpi_release_handle(handle);
}

static void check_sample_record(vhpiHandleT sample, const char *label)
{
   check_field(sample, label, "data", 8, 7, 0, "00000000");
   check_field(sample, label, "tag", 2, 1, 0, "00");
   check_field(sample, label, "addr", 4, 3, 0, "0000");
   check_field(sample, label, "valid", 1, 0, 0, "0");
}

static void check_first_selected_field(vhpiHandleT sample, const char *label)
{
   fail_unless(vhpi_get(vhpiKindP, sample) == vhpiIndexedNameK);

   vhpiHandleT base = VHPI_CHECK(vhpi_handle(vhpiBaseType, sample));
   check_handle(base);
   fail_unless(vhpi_get(vhpiKindP, base) == vhpiRecordTypeDeclK);
   (void)vhpi_get_str(vhpiKindStrP, base);
   (void)vhpi_get_str(vhpiFullCaseNameP, sample);
   vhpi_release_handle(base);

   vhpiHandleT selected = VHPI_CHECK(vhpi_iterator(vhpiSelectedNames, sample));
   check_handle(selected);

   vhpiHandleT field = VHPI_CHECK(vhpi_scan(selected));
   check_handle(field);
   check_vector(field, label, 8, 7, 0, "00000000");

   vhpi_release_handle(field);
   vhpi_release_handle(selected);
}

static void check_sample_instance(vhpiHandleT root)
{
   vhpiHandleT inst = VHPI_CHECK(vhpi_handle_by_name("u_samples", root));
   check_handle(inst);

   vhpiHandleT sample = VHPI_CHECK(vhpi_handle_by_name("sample", inst));
   check_handle(sample);

   vhpiHandleT sample0 =
      VHPI_CHECK(vhpi_handle_by_index(vhpiIndexedNames, sample, 0));
   check_handle(sample0);
   check_first_selected_field(sample0, "u_samples.sample(0).data selected");
   check_sample_record(sample0, "u_samples.sample(0)");

   vhpiHandleT sample_copy =
      VHPI_CHECK(vhpi_handle_by_name("sample_copy", inst));
   check_handle(sample_copy);

   vhpiHandleT sample_copy0 =
      VHPI_CHECK(vhpi_handle_by_index(vhpiIndexedNames, sample_copy, 0));
   check_handle(sample_copy0);
   check_sample_record(sample_copy0, "u_samples.sample_copy(0)");

   vhpi_release_handle(sample_copy0);
   vhpi_release_handle(sample_copy);
   vhpi_release_handle(sample0);
   vhpi_release_handle(sample);
   vhpi_release_handle(inst);
}

static void after_delay(const vhpiCbDataT *cb_data)
{
   vhpiHandleT root = VHPI_CHECK(vhpi_handle(vhpiRootInst, NULL));
   fail_if(root == NULL);

   check_sample_instance(root);

   vhpiHandleT nested = VHPI_CHECK(vhpi_handle_by_name("u_nested", root));
   check_handle(nested);

   vhpiHandleT leaf = VHPI_CHECK(vhpi_handle_by_name("u_leaf", nested));
   check_handle(leaf);

   vhpiHandleT status = VHPI_CHECK(vhpi_handle_by_name("status", root));
   check_handle(status);

   vhpiHandleT inner = VHPI_CHECK(vhpi_handle_by_name("inner", status));
   check_handle(inner);

   vhpiHandleT item = VHPI_CHECK(vhpi_handle_by_name("item", inner));
   check_handle(item);

   vhpiHandleT item0 =
      VHPI_CHECK(vhpi_handle_by_index(vhpiIndexedNames, item, 0));
   check_handle(item0);

   vhpiHandleT nested_payload =
      VHPI_CHECK(vhpi_handle_by_name("payload", item0));
   check_payload(nested_payload, "status.inner.item(0).payload");

   vhpiHandleT cache = VHPI_CHECK(vhpi_handle_by_name("cache", root));
   check_handle(cache);

   vhpiHandleT cache0 =
      VHPI_CHECK(vhpi_handle_by_index(vhpiIndexedNames, cache, 0));
   check_handle(cache0);

   vhpiHandleT cache_payload =
      VHPI_CHECK(vhpi_handle_by_name("payload", cache0));
   check_payload(cache_payload, "cache(0).payload");

   vhpiHandleT sample = VHPI_CHECK(vhpi_handle_by_name("sample", root));
   check_handle(sample);

   vhpiHandleT sample0 =
      VHPI_CHECK(vhpi_handle_by_index(vhpiIndexedNames, sample, 0));
   check_handle(sample0);
   check_sample_record(sample0, "sample(0)");

   vhpi_release_handle(sample0);
   vhpi_release_handle(sample);
   vhpi_release_handle(cache_payload);
   vhpi_release_handle(cache0);
   vhpi_release_handle(cache);
   vhpi_release_handle(nested_payload);
   vhpi_release_handle(item0);
   vhpi_release_handle(item);
   vhpi_release_handle(inner);
   vhpi_release_handle(status);
   vhpi_release_handle(leaf);
   vhpi_release_handle(nested);
   vhpi_release_handle(root);
}

static void start_of_sim(const vhpiCbDataT *cb_data)
{
   static vhpiTimeT time_1fs = {
      .low = 1,
   };

   vhpiCbDataT delay_cb = {
      .reason = vhpiCbAfterDelay,
      .cb_rtn = after_delay,
      .time   = &time_1fs,
   };
   VHPI_CHECK(vhpi_register_cb(&delay_cb, 0));
}

void vhpi21_startup(void)
{
   vhpiCbDataT cb_data = {
      .reason = vhpiCbStartOfSimulation,
      .cb_rtn = start_of_sim,
   };
   VHPI_CHECK(vhpi_register_cb(&cb_data, 0));
}
