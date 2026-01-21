
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stdbool.h>
#include <ctype.h>

#include "vhpi_user.h"

#define STD_ULOGIC_TYPE "@IEEE:STD_LOGIC_1164:STD_ULOGIC"

typedef enum {
    CFG_ZERO,
    CFG_ONE,
    CFG_RND,
} cfg_t;

typedef enum {
    SKIP_FILTERED,
    SKIP_INVALID_TYPE,
} skip_kind_t;

typedef struct {
    // Randomizer configuration
    cfg_t       cfg;

    // Randomization seed;
    uint32_t    seed;

    // Number of errors ocurred
    size_t      errors;

    // Number of warnings occured
    size_t      warnings;

    // Number of initialized signals
    size_t      inits;

    // Number of skippted signals
    size_t      skips;

    // Name of entity to initialize
    char       *block;

    // Path where to write report
    char       *rpt_path;

    // Flag if current hierarchy shall be initialized
    bool        collect;

    // Report descriptor
    FILE       *rpt;

    // Current hierarchy
    char       *hier;
    size_t      hier_len;
} ctx_t;

///////////////////////////////////////////////////////////////////////////////
// VHPI Interface, wrappers and helpers
///////////////////////////////////////////////////////////////////////////////

static vhpiHandleT xvhpi_handle(ctx_t *ctx, vhpiOneToOneT type,
                                vhpiHandleT referenceHandle)
{
    vhpiHandleT h = vhpi_handle(type, referenceHandle);
    vhpiErrorInfoT ei;

    if (vhpi_check_error(&ei) > 0) {
        if (ei.severity == vhpiWarning)
            ctx->warnings++;
        else
            ctx->errors++;
        vhpi_assert(ei.severity, ei.message);
    }

    if (h == NULL)
        ctx->errors++;

    return h;
}

static vhpiIntT xvhpi_get(ctx_t *ctx, vhpiIntPropertyT property, vhpiHandleT handle)
{

    vhpiIntT v = vhpi_get(property, handle);
    vhpiErrorInfoT ei;

    if (vhpi_check_error(&ei) > 0) {
        if (ei.severity == vhpiWarning)
            ctx->warnings++;
        else
            ctx->errors++;
        vhpi_assert(ei.severity, ei.message);
    }

    return v;
}

///////////////////////////////////////////////////////////////////////////////
// Signal randomization
///////////////////////////////////////////////////////////////////////////////

static uint32_t fnv32_hash(const char *str, size_t len)
{
    unsigned char *s = (unsigned char *)str;
    const uint32_t FNV_32_PRIME = 0x01000193;

    uint32_t h = 0x811c9dc5;
    while (len--) {
        h ^= *s++;
        h *= FNV_32_PRIME;
    }

    return h;
}

static uint32_t oat_hash(const char *s, size_t len)
{
    unsigned char *p = (unsigned char*) s;
    uint32_t h = 0;

    while (len--) {
        h += *p++;
        h += (h << 10);
        h ^= (h >> 6);
    }

    h += (h << 3);
    h ^= (h >> 11);
    h += (h << 15);

    return h;
}

static uint32_t hash_string(ctx_t *ctx, const char *str, size_t salt)
{
    size_t sz1 = 32;
    char *b1 = malloc(sz1);
    memset(b1, 0, sz1);
    sprintf(b1, "%010u%020lu", ctx->seed, salt);

    uint32_t h1 = oat_hash(b1, strlen(b1));

    size_t sz2 = strlen(ctx->hier) + strlen(str) + 12;
    char *b2 = malloc(sz2);
    memset(b2, 0, sz2);
    sprintf(b2, "%010u%s:%s", h1, ctx->hier, str);

    uint32_t h2 = fnv32_hash(b2, strlen(b2));

    free(b1);
    free(b2);

    return h2;
}

///////////////////////////////////////////////////////////////////////////////
// Reporting
///////////////////////////////////////////////////////////////////////////////

static void print_signal_init(ctx_t *ctx, const char *sig_name, vhpiValueT *v)
{
    if (ctx->rpt == NULL)
        return;

    fprintf(ctx->rpt, "Initialized signal:\n");
    fprintf(ctx->rpt, "    Hierarchy:  %s\n", ctx->hier);
    fprintf(ctx->rpt, "    Name:       %s\n", sig_name);

    switch (v->format) {
    case vhpiSmallEnumVecVal:
        if (v->bufSize == 1)
            fprintf(ctx->rpt, "    Value:      '%c'\n",
                    (v->value.smallenumvs[0] == vhpi1) ? '1' : '0');
        else {
            size_t sz = v->bufSize + 3;
            char *buf = calloc(1, sz);

            buf[0] = '"';

            for (size_t i = 0; i < v->bufSize; i++)
                buf[i + 1] = (v->value.smallenumvs[i] == vhpi1) ? '1' : '0';

            buf[sz - 2] = '"';
            buf[sz - 1] = '\0';

            fprintf(ctx->rpt, "    Value:      %s\n", buf);
            free(buf);
        }
        break;

    case vhpiEnumVecVal:
        break;

    case vhpiIntVal:
        break;

    default:
        break;
    }

    fprintf(ctx->rpt, "\n");
}

static void print_signal_skip(ctx_t *ctx, const char *sig_name,
                              skip_kind_t skip_kind)
{
    if (ctx->rpt == NULL)
        return;

    fprintf(ctx->rpt, "Skipped signal:\n");
    fprintf(ctx->rpt, "    Hierarchy:  %s\n", ctx->hier);
    fprintf(ctx->rpt, "    Name:       %s\n", sig_name);
    fprintf(ctx->rpt, "    Reason:     %s\n\n", (skip_kind == SKIP_FILTERED) ?
                                              "+siginit+block+" :
                                              "invalid type") ;
}

///////////////////////////////////////////////////////////////////////////////
// Design walkthrough
///////////////////////////////////////////////////////////////////////////////

static void init_signal(ctx_t *ctx, vhpiHandleT sig_h)
{
    vhpiHandleT type_h = xvhpi_handle(ctx, vhpiType, sig_h);
    if (type_h == NULL)
        return;

    vhpiIntT type_kind = xvhpi_get(ctx, vhpiKindP, type_h);

    const char *sig_name = (const char *) vhpi_get_str(vhpiNameP, sig_h);

    // Skip if we are in hierarchy that should not be collected
    if (!ctx->collect) {
        print_signal_skip(ctx, sig_name, SKIP_FILTERED);
        ctx->skips++;

        vhpi_release_handle(type_h);
        return;
    }

    // Skip types that are not used in RTL designs
    if (type_kind == vhpiFileDeclK       || type_kind == vhpiPhysTypeDeclK   ||
        type_kind == vhpiAccessTypeDeclK || type_kind == vhpiFloatTypeDeclK) {

        print_signal_skip(ctx, sig_name, SKIP_INVALID_TYPE);
        ctx->skips++;

        vhpi_release_handle(type_h);
        return;
    }

    if (type_kind == vhpiRecordTypeDeclK) {
        vhpiHandleT elem_it = vhpi_iterator(vhpiSelectedNames, sig_h);
        vhpiHandleT elem_h;

        while ((elem_h = vhpi_scan(elem_it)) != NULL) {
            init_signal(ctx, elem_h);
            vhpi_release_handle(elem_h);
        }

        vhpi_release_handle(type_h);
        return;
    }

    // Get array element
    vhpiHandleT e_type_h = type_h;
    vhpiIntT e_type_kind = xvhpi_get(ctx, vhpiKindP, e_type_h);

    while (e_type_kind == vhpiArrayTypeDeclK) {
        vhpiHandleT nh = xvhpi_handle(ctx, vhpiElemType, e_type_h);

        if (e_type_h != type_h)
            vhpi_release_handle(e_type_h);

        e_type_h = nh;
        e_type_kind = xvhpi_get(ctx, vhpiKindP, e_type_h);
    }

    // Get base type
    vhpiHandleT b_type_h = e_type_h;
    vhpiIntT b_type_kind = xvhpi_get(ctx, vhpiKindP, b_type_h);

    while (b_type_kind == vhpiSubtypeDeclK) {
        vhpiHandleT nh = xvhpi_handle(ctx, vhpiBaseType, b_type_h);

        if (b_type_h != e_type_h)
            vhpi_release_handle(b_type_h);

        b_type_h = nh;
        b_type_kind = xvhpi_get(ctx, vhpiKindP, b_type_h);
    }

    const char *b_type_name = (const char *) vhpi_get_str(vhpiFullNameP, b_type_h);
    size_t n_elems = xvhpi_get(ctx, vhpiSizeP, sig_h);

    switch (b_type_kind) {
    case vhpiEnumTypeDeclK:
        if (!strcmp(b_type_name, STD_ULOGIC_TYPE)) {
            vhpiValueT v;
            v.format = vhpiSmallEnumVecVal;
            v.bufSize = sizeof(vhpiSmallEnumT) * n_elems;
            v.value.smallenumvs = malloc(v.bufSize);

            for (size_t i = 0; i < v.bufSize; i++) {
                switch (ctx->cfg) {
                case CFG_ZERO:
                    v.value.smallenumvs[i] = vhpi0;
                    break;
                case CFG_ONE:
                    v.value.smallenumvs[i] = vhpi1;
                    break;
                case CFG_RND:
                    {
                        vhpiSmallEnumT val = (hash_string(ctx, sig_name, i) % 2) ? vhpi0 : vhpi1;
                        v.value.smallenumvs[i] = val;
                        break;
                    }
                default:
                    vhpi_assert(vhpiFailure, "Unhandled cfg kind %d in 'init_signal'", ctx->cfg);
                }
            }

            print_signal_init(ctx, sig_name, &v);
            vhpi_put_value(sig_h, &v, vhpiDepositPropagate);
            free(v.value.smallenumvs);
        }
        else {
            vhpiHandleT enum_iter = vhpi_iterator(vhpiEnumLiterals, e_type_h);
            vhpiHandleT lit_h;

            // TODO: If there are many signals with long enums this may be ineffective!
            int enum_lits = 0;
            while ((lit_h = vhpi_scan(enum_iter)) != NULL) {
                enum_lits++;
                vhpi_release_handle(lit_h);
            }

            vhpiValueT v;
            v.format = vhpiEnumVecVal;
            v.bufSize = sizeof(vhpiEnumT) * n_elems;
            v.value.enumvs = malloc(v.bufSize);

            for (size_t i = 0; i < n_elems; i++) {
                if (ctx->cfg == CFG_ZERO)
                    v.value.enumvs[i] = 0;
                else if (ctx->cfg == CFG_ONE)
                    v.value.enumvs[i] = enum_lits - 1;
                else if (ctx->cfg == CFG_RND) {
                    vhpiEnumT val = hash_string(ctx, sig_name, i);
                    v.value.enumvs[i] = (vhpiEnumT)(val % enum_lits);
                }
            }

            print_signal_init(ctx, sig_name, &v);
            vhpi_put_value(sig_h, &v, vhpiDepositPropagate);
            free(v.value.enumvs);
        }
        break;

    case vhpiIntTypeDeclK:
        {
            vhpiHandleT constr_it = vhpi_iterator(vhpiConstraints, e_type_h);
            vhpiHandleT constr = vhpi_scan(constr_it);

            // TODO: Resolve this for 64-bit integer in VHDL 2019
            // TODO: Check if int can be with "downto" !
            int32_t min = xvhpi_get(ctx, vhpiLeftBoundP, constr);
            int32_t max = xvhpi_get(ctx, vhpiRightBoundP, constr);

            vhpiValueT v;
            v.format = vhpiIntVecVal;
            v.bufSize = sizeof(vhpiIntT) * n_elems;
            v.value.intgs = malloc(v.bufSize);

            for (size_t i = 0; i < n_elems; i++) {
                if (ctx->cfg == CFG_ZERO)
                    v.value.intgs[i] = min;
                else if (ctx->cfg == CFG_ONE)
                    v.value.intgs[i] = max;
                else if (ctx->cfg == CFG_RND) {
                    vhpiIntT val = hash_string(ctx, sig_name, i);
                    v.value.intgs[i] = (vhpiIntT)(min + (val % (max - min + 1)));
                }
            }

            print_signal_init(ctx, sig_name, &v);
            vhpi_put_value(sig_h, &v, vhpiDepositPropagate);
            free(v.value.intgs);

            break;
        }

    default:
        vhpi_assert(vhpiFailure, "Unhandled type kind: %d in 'init_signal'", b_type_kind);
    }

    ctx->inits++;

    if (b_type_h != e_type_h)
        vhpi_release_handle(b_type_h);
    if (e_type_h != type_h)
        vhpi_release_handle(e_type_h);

    vhpi_release_handle(type_h);
}

static void init_signals(ctx_t *ctx, vhpiHandleT inst_h)
{
    vhpiHandleT decls_it = vhpi_iterator(vhpiDecls, inst_h);
    vhpiHandleT decl_h;

    while ((decl_h = vhpi_scan(decls_it)) != NULL) {

        if (xvhpi_get(ctx, vhpiKindP, decl_h) != vhpiSigDeclK) {
            vhpi_release_handle(decl_h);
            continue;
        }

        init_signal(ctx, decl_h);
        vhpi_release_handle(decl_h);
    }
}

static void hier_push(ctx_t *ctx, vhpiHandleT h)
{
    const char *name = (const char *)vhpi_get_str(vhpiNameP, h);
    size_t new_hier_len = strlen(name) + strlen(ctx->hier) + 1;

    while (new_hier_len > ctx->hier_len) {
        ctx->hier_len *= 2;
        ctx->hier = realloc(ctx->hier, ctx->hier_len);
    }

    strcat(ctx->hier, ":");
    strcat(ctx->hier, name);
}

static void hier_pop(ctx_t *ctx)
{
    char *col = strrchr(ctx->hier, ':');
    *col = '\0';
}

static void walk_instances(ctx_t *ctx, vhpiHandleT h)
{
    bool cached_collect = ctx->collect;

    hier_push(ctx, h);

    vhpiHandleT ent_h = vhpi_handle(vhpiDesignUnit, h);
    const char *ent_name = (const char *)vhpi_get_str(vhpiNameP, ent_h);

    if (ent_name) {
        char *dash = strchr(ent_name, '-');
        size_t ent_name_len = dash - ent_name;
        if (ent_name_len == strlen(ctx->block) &&
            !strncmp(ctx->block, ent_name, strlen(ctx->block)))
            ctx->collect = true;
    }

    vhpi_release_handle(ent_h);

    init_signals(ctx, h);

    vhpiHandleT regions_it = vhpi_iterator(vhpiInternalRegions, h);
    vhpiHandleT inst_h;

    while ((inst_h = vhpi_scan(regions_it)) != NULL) {
        walk_instances(ctx, inst_h);
        vhpi_release_handle(inst_h);
    }

    if (!cached_collect && ctx->collect)
        ctx->collect = false;

    hier_pop(ctx);
}

static void print_stats(ctx_t *ctx)
{
    fprintf(ctx->rpt, "Initialization statistics:\n");
    fprintf(ctx->rpt, "   initialized signals:    %lu\n", ctx->inits);
    fprintf(ctx->rpt, "   skipped signals:        %lu\n", ctx->skips);
    fprintf(ctx->rpt, "   errors ocurred:         %lu\n", ctx->errors);
    fprintf(ctx->rpt, "   warnings ocurred:       %lu\n", ctx->warnings);
}

static void vhpi_cb(const struct vhpiCbDataS *cb_data)
{
    ctx_t *ctx = (ctx_t *)cb_data->user_data;

    vhpi_printf("initializing signals:");

    switch (ctx->cfg) {
    case CFG_ZERO:
        vhpi_printf("   enum types:             TYPE'left");
        vhpi_printf("   integer types:          TYPE'low");
        vhpi_printf("   std_logic types:        '0'");
        break;
    case CFG_ONE:
        vhpi_printf("   enum types:             TYPE'right");
        vhpi_printf("   integer types:          TYPE'high");
        vhpi_printf("   std_logic types:        '1'");
        break;
    case CFG_RND:
        vhpi_printf("   enum types:             random between TYPE'left and TYPE'right");
        vhpi_printf("   integer types:          random between TYPE'low and TYPE'high");
        vhpi_printf("   std_logic types:        random between '0' and '1'");
        vhpi_printf("   randomization seed:     %u", ctx->seed);
        break;
    }

    if (ctx->block != NULL) {
        vhpi_printf("   block to initialize:    %s", ctx->block);
    }
    else
        vhpi_printf("   block to initialize:    whole design");

    if (ctx->rpt_path != NULL) {
        ctx->rpt = fopen(ctx->rpt_path, "w");
        if (!ctx->rpt) {
            ctx->errors++;
            vhpi_assert(vhpiError, "Failed to open +signit+report+ file: %s", ctx->rpt_path);
        }
    }

    ctx->hier_len = 16;
    ctx->hier = calloc(1, ctx->hier_len);

    vhpiHandleT root_h = xvhpi_handle(ctx, vhpiRootInst, NULL);

    if (root_h != NULL) {
        walk_instances(ctx, root_h);
        vhpi_release_handle(root_h);
    }
    else
        ctx->errors++;

    vhpi_printf("initialization finished with %d errors.", ctx->errors);

    if (ctx->rpt) {
        print_stats(ctx);
        fclose(ctx->rpt);
    }

    free(ctx->hier);
}

static void register_callback(void)
{
    static ctx_t ctx = {
        .cfg        = CFG_RND,
        .seed       = 0,
        .errors     = 0,
        .warnings   = 0,
        .inits      = 0,
        .skips      = 0,
        .block      = 0,
        .collect    = false,
        .rpt_path   = NULL,
        .rpt        = NULL,
        .hier       = NULL,
        .hier_len   = 0,
    };

    // Read plugin arguments - Should stay here
    vhpiHandleT tool_h = vhpi_handle(vhpiTool, NULL);
    vhpiHandleT args_it = vhpi_iterator(vhpiArgvs, tool_h);

    for (vhpiHandleT arg = vhpi_scan(args_it); arg != NULL; arg = vhpi_scan(args_it)) {
        const char *s = (const char *)vhpi_get_str(vhpiStrValP, arg);

        if (s == NULL)
            continue;

        if (!strcmp(s, "+siginit+rand"))
            ctx.cfg = CFG_RND;
        else if (!strcmp(s, "+siginit+zero"))
            ctx.cfg = CFG_ZERO;
        else if (!strcmp(s, "+siginit+one"))
            ctx.cfg = CFG_ONE;
        else if (!strncmp(s, "+siginit+seed+", 14))
            // TODO: atoi works on signed int. But that should be OK since we have
            //       seed uint32_t and int is very likely 64 bit!
            ctx.seed = atoi(s + 14);
        else if (!strncmp(s, "+siginit+block+", 15)) {
            ctx.block = calloc(1, strlen(s) - 14);
            strcpy(ctx.block, s + 15);
            for (char *c = ctx.block; *c != 0; c++)
                *c = toupper(*c);
        }
        else if (!strncmp(s, "+siginit+report+", 16)) {
            size_t l = strlen(s + 16);
            ctx.rpt_path = malloc(l + 1);
            strcpy(ctx.rpt_path, s + 16);
        }
        else {
            vhpi_assert(vhpiError, "Invalid argument: %s", s);
            ctx.errors++;
        }

        vhpi_release_handle(arg);
    }

    vhpi_release_handle(tool_h);

    if (ctx.block)
        ctx.collect = false;

    /* Start of simulation callback */
    static vhpiCbDataT cb = {};
    cb.reason = vhpiCbStartOfSimulation;
    cb.cb_rtn = vhpi_cb;
    cb.user_data = (vhpiHandleT)&ctx;

    vhpi_register_cb(&cb, 0);

    vhpiErrorInfoT error_info;
    vhpi_check_error (&error_info);

    if (error_info.severity > 0) {
        vhpi_printf("Failed to register the callback...");
    }
}

void (*vhpi_startup_routines[])() = {
   register_callback,
   0
};