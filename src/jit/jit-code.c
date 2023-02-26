//
//  Copyright (C) 2022-2023  Nick Gasson
//
//  This program is free software: you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation, either version 3 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program.  If not, see <http://www.gnu.org/licenses/>.
//

#include "util.h"
#include "cpustate.h"
#include "debug.h"
#include "hash.h"
#include "ident.h"
#include "jit/jit-priv.h"
#include "option.h"
#include "thread.h"

#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <unistd.h>

#if defined __MINGW32__
#include <winnt.h>
#include <math.h>   // XXX: temporary
#elif defined __APPLE__
#include <mach-o/loader.h>
#include <mach-o/reloc.h>
#include <mach-o/nlist.h>
#include <mach-o/stab.h>
#include <mach-o/arm64/reloc.h>
#else
#include <elf.h>
#endif

#ifdef HAVE_CAPSTONE
#include <capstone.h>
#endif

#define CODECACHE_ALIGN   4096
#define CODECACHE_SIZE    0x400000
#define THREAD_CACHE_SIZE 0x10000
#define CODE_BLOB_ALIGN   256
#define MIN_BLOB_SIZE     0x4000

#define __IMM64(x) __IMM32(x), __IMM32((x) >> 32)
#define __IMM32(x) __IMM16(x), __IMM16((x) >> 16)
#define __IMM16(x) (x) & 0xff, ((x) >> 8) & 0xff

STATIC_ASSERT(MIN_BLOB_SIZE <= THREAD_CACHE_SIZE);
STATIC_ASSERT(MIN_BLOB_SIZE % CODE_BLOB_ALIGN == 0);
STATIC_ASSERT(CODECACHE_SIZE % THREAD_CACHE_SIZE == 0);

typedef struct _code_span {
   code_cache_t *owner;
   code_span_t  *next;
   ident_t       name;
   uint8_t      *base;
   size_t        size;
} code_span_t;

typedef struct _patch_list {
   patch_list_t    *next;
   uint8_t         *wptr;
   jit_label_t      label;
   code_patch_fn_t  fn;
} patch_list_t;

typedef struct _code_cache {
   nvc_lock_t   lock;
   uint8_t     *mem;
   code_span_t *spans;
   code_span_t *freelist[MAX_THREADS];
   code_span_t *globalfree;
   FILE        *perfmap;
#ifdef HAVE_CAPSTONE
   csh          capstone;
#endif
#ifdef DEBUG
   size_t       used;
#endif
} code_cache_t;

static void code_disassemble(code_span_t *span, uintptr_t mark,
                             struct cpu_state *cpu);

static void code_cache_unwinder(uintptr_t addr, debug_frame_t *frame,
                                void *context)
{
   code_cache_t *code = context;

   const uint8_t *pc = (uint8_t *)addr;
   for (code_span_t *span = code->spans; span; span = span->next) {
      if (pc >= span->base && pc < span->base + span->size) {
         frame->kind = FRAME_VHDL;
         frame->disp = pc - span->base;
         frame->symbol = istr(span->name);
      }
   }
}

static void code_fault_handler(int sig, void *addr, struct cpu_state *cpu,
                               void *context)
{
   code_cache_t *code = context;

   const uint8_t *pc = (uint8_t *)cpu->pc;
   if (pc < code->mem || pc > code->mem + CODECACHE_SIZE)
      return;

   uintptr_t mark = cpu->pc;
#ifndef __MINGW32__
   if (sig == SIGTRAP)
      mark--;   // Point to faulting instruction
#endif

   for (code_span_t *span = code->spans; span; span = span->next) {
      if (pc >= span->base && pc < span->base + span->size && span->name)
         code_disassemble(span, mark, cpu);
   }
}

static code_span_t *code_span_new(code_cache_t *code, ident_t name,
                                  uint8_t *base, size_t size)
{
   assert(base >= code->mem);
   assert(base + size <= code->mem + CODECACHE_SIZE);

   SCOPED_LOCK(code->lock);

   code_span_t *span = xcalloc(sizeof(code_span_t));
   span->name  = name;
   span->next  = code->spans;
   span->base  = base;
   span->size  = size;
   span->owner = code;

   code->spans = span;
   return span;
}

code_cache_t *code_cache_new(void)
{
   code_cache_t *code = xcalloc(sizeof(code_cache_t));
   code->mem = map_jit_pages(CODECACHE_ALIGN, CODECACHE_SIZE);

#ifdef HAVE_CAPSTONE
#if defined ARCH_X86_64
   if (cs_open(CS_ARCH_X86, CS_MODE_64, &(code->capstone)) != CS_ERR_OK)
      fatal_trace("failed to init capstone for x86_64");
#elif defined ARCH_ARM64
   if (cs_open(CS_ARCH_ARM64, CS_MODE_ARM, &(code->capstone)) != CS_ERR_OK)
      fatal_trace("failed to init capstone for Arm64");
#else
#error Cannot configure capstone for this architecture
#endif
#endif

   add_fault_handler(code_fault_handler, code);
   debug_add_unwinder(code->mem, CODECACHE_SIZE, code_cache_unwinder, code);

   code->globalfree = code_span_new(code, NULL, code->mem, CODECACHE_SIZE);

   return code;
}

void code_cache_free(code_cache_t *code)
{
   debug_remove_unwinder(code->mem);
   remove_fault_handler(code_fault_handler, code);

   nvc_munmap(code->mem, CODECACHE_SIZE);

   for (code_span_t *it = code->spans, *tmp; it; it = tmp) {
      tmp = it->next;
      free(it);
   }

#ifdef HAVE_CAPSTONE
   cs_close(&(code->capstone));
#endif

#ifdef DEBUG
   if (!opt_get_int(OPT_UNIT_TEST))
      debugf("JIT code footprint: %zu bytes", code->used);
#endif

   free(code);
}

static void code_disassemble(code_span_t *span, uintptr_t mark,
                             struct cpu_state *cpu)
{
#ifdef HAVE_CAPSTONE
   SCOPED_LOCK(span->owner->lock);

   printf("--");

   const int namelen = ident_len(span->name);
   for (int i = 0; i < 72 - namelen; i++)
      fputc('-', stdout);

   printf(" %s ----\n", istr(span->name));

   cs_insn *insn = cs_malloc(span->owner->capstone);

   const uint8_t *const eptr = span->base + span->size;
   for (const uint8_t *ptr = span->base; ptr < eptr; ) {
      size_t size = eptr - ptr;
      uint64_t address = (uint64_t)ptr;
      int col = 0;
      if (cs_disasm_iter(span->owner->capstone, &ptr, &size, &address, insn)) {
         char hex1[33], *p = hex1;
         for (size_t k = 0; k < insn->size; k++)
            p += checked_sprintf(p, hex1 + sizeof(hex1) - p, "%02x",
                                 insn->bytes[k]);

         col = printf("%-12" PRIx64 " %-16.16s %s %s", insn->address,
                          hex1, insn->mnemonic, insn->op_str);

         if (strlen(hex1) > 16)
            col = printf("\n%15s -%-16s", "", hex1 + 16) - 1;
      }
      else {
#ifdef ARCH_ARM64
         col = printf("%-12" PRIx64 " %-16.08x %s 0x%08x", (uint64_t)ptr,
                      *(uint32_t *)ptr, ".word", *(uint32_t *)ptr);
         ptr += 4;
#else
         col = printf("%-12" PRIx64 " %-16.02x %s 0x%02x", (uint64_t)ptr,
                      *ptr, ".byte", *ptr);
         ptr++;
#endif
      }

      if (mark != 0 && (ptr >= eptr || address > mark)) {
         for (; col < 66; col++)
            fputc(' ', stdout);
         printf("<=============\n");
         if (cpu != NULL) {
#ifdef ARCH_X86_64
            const char *names[] = {
               "RAX", "RCX", "RDX", "RBX", "RSP", "RBP", "RSI", "RDI",
               "R8", "R9", "R10", "R11", "R12", "R13", "R14", "R15"
            };
            for (int i = 0; i < ARRAY_LEN(names); i++)
               printf("\t%s\t%"PRIx64"\n", names[i], cpu->regs[i]);
#else
            for (int i = 0; i < 32; i++)
               printf("\tR%d\t%"PRIxPTR"\n", i, cpu->regs[i]);
#endif
         }
         mark = 0;
      }
      else
         printf("\n");
   }

   cs_free(insn, 1);

   for (int i = 0; i < 80; i++)
      fputc('-', stdout);
   printf("\n");
   fflush(stdout);
#endif
}

static void code_write_perf_map(code_span_t *span)
{
   SCOPED_LOCK(span->owner->lock);

   if (span->owner->perfmap == NULL) {
      char *fname LOCAL = xasprintf("/tmp/perf-%d.map", getpid());
      if ((span->owner->perfmap = fopen(fname, "w")) == NULL) {
         warnf("cannot create %s: %s", fname, last_os_error());
         opt_set_int(OPT_PERF_MAP, 0);
         return;
      }
      else
         debugf("writing perf map to %s", fname);
   }

   fprintf(span->owner->perfmap, "%p 0x%zx %s\n", span->base, span->size,
           istr(span->name));
   fflush(span->owner->perfmap);
}

code_blob_t *code_blob_new(code_cache_t *code, ident_t name, size_t hint)
{
   code_span_t **freeptr = &(code->freelist[thread_id()]);

   code_span_t *free = relaxed_load(freeptr);
   if (free == NULL) {
      free = code_span_new(code, NULL, code->mem, 0);
      relaxed_store(freeptr, free);
   }

   const size_t reqsz = hint ?: MIN_BLOB_SIZE;

   if (free->size < reqsz) {
      SCOPED_LOCK(code->lock);

      if (code->globalfree->size == 0)
         return NULL;

#ifdef DEBUG
      if (free->size > 0)
         debugf("thread %d needs new code cache from global free list "
                "(requested %zu bytes, wasted %zu bytes)",
                thread_id(), reqsz, free->size);
#endif

      const size_t chunksz = MAX(reqsz, THREAD_CACHE_SIZE);
      const size_t alignedsz = ALIGN_UP(chunksz, CODE_BLOB_ALIGN);
      const size_t take = MIN(code->globalfree->size, alignedsz);

      free->size = take;
      free->base = code->globalfree->base;

      code->globalfree->base += take;
      code->globalfree->size -= take;

      if (code->globalfree->size == 0)
         warnf("global JIT code buffer exhausted");
   }

   assert(reqsz <= free->size);
   assert(((uintptr_t)free->base & (CODE_BLOB_ALIGN - 1)) == 0);

   code_span_t *span = code_span_new(code, name, free->base, free->size);

   free->base += span->size;
   free->size -= span->size;

   code_blob_t *blob = xcalloc(sizeof(code_blob_t));
   blob->span = span;
   blob->wptr = span->base;

   thread_wx_mode(WX_WRITE);

   return blob;
}

void code_blob_finalise(code_blob_t *blob, jit_entry_fn_t *entry)
{
   code_span_t *span = blob->span;
   span->size = blob->wptr - span->base;

   code_span_t *freespan = relaxed_load(&(span->owner->freelist[thread_id()]));
   assert(freespan->size == 0);

   ihash_free(blob->labels);
   blob->labels = NULL;

   if (unlikely(blob->patches != NULL))
      fatal_trace("not all labels in %s were patched", istr(span->name));
   else if (unlikely(blob->overflow)) {
      // Return all the memory
      freespan->size = freespan->base - span->base;
      freespan->base = span->base;
      free(blob);
      return;
   }
   else if (span->size == 0)
      fatal_trace("code span %s is empty", istr(span->name));

   uint8_t *aligned = ALIGN_UP(blob->wptr, CODE_BLOB_ALIGN);
   freespan->size = freespan->base - aligned;
   freespan->base = aligned;

   if (opt_get_verbose(OPT_ASM_VERBOSE, istr(span->name))) {
      color_printf("\n$bold$$blue$");
      code_disassemble(span, 0, NULL);
      color_printf("$$\n");
   }

   __builtin___clear_cache((char *)span->base, (char *)blob->wptr);

   thread_wx_mode(WX_EXECUTE);

   store_release(entry, (jit_entry_fn_t)span->base);

   DEBUG_ONLY(relaxed_add(&span->owner->used, span->size));
   free(blob);

   if (opt_get_int(OPT_PERF_MAP))
      code_write_perf_map(span);
}

void code_blob_emit(code_blob_t *blob, const uint8_t *bytes, size_t len)
{
   if (unlikely(blob->overflow))
      return;
   else if (unlikely(blob->wptr + len >= blob->span->base + blob->span->size)) {
      warnf("JIT code buffer for %s too small", istr(blob->span->name));
      for (patch_list_t *it = blob->patches, *tmp; it; it = tmp) {
         tmp = it->next;
         free(it);
      }
      blob->patches = NULL;
      blob->overflow = true;
      return;
   }

   for (size_t i = 0; i < len; i++)
      *(blob->wptr++) = bytes[i];
}

void code_blob_align(code_blob_t *blob, unsigned align)
{
#ifdef ARCH_X86_64
   const uint8_t pad[] = { 0x90 };
#else
   const uint8_t pad[] = { 0x00 };
#endif

   assert(is_power_of_2(align));
   assert(align % ARRAY_LEN(pad) == 0);

   while (((uintptr_t)blob->wptr & (align - 1)) && !blob->overflow)
      code_blob_emit(blob, pad, ARRAY_LEN(pad));
}

void code_blob_mark(code_blob_t *blob, jit_label_t label)
{
   if (unlikely(blob->overflow))
      return;
   else if (blob->labels == NULL)
      blob->labels = ihash_new(256);

   ihash_put(blob->labels, label, blob->wptr);

   for (patch_list_t **p = &(blob->patches); *p; ) {
      if ((*p)->label == label) {
         patch_list_t *next = (*p)->next;
         (*(*p)->fn)(blob, label, (*p)->wptr, blob->wptr);
         free(*p);
         *p = next;
      }
      else
         p = &((*p)->next);
   }
}

void code_blob_patch(code_blob_t *blob, jit_label_t label, code_patch_fn_t fn)
{
   void *ptr = NULL;
   if (unlikely(blob->overflow))
      return;
   else if (blob->labels != NULL && (ptr = ihash_get(blob->labels, label)))
      (*fn)(blob, label, blob->wptr, ptr);
   else {
      patch_list_t *new = xmalloc(sizeof(patch_list_t));
      new->next  = blob->patches;
      new->fn    = fn;
      new->label = label;
      new->wptr  = blob->wptr;

      blob->patches = new;
   }
}

#ifdef ARCH_ARM64
static void *arm64_emit_trampoline(code_blob_t *blob, uintptr_t dest)
{
   const uint8_t veneer[] = {
      0x50, 0x00, 0x00, 0x58,   // LDR X16, [PC+8]
      0x00, 0x02, 0x1f, 0xd6,   // BR X16
      __IMM64(dest)
   };

   void *prev = memmem(blob->span->base, blob->span->size,
                       veneer, ARRAY_LEN(veneer));
   if (prev != NULL)
      return prev;
   else {
      void *addr = blob->wptr;
      code_blob_emit(blob, veneer, ARRAY_LEN(veneer));
      return addr;
   }
}
#else
#define arm64_emit_trampoline(blob, dest) NULL
#endif

#if defined __MINGW32__
static void code_load_pe(code_blob_t *blob, const void *data, size_t size)
{
   const IMAGE_FILE_HEADER *imghdr = data;

   if (imghdr->Machine != IMAGE_FILE_MACHINE_AMD64)
      fatal_trace("unknown target machine %x", imghdr->Machine);

   const IMAGE_SYMBOL *symtab = data + imghdr->PointerToSymbolTable;
   const char *strtab = data + imghdr->PointerToSymbolTable
      + imghdr->NumberOfSymbols * sizeof(IMAGE_SYMBOL);

   const IMAGE_SECTION_HEADER *sections =
      data + IMAGE_SIZEOF_FILE_HEADER + imghdr->SizeOfOptionalHeader;

   void **load_addr LOCAL =
      xmalloc_array(imghdr->NumberOfSections, sizeof(void *));

   for (int i = 0; i < imghdr->NumberOfSections; i++) {
      if ((sections[i].Characteristics & IMAGE_SCN_CNT_CODE)
          || (sections[i].Characteristics & IMAGE_SCN_CNT_INITIALIZED_DATA)) {
         const int align = sections[i].Characteristics & IMAGE_SCN_ALIGN_MASK;
         code_blob_align(blob, 1 << ((align >> 20) - 1));
         load_addr[i] = blob->wptr;
         code_blob_emit(blob, data + sections[i].PointerToRawData,
                        sections[i].SizeOfRawData);
      }
      else if ((sections[i].Characteristics & IMAGE_SCN_CNT_UNINITIALIZED_DATA)
               && sections[i].Misc.VirtualSize > 0)
         fatal_trace("non-empty BSS not supported");
   }

   if (blob->overflow)
      return;   // Relocations might point outside of code span

   for (int i = 0; i < imghdr->NumberOfSections; i++) {
      const IMAGE_RELOCATION *relocs = data + sections[i].PointerToRelocations;
      for (int j = 0; j < sections[i].NumberOfRelocations; j++) {
         const char *name = NULL;
         char tmp[9];

         assert(relocs[j].SymbolTableIndex < imghdr->NumberOfSymbols);
         const IMAGE_SYMBOL *sym = symtab + relocs[j].SymbolTableIndex;

         if (sym->N.Name.Short) {
            memcpy(tmp, sym->N.ShortName, 8);
            tmp[8] = '\0';
            name = tmp;
         }
         else
            name = strtab + sym->N.Name.Long;

         void *ptr = NULL;
         if (sym->SectionNumber > 0) {
            assert(sym->SectionNumber - 1 < imghdr->NumberOfSections);
            ptr = load_addr[sym->SectionNumber - 1];
         }
         // TODO: allow ffi_find_symbol to find libc symbols
         else if (strcmp(name, "memmove") == 0)
            ptr = &memmove;
         else if (strcmp(name, "memset") == 0)
            ptr = &memset;
         else if (strcmp(name, "round") == 0)
            ptr = &round;
         else if (strcmp(name, "pow") == 0)
            ptr = &pow;
         else
            ptr = ffi_find_symbol(NULL, name);

         if (ptr == NULL)
            fatal_trace("failed to resolve symbol %s", name);

         void *patch = load_addr[i] + relocs[j].VirtualAddress;
         assert((uint8_t *)patch >= blob->span->base);
         assert((uint8_t *)patch < blob->span->base + blob->span->size);

         switch (relocs[j].Type) {
         case IMAGE_REL_AMD64_ADDR64:
            *(uint64_t *)patch += (uint64_t)ptr;
            break;
         case IMAGE_REL_AMD64_ADDR32NB:
            *(uint32_t *)patch += (uint32_t)(ptr - (void *)blob->span->base);
            break;
         default:
            blob->span->size = blob->wptr - blob->span->base;
            code_disassemble(blob->span, (uintptr_t)patch, NULL);
            fatal_trace("cannot handle relocation type %d for symbol %s",
                        relocs[j].Type, name);
         }
      }

      if (strncmp((const char *)sections[i].Name, ".pdata",
                  IMAGE_SIZEOF_SHORT_NAME) == 0) {
         assert(sections[i].SizeOfRawData % sizeof(RUNTIME_FUNCTION) == 0);
         const int count = sections[i].SizeOfRawData / sizeof(RUNTIME_FUNCTION);
         const DWORD64 base = (DWORD64)blob->span->base;

         // TODO: we should also call RtlDeleteFunctionTable at some point
         if (!RtlAddFunctionTable(load_addr[i], count, base))
            fatal_trace("RtlAddFunctionTable failed: %s", last_os_error());
      }
   }
}
#elif defined __APPLE__
static void code_load_macho(code_blob_t *blob, const void *data, size_t size)
{
   const void *rptr = data;

   const struct mach_header_64 *fhdr = rptr;
   rptr += sizeof(struct mach_header_64);

   if (fhdr->magic != MH_MAGIC_64)
      fatal_trace("bad Mach-O magic %x", fhdr->magic);

   const struct segment_command_64 *seg = NULL;
   const struct symtab_command *symtab = NULL;

   void **load_addr LOCAL = NULL;

   for (int i = 0; i < fhdr->ncmds; i++) {
      const struct load_command *load = rptr;
      switch (load->cmd) {
      case LC_SEGMENT_64:
         {
            seg = rptr;
            load_addr = xmalloc_array(seg->nsects, sizeof(void *));

            for (int j = 0; j < seg->nsects; j++) {
               const struct section_64 *sec =
                  (void *)seg + sizeof(struct segment_command_64)
                  + j * sizeof(struct section_64);
               code_blob_align(blob, 1 << sec->align);
               load_addr[j] = blob->wptr;
               code_blob_emit(blob, data + sec->offset, sec->size);
            }
         }
         break;
      case LC_SYMTAB:
         symtab = rptr;
         assert(symtab->cmdsize == sizeof(struct symtab_command));
         break;
      case LC_LINKER_OPTIMIZATION_HINT:
      case LC_BUILD_VERSION:
      case LC_DYSYMTAB:
         break;
      default:
         warnf("unrecognised load command 0x%0x", seg->cmd);
      }

      rptr += load->cmdsize;
   }
   assert(rptr == data + sizeof(struct mach_header_64) + fhdr->sizeofcmds);

   if (blob->overflow)
      return;   // Relocations might point outside of code span

   assert(seg != NULL);
   assert(symtab != NULL);

   for (int i = 0; i < seg->nsects; i++) {
      const struct section_64 *sec =
         (void *)seg + sizeof(struct segment_command_64)
         + i * sizeof(struct section_64);

      uint32_t addend = 0;
      for (int j = 0; j < sec->nreloc; j++) {
         const struct relocation_info *rel =
            data + sec->reloff + j * sizeof(struct relocation_info);
         const char *name = NULL;
         void *ptr = NULL;
         if (rel->r_extern) {
            assert(rel->r_symbolnum < symtab->nsyms);
            const struct nlist_64 *nl = data + symtab->symoff
               + rel->r_symbolnum * sizeof(struct nlist_64);
            name = data + symtab->stroff + nl->n_un.n_strx;

            if (nl->n_type & N_EXT) {
               if (icmp(blob->span->name, name + 1))
                  ptr = blob->span->base;
               else if ((ptr = ffi_find_symbol(NULL, name + 1)) == NULL)
                  fatal_trace("failed to resolve symbol %s", name + 1);
            }
            else if (nl->n_sect != NO_SECT)
               ptr = blob->span->base + nl->n_value;
         }
         else
            ptr = blob->span->base;

         ptr += addend;
         addend = 0;

         void *patch = load_addr[i] + rel->r_address;
         assert((uint8_t *)patch >= blob->span->base);
         assert((uint8_t *)patch < blob->span->base + blob->span->size);

         switch (rel->r_type) {
         case ARM64_RELOC_UNSIGNED:
            assert(rel->r_length == 3);
            *(void **)patch = ptr;
            break;
         case ARM64_RELOC_SUBTRACTOR:
            break;   // What is this?
         case ARM64_RELOC_GOT_LOAD_PAGEOFF12:
         case ARM64_RELOC_PAGEOFF12:
            switch ((*(uint32_t *)patch >> 23) & 0x7f) {
            case 0b1111010:   // LDR (immediate, SIMD&FP)
            case 0b1110010:   // LDR (immediate)
               assert(*(uint32_t *)patch & (1 << 30));  // Quadword
               assert(((uintptr_t)ptr & 7) == 0);
               *(uint32_t *)patch |= (((uintptr_t)ptr & 0xfff) >> 3) << 10;
               break;
            case 0b0100010:   // ADD (immediate)
               *(uint32_t *)patch |= ((uintptr_t)ptr & 0xfff) << 10;
               break;
            default:
               blob->span->size = blob->wptr - blob->span->base;
               code_disassemble(blob->span, (uintptr_t)patch, NULL);
               fatal_trace("cannot patch instruction");
            }
            break;
         case ARM64_RELOC_GOT_LOAD_PAGE21:
         case ARM64_RELOC_PAGE21:
            {
               const intptr_t dst_page = (intptr_t)ptr & ~UINT64_C(0xfff);
               const intptr_t src_page = (intptr_t)patch & ~UINT64_C(0xfff);
               const intptr_t upper21 = (dst_page - src_page) >> 12;
               *(uint32_t *)patch |= (upper21 & 3) << 29;
               *(uint32_t *)patch |= ((upper21 >> 2) & 0x7ffff) << 5;
            }
            break;
         case ARM64_RELOC_BRANCH26:
            {
               void *veneer = arm64_emit_trampoline(blob, (uintptr_t)ptr);
               const ptrdiff_t pcrel = (veneer - patch) >> 2;
               *(uint32_t *)patch &= ~0x3ffffff;
               *(uint32_t *)patch |= pcrel & 0x3ffffff;
            }
            break;
         case ARM64_RELOC_ADDEND:
            addend = rel->r_symbolnum;
            break;
         default:
            blob->span->size = blob->wptr - blob->span->base;
            code_disassemble(blob->span, (uintptr_t)patch, NULL);
            fatal_trace("cannot handle relocation type %d for symbol %s",
                        rel->r_type, name);
         }
      }
   }
}
#elif !defined __MINGW32__
static void code_load_elf(code_blob_t *blob, const void *data, size_t size)
{
   const Elf64_Ehdr *ehdr = data;

   if (ehdr->e_ident[EI_MAG0] != ELFMAG0
       || ehdr->e_ident[EI_MAG1] != ELFMAG1
       || ehdr->e_ident[EI_MAG2] != ELFMAG2
       || ehdr->e_ident[EI_MAG3] != ELFMAG3)
      fatal_trace("bad ELF magic");
   else if (ehdr->e_shentsize != sizeof(Elf64_Shdr))
      fatal_trace("bad section header size %d != %zu", ehdr->e_shentsize,
                  sizeof(Elf64_Shdr));

   const Elf64_Shdr *strtab_hdr =
      data + ehdr->e_shoff + ehdr->e_shstrndx * ehdr->e_shentsize;
   const char *strtab = data + strtab_hdr->sh_offset;

   void **load_addr LOCAL = xcalloc_array(ehdr->e_shnum, sizeof(void *));

   for (int i = 0; i < ehdr->e_shnum; i++) {
      const Elf64_Shdr *shdr = data + ehdr->e_shoff + i * ehdr->e_shentsize;

      switch (shdr->sh_type) {
      case SHT_PROGBITS:
         if (shdr->sh_flags & SHF_ALLOC) {
            code_blob_align(blob, shdr->sh_addralign);
            load_addr[i] = blob->wptr;
            code_blob_emit(blob, data + shdr->sh_offset, shdr->sh_size);
         }
         break;

      case SHT_RELA:
         // Handled in second pass
         break;

      case SHT_NULL:
      case SHT_STRTAB:
      case SHT_X86_64_UNWIND:
      case SHT_SYMTAB:
         break;

      default:
         warnf("ignoring ELF section %s with type %x", strtab + shdr->sh_name,
               shdr->sh_type);
      }
   }

   if (blob->overflow)
      return;   // Relocations might point outside of code span

   for (int i = 0; i < ehdr->e_shnum; i++) {
      const Elf64_Shdr *shdr = data + ehdr->e_shoff + i * ehdr->e_shentsize;
      if (shdr->sh_type != SHT_RELA)
         continue;

      const Elf64_Shdr *mod =
         data + ehdr->e_shoff + shdr->sh_info * ehdr->e_shentsize;
      if (mod->sh_type != SHT_PROGBITS || !(mod->sh_flags & SHF_ALLOC))
         continue;
      else if (load_addr[shdr->sh_info] == NULL)
         fatal_trace("section %s not loaded", strtab + mod->sh_name);

      const Elf64_Shdr *symtab =
         data + ehdr->e_shoff + shdr->sh_link * ehdr->e_shentsize;
      if (symtab->sh_type != SHT_SYMTAB)
         fatal_trace("section %s is not a symbol table",
                     strtab + symtab->sh_name);

      const Elf64_Rela *endp = data + shdr->sh_offset + shdr->sh_size;
      for (const Elf64_Rela *r = data + shdr->sh_offset; r < endp; r++) {
         const Elf64_Sym *sym = data + symtab->sh_offset
            + ELF64_R_SYM(r->r_info) * symtab->sh_entsize;

         char *ptr = NULL;
         switch (ELF64_ST_TYPE(sym->st_info)) {
         case STT_NOTYPE:
         case STT_FUNC:
            ptr = ffi_find_symbol(NULL, strtab + sym->st_name);
            break;
         case STT_SECTION:
            ptr = load_addr[sym->st_shndx];
            break;
         }

         if (ptr == NULL)
            fatal_trace("cannot resolve symbol %s type %d",
                        strtab + sym->st_name, ELF64_ST_TYPE(sym->st_info));

         ptr += r->r_addend;

         void *patch = load_addr[shdr->sh_info] + r->r_offset;
         assert(r->r_offset < mod->sh_size);

         switch (ELF64_R_TYPE(r->r_info)) {
         case R_X86_64_64:
            *(uint64_t *)patch = (uint64_t)ptr;
            break;
         case R_AARCH64_CALL26:
            {
               void *veneer = arm64_emit_trampoline(blob, (uintptr_t)ptr);
               const ptrdiff_t pcrel = (veneer - patch) >> 2;
               *(uint32_t *)patch &= ~0x3ffffff;
               *(uint32_t *)patch |= pcrel & 0x3ffffff;
            }
            break;
         case R_AARCH64_PREL64:
            *(uint64_t *)patch = ptr - (char *)patch;
            break;
         case R_AARCH64_MOVW_UABS_G0_NC:
            *(uint32_t *)patch |= ((uintptr_t)ptr & 0xffff) << 5;
            break;
         case R_AARCH64_MOVW_UABS_G1_NC:
            *(uint32_t *)patch |= (((uintptr_t)ptr >> 16) & 0xffff) << 5;
            break;
         case R_AARCH64_MOVW_UABS_G2_NC:
            *(uint32_t *)patch |= (((uintptr_t)ptr >> 32) & 0xffff) << 5;
            break;
         case R_AARCH64_MOVW_UABS_G3:
            *(uint32_t *)patch |= (((uintptr_t)ptr >> 48) & 0xffff) << 5;
            break;
         default:
            blob->span->size = blob->wptr - blob->span->base;
            code_disassemble(blob->span, (uintptr_t)patch, NULL);
            fatal_trace("cannot handle relocation type %ld for symbol %s",
                        ELF64_R_TYPE(r->r_info), strtab + sym->st_name);
         }
      }
   }
}
#endif

void code_load_object(code_blob_t *blob, const void *data, size_t size)
{
#if defined __APPLE__
   code_load_macho(blob, data, size);
#elif defined __MINGW32__
   code_load_pe(blob, data, size);
#else
   code_load_elf(blob, data, size);
#endif
}
