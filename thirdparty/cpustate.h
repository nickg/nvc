/*
 * Copyright (c) 2022 Nick Gasson
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */

#ifndef _CPUSTATE_H
#define _CPUSTATE_H

#include <stdint.h>

#if defined __MINGW32__
#include <windef.h>
#elif defined __OpenBSD__
#include <signal.h>
#else
#include <sys/ucontext.h>
#endif

#define MAX_CPU_REGS 32

struct cpu_state {
   uintptr_t pc;
   uintptr_t sp;
   uintptr_t regs[MAX_CPU_REGS];
};

#if defined __MINGW32__
void fill_cpu_state(struct cpu_state *cpu, PCONTEXT context);
#else
void fill_cpu_state(struct cpu_state *cpu, ucontext_t *uc);
#endif

#endif
