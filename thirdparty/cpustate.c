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

#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif

#include "cpustate.h"

#include <string.h>

#if defined __MINGW32__
#include <windows.h>
#endif

#if GCC_VERSION > 40600
#define STATIC_ASSERT(x) _Static_assert((x), "Static assertion failed");
#else
#define STATIC_ASSERT(x)
#endif

#if defined __unix__ || defined __APPLE__

void fill_cpu_state(struct cpu_state *cpu, ucontext_t *uc)
{
   memset(cpu, '\0', sizeof(struct cpu_state));

#if defined __linux__ && defined __x86_64__
   cpu->pc = uc->uc_mcontext.gregs[REG_RIP];
   cpu->sp = uc->uc_mcontext.gregs[REG_RSP];

#if !defined __NGREG && defined NGREG
#define __NGREG NGREG    // Glibc prior to 2.36
#endif

   STATIC_ASSERT(__NGREG <= MAX_CPU_REGS);
   for (int i = 0; i < __NGREG; i++)
      cpu->regs[i] = uc->uc_mcontext.gregs[i];
#elif defined __linux__ && defined __aarch64__
   cpu->pc = uc->uc_mcontext.pc;
   cpu->sp = uc->uc_mcontext.sp;

   for (int i = 0; i < 31; i++)
      cpu->regs[i] = uc->uc_mcontext.regs[i];
#elif defined __linux__ && defined __arm__
   cpu->pc = uc->uc_mcontext.arm_pc;
   cpu->sp = uc->uc_mcontext.arm_sp;

   cpu->regs[0]  = uc->uc_mcontext.arm_r0;
   cpu->regs[1]  = uc->uc_mcontext.arm_r1;
   cpu->regs[2]  = uc->uc_mcontext.arm_r2;
   cpu->regs[3]  = uc->uc_mcontext.arm_r3;
   cpu->regs[4]  = uc->uc_mcontext.arm_r4;
   cpu->regs[5]  = uc->uc_mcontext.arm_r5;
   cpu->regs[6]  = uc->uc_mcontext.arm_r6;
   cpu->regs[7]  = uc->uc_mcontext.arm_r7;
   cpu->regs[8]  = uc->uc_mcontext.arm_r8;
   cpu->regs[9]  = uc->uc_mcontext.arm_r9;
   cpu->regs[10] = uc->uc_mcontext.arm_r10;
   cpu->regs[11] = uc->uc_mcontext.arm_fp;
   cpu->regs[12] = uc->uc_mcontext.arm_ip;
   cpu->regs[13] = uc->uc_mcontext.arm_lr;
#elif defined __FreeBSD__ && defined __x86_64__
   cpu->pc = uc->uc_mcontext.mc_rip;
   cpu->sp = uc->uc_mcontext.mc_rsp;

   cpu->regs[0]  = uc->uc_mcontext.mc_rax;
   cpu->regs[1]  = uc->uc_mcontext.mc_rcx;
   cpu->regs[2]  = uc->uc_mcontext.mc_rdx;
   cpu->regs[3]  = uc->uc_mcontext.mc_rbx;
   cpu->regs[4]  = uc->uc_mcontext.mc_rsp;
   cpu->regs[5]  = uc->uc_mcontext.mc_rbp;
   cpu->regs[6]  = uc->uc_mcontext.mc_rsi;
   cpu->regs[7]  = uc->uc_mcontext.mc_rdi;
   cpu->regs[8]  = uc->uc_mcontext.mc_r8;
   cpu->regs[9]  = uc->uc_mcontext.mc_r9;
   cpu->regs[10] = uc->uc_mcontext.mc_r10;
   cpu->regs[11] = uc->uc_mcontext.mc_r11;
   cpu->regs[12] = uc->uc_mcontext.mc_r12;
   cpu->regs[13] = uc->uc_mcontext.mc_r13;
   cpu->regs[14] = uc->uc_mcontext.mc_r14;
   cpu->regs[15] = uc->uc_mcontext.mc_r15;
#elif defined __FreeBSD__ && defined __i386__
   cpu->pc = uc->uc_mcontext.mc_eip;
   cpu->sp = uc->uc_mcontext.mc_esp;

   cpu->regs[0]  = uc->uc_mcontext.mc_eax;
   cpu->regs[1]  = uc->uc_mcontext.mc_ecx;
   cpu->regs[2]  = uc->uc_mcontext.mc_edx;
   cpu->regs[3]  = uc->uc_mcontext.mc_ebx;
   cpu->regs[4]  = uc->uc_mcontext.mc_esp;
   cpu->regs[5]  = uc->uc_mcontext.mc_ebp;
   cpu->regs[6]  = uc->uc_mcontext.mc_esi;
   cpu->regs[7]  = uc->uc_mcontext.mc_edi;
#elif defined __FreeBSD__ && defined __aarch64__
   cpu->pc = uc->uc_mcontext.mc_gpregs.gp_elr;
   cpu->sp = uc->uc_mcontext.mc_gpregs.gp_sp;

   for (int i = 0; i < 30; i++)
      cpu->regs[i] = uc->uc_mcontext.mc_gpregs.gp_x[i];

   cpu->regs[30] = uc->uc_mcontext.mc_gpregs.gp_lr;
#elif defined __FreeBSD__ && defined __arm__
   cpu->pc = uc->uc_mcontext.__gregs[_REG_PC];
   cpu->sp = uc->uc_mcontext.__gregs[_REG_SP];

   for (int i = 0; i < 16; i++)
      cpu->regs[i] = uc->uc_mcontext.__gregs[i];
#elif defined __FreeBSD__ && defined __powerpc__
   cpu->pc = uc->uc_mcontext.mc_srr0;
   cpu->sp = uc->uc_mcontext.mc_gpr[1];

   for (int i = 0; i < 32; i++)
      cpu->regs[i] = uc->uc_mcontext.mc_gpr[i];
#elif defined __OpenBSD__ && defined __x86_64__
   cpu->pc = uc->sc_rip;
   cpu->sp = uc->sc_rsp;

   cpu->regs[0]  = uc->sc_rax;
   cpu->regs[1]  = uc->sc_rcx;
   cpu->regs[2]  = uc->sc_rdx;
   cpu->regs[3]  = uc->sc_rbx;
   cpu->regs[4]  = uc->sc_rsp;
   cpu->regs[5]  = uc->sc_rbp;
   cpu->regs[6]  = uc->sc_rsi;
   cpu->regs[7]  = uc->sc_rdi;
   cpu->regs[8]  = uc->sc_r8;
   cpu->regs[9]  = uc->sc_r9;
   cpu->regs[10] = uc->sc_r10;
   cpu->regs[11] = uc->sc_r11;
   cpu->regs[12] = uc->sc_r12;
   cpu->regs[13] = uc->sc_r13;
   cpu->regs[14] = uc->sc_r14;
   cpu->regs[15] = uc->sc_r15;
#elif defined __APPLE__ && defined __arm64__
   cpu->pc = uc->uc_mcontext->__ss.__pc;
   cpu->sp = uc->uc_mcontext->__ss.__sp;

   for (int i = 0; i < 29; i++)
      cpu->regs[i] = uc->uc_mcontext->__ss.__x[i];
#elif defined __APPLE__ && defined __x86_64__
   cpu->pc = uc->uc_mcontext->__ss.__rip;
   cpu->sp = uc->uc_mcontext->__ss.__rsp;

   cpu->regs[0]  = uc->uc_mcontext->__ss.__rax;
   cpu->regs[1]  = uc->uc_mcontext->__ss.__rcx;
   cpu->regs[2]  = uc->uc_mcontext->__ss.__rdx;
   cpu->regs[3]  = uc->uc_mcontext->__ss.__rbx;
   cpu->regs[4]  = uc->uc_mcontext->__ss.__rsp;
   cpu->regs[5]  = uc->uc_mcontext->__ss.__rbp;
   cpu->regs[6]  = uc->uc_mcontext->__ss.__rsi;
   cpu->regs[7]  = uc->uc_mcontext->__ss.__rdi;
   cpu->regs[8]  = uc->uc_mcontext->__ss.__r8;
   cpu->regs[9]  = uc->uc_mcontext->__ss.__r9;
   cpu->regs[10] = uc->uc_mcontext->__ss.__r10;
   cpu->regs[11] = uc->uc_mcontext->__ss.__r11;
   cpu->regs[12] = uc->uc_mcontext->__ss.__r12;
   cpu->regs[13] = uc->uc_mcontext->__ss.__r13;
   cpu->regs[14] = uc->uc_mcontext->__ss.__r14;
   cpu->regs[15] = uc->uc_mcontext->__ss.__r15;
#else
#error Please port fill_cpu_state to this OS/CPU combination
#endif
}

#elif defined __MINGW32__

void fill_cpu_state(struct cpu_state *cpu, PCONTEXT context)
{
   memset(cpu, '\0', sizeof(struct cpu_state));

   cpu->pc = context->Rip;
   cpu->sp = context->Rsp;

   cpu->regs[0]  = context->Rax;
   cpu->regs[1]  = context->Rcx;
   cpu->regs[2]  = context->Rdx;
   cpu->regs[3]  = context->Rbx;
   cpu->regs[4]  = context->Rsp;
   cpu->regs[5]  = context->Rbp;
   cpu->regs[6]  = context->Rsi;
   cpu->regs[7]  = context->Rdi;
   cpu->regs[8]  = context->R8;
   cpu->regs[9]  = context->R9;
   cpu->regs[10] = context->R10;
   cpu->regs[11] = context->R11;
   cpu->regs[12] = context->R12;
   cpu->regs[13] = context->R13;
   cpu->regs[14] = context->R14;
   cpu->regs[15] = context->R15;

   for (int i = 16; i < MAX_CPU_REGS; i++)
      cpu->regs[i] = 0;
}

#else
#error Please port fill_cpu_state to this platform
#endif
