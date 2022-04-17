/* --------------------------------------------------------------------
 *
 * Copyright 2019 IEEE P1076 WG Authors
 *
 * See the LICENSE file distributed with this work for copyright and
 * licensing information and the AUTHORS file.
 *
 * This file to you under the Apache License, Version 2.0 (the "License").
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
 * implied.  See the License for the specific language governing
 * permissions and limitations under the License.
 *
 *
 *   Title     :  vhpi_sens.c
 *             :
 *   Developers:  IEEE P1076 Working Group, VHPI Task Force
 *             :
 *   Purpose   :  This file defines function for manipulating
 *             :  sensitivity-set bitmaps. The definitions of the macros
 *             :  in the header file invoke functions defined in this file.
 *             :
 *   Note      :  A tool provider may replace the definitions with
 *             :  implementation-specific definitions that have the effect
 *             :  described in this clause. Such definitions may invoke
 *             :  implementation-defined functions or may be in the form
 *             :  of in-line code.
 *             :
 * --------------------------------------------------------------------
 * modification history :
 * --------------------------------------------------------------------
 * $Revision: 1219 $
 * $Date: 2008-04-10 16:40:28 +0930 (Thu, 10 Apr 2008) $
 * --------------------------------------------------------------------
 */

#include <limits.h>
#include "vhpi_user.h"

#define BITS_vhpiIntT (CHAR_BIT * sizeof(vhpiIntT))
int vhpi_sens_first(vhpiValueT *sens) {
	int i;
	vhpiIntT t;

	if (sens->format != vhpiIntVecVal)
		return -1;
	for (i = 0; i < sens->numElems; i++) {
		if ((t = sens->value.intgs[i])) {
			int f;

			for (f = 0; f < BITS_vhpiIntT; f++) {
				if (t & (1 << f))
					return f + (i * BITS_vhpiIntT);
			}
		}
	}
	return -1;
}

int vhpi_sens_zero(vhpiValueT *sens) {
	int i;

	if (sens->format != vhpiIntVecVal)
		return -1;
	for (i = 0; i < sens->numElems; i++) {
		sens->value.intgs[i] = 0;
	}
	return 0;
}

int vhpi_sens_clr(int obj, vhpiValueT *sens) {
	int i, r;

	if (sens->format != vhpiIntVecVal)
		return -1;

	if (obj >= (sens->numElems * BITS_vhpiIntT))
		return -1;

	i = obj / BITS_vhpiIntT;
	r = obj % BITS_vhpiIntT;

	sens->value.intgs[i] &= ~(1 << r);

	return 0;
}

int vhpi_sens_set(int obj, vhpiValueT *sens) {
	int i, r;

	if (sens->format != vhpiIntVecVal)
		return -1;

	if (obj >= (sens->numElems * BITS_vhpiIntT))
		return -1;

	i = obj / BITS_vhpiIntT;
	r = obj % BITS_vhpiIntT;

	sens->value.intgs[i] |= (1 << r);

	return 0;
}

int vhpi_sens_isset(int obj, vhpiValueT *sens) {
	int i, r;

	if (sens->format != vhpiIntVecVal)
		return -1;

	if (obj >= (sens->numElems * BITS_vhpiIntT))
		return -1;

	i = obj / BITS_vhpiIntT;
	r = obj % BITS_vhpiIntT;

	return (sens->value.intgs[i] & (1 << r)) ? 1 : 0;
}


#ifdef REGRESS_TEST
main() {
	int i;
	vhpiValueT	sens;
	vhpiValueT	*s = &sens;

	s->numElems = 4;
	s->format = vhpiIntVecVal;
	s->value.intgs = (vhpiIntT *) malloc(4 * sizeof(vhpiIntT));
#define TRACE(action, arg, result) { int r = (result); printf("%s\t%d\t%d\t%08x  %08x  %08x  %08x\n", action, arg, r, s->value.intgs[0], s->value.intgs[1], s->value.intgs[2], s->value.intgs[3]); }

	TRACE("START", 0, 0);
	TRACE("ZERO", 0, vhpi_sens_zero(s));
	for (i = 0; i < ((4 * BITS_vhpiIntT) + 1); i++) {
		TRACE("ISSET", i, vhpi_sens_isset(i, s));
		TRACE("SET", i, vhpi_sens_set(i, s));
		TRACE("ISSET", i, vhpi_sens_isset(i, s));
		TRACE("FIRST", 0, vhpi_sens_first(s));
		TRACE("ISSET", (i-1), vhpi_sens_isset((i-1), s));
		TRACE("CLR", (i - 1), vhpi_sens_clr((i-1), s));
		TRACE("ISSET", (i-1), vhpi_sens_isset((i-1), s));
	}
}
#endif
