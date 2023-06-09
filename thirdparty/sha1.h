/*
SHA-1 in C
By Steve Reid <steve@edmweb.com>
100% Public Domain
*/

#ifndef _THIRDPARTY_SHA1_H
#define _THIRDPARTY_SHA1_H

#include <stdint.h>
#include <stddef.h>

typedef struct {
    uint32_t state[5];
    uint32_t count[2];
    unsigned char buffer[64];
} SHA1_CTX;

#define SHA1_LEN 20

void SHA1Init(SHA1_CTX *);
void SHA1Update(SHA1_CTX *, const unsigned char *, size_t);
void SHA1Final(unsigned char [SHA1_LEN], SHA1_CTX *);

#endif  // _THIRDPARTY_SHA1_H
