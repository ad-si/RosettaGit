+++
title = "Data Encryption Standard"
description = ""
date = 2019-09-08T09:31:06Z
aliases = []
[extra]
id = 21396
[taxonomies]
categories = []
tags = []
+++

{{draft task|Data_Encryption_Standard}}
Demonstrate the [[wp:Data_Encryption_Standard|Data Encryption Standard]]. For a complete description of the algorithm see: [http://page.math.tu-berlin.de/~kant/teaching/hess/krypto-ws2006/des.htm The DES Algorithm Illustrated]


'''Task:'''

Use the 
:Key 0e329232ea6d0d73 
:to encrypt 8787878787878787 
:and display the result 0000000000000000.

Bonus (optional): add standard padding to match the C#, Java, Modula-2, Kotlin, and Phix entries, so the above encrypted result would instead be 0000000000000000A913F4CB0BD30F97.



## C

{{trans|D}}

```c>#include <stdio.h

#include <stdlib.h>
#include <string.h>

typedef unsigned char ubyte;

#define KEY_LEN 8
typedef ubyte key_t[KEY_LEN];

const static ubyte PC1[] = {
    57, 49, 41, 33, 25, 17,  9,
     1, 58, 50, 42, 34, 26, 18,
    10,  2, 59, 51, 43, 35, 27,
    19, 11,  3, 60, 52, 44, 36,
    63, 55, 47, 39, 31, 23, 15,
     7, 62, 54, 46, 38, 30, 22,
    14,  6, 61, 53, 45, 37, 29,
    21, 13,  5, 28, 20, 12,  4
};

const static ubyte PC2[] = {
    14, 17, 11, 24,  1,  5,
     3, 28, 15,  6, 21, 10,
    23, 19, 12,  4, 26,  8,
    16,  7, 27, 20, 13,  2,
    41, 52, 31, 37, 47, 55,
    30, 40, 51, 45, 33, 48,
    44, 49, 39, 56, 34, 53,
    46, 42, 50, 36, 29, 32
};

const static ubyte IP[] = {
    58, 50, 42, 34, 26, 18, 10,  2,
    60, 52, 44, 36, 28, 20, 12,  4,
    62, 54, 46, 38, 30, 22, 14,  6,
    64, 56, 48, 40, 32, 24, 16,  8,
    57, 49, 41, 33, 25, 17,  9,  1,
    59, 51, 43, 35, 27, 19, 11,  3,
    61, 53, 45, 37, 29, 21, 13,  5,
    63, 55, 47, 39, 31, 23, 15,  7
};

const static ubyte E[] = {
    32,  1,  2,  3,  4,  5,
     4,  5,  6,  7,  8,  9,
     8,  9, 10, 11, 12, 13,
    12, 13, 14, 15, 16, 17,
    16, 17, 18, 19, 20, 21,
    20, 21, 22, 23, 24, 25,
    24, 25, 26, 27, 28, 29,
    28, 29, 30, 31, 32,  1
};

const static ubyte S[][64] = {
    {
        14,  4, 13,  1,  2, 15, 11,  8,  3, 10,  6, 12,  5,  9,  0,  7,
         0, 15,  7,  4, 14,  2, 13,  1, 10,  6, 12, 11,  9,  5,  3,  8,
         4,  1, 14,  8, 13,  6,  2, 11, 15, 12,  9,  7,  3, 10,  5,  0,
        15, 12,  8,  2,  4,  9,  1,  7,  5, 11,  3, 14, 10,  0,  6, 13
    },
    {
        15,  1,  8, 14,  6, 11,  3,  4,  9,  7,  2, 13, 12,  0,  5, 10,
         3, 13,  4,  7, 15,  2,  8, 14, 12,  0,  1, 10,  6,  9, 11,  5,
         0, 14,  7, 11, 10,  4, 13,  1,  5,  8, 12,  6,  9,  3,  2, 15,
        13,  8, 10,  1,  3, 15,  4,  2, 11,  6,  7, 12,  0,  5, 14,  9
    },
    {
        10,  0,  9, 14,  6,  3, 15,  5,  1, 13, 12,  7, 11,  4,  2,  8,
        13,  7,  0,  9,  3,  4,  6, 10,  2,  8,  5, 14, 12, 11, 15,  1,
        13,  6,  4,  9,  8, 15,  3,  0, 11,  1,  2, 12,  5, 10, 14,  7,
         1, 10, 13,  0,  6,  9,  8,  7,  4, 15, 14,  3, 11,  5,  2, 12
    },
    {
         7, 13, 14,  3,  0,  6,  9, 10,  1,  2,  8,  5, 11, 12,  4, 15,
        13,  8, 11,  5,  6, 15,  0,  3,  4,  7,  2, 12,  1, 10, 14,  9,
        10,  6,  9,  0, 12, 11,  7, 13, 15,  1,  3, 14,  5,  2,  8,  4,
         3, 15,  0,  6, 10,  1, 13,  8,  9,  4,  5, 11, 12,  7,  2, 14
    },
    {
         2, 12,  4,  1,  7, 10, 11,  6,  8,  5,  3, 15, 13,  0, 14,  9,
        14, 11,  2, 12,  4,  7, 13,  1,  5,  0, 15, 10,  3,  9,  8,  6,
         4,  2,  1, 11, 10, 13,  7,  8, 15,  9, 12,  5,  6,  3,  0, 14,
        11,  8, 12,  7,  1, 14,  2, 13,  6, 15,  0,  9, 10,  4,  5,  3
    },
    {
        12,  1, 10, 15,  9,  2,  6,  8,  0, 13,  3,  4, 14,  7,  5, 11,
        10, 15,  4,  2,  7, 12,  9,  5,  6,  1, 13, 14,  0, 11,  3,  8,
         9, 14, 15,  5,  2,  8, 12,  3,  7,  0,  4, 10,  1, 13, 11,  6,
         4,  3,  2, 12,  9,  5, 15, 10, 11, 14,  1,  7,  6,  0,  8, 13
    },
    {
         4, 11,  2, 14, 15,  0,  8, 13,  3, 12,  9,  7,  5, 10,  6,  1,
        13,  0, 11,  7,  4,  9,  1, 10, 14,  3,  5, 12,  2, 15,  8,  6,
         1,  4, 11, 13, 12,  3,  7, 14, 10, 15,  6,  8,  0,  5,  9,  2,
         6, 11, 13,  8,  1,  4, 10,  7,  9,  5,  0, 15, 14,  2,  3, 12
    },
    {
        13,  2,  8,  4,  6, 15, 11,  1, 10,  9,  3, 14,  5,  0, 12,  7,
         1, 15, 13,  8, 10,  3,  7,  4, 12,  5,  6, 11,  0, 14,  9,  2,
         7, 11,  4,  1,  9, 12, 14,  2,  0,  6, 10, 13, 15,  3,  5,  8,
         2,  1, 14,  7,  4, 10,  8, 13, 15, 12,  9,  0,  3,  5,  6, 11
    }
};

const static ubyte P[] = {
    16,  7, 20, 21,
    29, 12, 28, 17,
     1, 15, 23, 26,
     5, 18, 31, 10,
     2,  8, 24, 14,
    32, 27,  3,  9,
    19, 13, 30,  6,
    22, 11,  4, 25
};

const static ubyte IP2[] = {
    40,  8, 48, 16, 56, 24, 64, 32,
    39,  7, 47, 15, 55, 23, 63, 31,
    38,  6, 46, 14, 54, 22, 62, 30,
    37,  5, 45, 13, 53, 21, 61, 29,
    36,  4, 44, 12, 52, 20, 60, 28,
    35,  3, 43, 11, 51, 19, 59, 27,
    34,  2, 42, 10, 50, 18, 58, 26,
    33,  1, 41,  9, 49, 17, 57, 25
};

const static ubyte SHIFTS[] = {
    1, 1, 2, 2, 2, 2, 2, 2, 1, 2, 2, 2, 2, 2, 2, 1
};

typedef struct {
    ubyte *data;
    int len;
} String;

/*
 * Transform a single nibble into a hex character
 *
 * in: a value < 0x10
 *
 * returns: the character that represents the nibble
 */
static char toHex(ubyte in) {
    if (0x00 <= in && in < 0x0A) {
        return '0' + in;
    }
    if (0x0A <= in && in <= 0x0F) {
        return 'A' + in - 0x0A;
    }
    return 0;
}

/*
 * Convert an array of bytes into a string
 *
 * ptr: the array of bytes
 * len: the number of bytes
 * out: a buffer allocated by the caller with enough space for 2*len+1 characters
 */
static void printBytes(const ubyte *ptr, int len, char *out) {
    while (len-- > 0) {
        *out++ = toHex(*ptr >> 4);
        *out++ = toHex(*ptr & 0x0F);

        ptr++;
    }
    *out = 0;
}

/*
 * Gets the value of a bit in an array of bytes
 *
 * src: the array of bytes to index
 * index: the desired bit to test the value of
 *
 * returns: the bit at the specified position in the array
 */
static int peekBit(const ubyte *src, int index) {
    int cell = index / 8;
    int bit = 7 - index % 8;
    return (src[cell] & (1 << bit)) != 0;
}

/*
 * Sets the value of a bit in an array of bytes
 *
 * dst: the array of bits to set a bit in
 * index: the position of the bit to set
 * value: the value for the bit to set
 */
static void pokeBit(ubyte *dst, int index, int value) {
    int cell = index / 8;
    int bit = 7 - index % 8;
    if (value == 0) {
        dst[cell] &= ~(1 << bit);
    } else {
        dst[cell] |= (1 << bit);
    }
}

/*
 * Transforms one array of bytes by shifting the bits the specified number of positions
 *
 * src: the array to shift bits from
 * len: the length of the src array
 * times: the number of positions that the bits should be shifted
 * dst: a bytes array allocated by the caller to store the shifted values
 */
static void shiftLeft(const ubyte *src, int len, int times, ubyte *dst) {
    int i, t;
    for (i = 0; i <= len; ++i) {
        pokeBit(dst, i, peekBit(src, i));
    }
    for (t = 1; t <= times; ++t) {
        int temp = peekBit(dst, 0);
        for (i = 1; i <= len; ++i) {
            pokeBit(dst, i - 1, peekBit(dst, i));
        }
        pokeBit(dst, len - 1, temp);
    }
}

/*
 * Calculates the sub keys to be used in processing the messages
 *
 * key: the array of bytes representing the key
 * ks: the subkeys that have been allocated by the caller
 */
typedef ubyte subkey_t[17][6]; /* 17 sets of 48 bits */
static void getSubKeys(const key_t key, subkey_t ks) {
    ubyte c[17][7];  /* 56 bits */
    ubyte d[17][4];  /* 28 bits */
    ubyte kp[7];
    int i, j;

    /* intialize */
    memset(c, 0, sizeof(c));
    memset(d, 0, sizeof(d));
    memset(ks, 0, sizeof(subkey_t));

    /* permute 'key' using table PC1 */
    for (i = 0; i < 56; ++i) {
        pokeBit(kp, i, peekBit(key, PC1[i] - 1));
    }

    /* split 'kp' in half and process the resulting series of 'c' and 'd' */
    for (i = 0; i < 28; ++i) {
        pokeBit(c[0], i, peekBit(kp, i));
        pokeBit(d[0], i, peekBit(kp, i + 28));
    }

    /* shift the components of c and d */
    for (i = 1; i < 17; ++i) {
        shiftLeft(c[i - 1], 28, SHIFTS[i - 1], c[i]);
        shiftLeft(d[i - 1], 28, SHIFTS[i - 1], d[i]);
    }

    /* merge 'd' into 'c' */
    for (i = 1; i < 17; ++i) {
        for (j = 28; j < 56; ++j) {
            pokeBit(c[i], j, peekBit(d[i], j - 28));
        }
    }

    /* form the sub-keys and store them in 'ks'
     * permute 'c' using table PC2 */
    for (i = 1; i < 17; ++i) {
        for (j = 0; j < 48; ++j) {
            pokeBit(ks[i], j, peekBit(c[i], PC2[j] - 1));
        }
    }
}

/*
 * Function used in processing the messages
 *
 * r: an array of bytes to be processed
 * ks: one of the subkeys to be used for processing
 * sp: output from the processing
 */
static void f(ubyte *r, ubyte *ks, ubyte *sp) {
    ubyte er[6]; /* 48 bits */
    ubyte sr[4]; /* 32 bits */
    int i;

    /* initialize */
    memset(er, 0, sizeof(er));
    memset(sr, 0, sizeof(sr));

    /* permute 'r' using table E */
    for (i = 0; i < 48; ++i) {
        pokeBit(er, i, peekBit(r, E[i] - 1));
    }

    /* xor 'er' with 'ks' and store back into 'er' */
    for (i = 0; i < 6; ++i) {
        er[i] ^= ks[i];
    }

    /* process 'er' six bits at a time and store resulting four bits in 'sr' */
    for (i = 0; i < 8; ++i) {
        int j = i * 6;
        int b[6];
        int k, row, col, m, n;

        for (k = 0; k < 6; ++k) {
            b[k] = peekBit(er, j + k) != 0 ? 1 : 0;
        }

        row = 2 * b[0] + b[5];
        col = 8 * b[1] + 4 * b[2] + 2 * b[3] + b[4];
        m = S[i][row * 16 + col]; /* apply table s */
        n = 1;

        while (m > 0) {
            int p = m % 2;
            pokeBit(sr, (i + 1) * 4 - n, p == 1);
            m /= 2;
            n++;
        }
    }

    /* permute sr using table P */
    for (i = 0; i < 32; ++i) {
        pokeBit(sp, i, peekBit(sr, P[i] - 1));
    }
}

/*
 * Processing of block of the message
 *
 * message: an 8 byte block from the message
 * ks: the subkeys to use in processing
 * ep: space for an encoded 8 byte block allocated by the caller
 */
static void processMessage(const ubyte *message, subkey_t ks, ubyte *ep) {
    ubyte left[17][4];  /* 32 bits */
    ubyte right[17][4]; /* 32 bits */
    ubyte mp[8];        /* 64 bits */
    ubyte e[8];         /* 64 bits */
    int i, j;

    /* permute 'message' using table IP */
    for (i = 0; i < 64; ++i) {
        pokeBit(mp, i, peekBit(message, IP[i] - 1));
    }

    /* split 'mp' in half and process the resulting series of 'l' and 'r */
    for (i = 0; i < 32; ++i) {
        pokeBit(left[0], i, peekBit(mp, i));
        pokeBit(right[0], i, peekBit(mp, i + 32));
    }
    for (i = 1; i < 17; ++i) {
        ubyte fs[4]; /* 32 bits */

        memcpy(left[i], right[i - 1], 4);
        f(right[i - 1], ks[i], fs);
        for (j = 0; j < 4; ++j) {
            left[i - 1][j] ^= fs[j];
        }
        memcpy(right[i], left[i - 1], 4);
    }

    /* amalgamate r[16] and l[16] (in that order) into 'e' */
    for (i = 0; i < 32; ++i) {
        pokeBit(e, i, peekBit(right[16], i));
    }
    for (i = 32; i < 64; ++i) {
        pokeBit(e, i, peekBit(left[16], i - 32));
    }

    /* permute 'e' using table IP2 ad return result as a hex string */
    for (i = 0; i < 64; ++i) {
        pokeBit(ep, i, peekBit(e, IP2[i] - 1));
    }
}

/*
 * Encrypts a message using DES
 *
 * key: the key to use to encrypt the message
 * message: the message to be encrypted
 * len: the length of the message
 *
 * returns: a paring of dynamically allocated memory for the encoded message,
 *          and the length of the encoded message.
 *          the caller will need to free the memory after use.
 */
String encrypt(const key_t key, const ubyte *message, int len) {
    String result = { 0, 0 };
    subkey_t ks;
    ubyte padByte;
    int i;

    getSubKeys(key, ks);

    padByte = 8 - len % 8;
    result.len = len + padByte;
    result.data = (ubyte*)malloc(result.len);
    memcpy(result.data, message, len);
    memset(&result.data[len], padByte, padByte);

    for (i = 0; i < result.len; i += 8) {
        processMessage(&result.data[i], ks, &result.data[i]);
    }

    return result;
}

/*
 * Decrypts a message using DES
 *
 * key: the key to use to decrypt the message
 * message: the message to be decrypted
 * len: the length of the message
 *
 * returns: a paring of dynamically allocated memory for the decoded message,
 *          and the length of the decoded message.
 *          the caller will need to free the memory after use.
 */
String decrypt(const key_t key, const ubyte *message, int len) {
    String result = { 0, 0 };
    subkey_t ks;
    int i, j;
    ubyte padByte;

    getSubKeys(key, ks);
    /* reverse the subkeys */
    for (i = 1; i < 9; ++i) {
        for (j = 0; j < 6; ++j) {
            ubyte temp = ks[i][j];
            ks[i][j] = ks[17 - i][j];
            ks[17 - i][j] = temp;
        }
    }

    result.data = (ubyte*)malloc(len);
    memcpy(result.data, message, len);
    result.len = len;
    for (i = 0; i < result.len; i += 8) {
        processMessage(&result.data[i], ks, &result.data[i]);
    }

    padByte = result.data[len - 1];
    result.len -= padByte;
    return result;
}

/*
 * Convienience method for showing the round trip processing of a message
 */
void driver(const key_t key, const ubyte *message, int len) {
    String encoded, decoded;
    char buffer[128];

    printBytes(key, KEY_LEN, buffer);
    printf("Key     : %s\n", buffer);

    printBytes(message, len, buffer);
    printf("Message : %s\n", buffer);

    encoded = encrypt(key, message, len);
    printBytes(encoded.data, encoded.len, buffer);
    printf("Encoded : %s\n", buffer);

    decoded = decrypt(key, encoded.data, encoded.len);
    printBytes(decoded.data, decoded.len, buffer);
    printf("Decoded : %s\n\n", buffer);

    /* release allocated memory */
    if (encoded.len > 0) {
        free(encoded.data);
        encoded.data = 0;
    }
    if (decoded.len > 0) {
        free(decoded.data);
        decoded.data = 0;
    }
}

int main() {
    const key_t keys[] = {
        {0x13, 0x34, 0x57, 0x79, 0x9B, 0xBC, 0xDF, 0xF1},
        {0x0E, 0x32, 0x92, 0x32, 0xEA, 0x6D, 0x0D, 0x73},
        {0x0E, 0x32, 0x92, 0x32, 0xEA, 0x6D, 0x0D, 0x73}
    };
    const ubyte message1[] = { 0x01, 0x23, 0x45, 0x67, 0x89, 0xAB, 0xCD, 0xEF };
    const ubyte message2[] = { 0x87, 0x87, 0x87, 0x87, 0x87, 0x87, 0x87, 0x87 };
    const ubyte message3[] = { 0x59, 0x6F, 0x75, 0x72, 0x20, 0x6C, 0x69, 0x70, 0x73, 0x20, 0x61, 0x72, 0x65, 0x20, 0x73, 0x6D, 0x6F, 0x6F, 0x74, 0x68, 0x65, 0x72, 0x20, 0x74, 0x68, 0x61, 0x6E, 0x20, 0x76, 0x61, 0x73, 0x65, 0x6C, 0x69, 0x6E, 0x65, 0x0D, 0x0A };
    int len;

    len = sizeof(message1) / sizeof(ubyte);
    driver(keys[0], message1, len);

    len = sizeof(message2) / sizeof(ubyte);
    driver(keys[1], message2, len);

    len = sizeof(message3) / sizeof(ubyte);
    driver(keys[2], message3, len);
    return 0;
}
```

{{out}}

```txt
Key     : 133457799BBCDFF1
Message : 0123456789ABCDEF
Encoded : 85E813540F0AB405FDF2E174492922F8
Decoded : 0123456789ABCDEF

Key     : 0E329232EA6D0D73
Message : 8787878787878787
Encoded : 0000000000000000A913F4CB0BD30F97
Decoded : 8787878787878787

Key     : 0E329232EA6D0D73
Message : 596F7572206C6970732061726520736D6F6F74686572207468616E20766173656C696E650D0A
Encoded : C0999FDDE378D7ED727DA00BCA5A84EE47F269A4D6438190D9D52F78F53584997F922CCB5B068D99
Decoded : 596F7572206C6970732061726520736D6F6F74686572207468616E20766173656C696E650D0A
```



## C++

{{trans|D}}

```cpp>#include <algorithm

#include <array>
#include <bitset>
#include <iomanip>
#include <iostream>
#include <ostream>
#include <vector>

template <size_t N>
std::ostream& operator<<(std::ostream& out, std::bitset<N>& bs) {
    // debug
    for (int i = 0; i < N; i += 8) {
        out << bs.test(i + 0) << bs.test(i + 1) << bs.test(i + 2) << bs.test(i + 3) << '_';
        if (i + 7 < N) {
            out << bs.test(i + 4) << bs.test(i + 5) << bs.test(i + 6) << bs.test(i + 7) << ' ';
        } else {
            out << "0000 ";
        }
    }
    return out;
}

namespace DES {
    typedef unsigned char ubyte;
    typedef std::array<ubyte, 8> key_t;

    namespace impl {
        const int PC1[] = {
            57, 49, 41, 33, 25, 17,  9,
             1, 58, 50, 42, 34, 26, 18,
            10,  2, 59, 51, 43, 35, 27,
            19, 11,  3, 60, 52, 44, 36,
            63, 55, 47, 39, 31, 23, 15,
             7, 62, 54, 46, 38, 30, 22,
            14,  6, 61, 53, 45, 37, 29,
            21, 13,  5, 28, 20, 12,  4
        };

        const int PC2[] = {
            14, 17, 11, 24,  1,  5,
             3, 28, 15,  6, 21, 10,
            23, 19, 12,  4, 26,  8,
            16,  7, 27, 20, 13,  2,
            41, 52, 31, 37, 47, 55,
            30, 40, 51, 45, 33, 48,
            44, 49, 39, 56, 34, 53,
            46, 42, 50, 36, 29, 32
        };

        const int IP[] = {
            58, 50, 42, 34, 26, 18, 10,  2,
            60, 52, 44, 36, 28, 20, 12,  4,
            62, 54, 46, 38, 30, 22, 14,  6,
            64, 56, 48, 40, 32, 24, 16,  8,
            57, 49, 41, 33, 25, 17,  9,  1,
            59, 51, 43, 35, 27, 19, 11,  3,
            61, 53, 45, 37, 29, 21, 13,  5,
            63, 55, 47, 39, 31, 23, 15,  7
        };

        const int E[] = {
            32,  1,  2,  3,  4,  5,
             4,  5,  6,  7,  8,  9,
             8,  9, 10, 11, 12, 13,
            12, 13, 14, 15, 16, 17,
            16, 17, 18, 19, 20, 21,
            20, 21, 22, 23, 24, 25,
            24, 25, 26, 27, 28, 29,
            28, 29, 30, 31, 32,  1
        };

        const int S[][64] = {
            {
                14,  4, 13,  1,  2, 15, 11,  8,  3, 10,  6, 12,  5,  9,  0,  7,
                 0, 15,  7,  4, 14,  2, 13,  1, 10,  6, 12, 11,  9,  5,  3,  8,
                 4,  1, 14,  8, 13,  6,  2, 11, 15, 12,  9,  7,  3, 10,  5,  0,
                15, 12,  8,  2,  4,  9,  1,  7,  5, 11,  3, 14, 10,  0,  6, 13
            },
            {
                15,  1,  8, 14,  6, 11,  3,  4,  9,  7,  2, 13, 12,  0,  5, 10,
                 3, 13,  4,  7, 15,  2,  8, 14, 12,  0,  1, 10,  6,  9, 11,  5,
                 0, 14,  7, 11, 10,  4, 13,  1,  5,  8, 12,  6,  9,  3,  2, 15,
                13,  8, 10,  1,  3, 15,  4,  2, 11,  6,  7, 12,  0,  5, 14,  9
            },
            {
                10,  0,  9, 14,  6,  3, 15,  5,  1, 13, 12,  7, 11,  4,  2,  8,
                13,  7,  0,  9,  3,  4,  6, 10,  2,  8,  5, 14, 12, 11, 15,  1,
                13,  6,  4,  9,  8, 15,  3,  0, 11,  1,  2, 12,  5, 10, 14,  7,
                 1, 10, 13,  0,  6,  9,  8,  7,  4, 15, 14,  3, 11,  5,  2, 12
            },
            {
                 7, 13, 14,  3,  0,  6,  9, 10,  1,  2,  8,  5, 11, 12,  4, 15,
                13,  8, 11,  5,  6, 15,  0,  3,  4,  7,  2, 12,  1, 10, 14,  9,
                10,  6,  9,  0, 12, 11,  7, 13, 15,  1,  3, 14,  5,  2,  8,  4,
                 3, 15,  0,  6, 10,  1, 13,  8,  9,  4,  5, 11, 12,  7,  2, 14
            },
            {
                 2, 12,  4,  1,  7, 10, 11,  6,  8,  5,  3, 15, 13,  0, 14,  9,
                14, 11,  2, 12,  4,  7, 13,  1,  5,  0, 15, 10,  3,  9,  8,  6,
                 4,  2,  1, 11, 10, 13,  7,  8, 15,  9, 12,  5,  6,  3,  0, 14,
                11,  8, 12,  7,  1, 14,  2, 13,  6, 15,  0,  9, 10,  4,  5,  3
            },
            {
                12,  1, 10, 15,  9,  2,  6,  8,  0, 13,  3,  4, 14,  7,  5, 11,
                10, 15,  4,  2,  7, 12,  9,  5,  6,  1, 13, 14,  0, 11,  3,  8,
                 9, 14, 15,  5,  2,  8, 12,  3,  7,  0,  4, 10,  1, 13, 11,  6,
                 4,  3,  2, 12,  9,  5, 15, 10, 11, 14,  1,  7,  6,  0,  8, 13
            },
            {
                 4, 11,  2, 14, 15,  0,  8, 13,  3, 12,  9,  7,  5, 10,  6,  1,
                13,  0, 11,  7,  4,  9,  1, 10, 14,  3,  5, 12,  2, 15,  8,  6,
                 1,  4, 11, 13, 12,  3,  7, 14, 10, 15,  6,  8,  0,  5,  9,  2,
                 6, 11, 13,  8,  1,  4, 10,  7,  9,  5,  0, 15, 14,  2,  3, 12
            },
            {
                13,  2,  8,  4,  6, 15, 11,  1, 10,  9,  3, 14,  5,  0, 12,  7,
                 1, 15, 13,  8, 10,  3,  7,  4, 12,  5,  6, 11,  0, 14,  9,  2,
                 7, 11,  4,  1,  9, 12, 14,  2,  0,  6, 10, 13, 15,  3,  5,  8,
                 2,  1, 14,  7,  4, 10,  8, 13, 15, 12,  9,  0,  3,  5,  6, 11
            }
        };

        const int P[] = {
            16,  7, 20, 21,
            29, 12, 28, 17,
             1, 15, 23, 26,
             5, 18, 31, 10,
             2,  8, 24, 14,
            32, 27,  3,  9,
            19, 13, 30,  6,
            22, 11,  4, 25
        };

        const int IP2[] = {
            40,  8, 48, 16, 56, 24, 64, 32,
            39,  7, 47, 15, 55, 23, 63, 31,
            38,  6, 46, 14, 54, 22, 62, 30,
            37,  5, 45, 13, 53, 21, 61, 29,
            36,  4, 44, 12, 52, 20, 60, 28,
            35,  3, 43, 11, 51, 19, 59, 27,
            34,  2, 42, 10, 50, 18, 58, 26,
            33,  1, 41,  9, 49, 17, 57, 25
        };

        const int SHIFTS[] = { 1, 1, 2, 2, 2, 2, 2, 2, 1, 2, 2, 2, 2, 2, 2, 1 };

        std::bitset<64> toBitSet(const key_t& key) {
            std::bitset<64> bs;
            for (int i = 0; i < 8; ++i) {
                bs.set(8 * i + 0, key[i] & 0x80);
                bs.set(8 * i + 1, key[i] & 0x40);
                bs.set(8 * i + 2, key[i] & 0x20);
                bs.set(8 * i + 3, key[i] & 0x10);

                bs.set(8 * i + 4, key[i] & 0x08);
                bs.set(8 * i + 5, key[i] & 0x04);
                bs.set(8 * i + 6, key[i] & 0x02);
                bs.set(8 * i + 7, key[i] & 0x01);
            }
            return bs;
        }

        template <size_t N>
        std::bitset<N * 8> toBitSet(const std::array<ubyte, N>& src) {
            std::bitset<N * 8> bs;
            for (int i = 0; i < N; ++i) {
                bs.set(8 * i + 0, src[i] & 0x80);
                bs.set(8 * i + 1, src[i] & 0x40);
                bs.set(8 * i + 2, src[i] & 0x20);
                bs.set(8 * i + 3, src[i] & 0x10);

                bs.set(8 * i + 4, src[i] & 0x08);
                bs.set(8 * i + 5, src[i] & 0x04);
                bs.set(8 * i + 6, src[i] & 0x02);
                bs.set(8 * i + 7, src[i] & 0x01);
            }
            return bs;
        }

        template <size_t N>
        std::array<ubyte, N / 8> toArray(const std::bitset<N>& bs) {
            std::array<ubyte, N / 8> arr;
            std::fill_n(arr.begin(), N / 8, 0);
            for (int i = 0; i < N / 8; ++i) {
                arr[i] |= (bs[8 * i + 0] << 7);
                arr[i] |= (bs[8 * i + 1] << 6);
                arr[i] |= (bs[8 * i + 2] << 5);
                arr[i] |= (bs[8 * i + 3] << 4);

                arr[i] |= (bs[8 * i + 4] << 3);
                arr[i] |= (bs[8 * i + 5] << 2);
                arr[i] |= (bs[8 * i + 6] << 1);
                arr[i] |= (bs[8 * i + 7] << 0);
            }
            return arr;
        }

        template <size_t N>
        std::bitset<N> shiftLeft(const std::bitset<N>& bs, int len, int times) {
            std::bitset<N> output;
            for (int i = 0; i < len; ++i) {
                output[i] = bs[i];
            }
            for (int t = 0; t < times; ++t) {
                int temp = output[0];
                for (int i = 1; i < len; ++i) {
                    output[i - 1] = output[i];
                }
                output[len - 1] = temp;
            }
            return output;
        }

        std::array<std::bitset<48>, 17> getSubKeys(const key_t& key) {
            std::array<std::bitset<56>, 17> c;
            std::array<std::bitset<28>, 17> d;
            std::bitset<56> kp;

            auto k = toBitSet(key);

            /* permute 'key' using table PC1 */
            for (int i = 0; i < 56; ++i) {
                kp[i] = k[PC1[i] - 1];
            }

            /* split 'kp' in half and process the resulting series of 'c' and 'd' */
            for (int i = 0; i < 28; ++i) {
                c[0][i] = kp[i];
                d[0][i] = kp[i + 28];
            }

            /* shift the components of c and d */
            for (int i = 1; i < 17; ++i) {
                c[i] = shiftLeft(c[i - 1], 28, SHIFTS[i - 1]);
                d[i] = shiftLeft(d[i - 1], 28, SHIFTS[i - 1]);
            }

            /* merge 'd' into 'c' */
            for (int i = 1; i < 17; ++i) {
                for (int j = 28; j < 56; ++j) {
                    c[i][j] = d[i][j - 28];
                }
            }

            /* form the sub-keys and store them in 'ks'
             * permute 'c' using table PC2 */
            std::array<std::bitset<48>, 17> ks;
            for (int i = 1; i < 17; ++i) {
                for (int j = 0; j < 48; ++j) {
                    ks[i][j] = c[i][PC2[j] - 1];
                }
            }

            return ks;
        }

        std::bitset<32> f(const std::bitset<48>& ks, std::bitset<32>& r) {
            // permute 'r' using table E
            std::bitset<48> er;
            for (int i = 0; i < 48; ++i) {
                er[i] = r[E[i] - 1];
            }

            // xor 'er' with 'ks' and store back into 'er'
            er ^= ks;

            // process 'er' six bits at a time and store resulting four bits in 'sr'
            std::bitset<32> sr;
            for (int i = 0; i < 8; ++i) {
                int j = 6 * i;
                std::bitset<6> b;
                for (int k = 0; k < 6; ++k) {
                    b[k] = er[j + k] != 0;
                }
                int row = 2 * b[0] + b[5];
                int col = 8 * b[1] + 4 * b[2] + 2 * b[3] + b[4];
                int m = S[i][row * 16 + col];   // apply table s
                int n = 1;
                while (m > 0) {
                    int p = m % 2;
                    sr[(i + 1) * 4 - n] = (p == 1);
                    m /= 2;
                    n++;
                }
            }

            // permute sr using table P
            std::bitset<32> sp;
            for (int i = 0; i < 32; ++i) {
                sp[i] = sr[P[i] - 1];
            }
            return sp;
        }

        std::array<ubyte, 8> processMessage(const std::array<std::bitset<48>, 17>& ks, const std::array<ubyte, 8>& message) {
            auto m = toBitSet(message);

            // permute 'message' using table IP
            std::bitset<64> mp;
            for (int i = 0; i < 64; ++i) {
                mp[i] = m[IP[i] - 1];
            }

            // split 'mp' in half and process the resulting series of 'l' and 'r
            std::array<std::bitset<32>, 17> left;
            std::array<std::bitset<32>, 17> right;
            for (int i = 0; i < 32; ++i) {
                left[0][i] = mp[i];
                right[0][i] = mp[i + 32];
            }
            for (int i = 1; i < 17; ++i) {
                left[i] = right[i - 1];
                auto fs = f(ks[i], right[i - 1]);
                left[i - 1] ^= fs;
                right[i] = left[i - 1];
            }

            // amalgamate r[16] and l[16] (in that order) into 'e'
            std::bitset<64> e;
            for (int i = 0; i < 32; ++i) {
                e[i] = right[16][i];
            }
            for (int i = 32; i < 64; ++i) {
                e[i] = left[16][i - 32];
            }

            // permute 'e' using table IP2 ad return result as a hex string
            std::bitset<64> ep;
            for (int i = 0; i < 64; ++i) {
                ep[i] = e[IP2[i] - 1];
            }
            return toArray(ep);
        }
    }

    std::vector<ubyte> encrypt(const key_t& key, const std::vector<ubyte>& message) {
        auto ks = impl::getSubKeys(key);
        std::vector<ubyte> m(message);

        // pad the message so there are 8 byte groups
        ubyte padByte = 8 - m.size() % 8;
        for (int i = 0; i < padByte; ++i) {
            m.push_back(padByte);
        }

        std::vector<ubyte> sb;
        for (size_t i = 0; i < m.size(); i += 8) {
            std::array<ubyte, 8> part;
            std::copy_n(m.begin() + i, 8, part.begin());
            part = impl::processMessage(ks, part);
            std::copy(part.begin(), part.end(), std::back_inserter(sb));
        }

        return sb;
    }

    std::vector<ubyte> decrypt(const key_t& key, const std::vector<ubyte>& encoded) {
        auto ks = impl::getSubKeys(key);
        // reverse the subkeys
        std::reverse(ks.begin() + 1, ks.end());

        std::vector<ubyte> decoded;
        for (int i = 0; i < encoded.size(); i += 8) {
            std::array<ubyte, 8> part;
            std::copy_n(encoded.begin() + i, 8, part.begin());
            part = impl::processMessage(ks, part);
            std::copy(part.begin(), part.end(), std::back_inserter(decoded));
        }

        // remove the padding bytes from the decoded message
        auto padByte = decoded.back();
        decoded.resize(decoded.size() - padByte);
        return decoded;
    }

    std::ostream& operator<<(std::ostream& os, const key_t& key) {
        os << std::setfill('0') << std::uppercase << std::hex;
        for (int i = 0; i < 8; ++i) {
            os << std::setw(2) << (int)key[i];
        }
        return os;
    }

    std::ostream& operator<<(std::ostream& os, const std::vector<ubyte>& msg) {
        os << std::setfill('0') << std::uppercase << std::hex;
        for (auto b : msg) {
            os << std::setw(2) << (int)b;
        }
        return os;
    }
}

int main() {
    using namespace std;
    using namespace DES;

    key_t keys[] = {
        {0x13, 0x34, 0x57, 0x79, 0x9B, 0xBC, 0xDF, 0xF1},
        {0x0E, 0x32, 0x92, 0x32, 0xEA, 0x6D, 0x0D, 0x73},
        {0x0E, 0x32, 0x92, 0x32, 0xEA, 0x6D, 0x0D, 0x73}
    };
    vector<vector<ubyte>> messages = {
        {0x01, 0x23, 0x45, 0x67, 0x89, 0xAB, 0xCD, 0xEF},
        {0x87, 0x87, 0x87, 0x87, 0x87, 0x87, 0x87, 0x87},
        {0x59, 0x6F, 0x75, 0x72, 0x20, 0x6C, 0x69, 0x70, 0x73, 0x20, 0x61, 0x72, 0x65, 0x20, 0x73, 0x6D, 0x6F, 0x6F, 0x74, 0x68, 0x65, 0x72, 0x20, 0x74, 0x68, 0x61, 0x6E, 0x20, 0x76, 0x61, 0x73, 0x65, 0x6C, 0x69, 0x6E, 0x65, 0x0D, 0x0A}
    };

    for (int i = 0; i < 3; ++i) {
        cout << "Key     : " << keys[i] << '\n';
        cout << "Message : " << messages[i] << '\n';

        auto encoded = encrypt(keys[i], messages[i]);
        cout << "Encoded : " << encoded << endl;

        auto decoded = decrypt(keys[i], encoded);
        cout << "Decoded : " << decoded << endl;

        cout << '\n';
    }

    return 0;
}
```

{{out}}

```txt
Key     : 133457799BBCDFF1
Message : 0123456789ABCDEF
Encoded : 85E813540F0AB405FDF2E174492922F8
Decoded : 0123456789ABCDEF

Key     : 0E329232EA6D0D73
Message : 8787878787878787
Encoded : 0000000000000000A913F4CB0BD30F97
Decoded : 8787878787878787

Key     : 0E329232EA6D0D73
Message : 596F7572206C6970732061726520736D6F6F74686572207468616E20766173656C696E650D0A
Encoded : C0999FDDE378D7ED727DA00BCA5A84EE47F269A4D6438190D9D52F78F53584997F922CCB5B068D99
Decoded : 596F7572206C6970732061726520736D6F6F74686572207468616E20766173656C696E650D0A
```


=={{header|C#|C sharp}}==

```csharp
using System;
using System.IO;
using System.Security.Cryptography;

namespace DES {
    class Program {
        //Taken from https://stackoverflow.com/a/311179
        static string ByteArrayToString(byte[] ba) {
            return BitConverter.ToString(ba).Replace("-", "");
        }

        //Modified from https://stackoverflow.com/q/4100996
        //The passwordBytes parameter must be 8 bytes long
        static byte[] Encrypt(byte[] messageBytes, byte[] passwordBytes) {
            byte[] iv = new byte[] { 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 };

            // Set encryption settings -- Use password for both key and init. vector
            DESCryptoServiceProvider provider = new DESCryptoServiceProvider();
            ICryptoTransform transform = provider.CreateEncryptor(passwordBytes, iv);
            CryptoStreamMode mode = CryptoStreamMode.Write;

            // Set up streams and encrypt
            MemoryStream memStream = new MemoryStream();
            CryptoStream cryptoStream = new CryptoStream(memStream, transform, mode);
            cryptoStream.Write(messageBytes, 0, messageBytes.Length);
            cryptoStream.FlushFinalBlock();

            // Read the encrypted message from the memory stream
            byte[] encryptedMessageBytes = new byte[memStream.Length];
            memStream.Position = 0;
            memStream.Read(encryptedMessageBytes, 0, encryptedMessageBytes.Length);

            return encryptedMessageBytes;
        }

        //Modified from https://stackoverflow.com/q/4100996
        //The passwordBytes parameter must be 8 bytes long
        static byte[] Decrypt(byte[] encryptedMessageBytes, byte[] passwordBytes) {
            byte[] iv = new byte[] { 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 };

            // Set encryption settings -- Use password for both key and init. vector
            DESCryptoServiceProvider provider = new DESCryptoServiceProvider();
            ICryptoTransform transform = provider.CreateDecryptor(passwordBytes, iv);
            CryptoStreamMode mode = CryptoStreamMode.Write;

            // Set up streams and decrypt
            MemoryStream memStream = new MemoryStream();
            CryptoStream cryptoStream = new CryptoStream(memStream, transform, mode);
            cryptoStream.Write(encryptedMessageBytes, 0, encryptedMessageBytes.Length);
            cryptoStream.FlushFinalBlock();

            // Read decrypted message from memory stream
            byte[] decryptedMessageBytes = new byte[memStream.Length];
            memStream.Position = 0;
            memStream.Read(decryptedMessageBytes, 0, decryptedMessageBytes.Length);

            return decryptedMessageBytes;
        }

        static void Main(string[] args) {
            byte[] keyBytes = new byte[] { 0x0e, 0x32, 0x92, 0x32, 0xea, 0x6d, 0x0d, 0x73 };
            byte[] plainBytes = new byte[] { 0x87, 0x87, 0x87, 0x87, 0x87, 0x87, 0x87, 0x87 };

            byte[] encStr = Encrypt(plainBytes, keyBytes);
            Console.WriteLine("Encoded: {0}", ByteArrayToString(encStr));

            byte[] decBytes = Decrypt(encStr, keyBytes);
            Console.WriteLine("Decoded: {0}", ByteArrayToString(decBytes));
        }
    }
}
```

{{out}}

```txt
Encoded: 0000000000000000A913F4CB0BD30F97
Decoded: 8787878787878787
```



## D

{{trans|kotlin}}

```d
import std.array;
import std.bitmanip;
import std.stdio;

immutable PC1 = [
    57, 49, 41, 33, 25, 17,  9,
     1, 58, 50, 42, 34, 26, 18,
    10,  2, 59, 51, 43, 35, 27,
    19, 11,  3, 60, 52, 44, 36,
    63, 55, 47, 39, 31, 23, 15,
     7, 62, 54, 46, 38, 30, 22,
    14,  6, 61, 53, 45, 37, 29,
    21, 13,  5, 28, 20, 12,  4
];

immutable PC2 = [
    14, 17, 11, 24,  1,  5,
     3, 28, 15,  6, 21, 10,
    23, 19, 12,  4, 26,  8,
    16,  7, 27, 20, 13,  2,
    41, 52, 31, 37, 47, 55,
    30, 40, 51, 45, 33, 48,
    44, 49, 39, 56, 34, 53,
    46, 42, 50, 36, 29, 32
];

immutable IP = [
    58, 50, 42, 34, 26, 18, 10,  2,
    60, 52, 44, 36, 28, 20, 12,  4,
    62, 54, 46, 38, 30, 22, 14,  6,
    64, 56, 48, 40, 32, 24, 16,  8,
    57, 49, 41, 33, 25, 17,  9,  1,
    59, 51, 43, 35, 27, 19, 11,  3,
    61, 53, 45, 37, 29, 21, 13,  5,
    63, 55, 47, 39, 31, 23, 15,  7
];

immutable E = [
    32,  1,  2,  3,  4,  5,
     4,  5,  6,  7,  8,  9,
     8,  9, 10, 11, 12, 13,
    12, 13, 14, 15, 16, 17,
    16, 17, 18, 19, 20, 21,
    20, 21, 22, 23, 24, 25,
    24, 25, 26, 27, 28, 29,
    28, 29, 30, 31, 32,  1
];

immutable S = [
    [
        14,  4, 13,  1,  2, 15, 11,  8,  3, 10,  6, 12,  5,  9,  0,  7,
         0, 15,  7,  4, 14,  2, 13,  1, 10,  6, 12, 11,  9,  5,  3,  8,
         4,  1, 14,  8, 13,  6,  2, 11, 15, 12,  9,  7,  3, 10,  5,  0,
        15, 12,  8,  2,  4,  9,  1,  7,  5, 11,  3, 14, 10,  0,  6, 13
    ],
    [
        15,  1,  8, 14,  6, 11,  3,  4,  9,  7,  2, 13, 12,  0,  5, 10,
         3, 13,  4,  7, 15,  2,  8, 14, 12,  0,  1, 10,  6,  9, 11,  5,
         0, 14,  7, 11, 10,  4, 13,  1,  5,  8, 12,  6,  9,  3,  2, 15,
        13,  8, 10,  1,  3, 15,  4,  2, 11,  6,  7, 12,  0,  5, 14,  9
    ],
    [
        10,  0,  9, 14,  6,  3, 15,  5,  1, 13, 12,  7, 11,  4,  2,  8,
        13,  7,  0,  9,  3,  4,  6, 10,  2,  8,  5, 14, 12, 11, 15,  1,
        13,  6,  4,  9,  8, 15,  3,  0, 11,  1,  2, 12,  5, 10, 14,  7,
         1, 10, 13,  0,  6,  9,  8,  7,  4, 15, 14,  3, 11,  5,  2, 12
    ],
    [
         7, 13, 14,  3,  0,  6,  9, 10,  1,  2,  8,  5, 11, 12,  4, 15,
        13,  8, 11,  5,  6, 15,  0,  3,  4,  7,  2, 12,  1, 10, 14,  9,
        10,  6,  9,  0, 12, 11,  7, 13, 15,  1,  3, 14,  5,  2,  8,  4,
         3, 15,  0,  6, 10,  1, 13,  8,  9,  4,  5, 11, 12,  7,  2, 14
    ],
    [
         2, 12,  4,  1,  7, 10, 11,  6,  8,  5,  3, 15, 13,  0, 14,  9,
        14, 11,  2, 12,  4,  7, 13,  1,  5,  0, 15, 10,  3,  9,  8,  6,
         4,  2,  1, 11, 10, 13,  7,  8, 15,  9, 12,  5,  6,  3,  0, 14,
        11,  8, 12,  7,  1, 14,  2, 13,  6, 15,  0,  9, 10,  4,  5,  3
    ],
    [
        12,  1, 10, 15,  9,  2,  6,  8,  0, 13,  3,  4, 14,  7,  5, 11,
        10, 15,  4,  2,  7, 12,  9,  5,  6,  1, 13, 14,  0, 11,  3,  8,
         9, 14, 15,  5,  2,  8, 12,  3,  7,  0,  4, 10,  1, 13, 11,  6,
         4,  3,  2, 12,  9,  5, 15, 10, 11, 14,  1,  7,  6,  0,  8, 13
    ],
    [
         4, 11,  2, 14, 15,  0,  8, 13,  3, 12,  9,  7,  5, 10,  6,  1,
        13,  0, 11,  7,  4,  9,  1, 10, 14,  3,  5, 12,  2, 15,  8,  6,
         1,  4, 11, 13, 12,  3,  7, 14, 10, 15,  6,  8,  0,  5,  9,  2,
         6, 11, 13,  8,  1,  4, 10,  7,  9,  5,  0, 15, 14,  2,  3, 12
    ],
    [
        13,  2,  8,  4,  6, 15, 11,  1, 10,  9,  3, 14,  5,  0, 12,  7,
         1, 15, 13,  8, 10,  3,  7,  4, 12,  5,  6, 11,  0, 14,  9,  2,
         7, 11,  4,  1,  9, 12, 14,  2,  0,  6, 10, 13, 15,  3,  5,  8,
         2,  1, 14,  7,  4, 10,  8, 13, 15, 12,  9,  0,  3,  5,  6, 11
    ]
];

immutable P = [
    16,  7, 20, 21,
    29, 12, 28, 17,
     1, 15, 23, 26,
     5, 18, 31, 10,
     2,  8, 24, 14,
    32, 27,  3,  9,
    19, 13, 30,  6,
    22, 11,  4, 25
];

immutable IP2 = [
    40,  8, 48, 16, 56, 24, 64, 32,
    39,  7, 47, 15, 55, 23, 63, 31,
    38,  6, 46, 14, 54, 22, 62, 30,
    37,  5, 45, 13, 53, 21, 61, 29,
    36,  4, 44, 12, 52, 20, 60, 28,
    35,  3, 43, 11, 51, 19, 59, 27,
    34,  2, 42, 10, 50, 18, 58, 26,
    33,  1, 41,  9, 49, 17, 57, 25
];

immutable SHIFTS = [1, 1, 2, 2, 2, 2, 2, 2, 1, 2, 2, 2, 2, 2, 2, 1];

BitArray bitArrayOfSize(uint count) {
    bool[] buffer = new bool[count];
    return BitArray(buffer);
}

ubyte[] encrypt(const ubyte[] key, const ubyte[] message) in {
    assert(key.length == 8, "Incorrect key size");
} body {
    BitArray[] ks = getSubKeys(key);
    ubyte[] m = message.dup;

    // pad the message so there are 8 byte groups
    ubyte padByte = 8 - m.length % 8;
    foreach (_; 0..padByte) {
        m ~= padByte;
    }
    assert(m.length % 8 == 0);

    ubyte[] sb;
    foreach (i; 0..m.length / 8) {
        auto j = i * 8;
        auto enc = processMessage(m[j..j+8], ks);
        sb ~= enc;
    }

    return sb;
}

ubyte[] decrypt(const ubyte[] key, const ubyte[] encoded) in {
    assert(key.length == 8, "Incorrect key size");
} body {
    BitArray[] ks = getSubKeys(key);
    // reverse the subkeys
    foreach (i; 1..9) {
        auto temp = ks[i];
        ks[i] = ks[17 - i];
        ks[17 - i] = temp;
    }

    ubyte[] decoded;
    foreach (i; 0..encoded.length / 8) {
        auto j = i * 8;
        auto dec = processMessage(encoded[j..j+8], ks);
        decoded ~= dec;
    }

    // remove the padding bytes from the decoded message
    ubyte padByte = decoded[$ - 1];
    decoded.length -= padByte;

    return decoded;
}

private BitArray[] getSubKeys(const ubyte[] key) in {
    assert(key.length == 8);
} body {
    auto k = key.toBitArray();

    // permute 'key' using table PC1
    auto kp = bitArrayOfSize(56);
    foreach (i; 0..56) {
        kp[i] = k[PC1[i] - 1];
    }

    // split 'kp' in half and process the resulting series of 'c' and 'd'
    BitArray[] c;
    BitArray[] d;
    foreach (_; 0..18) {
        c ~= bitArrayOfSize(56);
        d ~= bitArrayOfSize(28);
    }
    foreach (i; 0..28) {
        c[0][i] = kp[i];
        d[0][i] = kp[i + 28];
    }
    foreach (i; 1..17) {
        c[i - 1].shiftLeft(SHIFTS[i - 1], 28, c[i]);
        d[i - 1].shiftLeft(SHIFTS[i - 1], 28, d[i]);
    }

    // merge 'd' into 'c'
    foreach (i; 1..17) {
        foreach (j; 28..56) {
            c[i][j] = d[i][j - 28];
        }
    }

    // form the sub-keys and store them in 'ks'
    BitArray[] ks;
    foreach (_; 0..17) {
        ks ~= bitArrayOfSize(48);
    }

    // permute 'c' using table PC2
    foreach (i; 1..17) {
        foreach (j; 0..48) {
            ks[i][j] = c[i][PC2[j] - 1];
        }
    }

    return ks;
}

private ubyte[] processMessage(const ubyte[] message, BitArray[] ks) {
    auto m = message.toBitArray();

    // permute 'message' using table IP
    auto mp = bitArrayOfSize(64);
    foreach (i; 0..64) {
        mp[i] = m[IP[i] - 1];
    }

    // split 'mp' in half and process the resulting series of 'l' and 'r
    BitArray[] left;
    BitArray[] right;
    foreach (_; 0..17) {
        left ~= bitArrayOfSize(32);
        right ~= bitArrayOfSize(32);
    }
    foreach (i; 0..32) {
        left[0][i] = mp[i];
        right[0][i] = mp[i + 32];
    }
    foreach (i; 1..17) {
        left[i] = right[i - 1];
        auto fs = f(right[i - 1], ks[i]);
        left[i - 1] ^= fs;
        right[i] = left[i - 1];
    }

    // amalgamate r[16] and l[16] (in that order) into 'e'
    auto e = bitArrayOfSize(64);
    foreach (i; 0..32) {
        e[i] = right[16][i];
    }
    foreach (i; 32..64) {
        e[i] = left[16][i - 32];
    }

    // permute 'e' using table IP2 ad return result as a hex string
    auto ep = bitArrayOfSize(64);
    foreach (i; 0..64) {
        ep[i] = e[IP2[i] - 1];
    }
    return ep.toByteArray();
}

private BitArray toBitArray(const ubyte[] byteArr) {
    auto bitArr = bitArrayOfSize(8 * byteArr.length);
    for (int i=0; i<byteArr.length; i++) {
        bitArr[8*i+0] = (byteArr[i] & 128) != 0;
        bitArr[8*i+1] = (byteArr[i] & 64) != 0;
        bitArr[8*i+2] = (byteArr[i] & 32) != 0;
        bitArr[8*i+3] = (byteArr[i] & 16) != 0;
        bitArr[8*i+4] = (byteArr[i] & 8) != 0;
        bitArr[8*i+5] = (byteArr[i] & 4) != 0;
        bitArr[8*i+6] = (byteArr[i] & 2) != 0;
        bitArr[8*i+7] = (byteArr[i] & 1) != 0;
    }
    return bitArr;
}

ubyte[] toByteArray(const ref BitArray bitArr) {
    auto len = bitArr.length / 8;
    ubyte[] byteArr = new ubyte[len];
    foreach (i; 0..len) {
        byteArr[i]  = bitArr[8 * i + 0] << 7;
        byteArr[i] |= bitArr[8 * i + 1] << 6;
        byteArr[i] |= bitArr[8 * i + 2] << 5;
        byteArr[i] |= bitArr[8 * i + 3] << 4;
        byteArr[i] |= bitArr[8 * i + 4] << 3;
        byteArr[i] |= bitArr[8 * i + 5] << 2;
        byteArr[i] |= bitArr[8 * i + 6] << 1;
        byteArr[i] |= bitArr[8 * i + 7] << 0;
    }
    return byteArr;
}

void shiftLeft(const ref BitArray self, int times, int len, ref BitArray output) {
    for (int i=0; i<=len; i++) {
        output[i] = self[i];
    }
    for (int t=1; t<=times; t++) {
        auto temp = output[0];
        for (int i=1; i<=len; i++) {
            output[i - 1] = output[i];
        }
        output[len - 1] = temp;
    }
}

private BitArray f(const ref BitArray r, const ref BitArray ks) {
    // permute 'r' using table E
    auto er = bitArrayOfSize(48);
    foreach (i; 0..48) {
        er[i] = r[E[i] - 1];
    }

    // xor 'er' with 'ks' and store back into 'er'
    er ^= ks;

    // process 'er' six bits at a time and store resulting four bits in 'sr'
    auto sr = bitArrayOfSize(32);
    foreach (i; 0..8) {
        auto j = i * 6;
        auto b = new int[6];
        foreach (k; 0..6) {
            b[k] = (er[j+k] != 0) ? 1 : 0;
        }
        auto row = 2 * b[0] + b[5];
        auto col = 8 * b[1] + 4 * b[2] + 2 * b[3] + b[4];
        int m = S[i][row * 16 + col];   // apply table s
        int n = 1;
        while (m > 0) {
            auto p = m % 2;
            sr[(i + 1) * 4 - n] = (p == 1);
            m /= 2;
            n++;
        }
    }

    // permute sr using table P
    auto sp = bitArrayOfSize(32);
    foreach (i; 0..32) {
        sp[i] = sr[P[i] - 1];
    }
    return sp;
}

void main() {
    immutable ubyte[][] keys = [
        [cast(ubyte)0x13, 0x34, 0x57, 0x79, 0x9B, 0xBC, 0xDF, 0xF1],
        [0x0E, 0x32, 0x92, 0x32, 0xEA, 0x6D, 0x0D, 0x73],
        [0x0E, 0x32, 0x92, 0x32, 0xEA, 0x6D, 0x0D, 0x73],
    ];
    immutable ubyte[][] messages = [
        [cast(ubyte)0x01, 0x23, 0x45, 0x67, 0x89, 0xAB, 0xCD, 0xEF],
        [0x87, 0x87, 0x87, 0x87, 0x87, 0x87, 0x87, 0x87],
        [0x59, 0x6F, 0x75, 0x72, 0x20, 0x6C, 0x69, 0x70, 0x73, 0x20, 0x61, 0x72, 0x65, 0x20, 0x73, 0x6D, 0x6F, 0x6F, 0x74, 0x68, 0x65, 0x72, 0x20, 0x74, 0x68, 0x61, 0x6E, 0x20, 0x76, 0x61, 0x73, 0x65, 0x6C, 0x69, 0x6E, 0x65, 0x0D, 0x0A],
    ];
    assert(keys.length == messages.length);

    foreach (i; 0..messages.length) {
        writefln("Key     : %(%02X%)", keys[i]);
        writefln("Message : %(%02X%)", messages[i]);
        ubyte[] encoded = encrypt(keys[i], messages[i]);
        writefln("Encoded : %(%02X%)", encoded);
        ubyte[] decoded = decrypt(keys[i], encoded);
        writefln("Decoded : %(%02X%)", decoded);
        writeln;
    }
}

```

{{out}}

```txt
Key     : 133457799BBCDFF1
Message : 0123456789ABCDEF
Encoded : 85E813540F0AB405FDF2E174492922F8
Decoded : 0123456789ABCDEF

Key     : 0E329232EA6D0D73
Message : 8787878787878787
Encoded : 0000000000000000A913F4CB0BD30F97
Decoded : 8787878787878787

Key     : 0E329232EA6D0D73
Message : 596F7572206C6970732061726520736D6F6F74686572207468616E20766173656C696E650D0A
Encoded : C0999FDDE378D7ED727DA00BCA5A84EE47F269A4D6438190D9D52F78F53584997F922CCB5B068D99
Decoded : 596F7572206C6970732061726520736D6F6F74686572207468616E20766173656C696E650D0A
```


=={{header|F#|F sharp}}==
{{trans|C#}}

```fsharp
open System
open System.Security.Cryptography
open System.IO

let ByteArrayToString ba =
    ba |> Array.map (fun (b : byte) -> b.ToString("X2")) |> String.Concat

let Encrypt passwordBytes messageBytes =
    // Configure encryption settings
    let iv = Array.zeroCreate 8
    let provider = new DESCryptoServiceProvider()
    let transform = provider.CreateEncryptor(passwordBytes, iv)

    // Setup streams and encrypt
    let memStream = new MemoryStream()
    let cryptoStream = new CryptoStream(memStream, transform, CryptoStreamMode.Write)
    cryptoStream.Write(messageBytes, 0, messageBytes.Length)
    cryptoStream.FlushFinalBlock()

    // Read the encrypted message from the stream
    let encryptedMessageBytes = Array.zeroCreate ((int) memStream.Length)
    memStream.Position <- 0L
    memStream.Read(encryptedMessageBytes, 0, encryptedMessageBytes.Length) |> ignore

    // Return the encrypted bytes
    encryptedMessageBytes

let Decrypt passwordBytes encryptedBytes =
    // Configure encryption settings
    let iv = Array.zeroCreate 8
    let provider = new DESCryptoServiceProvider()
    let transform = provider.CreateDecryptor(passwordBytes, iv)

    // Setup streams and decrypt
    let memStream = new MemoryStream()
    let cryptoStream = new CryptoStream(memStream, transform, CryptoStreamMode.Write)
    cryptoStream.Write(encryptedBytes, 0, encryptedBytes.Length)
    cryptoStream.FlushFinalBlock()

    // Read the message from the stream
    let messageBytes = Array.zeroCreate ((int) memStream.Length)
    memStream.Position <- 0L
    memStream.Read(messageBytes, 0, messageBytes.Length) |> ignore

    // Return the encrypted bytes
    messageBytes

[<EntryPoint>]
let main _ = 
    let keyBytes = [|0x0euy; 0x32uy; 0x92uy; 0x32uy; 0xeauy; 0x6duy; 0x0duy; 0x73uy|]
    let plainbytes = [|0x87uy; 0x87uy; 0x87uy; 0x87uy; 0x87uy; 0x87uy; 0x87uy; 0x87uy|]

    let encStr = Encrypt keyBytes plainbytes
    printfn "Encoded: %s" (ByteArrayToString encStr)

    let decBytes = Decrypt keyBytes encStr
    printfn "Decoded: %s" (ByteArrayToString decBytes)

    0 // return an integer exit code
```

{{out}}

```txt
Encoded: 0000000000000000A913F4CB0BD30F97
Decoded: 8787878787878787
```


## FreeBASIC


```freebasic
' version 20-01-2019
' compile with: fbc -s console

Dim Shared As String * 48 subkey_k()
Dim Shared As String code2

Sub make_subkeys(key As String)

    Dim As UInteger<32> pc_1(55) => _
       {57, 49, 41, 33, 25, 17,  9, _
         1, 58, 50, 42, 34, 26, 18, _
        10,  2, 59, 51, 43, 35, 27, _
        19, 11,  3, 60, 52, 44, 36, _
        63, 55, 47, 39, 31, 23, 15, _
         7, 62, 54, 46, 38, 30, 22, _
        14,  6, 61, 53, 45, 37, 29, _
         21, 13,  5, 28, 20, 12, 4}

    Dim As UInteger<32> pc_2(47) => _
           {14, 17, 11, 24,  1,  5, _
             3, 28, 15,  6, 21, 10, _
            23, 19, 12,  4, 26,  8, _
            16,  7, 27, 20, 13,  2, _
            41, 52, 31, 37, 47, 55, _
            30, 40, 51, 45, 33, 48, _
            44, 49, 39, 56, 34, 53, _
            46, 42, 50, 36, 29, 32}

    Dim As UInteger<32> number_of_left_shl(1 To 16) => _
            {1, 1, 2, 2, 2, 2, 2, 2, 1, 2, 2, 2, 2, 2, 2, 1}

    Dim As UInteger<32> p1 = Val("&H" + Left(key, 8)), _
                        p2 = Val("&H" + Right(key, 8))
    Dim As String * 64 str_key = Bin(p1, 32) + Bin(p2, 32)
    Dim As String * 56 perm_key
    Dim As UInteger<32> i, k, s = 1
    ReDim subkey_k(1 To 16)

    For i = 0 To 55
        perm_key[i] = str_key[pc_1(i) -1]
    Next

    Dim As String cd, c0 = Left(perm_key, 28), d0 = Right(perm_key, 28)
    c0 += c0 : d0 += d0

    For i = 1 To 16
        s += number_of_left_shl(i)
        cd = Mid(c0, s, 28) + Mid(d0, s, 28)
        For k = 0 To 47
            subkey_k(i)[k] = cd[pc_2(k) -1]
        Next
    Next

End Sub

Function encode_64bit_data(message As String) As String

    Dim As UInteger<32> ip(63) =>   _
    {58, 50, 42, 34, 26, 18, 10, 2, _
     60, 52, 44, 36, 28, 20, 12, 4, _
     62, 54, 46, 38, 30, 22, 14, 6, _
     64, 56, 48, 40, 32, 24, 16, 8, _
     57, 49, 41, 33, 25, 17,  9, 1, _
     59, 51, 43, 35, 27, 19, 11, 3, _
     61, 53, 45, 37, 29, 21, 13, 5, _
     63, 55, 47, 39, 31, 23, 15, 7}

    Dim As UInteger<32> ip_inv(63) => _
      {40, 8, 48, 16, 56, 24, 64, 32, _
       39, 7, 47, 15, 55, 23, 63, 31, _
       38, 6, 46, 14, 54, 22, 62, 30, _
       37, 5, 45, 13, 53, 21, 61, 29, _
       36, 4, 44, 12, 52, 20, 60, 28, _
       35, 3, 43, 11, 51, 19, 59, 27, _
       34, 2, 42, 10, 50, 18, 58, 26, _
       33, 1, 41,  9, 49, 17, 57, 25}

    Dim As UInteger<32> e(47) => _
        {32,  1,  2,  3,  4,  5, _
          4,  5,  6,  7,  8,  9, _
          8,  9, 10, 11, 12, 13, _
         12, 13, 14, 15, 16, 17, _
         16, 17, 18, 19, 20, 21, _
         20, 21, 22, 23, 24, 25, _
         24, 25, 26, 27, 28, 29, _
         28, 29, 30, 31, 32,  1}

    Dim As UInteger<32> p(31) => _
                {16,  7, 20, 21, _
                 29, 12, 28, 17, _
                  1, 15, 23, 26, _
                  5, 18, 31, 10, _
                  2,  8, 24, 14, _
                 32, 27,  3,  9, _
                 19, 13, 30,  6, _
                 22, 11,  4, 25}

    Dim As UInteger<32> S1_8(7, 63) => _
    {{14,  4, 13,  1,  2, 15, 11,  8,  3, 10,  6, 12,  5,  9,  0,  7,  _  's1
       0, 15,  7,  4, 14,  2, 13,  1, 10,  6, 12, 11,  9,  5,  3,  8,  _
       4,  1, 14,  8, 13,  6,  2, 11, 15, 12,  9,  7,  3, 10,  5,  0,  _
      15, 12,  8,  2,  4,  9,  1,  7,  5, 11,  3, 14, 10,  0,  6, 13}, _
_
     {15,  1,  8, 14,  6, 11,  3,  4,  9,  7,  2, 13, 12,  0,  5, 10,  _  's2
       3, 13,  4,  7, 15,  2,  8, 14, 12,  0,  1, 10,  6,  9, 11,  5,  _
       0, 14,  7, 11, 10,  4, 13,  1,  5,  8, 12,  6,  9,  3,  2, 15,  _
      13,  8, 10,  1,  3, 15,  4,  2, 11,  6,  7, 12,  0,  5, 14,  9}, _
_
     {10,  0,  9, 14,  6,  3, 15,  5,  1, 13, 12,  7, 11,  4,  2,  8,  _  's3
      13,  7,  0,  9,  3,  4,  6, 10,  2,  8,  5, 14, 12, 11, 15,  1,  _
      13,  6,  4,  9,  8, 15,  3,  0, 11,  1,  2, 12,  5, 10, 14,  7,  _
       1, 10, 13,  0,  6,  9,  8,  7,  4, 15, 14,  3, 11,  5,  2, 12}, _
 _
         {7, 13, 14, 3,  0,  6,  9, 10,  1, 2, 8,  5, 11, 12,  4, 15,  _  's4
         13,  8, 11, 5,  6, 15,  0,  3,  4, 7, 2, 12,  1, 10, 14,  9,  _
         10,  6,  9, 0, 12, 11,  7, 13, 15, 1, 3, 14,  5,  2,  8,  4,  _
          3, 15,  0, 6, 10,  1, 13,  8,  9, 4, 5, 11, 12,  7,  2, 14}, _
 _
       {2, 12,  4,  1,  7, 10, 11,  6,  8,  5,  3, 15, 13, 0, 14,  9,  _  's5
       14, 11,  2, 12,  4,  7, 13,  1,  5,  0, 15, 10,  3, 9,  8,  6,  _
        4,  2,  1, 11, 10, 13,  7,  8, 15,  9, 12,  5,  6, 3,  0, 14,  _
       11,  8, 12,  7,  1, 14,  2, 13,  6, 15,  0,  9, 10, 4,  5,  3}, _
 _
      {12,  1, 10, 15, 9,  2,  6,  8,  0, 13,  3,  4, 14,  7,  5, 11,  _  's6
       10, 15,  4,  2, 7, 12,  9,  5,  6,  1, 13, 14,  0, 11,  3,  8,  _
        9, 14, 15,  5, 2,  8, 12,  3,  7,  0,  4, 10,  1, 13, 11,  6,  _
        4,  3,  2, 12, 9,  5, 15, 10, 11, 14,  1,  7,  6,  0,  8, 13}, _
 _
         {4, 11,  2, 14, 15, 0,  8, 13,  3, 12, 9,  7,  5, 10, 6,  1,  _  's7
         13,  0, 11,  7,  4, 9,  1, 10, 14,  3, 5, 12,  2, 15, 8,  6,  _
          1,  4, 11, 13, 12, 3,  7, 14, 10, 15, 6,  8,  0,  5, 9,  2,  _
          6, 11, 13,  8,  1, 4, 10,  7,  9,  5, 0, 15, 14,  2, 3, 12}, _
 _
      {13,  2,  8, 4,  6, 15, 11,  1, 10,  9,  3, 14,  5,  0, 12,  7,  _  's8
        1, 15, 13, 8, 10,  3,  7,  4, 12,  5,  6, 11,  0, 14,  9,  2,  _
        7, 11,  4, 1,  9, 12, 14,  2,  0,  6, 10, 13, 15,  3,  5,  8,  _
        2,  1, 14, 7,  4, 10,  8, 13, 15, 12,  9,  0,  3,  5,  6, 11}}

    Dim As UInteger<32> i, i1, k, tmp1, tmp2, tmp3
    tmp1 = Val("&H"+ Left(message,8)) : tmp2 = Val("&H" + Right(message,8))
    Dim As String * 64 Str1, str_message = Bin(tmp1, 32) + Bin(tmp2, 32)
    Dim As String * 48 temp
    Dim As String * 32 l(16), r(16), Str2
    Dim As String Str3

    For i = 0 To 63
        Str1[i] = str_message[ip(i) -1]
    Next

    l(0) = Left(Str1, 32)
    r(0) = Right(Str1, 32)

    For i = 1 To 16
        i1 = i -1
        l(i) = r(i1)

        For k = 0 To 47
            temp[k] = ((subkey_k(i)[k] + r(i1)[e(k) -1]) And 1) + Asc("0")
        Next

        Str3 = ""
        For k = 0 To 42 Step 6
            tmp1 = Val("&B" + Mid(temp, k +1, 1) + Mid(temp, k +6, 1))
            tmp2 = Val("&B" + Mid(temp, k +2, 4))
            Str3 = Str3 + Bin(s1_8(k \ 6 , tmp1 * 16 + tmp2), 4)
        Next

        For k = 0 To 31
            r(i)[k] = ((l(i1)[k] + Str3[p(k) -1]) And 1) + Asc("0")
        Next
    Next

    Str3 = r(16) + l(16)
    For i = 0 To 63
        Str1[i] = Str3[ip_inv(i) -1]
    Next

    tmp1 = Val("&B" + Left(Str1,32))
    tmp2 = Val("&B" + Right(Str1,32))
    Return Hex(tmp1, 8) + Hex(tmp2, 8)

End Function

Function convert(key As String, message As String) As String

    Dim As UInteger<32> i, l
    Dim As String answer, crypto

    If Len(key) = 16 Then
        make_subkeys(key)
        If code2 = "decode" Then
            For i = 1 To 8
                Swap subkey_k(i), subkey_k(16 +1 -i)
            Next
        End If
    Else
        Beep
        Print "wrong key length, program stops"
        End
    End If

    Do Until InStr(message, " ") = 0
        i = InStr(message, " ")
        message = Left(message, i -1) + Mid(message, i +1)
    Loop

    l = Len(message) \ 16
    If Len(message) Mod 16 <> 0 Then
        l += 1
        message = Left((message + String(16, "0")), l * 16)
    End If

    For i = 0 To l -1
        answer = encode_64bit_data(Mid(message, i * 16 +1, 16))
        crypto = crypto + answer
    Next

    If code2 = "decode" And InStrRev(answer, "0D0A0") <> 0 Then
        crypto = RTrim(crypto, "0")
    End If

    Return crypto

End Function

Function encode(key As String, message As String) As String

    code2 = "encode"
    Return convert(key, message)

End Function

Function decode(key As String, message As String) As String

    code2 = "decode"
    Return convert(key, message)

End Function

' ------=< MAIN >=------

Dim As String key, message, crypto, answer

message = "0123456789ABCDEF"
key = "133457799BBCDFF1"
Print "    key "; key
Print "   text "; message
answer = encode(key, message)
Print "encoded "; answer
answer = decode(key, answer)
Print "decoded "; answer
Print

message = "8787878787878787"
key = "0E329232EA6D0D73"
Print "    key "; key
Print "   text "; message
answer = encode(key, message)
Print "encoded "; answer
answer = decode(key, answer)
Print "decoded "; answer
Print

message = "596F7572206C6970 732061726520736D 6F6F746865722074" + _
          " 68616E2076617365 6C696E650D0A"
key = "0E329232EA6D0D73"
Print "    key "; key
Print "   text "; message
answer = encode(key, message)
Print "encoded "; answer
answer = decode(key, answer)
Print "decoded "; answer

' empty keyboard buffer
While Inkey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

{{out}}

```txt
    key 133457799BBCDFF1
   text 0123456789ABCDEF
encoded 85E813540F0AB405
decoded 0123456789ABCDEF

    key 0E329232EA6D0D73
   text 8787878787878787
encoded 0000000000000000
decoded 8787878787878787

    key 0E329232EA6D0D73
   text 596F7572206C6970 732061726520736D 6F6F746865722074 68616E2076617365 6C696E650D0A
encoded C0999FDDE378D7ED727DA00BCA5A84EE47F269A4D6438190D9D52F78F5358499828AC9B453E0E653
decoded 596F7572206C6970732061726520736D6F6F74686572207468616E20766173656C696E650D0A
```



## Go

'''Library solution:'''

```go
package main

import (
    "crypto/des"
    "encoding/hex"
    "fmt"
    "log"
)

func main() {
    key, err := hex.DecodeString("0e329232ea6d0d73")
    if err != nil {
        log.Fatal(err)
    }
    c, err := des.NewCipher(key)
    if err != nil {
        log.Fatal(err)
    }
    src, err := hex.DecodeString("8787878787878787")
    if err != nil {
        log.Fatal(err)
    }
    dst := make([]byte, des.BlockSize)
    c.Encrypt(dst, src)
    fmt.Printf("%x\n", dst)
}
```

{{out}}

```txt

0000000000000000

```



## Java

{{trans|Kotlin}}

```Java
import javax.crypto.Cipher;
import javax.crypto.spec.SecretKeySpec;

public class DataEncryptionStandard {
    private static byte[] toHexByteArray(String self) {
        byte[] bytes = new byte[self.length() / 2];
        for (int i = 0; i < bytes.length; ++i) {
            bytes[i] = ((byte) Integer.parseInt(self.substring(i * 2, i * 2 + 2), 16));
        }
        return bytes;
    }

    private static void printHexBytes(byte[] self, String label) {
        System.out.printf("%s: ", label);
        for (byte b : self) {
            int bb = (b >= 0) ? ((int) b) : b + 256;
            String ts = Integer.toString(bb, 16);
            if (ts.length() < 2) {
                ts = "0" + ts;
            }
            System.out.print(ts);
        }
        System.out.println();
    }

    public static void main(String[] args) throws Exception {
        String strKey = "0e329232ea6d0d73";
        byte[] keyBytes = toHexByteArray(strKey);
        SecretKeySpec key = new SecretKeySpec(keyBytes, "DES");
        Cipher encCipher = Cipher.getInstance("DES");
        encCipher.init(Cipher.ENCRYPT_MODE, key);
        String strPlain = "8787878787878787";
        byte[] plainBytes = toHexByteArray(strPlain);
        byte[] encBytes = encCipher.doFinal(plainBytes);
        printHexBytes(encBytes, "Encoded");

        Cipher decCipher = Cipher.getInstance("DES");
        decCipher.init(Cipher.DECRYPT_MODE, key);
        byte[] decBytes = decCipher.doFinal(encBytes);
        printHexBytes(decBytes, "Decoded");
    }
}
```

{{out}}

```txt
Encoded: 0000000000000000a913f4cb0bd30f97
Decoded: 8787878787878787
```



## Kotlin

===Version 1 (using library functions)===
Presumably, one can use library functions to demonstrate DES as it would be very tedious to implement it from scratch: 

```scala
// version 1.1.3

import javax.crypto.Cipher
import javax.crypto.spec.SecretKeySpec

fun String.toHexByteArray(): ByteArray {
    val bytes = ByteArray(this.length / 2)
    for (i in 0 until bytes.size) {
        bytes[i] = this.substring(i * 2, i * 2 + 2).toInt(16).toByte()       
    }
    return bytes
}

fun ByteArray.printHexBytes(label: String) {
    print("$label: ")
    for (b in this) {
        val bb = if (b >= 0) b.toInt() else b + 256
        print(bb.toString(16).padStart(2, '0'))
    }
    println()
}

fun main(args: Array<String>) {
    val strKey = "0e329232ea6d0d73"
    val keyBytes = strKey.toHexByteArray()
    val key = SecretKeySpec(keyBytes, "DES") 
    val encCipher = Cipher.getInstance("DES")      
    encCipher.init(Cipher.ENCRYPT_MODE, key)    
    val strPlain = "8787878787878787"
    val plainBytes = strPlain.toHexByteArray()
    val encBytes = encCipher.doFinal(plainBytes)
    encBytes.printHexBytes("Encoded")

    val decCipher = Cipher.getInstance("DES")
    decCipher.init(Cipher.DECRYPT_MODE, key)
    val decBytes = decCipher.doFinal(encBytes)
    decBytes.printHexBytes("Decoded")
}
```


{{out}}
Note that the 'encoded' output includes 8 bytes of padding using the default JVM DES implementation:

```txt

Encoded: 0000000000000000a913f4cb0bd30f97
Decoded: 8787878787878787

```

===Version 2 (from scratch)===
It wasn't as tedious as I expected due to the admirably clear article linked to above:

```scala
// version 1.1.3

import java.util.BitSet

object DES {

    private val PC1 = intArrayOf(
        57, 49, 41, 33, 25, 17, 9,
        1, 58, 50, 42, 34, 26, 18,
        10, 2, 59, 51, 43, 35, 27,
        19, 11, 3, 60, 52, 44, 36,
        63, 55, 47, 39, 31, 23, 15,
        7, 62, 54, 46, 38, 30, 22,
        14, 6, 61, 53, 45, 37, 29,
        21, 13, 5, 28, 20, 12, 4
    )

    private val PC2 = intArrayOf(
        14, 17, 11, 24, 1, 5,
        3, 28, 15, 6, 21, 10,
        23, 19, 12, 4, 26, 8,
        16, 7, 27, 20, 13, 2,
        41, 52, 31, 37, 47, 55,
        30, 40, 51, 45, 33, 48,
        44, 49, 39, 56, 34, 53,
        46, 42, 50, 36, 29, 32
    )

    private val IP = intArrayOf(
        58, 50, 42, 34, 26, 18, 10, 2,
        60, 52, 44, 36, 28, 20, 12, 4,
        62, 54, 46, 38, 30, 22, 14, 6,
        64, 56, 48, 40, 32, 24, 16, 8,
        57, 49, 41, 33, 25, 17, 9, 1,
        59, 51, 43, 35, 27, 19, 11, 3,
        61, 53, 45, 37, 29, 21, 13, 5,
        63, 55, 47, 39, 31, 23, 15, 7
    )

    private val E = intArrayOf(
        32, 1, 2, 3, 4, 5,
        4, 5, 6, 7, 8, 9,
        8, 9, 10, 11, 12, 13,
        12, 13, 14, 15, 16, 17,
        16, 17, 18, 19, 20, 21,
        20, 21, 22, 23, 24, 25,
        24, 25, 26, 27, 28, 29,
        28, 29, 30, 31, 32, 1
    )

    private val S = arrayOf(
        intArrayOf(
            14, 4, 13, 1, 2, 15, 11, 8, 3, 10, 6, 12, 5, 9, 0, 7,
            0, 15, 7, 4, 14, 2, 13, 1, 10, 6, 12, 11, 9, 5, 3, 8,
            4, 1, 14, 8, 13, 6, 2, 11, 15, 12, 9, 7, 3, 10, 5, 0,
            15, 12, 8, 2, 4, 9, 1, 7, 5, 11, 3, 14, 10, 0, 6, 13
        ),

        intArrayOf(
            15, 1, 8, 14, 6, 11, 3, 4, 9, 7, 2, 13, 12, 0, 5, 10,
            3, 13, 4, 7, 15, 2, 8, 14, 12, 0, 1, 10, 6, 9, 11, 5,
            0, 14, 7, 11, 10, 4, 13, 1, 5, 8, 12, 6, 9, 3, 2, 15,
            13, 8, 10, 1, 3, 15, 4, 2, 11, 6, 7, 12, 0, 5, 14, 9
        ),

        intArrayOf(
            10, 0, 9, 14, 6, 3, 15, 5, 1, 13, 12, 7, 11, 4, 2, 8,
            13, 7, 0, 9, 3, 4, 6, 10, 2, 8, 5, 14, 12, 11, 15, 1,
            13, 6, 4, 9, 8, 15, 3, 0, 11, 1, 2, 12, 5, 10, 14, 7,
            1, 10, 13, 0, 6, 9, 8, 7, 4, 15, 14, 3, 11, 5, 2, 12
        ),

        intArrayOf(
            7, 13, 14, 3, 0, 6, 9, 10, 1, 2, 8, 5, 11, 12, 4, 15,
            13, 8, 11, 5, 6, 15, 0, 3, 4, 7, 2, 12, 1, 10, 14, 9,
            10, 6, 9, 0, 12, 11, 7, 13, 15, 1, 3, 14, 5, 2, 8, 4,
            3, 15, 0, 6, 10, 1, 13, 8, 9, 4, 5, 11, 12, 7, 2, 14
        ),

        intArrayOf(
            2, 12, 4, 1, 7, 10, 11, 6, 8, 5, 3, 15, 13, 0, 14, 9,
            14, 11, 2, 12, 4, 7, 13, 1, 5, 0, 15, 10, 3, 9, 8, 6,
            4, 2, 1, 11, 10, 13, 7, 8, 15, 9, 12, 5, 6, 3, 0, 14,
            11, 8, 12, 7, 1, 14, 2, 13, 6, 15, 0, 9, 10, 4, 5, 3
        ),

        intArrayOf(
            12, 1, 10, 15, 9, 2, 6, 8, 0, 13, 3, 4, 14, 7, 5, 11,
            10, 15, 4, 2, 7, 12, 9, 5, 6, 1, 13, 14, 0, 11, 3, 8,
            9, 14, 15, 5, 2, 8, 12, 3, 7, 0, 4, 10, 1, 13, 11, 6,
            4, 3, 2, 12, 9, 5, 15, 10, 11, 14, 1, 7, 6, 0, 8, 13
        ),

        intArrayOf(
            4, 11, 2, 14, 15, 0, 8, 13, 3, 12, 9, 7, 5, 10, 6, 1,
            13, 0, 11, 7, 4, 9, 1, 10, 14, 3, 5, 12, 2, 15, 8, 6,
            1, 4, 11, 13, 12, 3, 7, 14, 10, 15, 6, 8, 0, 5, 9, 2,
            6, 11, 13, 8, 1, 4, 10, 7, 9, 5, 0, 15, 14, 2, 3, 12
        ),

        intArrayOf(
            13, 2, 8, 4, 6, 15, 11, 1, 10, 9, 3, 14, 5, 0, 12, 7,
            1, 15, 13, 8, 10, 3, 7, 4, 12, 5, 6, 11, 0, 14, 9, 2,
            7, 11, 4, 1, 9, 12, 14, 2, 0, 6, 10, 13, 15, 3, 5, 8,
            2, 1, 14, 7, 4, 10, 8, 13, 15, 12, 9, 0, 3, 5, 6, 11
        )
    )

    private val P = intArrayOf(
        16, 7, 20, 21,
        29, 12, 28, 17,
        1, 15, 23, 26,
        5, 18, 31, 10,
        2, 8, 24, 14,
        32, 27, 3, 9,
        19, 13, 30, 6,
        22, 11, 4, 25
    )

    private val IP2 = intArrayOf(
        40, 8, 48, 16, 56, 24, 64, 32,
        39, 7, 47, 15, 55, 23, 63, 31,
        38, 6, 46, 14, 54, 22, 62, 30,
        37, 5, 45, 13, 53, 21, 61, 29,
        36, 4, 44, 12, 52, 20, 60, 28,
        35, 3, 43, 11, 51, 19, 59, 27,
        34, 2, 42, 10, 50, 18, 58, 26,
        33, 1, 41, 9, 49, 17, 57, 25
    )

    private val SHIFTS = intArrayOf(1, 1, 2, 2, 2, 2, 2, 2, 1, 2, 2, 2, 2, 2, 2, 1)

    fun encrypt(key: String, message: String): String {
        val ks = getSubKeys(key)
        var m = message
        val r = m.length % 16 // check if multiple of 16 hex digits
        val rem = 8 - r / 2
        val remStr = "%02X".format(rem)
        for (i in 1..rem) {
            m += remStr
        }
        assert(m.length % 16 == 0)

        val sb = StringBuilder()
        for (i in 0 until m.length / 16) {
            val j = i * 16
            val enc = processMessage(m.substring(j, j + 16), ks)
            sb.append(enc)
        }
        return sb.toString()
    }

    fun decrypt(key: String, encoded: String): String {
        val ks = getSubKeys(key)
        // reverse the subkeys
        for (i in 1..8) {
            val temp = ks[i]
            ks[i] = ks[17 - i]
            ks[17 - i] = temp
        }
        val sb = StringBuilder()
        for (i in 0 until encoded.length / 16) {
            val j = i * 16
            val dec = processMessage(encoded.substring(j, j + 16), ks)
            sb.append(dec)
        }
        //remove the padding
        val padByte = sb[sb.length - 1] - '0'
        return sb.substring(0, sb.length - 2 * padByte)
    }

    private fun getSubKeys(key: String): Array<BitSet> {
        val k = key.toLittleEndianBitSet()

        // permute 'key' using table PC1
        val kp = BitSet(56)
        for (i in 0..55) kp[i] = k[PC1[i] - 1]

        // split 'kp' in half and process the resulting series of 'c' and 'd'
        val c = Array(17) { BitSet(56) }
        val d = Array(17) { BitSet(28) }
        for (i in 0..27) c[0][i] = kp[i]
        for (i in 0..27) d[0][i] = kp[i + 28]
        for (i in 1..16) {
            c[i - 1].shiftLeft(SHIFTS[i - 1], 28, c[i])
            d[i - 1].shiftLeft(SHIFTS[i - 1], 28, d[i])
        }

        // merge 'd' into 'c'
        for (i in 1..16) {
            for (j in 28..55) c[i][j] = d[i][j - 28]
        }

        // form the sub-keys and store them in 'ks'
        val ks = Array(17) { BitSet(48) }

        // permute 'c' using table PC2
        for (i in 1..16) {
            for (j in 0..47) ks[i][j] = c[i][PC2[j] - 1]
        }

        return ks
    }

    private fun processMessage(message: String, ks: Array<BitSet>): String {
        val m = message.toLittleEndianBitSet()

        // permute 'message' using table IP
        val mp = BitSet(64)
        for (i in 0..63) {
            mp[i] = m[IP[i] - 1]
        }

        // split 'mp' in half and process the resulting series of 'l' and 'r
        val l = Array(17) { BitSet(32) }
        val r = Array(17) { BitSet(32) }
        for (i in 0..31) l[0][i] = mp[i]
        for (i in 0..31) r[0][i] = mp[i + 32]
        for (i in 1..16) {
            l[i] = r[i - 1]
            val fs = f(r[i - 1], ks[i])
            l[i - 1].xor(fs)
            r[i] = l[i - 1]
        }

        // amalgamate r[16] and l[16] (in that order) into 'e'
        val e = BitSet(64)
        for (i in 0..31) e[i] = r[16][i]
        for (i in 32..63) e[i] = l[16][i - 32]

        // permute 'e' using table IP2 ad return result as a hex string
        val ep = BitSet(64)
        for (i in 0..63) ep[i] = e[IP2[i] - 1]
        return ep.toHexString(64)
    }

    /* assumes a hex string receiver */
    private fun String.toLittleEndianBitSet(): BitSet {
        val bs = BitSet(this.length * 4)
        for ((i, c) in this.withIndex()) {
            val s = c.toString().toByte(16).toString(2).padStart(4, '0')
            for (j in 0..3) bs[i * 4 + j] = (s[j] == '1')
        }
        return bs
    }

    /* assumes a little-endian bitset receiver */
    private fun BitSet.toHexString(len: Int): String {
        val size = len / 4
        val sb = StringBuilder(size)
        val ba = ByteArray(4)
        for (i in 0 until size) {
            for (j in 0..3) ba[j] = if (this[i * 4 + j]) 1 else 0
            val c = "%X".format(ba[0] * 8 + ba[1] * 4 + ba[2] * 2 + ba[3])
            sb.append(c)
        }
        return sb.toString()
    }

    private fun BitSet.shiftLeft(times: Int, len: Int, out: BitSet) {
        for (i in 0 until len) out[i] = this[i]
        for (t in 1..times) {
            val temp = out[0]
            for (i in 1 until len) out[i - 1] = out[i]
            out[len - 1] = temp
        }
    }

    private fun f(r: BitSet, ks: BitSet): BitSet {
        // permute 'r' using table E
        val er = BitSet(48)
        for (i in 0..47) er[i] = r[E[i] - 1]

        // xor 'er' with 'ks' and store back into 'er'
        er.xor(ks)

        // process 'er' six bits at a time and store resulting four bits in 'sr'
        val sr = BitSet(32)
        for (i in 0..7) {
            val j = i * 6
            val b = IntArray(6)
            for (k in 0..5) b[k] = if (er[j + k]) 1 else 0
            val row = 2 * b[0] + b[5]
            val col = 8 * b[1] + 4 * b[2] + 2 * b[3] + b[4]
            var m = S[i][row * 16 + col]   // apply table S
            var n = 1
            while (m > 0) {
                val p = m % 2
                sr[(i + 1) * 4 - n] = (p == 1)
                m /= 2
                n++
            }
        }

        // permute sr using table P
        val sp = BitSet(32)
        for (i in 0..31) sp[i] = sr[P[i] - 1]
        return sp
    }
}

fun main(args: Array<String>) {
    val keys = listOf("133457799BBCDFF1", "0E329232EA6D0D73", "0E329232EA6D0D73")
    val messages = listOf(
        "0123456789ABCDEF",
        "8787878787878787",
        "596F7572206C6970732061726520736D6F6F74686572207468616E20766173656C696E650D0A"
    )
    for (i in 0..2) {
        println("Key     : ${keys[i]}")
        println("Message : ${messages[i]}")
        val encoded = DES.encrypt(keys[i], messages[i])
        println("Encoded : $encoded")
        val decoded = DES.decrypt(keys[i], encoded)
        println("Decoded : $decoded")
        println()
    }
}
```

{{out}}

```txt
Key     : 133457799BBCDFF1
Message : 0123456789ABCDEF
Encoded : 85E813540F0AB405FDF2E174492922F8
Decoded : 0123456789ABCDEF

Key     : 0E329232EA6D0D73
Message : 8787878787878787
Encoded : 0000000000000000A913F4CB0BD30F97
Decoded : 8787878787878787

Key     : 0E329232EA6D0D73
Message : 596F7572206C6970732061726520736D6F6F74686572207468616E20766173656C696E650D0A
Encoded : C0999FDDE378D7ED727DA00BCA5A84EE47F269A4D6438190D9D52F78F53584997F922CCB5B068D99
Decoded : 596F7572206C6970732061726520736D6F6F74686572207468616E20766173656C696E650D0A
```


=={{header|Modula-2}}==

```modula2
MODULE DataEncryptionStandard;
FROM SYSTEM IMPORT BYTE,ADR;
FROM DES IMPORT DES,Key1,Create,Destroy,EncryptECB,DecryptECB;
FROM FormatString IMPORT FormatString;
FROM Terminal IMPORT WriteString,WriteLn,ReadChar;

PROCEDURE PrintHexBytes(str : ARRAY OF BYTE; limit : INTEGER);
VAR
    buf : ARRAY[0..7] OF CHAR;
    i,v : INTEGER;
BEGIN
    i := 0;
    WHILE i<limit DO
        v := ORD(str[i]);
        IF v < 16 THEN
            WriteString("0")
        END;
        FormatString("%h", buf, v);
        WriteString(buf);
        INC(i);
    END
END PrintHexBytes;

TYPE BA = ARRAY[0..15] OF BYTE;
VAR
    plain,encrypt : BA;
    key : ARRAY[0..0] OF Key1;
    cipher : DES;
BEGIN
    (* Account for the padding *)
    plain := BA{87H, 87H, 87H, 87H, 87H, 87H, 87H, 87H, 8, 8, 8, 8, 8, 8, 8, 8};

    key[0] := Key1{0EH, 32H, 92H, 32H, 0EAH, 6DH, 0DH, 73H};
    cipher := Create(key);

    WriteString("plain:   ");
    PrintHexBytes(plain, 8);
    WriteLn;

    EncryptECB(cipher,ADR(plain),ADR(encrypt),16);

    WriteString("encrypt: ");
    PrintHexBytes(encrypt, 16);
    WriteLn;

    DecryptECB(cipher,ADR(encrypt),ADR(plain),16);

    WriteString("plain:   ");
    PrintHexBytes(plain, 8);
    WriteLn;

    Destroy(cipher);
    ReadChar
END DataEncryptionStandard.
```

{{out}}

```txt
plain:   8787878787878787
encrypt: 0000000000000000A913F4CB0BD30F97
plain:   8787878787878787
```



## Perl 6

This is mainly a translation from the Phix entry, with an additional example on UTF-8.  Regarding the many conversions among different number/string formats, small (and hopefully reusable ) helper routines are created to serve the purpose.
Update 20190323: After a bug fixed an example does behave correctly and is now in line with the results from the C, D, Kotlin and Phix entries.  By the way it seems ''.comb'' handle "\r\n" inconsistently, why? [https://pastebin.com/d7dBpYkL]
Update 20190325: Thanks to SqrtNegInf for pointing out that the answer is already in the documentation.[https://docs.perl6.org/type/Str#routine_chomp], [https://docs.perl6.org/language/newline]
{{trans|Phix}}

```perl6
#!/usr/bin/env perl6
 
use v6.d;
use experimental :pack;
 
my \PC1 = <
   57 49 41 33 25 17  9        1 58 50 42 34 26 18
   10  2 59 51 43 35 27       19 11  3 60 52 44 36
   63 55 47 39 31 23 15        7 62 54 46 38 30 22
   14  6 61 53 45 37 29       21 13  5 28 20 12  4
>; # Permuted choice 1 (PC-1) - Parity Drop Table
 
my \PC2 = <
   14 17 11 24  1  5  3 28    15  6 21 10 23 19 12  4
   26  8 16  7 27 20 13  2    41 52 31 37 47 55 30 40
   51 45 33 48 44 49 39 56    34 53 46 42 50 36 29 32
>; # Permuted choice 2 (PC-2) - Key Compression Table
 
my \IP = <
   58 50 42 34 26 18 10  2    60 52 44 36 28 20 12  4
   62 54 46 38 30 22 14  6    64 56 48 40 32 24 16  8
   57 49 41 33 25 17  9  1    59 51 43 35 27 19 11  3
   61 53 45 37 29 21 13  5    63 55 47 39 31 23 15  7
>; # Initial permutation (IP)
 
my \IP2 = <
   40 8 48 16 56 24 64 32     39 7 47 15 55 23 63 31
   38 6 46 14 54 22 62 30     37 5 45 13 53 21 61 29
   36 4 44 12 52 20 60 28     35 3 43 11 51 19 59 27
   34 2 42 10 50 18 58 26     33 1 41  9 49 17 57 25
>; # Final permutation (IP⁻¹)
 
my \S = ( <
   14 4 13 1 2 15 11 8 3 10 6 12 5 9 0 7   0 15 7 4 14 2 13 1 10 6 12 11 9 5 3 8
   4 1 14 8 13 6 2 11 15 12 9 7 3 10 5 0   15 12 8 2 4 9 1 7 5 11 3 14 10 0 6 13
> , <
   15 1 8 14 6 11 3 4 9 7 2 13 12 0 5 10   3 13 4 7 15 2 8 14 12 0 1 10 6 9 11 5
   0 14 7 11 10 4 13 1 5 8 12 6 9 3 2 15   13 8 10 1 3 15 4 2 11 6 7 12 0 5 14 9
> , <
   10 0 9 14 6 3 15 5 1 13 12 7 11 4 2 8   13 7 0 9 3 4 6 10 2 8 5 14 12 11 15 1
   13 6 4 9 8 15 3 0 11 1 2 12 5 10 14 7   1 10 13 0 6 9 8 7 4 15 14 3 11 5 2 12
> , <
   7 13 14 3 0 6 9 10 1 2 8 5 11 12 4 15   13 8 11 5 6 15 0 3 4 7 2 12 1 10 14 9
   10 6 9 0 12 11 7 13 15 1 3 14 5 2 8 4   3 15 0 6 10 1 13 8 9 4 5 11 12 7 2 14
> , <
   2 12 4 1 7 10 11 6 8 5 3 15 13 0 14 9   14 11 2 12 4 7 13 1 5 0 15 10 3 9 8 6
   4 2 1 11 10 13 7 8 15 9 12 5 6 3 0 14   11 8 12 7 1 14 2 13 6 15 0 9 10 4 5 3
> , <
   12 1 10 15 9 2 6 8 0 13 3 4 14 7 5 11   10 15 4 2 7 12 9 5 6 1 13 14 0 11 3 8
   9 14 15 5 2 8 12 3 7 0 4 10 1 13 11 6   4 3 2 12 9 5 15 10 11 14 1 7 6 0 8 13
> , <
   4 11 2 14 15 0 8 13 3 12 9 7 5 10 6 1   13 0 11 7 4 9 1 10 14 3 5 12 2 15 8 6
   1 4 11 13 12 3 7 14 10 15 6 8 0 5 9 2   6 11 13 8 1 4 10 7 9 5 0 15 14 2 3 12
> , <
   13 2 8 4 6 15 11 1 10 9 3 14 5 0 12 7   1 15 13 8 10 3 7 4 12 5 6 11 0 14 9 2
   7 11 4 1 9 12 14 2 0 6 10 13 15 3 5 8   2 1 14 7 4 10 8 13 15 12 9 0 3 5 6 11
> ); # 8 Substitution boxes, each replaces a 6-bit input with a 4-bit output
 
my \P = <
   16 7 20 21   29 12 28 17     1 15 23 26     5 18 31 10
    2 8 24 14   32 27  3  9    19 13 30  6    22 11  4 25
>; # Permutation (P), shuffles the bits of a 32-bit half-block
 
# Expansion function (E), expand 32-bit half-block to 48 bits
my \E = flat 32,1..5,4..9,8..13,12..17,16..21,20..25,24..29,28..32,1;
 
my \SHIFTS = < 1 1 2 2 2 2 2 2 1 2 2 2 2 2 2 1 >; # schedule of left shifts
 
## Helper subs
 
# convert iso-8859-1 to hexadecimals
sub b2h (\b) { [~] map { .encode('iso-8859-1').unpack('H*') }, b.comb };
 
# convert UTF8s to bytes
sub u2b (\u) { [~] map { .chr }, @( [~] map { .encode('utf8') }, u.comb) };
 
# convert hexadecimals to UTF-8
sub h2u (\h) { pack("H" x h.chars/2, h ~~ m:g/../).decode('utf8') };
 
# convert quadbits to hex
sub q2h (\q) { [~] map { :2($_.Str).fmt('%X') }, q ~~ m:g/..../ };
 
# convert every two quadbits to bytes
sub q2b (\q) { map { :2($_.Str) }, q ~~ m:g/. ** 8/ };
 
# trun a 16 digit hexadecimal str to a 64 bits list
sub h2bits (\h) { ([~] map { :16($_).base(2).fmt('%04s') }, h.comb).split("")[1..64] };
 
sub infix:<⥀>(\a is copy, \b) { a.append: a.shift for ^b ; a } # XOR addition
 
# convert hexadecimals to bytes
sub h2bytes (\h) { [~] map { :16($_.Str).chr }, h ~~ m:g/../ };
 
# s is 16 digit hexadecimal str, M is a permuation matrix/vector
sub map64(\s, \M) { my \b = h2bits s; map { b[$_-1] }, M; }
 
## Core subs
 
sub get_subkeys(Str \key --> Seq) { # return a Seq with 16 bit vectors
   my \Kₚ = map64 key, PC1; # drop parity bits
   my @C = Kₚ[0..27] ; my @D = Kₚ[28..55]; # perform bits rotation next
   my \CD = map { [ flat @C ⥀= SHIFTS[$_], @D ⥀= SHIFTS[$_] ]}, ^16;
   return map { map { CD[$_][PC2[$^a]-1] }, ^48 }, ^16; # key compression rounds
}
 
sub ƒ (List \R is copy, Seq \Kₙ is copy --> Seq) {
   my @er = map { Kₙ[$_] +^ R[E[$_]-1] }, ^48;
   my @sr = flat map { # Sₙ(Bₙ) loop, process @er six bits at a time
      ((S.[$_][([~] @er[$_*6,$_*6+5]).parse-base(2)*16+([~]
         @er[$_*6+1 .. $_*6+4]).parse-base(2)]).fmt('%04b').split(""))[1..4]
   }, ^8;
   return map { @sr[$_-1] }, P;
}
 
sub process_block(Str \message, \K is copy --> Str) { # return 8 quadbits
   my \mp = map64 (b2h message) , IP; # turn message to hex then map to bits
   my @L = mp[0..31]; my @R = mp[32..63];
   my (@Lₙ , @Rₙ); # then apply 16 iterations with function ƒ
   { @Lₙ = @R; @Rₙ = @L Z+^ ƒ @R, K[$_]; @L = @Lₙ; @R = @Rₙ } for ^16;
   my \res = flat @R, @L; # reverse and join the final L₁₆ and R₁₆
   return [~] map { res[$_-1] }, IP2 ; # inverse of the initial permutation
}
 
sub des(Str \key, Str $msg is copy, Bool \DECODE --> Str) { # return hexdecimal
   my @K; my \length = $msg.encode('iso-8859-1').bytes;
   if ( DECODE and length % 8 ) { # early exit, avoid the subkeys computation
      die "Message must be in multiples of 8 bytes"
   } else { 
      @K = DECODE ?? reverse get_subkeys key !! get_subkeys key  
   }
   {  my \P = 8 - length % 8; # number of pad bytes
      $msg ~= P.chr x P ; # CMS style padding as per RFC 1423 & RFC 5652
   } unless DECODE;

   my $quad ~= process_block substr($msg,$_,8), @K for 
      0, 8 … $msg.encode('iso-8859-1').bytes-8;

   {  my @decrypt = q2b $quad; # quadbits to a byte code point list
      @decrypt.pop xx @decrypt.tail; # remove padding
      return b2h ( [~] map { .chr } , @decrypt )
   } if DECODE ;
 
   return q2h $quad
}
 
say "Encryption examples: ";
say des "133457799BBCDFF1", h2bytes("0123456789ABCDEF"), False;
say des "0E329232EA6D0D73", h2bytes("8787878787878787"), False;
say des "0E329232EA6D0D73", "Your lips are smoother than vaseline", False;
say des "0E329232EA6D0D73", "Your lips are smoother than vaseline\r\n", False;
say des "0E329232EA6D0D73", u2b("BMP: こんにちは ; Astral plane: 𝒳𝒴𝒵"), False;
 
say "Decryption examples: ";
say des "133457799BBCDFF1", h2bytes("85E813540F0AB405FDF2E174492922F8"), True;
say des "0E329232EA6D0D73", h2bytes("0000000000000000A913F4CB0BD30F97"), True;
say h2bytes des "0E329232EA6D0D73", h2bytes("C0999FDDE378D7ED727DA00BCA5A84EE47F269A4D6438190D9D52F78F535849980A2E7453703513E"), True;
say h2bytes des "0E329232EA6D0D73", h2bytes("C0999FDDE378D7ED727DA00BCA5A84EE47F269A4D6438190D9D52F78F53584997F922CCB5B068D99"), True;
say h2u des "0E329232EA6D0D73", h2bytes("C040FB6A6E72D7C36D60CA9B9A35EB38D3194468AD808103C28E33AEF0B268D0E0366C160B028DDACF340003DCA8969343EBBD289DB94774"), True;
```
 
{{out}}

```txt
Encryption examples:
85E813540F0AB405FDF2E174492922F8
0000000000000000A913F4CB0BD30F97
C0999FDDE378D7ED727DA00BCA5A84EE47F269A4D6438190D9D52F78F535849980A2E7453703513E
C0999FDDE378D7ED727DA00BCA5A84EE47F269A4D6438190D9D52F78F53584997F922CCB5B068D99
C040FB6A6E72D7C36D60CA9B9A35EB38D3194468AD808103C28E33AEF0B268D0E0366C160B028DDACF340003DCA8969343EBBD289DB94774
Decryption examples:
0123456789abcdef
8787878787878787
Your lips are smoother than vaseline
Your lips are smoother than vaseline

BMP: こんにちは ; Astral plane: 𝒳𝒴𝒵
```



## Phix

{{trans|Kotlin}} 
Implementation following the excellent paper by J. Orlin Grabbe, as linked above.
Like Kotlin version 2, this expands values into more manageable bit arrays, which are
easier to debug/verify, probably sidestep a few fiddly endian issues, and certainly 
simplify bit-wise permutations.

```Phix
-- demo\rosetta\Data_Encryption_Standard.exw
constant PC1 = {57, 49, 41, 33, 25, 17,  9,
                 1, 58, 50, 42, 34, 26, 18,
                10,  2, 59, 51, 43, 35, 27,
                19, 11,  3, 60, 52, 44, 36,
                63, 55, 47, 39, 31, 23, 15,
                 7, 62, 54, 46, 38, 30, 22,
                14,  6, 61, 53, 45, 37, 29,
                21, 13,  5, 28, 20, 12,  4},
                
      SHIFTS = {1, 1, 2, 2, 2, 2, 2, 2, 1, 2, 2, 2, 2, 2, 2, 1},

         PC2 = {14, 17, 11, 24,  1,  5,
                 3, 28, 15,  6, 21, 10,
                23, 19, 12,  4, 26,  8,
                16,  7, 27, 20, 13,  2,
                41, 52, 31, 37, 47, 55,
                30, 40, 51, 45, 33, 48,
                44, 49, 39, 56, 34, 53,
                46, 42, 50, 36, 29, 32},

          IP = {58, 50, 42, 34, 26, 18, 10, 2,
                60, 52, 44, 36, 28, 20, 12, 4,
                62, 54, 46, 38, 30, 22, 14, 6,
                64, 56, 48, 40, 32, 24, 16, 8,
                57, 49, 41, 33, 25, 17,  9, 1,
                59, 51, 43, 35, 27, 19, 11, 3,
                61, 53, 45, 37, 29, 21, 13, 5,
                63, 55, 47, 39, 31, 23, 15, 7},

          E = {32,  1,  2,  3,  4,  5,
                4,  5,  6,  7,  8,  9,
                8,  9, 10, 11, 12, 13,
               12, 13, 14, 15, 16, 17,
               16, 17, 18, 19, 20, 21,
               20, 21, 22, 23, 24, 25,
               24, 25, 26, 27, 28, 29,
               28, 29, 30, 31, 32,  1},
 
          S = {{14,  4, 13,  1,  2, 15, 11,  8,  3, 10,  6, 12,  5,  9,  0,  7,
                 0, 15,  7,  4, 14,  2, 13,  1, 10,  6, 12, 11,  9,  5,  3,  8,
                 4,  1, 14,  8, 13,  6,  2, 11, 15, 12,  9,  7,  3, 10,  5,  0,
                15, 12,  8,  2,  4,  9,  1,  7,  5, 11,  3, 14, 10,  0,  6, 13},
               {15,  1,  8, 14,  6, 11,  3,  4,  9,  7,  2, 13, 12,  0,  5, 10,
                 3, 13,  4,  7, 15,  2,  8, 14, 12,  0,  1, 10,  6,  9, 11,  5,
                 0, 14,  7, 11, 10,  4, 13,  1,  5,  8, 12,  6,  9,  3,  2, 15,
                13,  8, 10,  1,  3, 15,  4,  2, 11,  6,  7, 12,  0,  5, 14,  9},
               {10,  0,  9, 14,  6,  3, 15,  5,  1, 13, 12,  7, 11,  4,  2,  8,
                13,  7,  0,  9,  3,  4,  6, 10,  2,  8,  5, 14, 12, 11, 15,  1,
                13,  6,  4,  9,  8, 15,  3,  0, 11,  1,  2, 12,  5, 10, 14,  7,
                 1, 10, 13,  0,  6,  9,  8,  7,  4, 15, 14,  3, 11,  5,  2, 12},
               { 7, 13, 14,  3,  0,  6,  9, 10,  1,  2,  8,  5, 11, 12,  4, 15,
                13,  8, 11,  5,  6, 15,  0,  3,  4,  7,  2, 12,  1, 10, 14,  9,
                10,  6,  9,  0, 12, 11,  7, 13, 15,  1,  3, 14,  5,  2,  8,  4,
                 3, 15,  0,  6, 10,  1, 13,  8,  9,  4,  5, 11, 12,  7,  2, 14},
               { 2, 12,  4,  1,  7, 10, 11,  6,  8,  5,  3, 15, 13,  0, 14,  9,
                14, 11,  2, 12,  4,  7, 13,  1,  5,  0, 15, 10,  3,  9,  8,  6,
                 4,  2,  1, 11, 10, 13,  7,  8, 15,  9, 12,  5,  6,  3,  0, 14,
                11,  8, 12,  7,  1, 14,  2, 13,  6, 15,  0,  9, 10,  4,  5,  3},
               {12,  1, 10, 15,  9,  2,  6,  8,  0, 13,  3,  4, 14,  7,  5, 11,
                10, 15,  4,  2,  7, 12,  9,  5,  6,  1, 13, 14,  0, 11,  3,  8,
                 9, 14, 15,  5,  2,  8, 12,  3,  7,  0,  4, 10,  1, 13, 11,  6,
                 4,  3,  2, 12,  9,  5, 15, 10, 11, 14,  1,  7,  6,  0,  8, 13},
               { 4, 11,  2, 14, 15,  0,  8, 13,  3, 12,  9,  7,  5, 10,  6,  1,
                13,  0, 11,  7,  4,  9,  1, 10, 14,  3,  5, 12,  2, 15,  8,  6,
                 1,  4, 11, 13, 12,  3,  7, 14, 10, 15,  6,  8,  0,  5,  9,  2,
                 6, 11, 13,  8,  1,  4, 10,  7,  9,  5,  0, 15, 14,  2,  3, 12},
               {13,  2,  8,  4,  6, 15, 11,  1, 10,  9,  3, 14,  5,  0, 12,  7,
                 1, 15, 13,  8, 10,  3,  7,  4, 12,  5,  6, 11,  0, 14,  9,  2,
                 7, 11,  4,  1,  9, 12, 14,  2,  0,  6, 10, 13, 15,  3,  5,  8,
                 2,  1, 14,  7,  4, 10,  8, 13, 15, 12,  9,  0,  3,  5,  6, 11}},
 
         P = {16,  7, 20, 21, 29, 12, 28, 17,  1, 15, 23, 26,  5, 18, 31, 10,
               2,  8, 24, 14, 32, 27,  3,  9, 19, 13, 30,  6, 22, 11,  4, 25},

      IP_1 = {40,  8, 48, 16, 56, 24, 64, 32,
              39,  7, 47, 15, 55, 23, 63, 31,
              38,  6, 46, 14, 54, 22, 62, 30,
              37,  5, 45, 13, 53, 21, 61, 29,
              36,  4, 44, 12, 52, 20, 60, 28,
              35,  3, 43, 11, 51, 19, 59, 27,
              34,  2, 42, 10, 50, 18, 58, 26,
              33,  1, 41,  9, 49, 17, 57, 25}

function map64(string s, sequence P)
    if length(s)!=8 then ?9/0 end if -- 64 bits
    string b = "", res = ""
    for i=1 to length(s) do
        b &= sprintf("%08b",s[i])
    end for
    for i=1 to length(P) do
        res &= b[P[i]]-'0'
    end for
    return res
end function

function get_subkeys(string key)
    string kp = map64(key,PC1)
    sequence ks = repeat(repeat('\0',48),16)
    for i=1 to 16 do
        integer shift = SHIFTS[i]
        kp = kp[shift+1..28]&kp[1..shift]&
             kp[shift+29..56]&kp[29..shift+28]
        for j=1 to 48 do ks[i][j] = kp[PC2[j]] end for
    end for
    return ks
end function

function f(sequence r, kn)
    string er = "", sr = "", sp = ""
    for i=1 to 48 do er &= r[E[i]] xor kn[i] end for
    -- process 'er' six bits at a time and store resulting four bits in 'sr'
    for i=1 to 8 do
        integer j = (i-1)*6+1,
                k = sum(sq_mul(er[j..j+5],{32,8,4,2,1,16}))+1
        sr &= sprintf("%04b",S[i][k])
    end for
    for i=1 to 32 do sp &= sr[P[i]]-'0' end for
    return sp
end function

function process_block(string message, sequence k)
    string mp = map64(message,IP),
        {l,r} = {mp[1..32],mp[33..64]}
    for n=1 to 16 do
        {l,r} = {r,sq_xor(l,f(r,k[n]))}
    end for
    string e = r&l, res = ""
    for i=0 to 63 by 8 do
        integer byte = 0
        for bit=1 to 8 do
            byte = byte*2+e[IP_1[i+bit]]
        end for
        res &= byte
    end for
    return res
end function

function des(string key, message, bool decode=false)
    sequence k = get_subkeys(key)
    if decode then
        k = reverse(k)
    else
        -- (match the C#/Java/Modula-2 library implementations, in
        --  case we're swapping messages with something using them)
        integer p = 8-mod(length(message),8)
        for i=1 to p do message &= p end for
    end if
    -- check message is multiple of 8 bytes (= 64 bits)
    if mod(length(message),8)!=0 then ?9/0 end if
    string res = ""
    for i=1 to length(message) by 8 do
        res &= process_block(message[i..i+7], k)
    end for
    if decode then
        -- ditto
        res = res[1..length(res)-res[$]]
    end if
    return res
end function
 
constant TESTS = {{x"133457799BBCDFF1", x"0123456789ABCDEF", "85E813540F0AB405FDF2E174492922F8"},
                  {x"0E329232EA6D0D73", x"8787878787878787", "0000000000000000A913F4CB0BD30F97"},
                  {x"0E329232EA6D0D73",
--                 x"596F7572206C6970732061726520736D6F6F74686572207468616E20766173656C696E650D0A",
                   "Your lips are smoother than vaseline\r\n",
                   "C0999FDDE378D7ED727DA00BCA5A84EE47F269A4D6438190D9D52F78F53584997F922CCB5B068D99"}}

function as_hex(string s)
    string res = ""
    for i=1 to length(s) do
        res &= sprintf("%02x",s[i])
    end for 
    return res
end function

for i=1 to length(TESTS) do
    string {key,msg,expect} = TESTS[i],
           keytxt = as_hex(key),
           msgtxt = iff(i!=3?as_hex(msg):sprint(msg)),
           encoded = des(key, msg),
           enctxt = as_hex(encoded),
           error = iff(enctxt=expect?"":"\n********* "&expect&" expected"),
           decoded = des(key, encoded, true),
           dectxt = iff(i!=3?as_hex(decoded):sprint(decoded)),
           derror = iff(decoded=msg?"":" *** error")
    printf(1,"Key     : %s\n",{keytxt})
    printf(1,"Message : %s\n",{msgtxt})
    printf(1,"Encoded : %s%s\n",{enctxt,error})
    printf(1,"Decoded : %s%s\n\n",{dectxt,derror})
end for
```

{{out}}

```txt

Key     : 133457799BBCDFF1
Message : 0123456789ABCDEF
Encoded : 85E813540F0AB405FDF2E174492922F8
Decoded : 0123456789ABCDEF

Key     : 0E329232EA6D0D73
Message : 8787878787878787
Encoded : 0000000000000000A913F4CB0BD30F97
Decoded : 8787878787878787

Key     : 0E329232EA6D0D73
Message : "Your lips are smoother than vaseline\r\n"
Encoded : C0999FDDE378D7ED727DA00BCA5A84EE47F269A4D6438190D9D52F78F53584997F922CCB5B068D99
Decoded : "Your lips are smoother than vaseline\r\n"

```



## Python


implemented like in the article linked in description. 

really good article btw

```Python
#!/usr/bin/python
#!/usr/bin/python

# Permutation tables and Sboxes
IP = (
    58, 50, 42, 34, 26, 18, 10, 2,
    60, 52, 44, 36, 28, 20, 12, 4,
    62, 54, 46, 38, 30, 22, 14, 6,
    64, 56, 48, 40, 32, 24, 16, 8,
    57, 49, 41, 33, 25, 17, 9,  1,
    59, 51, 43, 35, 27, 19, 11, 3,
    61, 53, 45, 37, 29, 21, 13, 5,
    63, 55, 47, 39, 31, 23, 15, 7
)
IP_INV = (
    40,  8, 48, 16, 56, 24, 64, 32,
    39,  7, 47, 15, 55, 23, 63, 31,
    38,  6, 46, 14, 54, 22, 62, 30,
    37,  5, 45, 13, 53, 21, 61, 29,
    36,  4, 44, 12, 52, 20, 60, 28,
    35,  3, 43, 11, 51, 19, 59, 27,
    34,  2, 42, 10, 50, 18, 58, 26,
    33,  1, 41,  9, 49, 17, 57, 25
)
PC1 = (
    57, 49, 41, 33, 25, 17, 9,
    1,  58, 50, 42, 34, 26, 18,
    10, 2,  59, 51, 43, 35, 27,
    19, 11, 3,  60, 52, 44, 36,
    63, 55, 47, 39, 31, 23, 15,
    7,  62, 54, 46, 38, 30, 22,
    14, 6,  61, 53, 45, 37, 29,
    21, 13, 5,  28, 20, 12, 4
)
PC2 = (
    14, 17, 11, 24, 1,  5,
    3,  28, 15, 6,  21, 10,
    23, 19, 12, 4,  26, 8,
    16, 7,  27, 20, 13, 2,
    41, 52, 31, 37, 47, 55,
    30, 40, 51, 45, 33, 48,
    44, 49, 39, 56, 34, 53,
    46, 42, 50, 36, 29, 32
)

E  = (
    32, 1,  2,  3,  4,  5,
    4,  5,  6,  7,  8,  9,
    8,  9,  10, 11, 12, 13,
    12, 13, 14, 15, 16, 17,
    16, 17, 18, 19, 20, 21,
    20, 21, 22, 23, 24, 25,
    24, 25, 26, 27, 28, 29,
    28, 29, 30, 31, 32, 1
)

Sboxes = {
    0: (
        14,  4, 13,  1,  2, 15, 11,  8,  3, 10,  6, 12,  5,  9,  0,  7,
        0, 15,  7,  4, 14,  2, 13,  1, 10,  6, 12, 11,  9,  5,  3,  8,
        4,  1, 14,  8, 13,  6,  2, 11, 15, 12,  9,  7,  3, 10,  5,  0,
        15, 12,  8,  2,  4,  9,  1,  7,  5, 11,  3, 14, 10,  0,  6, 13
    ),
    1: (
        15,  1,  8, 14,  6, 11,  3,  4,  9,  7,  2, 13, 12,  0,  5, 10,
        3, 13,  4,  7, 15,  2,  8, 14, 12,  0,  1, 10,  6,  9, 11,  5,
        0, 14,  7, 11, 10,  4, 13,  1,  5,  8, 12,  6,  9,  3,  2, 15,
        13,  8, 10,  1,  3, 15,  4,  2, 11,  6,  7, 12,  0,  5, 14,  9 
    ),
    2: (
        10,  0,  9, 14,  6,  3, 15,  5,  1, 13, 12,  7, 11,  4,  2,  8,
        13,  7,  0,  9,  3,  4,  6, 10,  2,  8,  5, 14, 12, 11, 15,  1,
        13,  6,  4,  9,  8, 15,  3,  0, 11,  1,  2, 12,  5, 10, 14,  7,
        1, 10, 13,  0,  6,  9,  8,  7,  4, 15, 14,  3, 11,  5,  2, 12 
    ),
    3: (
        7, 13, 14,  3,  0,  6,  9, 10,  1,  2,  8,  5, 11, 12,  4, 15,
        13,  8, 11,  5,  6, 15,  0,  3,  4,  7,  2, 12,  1, 10, 14,  9,
        10,  6,  9,  0, 12, 11,  7, 13, 15,  1,  3, 14,  5,  2,  8,  4,
        3, 15,  0,  6, 10,  1, 13,  8,  9,  4,  5, 11, 12,  7,  2, 14
    ),
    4: (
        2, 12,  4,  1,  7, 10, 11,  6,  8,  5,  3, 15, 13,  0, 14,  9,
        14, 11,  2, 12,  4,  7, 13,  1,  5,  0, 15, 10,  3,  9,  8,  6,
        4,  2,  1, 11, 10, 13,  7,  8, 15,  9, 12,  5,  6,  3,  0, 14,
        11,  8, 12,  7,  1, 14,  2, 13,  6, 15,  0,  9, 10,  4,  5,  3
    ),
    5: (
        12,  1, 10, 15,  9,  2,  6,  8,  0, 13,  3,  4, 14,  7,  5, 11,
        10, 15,  4,  2,  7, 12,  9,  5,  6,  1, 13, 14,  0, 11,  3,  8,
        9, 14, 15,  5,  2,  8, 12,  3,  7,  0,  4, 10,  1, 13, 11,  6,
        4,  3,  2, 12,  9,  5, 15, 10, 11, 14,  1,  7,  6,  0,  8, 13
    ),
    6: (
        4, 11,  2, 14, 15,  0,  8, 13,  3, 12,  9,  7,  5, 10,  6,  1,
        13,  0, 11,  7,  4,  9,  1, 10, 14,  3,  5, 12,  2, 15,  8,  6,
        1,  4, 11, 13, 12,  3,  7, 14, 10, 15,  6,  8,  0,  5,  9,  2,
        6, 11, 13,  8,  1,  4, 10,  7,  9,  5,  0, 15, 14,  2,  3, 12
    ),
    7: (
        13,  2,  8,  4,  6, 15, 11,  1, 10,  9,  3, 14,  5,  0, 12,  7,
        1, 15, 13,  8, 10,  3,  7,  4, 12,  5,  6, 11,  0, 14,  9,  2,
        7, 11,  4,  1,  9, 12, 14,  2,  0,  6, 10, 13, 15,  3,  5,  8,
        2,  1, 14,  7,  4, 10,  8, 13, 15, 12,  9,  0,  3,  5,  6, 11
    )
}

P = (
    16,  7, 20, 21,
    29, 12, 28, 17,
    1, 15, 23, 26,
    5, 18, 31, 10,
    2,  8, 24, 14,
    32, 27,  3,  9,
    19, 13, 30,  6,
    22, 11, 4,  25
)
    
def encrypt(msg, key, decrypt=False):
    # only encrypt single blocks
    assert isinstance(msg, int) and isinstance(key, int)
    assert not msg.bit_length() > 64
    assert not key.bit_length() > 64

    # permutate by table PC1
    key = permutation_by_table(key, 64, PC1) # 64bit -> PC1 -> 56bit

    # split up key in two halves
    # generate the 16 round keys
    C0 = key >> 28
    D0 = key & (2**28-1)
    round_keys = generate_round_keys(C0, D0) # 56bit -> PC2 -> 48bit

    msg_block = permutation_by_table(msg, 64, IP)
    L0 = msg_block >> 32
    R0 = msg_block & (2**32-1)

    # apply thr round function 16 times in following scheme (feistel cipher):
    L_last = L0
    R_last = R0
    for i in range(1,17):
        if decrypt: # just use the round keys in reversed order
            i = 17-i
        L_round = R_last
        R_round = L_last ^ round_function(R_last, round_keys[i])
        L_last = L_round
        R_last = R_round

    # concatenate reversed
    cipher_block = (R_round<<32) + L_round

    # final permutation
    cipher_block = permutation_by_table(cipher_block, 64, IP_INV)

    return cipher_block

def round_function(Ri, Ki):
    # expand Ri from 32 to 48 bit using table E
    Ri = permutation_by_table(Ri, 32, E)

    # xor with round key
    Ri ^= Ki

    # split Ri into 8 groups of 6 bit
    Ri_blocks = [((Ri & (0b111111 << shift_val)) >> shift_val) for shift_val in (42,36,30,24,18,12,6,0)]

    # interpret each block as address for the S-boxes
    for i, block in enumerate(Ri_blocks):
        # grab the bits we need
        row = ((0b100000 & block) >> 4) + (0b1 & block)
        col = (0b011110 & block) >> 1
        # sboxes are stored as one-dimensional tuple, so we need to calc the index this way
        Ri_blocks[i] = Sboxes[i][16*row+col]

    # pack the blocks together again by concatenating
    Ri_blocks = zip(Ri_blocks, (28,24,20,16,12,8,4,0))
    Ri = 0
    for block, lshift_val in Ri_blocks:
        Ri += (block << lshift_val)

    # another permutation 32bit -> 32bit
    Ri = permutation_by_table(Ri, 32, P)

    return Ri

def permutation_by_table(block, block_len, table):
    # quick and dirty casting to str
    block_str = bin(block)[2:].zfill(block_len)
    perm = []
    for pos in range(len(table)):
        perm.append(block_str[table[pos]-1])
    return int(''.join(perm), 2)

def generate_round_keys(C0, D0):
    # returns dict of 16 keys (one for each round)

    round_keys = dict.fromkeys(range(0,17))
    lrot_values = (1,1,2,2,2,2,2,2,1,2,2,2,2,2,2,1)

    # left-rotation function
    lrot = lambda val, r_bits, max_bits: \
    (val << r_bits%max_bits) & (2**max_bits-1) | \
    ((val & (2**max_bits-1)) >> (max_bits-(r_bits%max_bits)))

    # initial rotation
    C0 = lrot(C0, 0, 28)
    D0 = lrot(D0, 0, 28)
    round_keys[0] = (C0, D0)

    # create 16 more different key pairs
    for i, rot_val in enumerate(lrot_values):
        i+=1
        Ci = lrot(round_keys[i-1][0], rot_val, 28)
        Di = lrot(round_keys[i-1][1], rot_val, 28)
        round_keys[i] = (Ci, Di)

    # round_keys[1] for first round
    #           [16] for 16th round
    # dont need round_keys[0] anymore, remove
    del round_keys[0]

    # now form the keys from concatenated CiDi 1<=i<=16 and by apllying PC2
    for i, (Ci, Di) in round_keys.items():
        Ki = (Ci << 28) + Di
        round_keys[i] = permutation_by_table(Ki, 56, PC2) # 56bit -> 48bit

    return round_keys

k = 0x0e329232ea6d0d73 # 64 bit
k2 = 0x133457799BBCDFF1
m = 0x8787878787878787
m2 = 0x0123456789ABCDEF

def prove(key, msg):
    print('key:       {:x}'.format(key))
    print('message:   {:x}'.format(msg))
    cipher_text = encrypt(msg, key)
    print('encrypted: {:x}'.format(cipher_text))
    plain_text = encrypt(cipher_text, key, decrypt=True)
    print('decrypted: {:x}'.format(plain_text))

prove(k, m)
print('----------')
prove(k2, m2)


```

{{Out}}
Note: This is just the algorithm for single 64-bit blocks. No padding or block chipher operation mode

```txt
key:       e329232ea6d0d73
message:   8787878787878787
encrypted: 0
decrypted: 8787878787878787
----------
key:       133457799bbcdff1
message:   123456789abcdef
encrypted: 85e813540f0ab405
decrypted: 123456789abcdef
```



## REXX

Implementation of the algorithm described in the cited article.

Decryption is now supported as well

```rexx
/* REXX for the sake of some platforms such as good old iron */
Parse Upper Arg action
Select
  When action='?' Then Do
    Say "REXX des shows     how '8787878787878787'X is encoded to"
    Say "                       '000000000000000'X"
    Say "REXX des DEC shows how '000000000000000'X is decoded to"
    Say "                       '8787878787878787'X"
    Exit
    End
  When action='' | action='ENC' Then
    encode=1
  When action='' | action='DEC' Then
    encode=0
  Otherwise Do
    Say 'Invalid argument' action '(must be ENC or DEC or omitted)'
    Exit
    End
  End
o='abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890.-'
Call init
Call debug 'tr_pc_1='tr_pc_1
Call debug 'tr_ip  ='tr_ip
Call debug 'tr_p   ='tr_p
Call debug 'tr_ipa ='tr_ipa

kx='0e329232ea6d0d73'
If encode Then
  mx='8787878787878787'
Else
  mx='0000000000000000'
k=x2b(kx)
m=x2b(mx)
Say 'Message:' mx
Say 'Key    :' kx
ka=translate(tr_pc_1,k,o)
Call debug 'ka='ka
Parse Var ka c.0 +28 d.0
shifts='1 1 2 2 2 2 2 2 1 2 2 2 2 2 2 1'
Do i=1 To 16
  ip=i-1
  c.i=shift(c.ip,word(shifts,i))
  d.i=shift(d.ip,word(shifts,i))
  End
Do i=1 To 16
  cd.i=c.i||d.i
  k.i=translate(tr_pc_2,cd.i,o)
  Call debug 'k.'i'='k.i
  End

If encode=0 Then Do /*revert the subkeys */
  Do i=1 To 16
    j=17-i
    kd.i=k.j
    End
  Do i=1 To 16
    k.i=kd.i
    End
  End

IP=translate(tr_ip,m,o)
Call debug 'ip='ip

Parse Var ip l.0 +32 r.0

Call debug 'E(R.0)='E(R.0)
Call debug 'k.1   ='k.1
t=xor(k.1,E(R.0))
Call debug 't     ='t
Call debug length(t)
zz=''
Do i=1 To 8
  Parse Var t b +6 t
  zz=zz||s(i,b)
  End
Call debug 'zz='zz
f=translate(tr_p,zz,o)
Call debug 'f='f
r.1=xor(l.0,f)
Call debug 'r.1='r.1
l.1=r.0
Do j=2 To 16
  ja=j-1
  l.j=r.ja
  z=xor(k.j,e(r.ja))
  zz=''
  Do i=1 To 8
    Parse Var z b +6 z
    zz=zz||s(i,b)
    End
  Call debug j zz
  f=translate(tr_p,zz,o)
  Call debug j f
  r.j=xor(l.ja,f)
  End
Call debug 'l.16='l.16
Call debug 'r.16='r.16

zzz=r.16||l.16
c=translate(tr_ipa,zzz,o)
Call debug c
Say 'Result :' b2x(c)
Exit

f: Procedure Expose s. o tr_p
  Parse Arg r,k
  z=xor(k,e(r))
  zz=translate(tr_p,z,o)
  Return zz

init:
PC_1='57 49 41 33 25 17  9',
     ' 1 58 50 42 34 26 18',
     '10  2 59 51 43 35 27',
     '19 11  3 60 52 44 36',
     '63 55 47 39 31 23 15',
     ' 7 62 54 46 38 30 22',
     '14  6 61 53 45 37 29',
     '21 13  5 28 20 12  4'

tr_pc_1=''
Do i=1 To words(pc_1)
  tr_pc_1=tr_pc_1||substr(o,word(pc_1,i),1)
  End
tr_pc_1=tr_pc_1

kb=translate(tr_pc_1,k,o)
kc=strip(kb,,'*')

PC_2='14  17  11  24   1   5',
     ' 3  28  15   6  21  10',
     '23  19  12   4  26   8',
     '16   7  27  20  13   2',
     '41  52  31  37  47  55',
     '30  40  51  45  33  48',
     '44  49  39  56  34  53',
     '46  42  50  36  29  32'
tr_pc_2=''
Do i=1 To words(pc_2)
  tr_pc_2=tr_pc_2||substr(o,word(pc_2,i),1)
  End

Do i=1 To 16
  cd.i=c.i||d.i
  k.i=translate(ok,cd.i,o)
  Call debug 'k.'i'='k.i
  End

IP='58 50 42 34 26 18 10  2',
   '60 52 44 36 28 20 12  4',
   '62 54 46 38 30 22 14  6',
   '64 56 48 40 32 24 16  8',
   '57 49 41 33 25 17  9  1',
   '59 51 43 35 27 19 11  3',
   '61 53 45 37 29 21 13  5',
   '63 55 47 39 31 23 15  7'
tr_ip=''
Do i=1 To words(IP)
  tr_ip=tr_ip||substr(o,word(ip,i),1)
  End


P='16  7 20 21',
  '29 12 28 17',
  ' 1 15 23 26',
  ' 5 18 31 10',
  ' 2  8 24 14',
  '32 27  3  9',
  '19 13 30  6',
  '22 11  4 25'
tr_p=''
Do i=1 To words(p)
  tr_p=tr_p||substr(o,word(p,i),1)
  End

SM.1='14  4  13  1   2 15  11  8   3 10   6 12   5  9   0  7',
     ' 0 15   7  4  14  2  13  1  10  6  12 11   9  5   3  8',
     ' 4  1  14  8  13  6   2 11  15 12   9  7   3 10   5  0',
     '15 12   8  2   4  9   1  7   5 11   3 14  10  0   6 13'
SM.2='15  1   8 14   6 11   3  4   9  7   2 13  12  0   5 10',
     ' 3 13   4  7  15  2   8 14  12  0   1 10   6  9  11  5',
     ' 0 14   7 11  10  4  13  1   5  8  12  6   9  3   2 15',
     '13  8  10  1   3 15   4  2  11  6   7 12   0  5  14  9'
SM.3='10  0   9 14   6  3  15  5   1 13  12  7  11  4   2  8',
     '13  7   0  9   3  4   6 10   2  8   5 14  12 11  15  1',
     '13  6   4  9   8 15   3  0  11  1   2 12   5 10  14  7',
     ' 1 10  13  0   6  9   8  7   4 15  14  3  11  5   2 12'
SM.4=' 7 13  14  3   0  6   9 10   1  2   8  5  11 12   4 15',
     '13  8  11  5   6 15   0  3   4  7   2 12   1 10  14  9',
     '10  6   9  0  12 11   7 13  15  1   3 14   5  2   8  4',
     ' 3 15   0  6  10  1  13  8   9  4   5 11  12  7   2 14'
SM.5=' 2 12   4  1   7 10  11  6   8  5   3 15  13  0  14  9',
     '14 11   2 12   4  7  13  1   5  0  15 10   3  9   8  6',
     ' 4  2   1 11  10 13   7  8  15  9  12  5   6  3   0 14',
     '11  8  12  7   1 14   2 13   6 15   0  9  10  4   5  3'
SM.6='12  1  10 15   9  2   6  8   0 13   3  4  14  7   5 11',
     '10 15   4  2   7 12   9  5   6  1  13 14   0 11   3  8',
     ' 9 14  15  5   2  8  12  3   7  0   4 10   1 13  11  6',
     ' 4  3   2 12   9  5  15 10  11 14   1  7   6  0   8 13'
SM.7=' 4 11   2 14  15  0   8 13   3 12   9  7   5 10   6  1',
     '13  0  11  7   4  9   1 10  14  3   5 12   2 15   8  6',
     ' 1  4  11 13  12  3   7 14  10 15   6  8   0  5   9  2',
     ' 6 11  13  8   1  4  10  7   9  5   0 15  14  2   3 12'
SM.8='13  2   8  4   6 15  11  1  10  9   3 14   5  0  12  7',
     ' 1 15  13  8  10  3   7  4  12  5   6 11   0 14   9  2',
     ' 7 11   4  1   9 12  14  2   0  6  10 13  15  3   5  8',
     ' 2  1  14  7   4 10   8 13  15 12   9  0   3  5   6 11'
Do i=1 To 8
  Do r=0 To 3
    Do c=0 To 15
      Parse Var sm.i s.i.r.c sm.i
      End
    End
  End

ipa='40  8 48 16 56 24 64 32',
    '39  7 47 15 55 23 63 31',
    '38  6 46 14 54 22 62 30',
    '37  5 45 13 53 21 61 29',
    '36  4 44 12 52 20 60 28',
    '35  3 43 11 51 19 59 27',
    '34  2 42 10 50 18 58 26',
    '33  1 41  9 49 17 57 25'
tr_ipa=''
Do i=1 To words(ipa)
  tr_ipa=tr_ipa||substr(o,word(ipa,i),1)
  End

Return

shift: Procedure
  Parse Arg in,s
  out=substr(in,s+1)left(in,s)
  Return out

E: Procedure
Parse Arg s
esel='32  1  2  3  4  5',
     ' 4  5  6  7  8  9',
     ' 8  9 10 11 12 13',
     '12 13 14 15 16 17',
     '16 17 18 19 20 21',
     '20 21 22 23 24 25',
     '24 25 26 27 28 29',
     '28 29 30 31 32  1'
r=''
Do i=1 To words(esel)
  r=r||substr(s,word(esel,i),1)
  End
Return r

xor: Procedure
Parse Arg u,v
r=''
Do i=1 To length(u)
  cc=substr(u,i,1)substr(v,i,1)
  r=r||(pos(cc,'01 10')>0)
  End
Return r

s: Procedure Expose s.
  Parse Arg i,b
  Parse Var b r1 +1 c +4 r2
  r=r1||r2
  rb=num(r)
  cb=num(c)
  result=s.i.rb.cb
  Return num2bits(result)

num: Procedure
  Parse Arg s
  res=0
  Do i=1 To length(s)
    Parse Var s c +1 s
    res=2*res+c
    End
  Return res

num2bits: Procedure
  Parse Arg n
  nx=d2x(n)
  r=''
  Do i=1 To 4
    dig=n//2
    r=dig||r
    n=n%2
    End
  Return r

debug: /* Say arg(1) */ Return
```

{{out}}

```txt
I:\>rexx des2
Message: 8787878787878787
Key    : 0e329232ea6d0d73
Result : 0000000000000000

I:\>rexx des2 dec
Message: 0000000000000000
Key    : 0e329232ea6d0d73
Result : 8787878787878787
```



## Scala


```Scala
import javax.crypto.Cipher
import javax.crypto.spec.SecretKeySpec

object DataEncryptionStandard extends App {

  private def toHexByteArray(self: String) = {
    val bytes = new Array[Byte](self.length / 2)
    for (i <- bytes.indices)
      bytes(i) = Integer.parseInt(self.substring(i * 2, i * 2 + 2), 16).toByte

    bytes
  }

  private def printHexBytes(self: Array[Byte], label: String): Unit = {
    printf("%s: ", label)
    for (b <- self) {
      val bb = if (b >= 0) b.toInt else b + 256
      var ts = Integer.toString(bb, 16)
      if (ts.length < 2) ts = "0" + ts
      print(ts)
    }
    println()
  }

  val strKey = "0e329232ea6d0d73"
  val keyBytes = toHexByteArray(strKey)
  val key = new SecretKeySpec(keyBytes, "DES")
  val encCipher = Cipher.getInstance("DES")
  encCipher.init(Cipher.ENCRYPT_MODE, key)
  val strPlain = "8787878787878787"
  val plainBytes = toHexByteArray(strPlain)
  val encBytes = encCipher.doFinal(plainBytes)
  printHexBytes(encBytes, "Encoded")
  val decCipher = Cipher.getInstance("DES")
  decCipher.init(Cipher.DECRYPT_MODE, key)
  val decBytes = decCipher.doFinal(encBytes)
  printHexBytes(decBytes, "Decoded")

}
```

{{Out}}See it running in your browser by [https://scastie.scala-lang.org/t6nGq1ebShKEA42LSIQ6Hg Scastie (JVM)].

## Symsyn


```Symsyn
pc1 : 56
 : 48
 : 40
 : 32
 : 24
 : 16
 : 8
 : 0
 : 57
 : 49
 : 41
 : 33
 : 25
 : 17
 : 9
 : 1
 : 58
 : 50
 : 42
 : 34
 : 26
 : 18
 : 10
 : 2
 : 59
 : 51
 : 43
 : 35
 : 62
 : 54
 : 46
 : 38
 : 30
 : 22
 : 14
 : 6
 : 61
 : 53
 : 45
 : 37
 : 29
 : 21
 : 13
 : 5
 : 60
 : 52
 : 44
 : 36
 : 28
 : 20
 : 12
 : 4
 : 27
 : 19
 : 11
 : 3
Pc2 : 13
 : 16
 : 10
 : 23
 : 0
 : 4
 : 2
 : 27
 : 14
 : 5
 : 20
 : 9
 : 22
 : 18
 : 11
 : 3
 : 25
 : 7
 : 15
 : 6
 : 26
 : 19
 : 12
 : 1
 : 40
 : 51
 : 30
 : 36
 : 46
 : 54
 : 29
 : 39
 : 50
 : 44
 : 32
 : 47
 : 43
 : 48
 : 38
 : 55
 : 33
 : 52
 : 45
 : 41
 : 49
 : 35
 : 28
 : 31
P : 15
 : 6
 : 19
 : 20
 : 28
 : 11
 : 27
 : 16
 : 0
 : 14
 : 22
 : 25
 : 4
 : 17
 : 30
 : 9
 : 1
 : 7
 : 23
 : 13
 : 31
 : 26
 : 2
 : 8
 : 18
 : 12
 : 29
 : 5
 : 21
 : 10
 : 3
 : 24
Ebit : 31
 : 0
 : 1
 : 2
 : 3
 : 4
 : 3
 : 4
 : 5
 : 6
 : 7
 : 8
 : 7
 : 8
 : 9
 : 10
 : 11
 : 12
 : 11
 : 12
 : 13
 : 14
 : 15
 : 16
 : 15
 : 16
 : 17
 : 18
 : 19
 : 20
 : 19
 : 20
 : 21
 : 22
 : 23
 : 24
 : 23
 : 24
 : 25
 : 26
 : 27
 : 28
 : 27
 : 28
 : 29
 : 30
 : 31
 : 0
DesIP : 57
 : 49
 : 41
 : 33
 : 25
 : 17
 : 9
 : 1
 : 59
 : 51
 : 43
 : 35
 : 27
 : 19
 : 11
 : 3
 : 61
 : 53
 : 45
 : 37
 : 29
 : 21
 : 13
 : 5
 : 63
 : 55
 : 47
 : 39
 : 31
 : 23
 : 15
 : 7
 : 56
 : 48
 : 40
 : 32
 : 24
 : 16
 : 8
 : 0
 : 58
 : 50
 : 42
 : 34
 : 26
 : 18
 : 10
 : 2
 : 60
 : 52
 : 44
 : 36
 : 28
 : 20
 : 12
 : 4
 : 62
 : 54
 : 46
 : 38
 : 30
 : 22
 : 14
 : 6
DesIPIV : 39
 : 7
 : 47
 : 15
 : 55
 : 23
 : 63
 : 31
 : 38
 : 6
 : 46
 : 14
 : 54
 : 22
 : 62
 : 30
 : 37
 : 5
 : 45
 : 13
 : 53
 : 21
 : 61
 : 29
 : 36
 : 4
 : 44
 : 12
 : 52
 : 20
 : 60
 : 28
 : 35
 : 3
 : 43
 : 11
 : 51
 : 19
 : 59
 : 27
 : 34
 : 2
 : 42
 : 10
 : 50
 : 18
 : 58
 : 26
 : 33
 : 1
 : 41
 : 9
 : 49
 : 17
 : 57
 : 25
 : 32
 : 0
 : 40
 : 8
 : 48
 : 16
 : 56
 : 24
DesS1 :  14
 :  0
 :  4
 :  15
 :  13
 :  7
 :  1
 :  4
 :  2
 :  14
 :  15
 :  2
 :  11
 :  13
 :  8
 :  1
 :  3
 :  10
 :  10
 :  6
 :  6
 :  12
 :  12
 :  11
 :  5
 :  9
 :  9
 :  5
 :  0
 :  3
 :  7
 :  8
 :  4
 :  15
 :  1
 :  12
 :  14
 :  8
 :  8
 :  2
 :  13
 :  4
 :  6
 :  9
 :  2
 :  1
 :  11
 :  7
 :  15
 :  5
 :  12
 :  11
 :  9
 :  3
 :  7
 :  14
 :  3
 :  10
 :  10
 :  0
 :  5
 :  6
 :  0
 :  13
S2 :  15
 :  3
 :  1
 :  13
 :  8
 :  4
 :  14
 :  7
 :  6
 :  15
 :  11
 :  2
 :  3
 :  8
 :  4
 :  14
 :  9
 :  12
 :  7
 :  0
 :  2
 :  1
 :  13
 :  10
 :  12
 :  6
 :  0
 :  9
 :  5
 :  11
 :  10
 :  5
 :  0
 :  13
 :  14
 :  8
 :  7
 :  10
 :  11
 :  1
 :  10
 :  3
 :  4
 :  15
 :  13
 :  4
 :  1
 :  2
 :  5
 :  11
 :  8
 :  6
 :  12
 :  7
 :  6
 :  12
 :  9
 :  0
 :  3
 :  5
 :  2
 :  14
 :  15
 :  9
S3 :  10
 :  13
 :  0
 :  7
 :  9
 :  0
 :  14
 :  9
 :  6
 :  3
 :  3
 :  4
 :  15
 :  6
 :  5
 :  10
 :  1
 :  2
 :  13
 :  8
 :  12
 :  5
 :  7
 :  14
 :  11
 :  12
 :  4
 :  11
 :  2
 :  15
 :  8
 :  1
 :  13
 :  1
 :  6
 :  10
 :  4
 :  13
 :  9
 :  0
 :  8
 :  6
 :  15
 :  9
 :  3
 :  8
 :  0
 :  7
 :  11
 :  4
 :  1
 :  15
 :  2
 :  14
 :  12
 :  3
 :  5
 :  11
 :  10
 :  5
 :  14
 :  2
 :  7
 :  12
S4 :  7
 :  13
 :  13
 :  8
 :  14
 :  11
 :  3
 :  5
 :  0
 :  6
 :  6
 :  15
 :  9
 :  0
 :  10
 :  3
 :  1
 :  4
 :  2
 :  7
 :  8
 :  2
 :  5
 :  12
 :  11
 :  1
 :  12
 :  10
 :  4
 :  14
 :  15
 :  9
 :  10
 :  3
 :  6
 :  15
 :  9
 :  0
 :  0
 :  6
 :  12
 :  10
 :  11
 :  1
 :  7
 :  13
 :  13
 :  8
 :  15
 :  9
 :  1
 :  4
 :  3
 :  5
 :  14
 :  11
 :  5
 :  12
 :  2
 :  7
 :  8
 :  2
 :  4
 :  14
S5 :  2
 :  14
 :  12
 :  11
 :  4
 :  2
 :  1
 :  12
 :  7
 :  4
 :  10
 :  7
 :  11
 :  13
 :  6
 :  1
 :  8
 :  5
 :  5
 :  0
 :  3
 :  15
 :  15
 :  10
 :  13
 :  3
 :  0
 :  9
 :  14
 :  8
 :  9
 :  6
 :  4
 :  11
 :  2
 :  8
 :  1
 :  12
 :  11
 :  7
 :  10
 :  1
 :  13
 :  14
 :  7
 :  2
 :  8
 :  13
 :  15
 :  6
 :  9
 :  15
 :  12
 :  0
 :  5
 :  9
 :  6
 :  10
 :  3
 :  4
 :  0
 :  5
 :  14
 :  3
S6 :  12
 :  10
 :  1
 :  15
 :  10
 :  4
 :  15
 :  2
 :  9
 :  7
 :  2
 :  12
 :  6
 :  9
 :  8
 :  5
 :  0
 :  6
 :  13
 :  1
 :  3
 :  13
 :  4
 :  14
 :  14
 :  0
 :  7
 :  11
 :  5
 :  3
 :  11
 :  8
 :  9
 :  4
 :  14
 :  3
 :  15
 :  2
 :  5
 :  12
 :  2
 :  9
 :  8
 :  5
 :  12
 :  15
 :  3
 :  10
 :  7
 :  11
 :  0
 :  14
 :  4
 :  1
 :  10
 :  7
 :  1
 :  6
 :  13
 :  0
 :  11
 :  8
 :  6
 :  13
S7 :  4
 :  13
 :  11
 :  0
 :  2
 :  11
 :  14
 :  7
 :  15
 :  4
 :  0
 :  9
 :  8
 :  1
 :  13
 :  10
 :  3
 :  14
 :  12
 :  3
 :  9
 :  5
 :  7
 :  12
 :  5
 :  2
 :  10
 :  15
 :  6
 :  8
 :  1
 :  6
 :  1
 :  6
 :  4
 :  11
 :  11
 :  13
 :  13
 :  8
 :  12
 :  1
 :  3
 :  4
 :  7
 :  10
 :  14
 :  7
 :  10
 :  9
 :  15
 :  5
 :  6
 :  0
 :  8
 :  15
 :  0
 :  14
 :  5
 :  2
 :  9
 :  3
 :  2
 :  12
S8 :  13
 :  1
 :  2
 :  15
 :  8
 :  13
 :  4
 :  8
 :  6
 :  10
 :  15
 :  3
 :  11
 :  7
 :  1
 :  4
 :  10
 :  12
 :  9
 :  5
 :  3
 :  6
 :  14
 :  11
 :  5
 :  0
 :  0
 :  14
 :  12
 :  9
 :  7
 :  2
 :  7
 :  2
 :  11
 :  1
 :  4
 :  14
 :  1
 :  7
 :  9
 :  4
 :  12
 :  10
 :  14
 :  8
 :  2
 :  13
 :  0
 :  15
 :  6
 :  12
 :  10
 :  9
 :  13
 :  0
 :  15
 :  3
 :  3
 :  5
 :  5
 :  6
 :  8
 :  11
DesShifts :  1
 :  1
 :  2
 :  2
 :  2
 :  2
 :  2
 :  2
 :  1
 :  2
 :  2
 :  2
 :  2
 :  2
 :  2
 :  1
DesHex : 0
 :   0
 :   0
 :   0
 :   0
 :   0
 :   0
 :   1
 :   0
 :   0
 :   1
 :   0
 :   0
 :   0
 :   1
 :   1
 :   0
 :   1
 :   0
 :   0
 :   0
 :   1
 :   0
 :   1
 :   0
 :   1
 :   1
 :   0
 :   0
 :   1
 :   1
 :   1
 :   1
 :   0
 :   0
 :   0
 :   1
 :   0
 :   0
 :   1
 :   1
 :   0
 :   1
 :   0
 :   1
 :   0
 :   1
 :   1
 :   1
 :   1
 :   0
 :   0
 :   1
 :   1
 :   0
 :   1
 :   1
 :   1
 :   1
 :   0
 :   1
 :   1
 :   1
 :   1

DesC     : 28 0 
DesD     : 28 0 
DesL     : 32 0 
DesR     : 32 0 
DesL1    : 32 0 
DesR1    : 32 0 
DesEK    : 48 0 
DesK     : 768 0 
DesWds   : 64 0 

DesI     : 0
DesJ     : 0
DesJJ    : 0xf000 
DesIter  : 0
DesSNum  : 0
OldDesKeyW : -1 
DesKeyW  : 0
DesDataW : 0
DesKey   = DesKeyW  
DesData  = DesDataW 
K : 0
Kc = K

kprime : x'0e329232ea6d0d73'

dprime : x'8787878787878787'


| Program Starts Here


 kprime deskey         | Load encryption key 
 dprime desdata        | Load data to be encrypted 
 call dodeskey         | Perform key setup
 call encryptdes       | Encrypt data
 desdata $s            | Move encrypted data to string
 unpackhex $s          | Unpack to display
 $s []                 | Display
 stop


| End of Program


Data2Wds
        63 DesJJ
        7 DesI
        if DesI GE 0
           DesData.DesI D
           DesJ
           if DesJ LE 7
              and 1 D D1
              D1 DesWds.DesJJ
              shr D 1
              - DesJJ
              + DesJ
              goif
           endif
           - DesI
           goif
        endif
        return
Wds2Data                                                                        
        DesJJ              
        DesI               
        if DesI LE 7       
            DesJ           
            if DesJ LE 7               
               shl D 1        
               if DesWds.DesJJ NE 0           
                   + D        
               endif
               + DesJJ
               + DesJ
               goif
            endif              
            D DesData.DesI       
            + DesI
            goif
        endif
        return

Key2Wds
   63 DesJJ                     
   7 DesI                       
   if DesI GE 0                 
      DesKey.DesI K             
      DesJ                      
      if DesJ LE 7              
         and K 1 K1
         K1 DesWds.DesJJ        
	 shr K 1                
         - DesJJ                
         + DesJ                 
         goif                   
      endif                     
      - DesI                    
      goif                      
   endif                        
   return                       

func
    DesI
    if DesI LE 47
       Ebit.DesI rx
       DesR.rx DesEK.DesI
       + DesI
       goif
    endif
    * 48 DesIter DesJ
    DesI
    if DesI LE 47
       + DesI DesJ IJ
       xor DesEK.DesI DesK.IJ DesEK.DesI
       + DesI
       goif
    endif
    DesI
    DesSNum
    if DesSNum LE 7
       DesEK.DesI ss
       shl ss 1
       + DesI
       + DesEK.DesI ss
       shl ss 1
       + DesI
       + DesEK.DesI ss
       shl ss 1
       + DesI
       + DesEK.DesI ss
       shl ss 1
       + DesI
       + DesEK.DesI ss
       shl ss 1
       + DesI
       + DesEK.DesI ss
       + DesI
       DesSNum DesS1x
       Shl DesS1x 6
       + ss DesS1x
       DesS1.DesS1x DesHexx
       shl DesHexx 2
       DesSNum DesWdsx
       shl DesWdsx 2
       DesHex.DesHexx DesWds.DesWdsx 4
       + DesSNum
       goif
    endif
    DesI
    if DesI LE 31
       P.DesI DesJ
       DesWds.DesJ DesR.DesI
       + DesI
       goif
    endif
    return

DoDesKey
    if DesKeyW EQ OldDesKeyW
       if DesJJ NE 0xf000
          return
       endif
    endif
    DesKeyW OldDesKeyW
    call Key2Wds
    DesI
    if DesI LE 55
       Pc1.DesI Pcx
       DesWds.Pcx DesC.DesI
       + DesI
       goif
    endif
    DesJJ
    DesI
    if DesI LE 15
       DesC.0 DesWd
       DesC.1 DesC 27
       DesWd DesC.27
       DesD.0 DesWd
       DesD.1 DesD 27
       DesWd DesD.27
       if DesShifts.DesI EQ 2
           DesC.0 DesWd
           DesC.1 DesC 27
           DesWd DesC.27
           DesD.0 DesWd
           DesD.1 DesD 27
           DesWd DesD.27
       endif
       DesJ
       if DesJ LE 47
          Pc2.DesJ DesCx
          DesC.DesCx DesK.DesJJ
          + DesJJ
          + DesJ
          goif
       endif
       + DesI
       goif
    endif
    return

EncryptDes
     Call Data2Wds
     DesI
     if DesI LE 63
        DesIP.DesI DesWdsx
        DesWds.DesWdsx DesL.DesI
        + DesI
        goif
     endif
     DesIter
     if DesIter LE 15
        DesR DesL1 32
        call func
        DesJ
        if DesJ LE 31
           xor DesL.DesJ DesR.DesJ
           + DesJ
           goif
        endif
        DesL1 DesL 32
        + DesIter
        goif
     endif
     DesL DesR1 32
     DesR DesL1 32
     DesI
     if DesI LE 63
        DesIPIV.DesI DesL1x
        DesL1.DesL1x DesWds.DesI
        + DesI
        goif
     endif

     call Wds2Data
     return

DecryptDes
     Call Data2Wds
     DesI
     if DesI LE 63
        DesIP.DesI DesWdsx
        DesWds.DesWdsx DesL.DesI
        + DesI
        goif
     endif
     15 DesIter
     if DesIter GE 0
        DesR DesL1 32
        call func
        DesJ
        if DesJ LE 31
           xor DesL.DesJ DesR.DesJ
           + DesJ
           goif
        endif
        DesL1 DesL 32
        - DesIter
        goif
     endif
     DesL DesR1 32
     DesR DesL1 32
     DesI
     if DesI LE 63
        DesIPIV.DesI DesL1x
        DesL1.DesL1x DesWds.DesI
        + DesI
        goif
     endif
     call Wds2Data
     return
```

A trivial solution using the des encryption instruction:
<lang>key :  x'0e329232ea6d0d73'
data : x'8787878787878787'

 edes key data            | encrypt data with key 
 data $s                  | move data to string
 unpackhex $s $s          | unpack
 $s []                    | output result - 0000000000000000


```

{{out}}

```txt
0000000000000000
```



## Visual Basic .NET

{{trans|C#}}

```vbnet
Imports System.IO
Imports System.Security.Cryptography

Module Module1

    'Taken from https://stackoverflow.com/a/311179
    Function ByteArrayToString(ba As Byte()) As String
        Return BitConverter.ToString(ba).Replace("-", "")
    End Function

    'Modified from https://stackoverflow.com/q/4100996
    'The passwordBytes parameter must be 8 bytes long
    Function Encrypt(messageBytes As Byte(), passwordBytes As Byte()) As Byte()
        Dim iv As Byte() = {&H0, &H0, &H0, &H0, &H0, &H0, &H0, &H0}

        'Set encryption settings -- Use password for both key and init. vector
        Dim provider As New DESCryptoServiceProvider
        Dim transform = provider.CreateEncryptor(passwordBytes, iv)
        Dim mode = CryptoStreamMode.Write

        'Set up streams and encrypt
        Dim memStream As New MemoryStream
        Dim cryptoStream As New CryptoStream(memStream, transform, mode)
        cryptoStream.Write(messageBytes, 0, messageBytes.Length)
        cryptoStream.FlushFinalBlock()

        'Read the encrypted message from the memory stream
        Dim encryptedMessageBytes(memStream.Length - 1) As Byte
        memStream.Position = 0
        memStream.Read(encryptedMessageBytes, 0, encryptedMessageBytes.Length)

        Return encryptedMessageBytes
    End Function

    'Modified from https://stackoverflow.com/q/4100996
    'The passwordBytes parameter must be 8 bytes long
    Function Decrypt(encryptedMessageBytes As Byte(), passwordBytes As Byte()) As Byte()
        Dim iv As Byte() = {&H0, &H0, &H0, &H0, &H0, &H0, &H0, &H0}

        'Set encryption settings -- Use password for both key and init. vector
        Dim provider As New DESCryptoServiceProvider
        Dim transform = provider.CreateDecryptor(passwordBytes, iv)
        Dim mode = CryptoStreamMode.Write

        'Set up streams and decrypt
        Dim memStream As New MemoryStream
        Dim cryptoStream As New CryptoStream(memStream, transform, mode)
        cryptoStream.Write(encryptedMessageBytes, 0, encryptedMessageBytes.Length)
        cryptoStream.FlushFinalBlock()

        'Read decrypted message from memory stream
        Dim decryptedMessageBytes(memStream.Length - 1) As Byte
        memStream.Position = 0
        memStream.Read(decryptedMessageBytes, 0, decryptedMessageBytes.Length)

        Return decryptedMessageBytes
    End Function

    Sub Main()
        Dim keyBytes As Byte() = {&HE, &H32, &H92, &H32, &HEA, &H6D, &HD, &H73}
        Dim plainBytes As Byte() = {&H87, &H87, &H87, &H87, &H87, &H87, &H87, &H87}

        Dim encStr = Encrypt(plainBytes, keyBytes)
        Console.WriteLine("Encoded: {0}", ByteArrayToString(encStr))

        Dim decStr = Decrypt(encStr, keyBytes)
        Console.WriteLine("Decoded: {0}", ByteArrayToString(decStr))
    End Sub

End Module
```

{{out}}

```txt
Encoded: 0000000000000000A913F4CB0BD30F97
Decoded: 8787878787878787
```

