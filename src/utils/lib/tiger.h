/* Copyright 2001, 2002 b8_bavard, b8_fee_carabine, INRIA */
/*
    This file is part of mldonkey.

    mldonkey is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    mldonkey is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with mldonkey; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
*/

#ifndef _TIGER_H
#define _TIGER_H

typedef unsigned long long int word64;
typedef unsigned long word32;
typedef unsigned char byte;


#define DIGEST_LEN 24
#define BLOCK_SIZE 1024
#define TREE_DEPTH 10


void tiger_hash(char prefix, const char *s, OFF_T len, unsigned char *digest);
OFF_T tiger_block_size(OFF_T len);
#endif
