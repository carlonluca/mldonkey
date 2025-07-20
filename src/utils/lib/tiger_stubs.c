#include "md4.h"
#include "md5.h"
#include "sha1_c.h"
#include "hashes_stubs.h"

ML_HASH(sha1,SHA1_CTX,sha1_begin,sha1_hash, sha1_end)
ML_HASH(md5,md5_state_t,md5_init,md5_append,md5_finish)
ML_HASH(md4,MD4_CTX,MD4Init,MD4Update,md4_finish)

/*******************************************************************


                     tiger


*******************************************************************/
#include "tiger.h"

static void tiger_tree_fd(OS_FD fd, OFF_T len, OFF_T pos, OFF_T block_size, char *digest)
{
  static char tiger_buffer[BLOCK_SIZE+1];
  if(block_size == BLOCK_SIZE){
    OFF_T length = (len - pos > BLOCK_SIZE) ? BLOCK_SIZE : len - pos;
    char *s = tiger_buffer+1;
    size_t toread = length;
    char *curs = s;
      while (toread!=0){
      int max_nread = toread;
/* HASH_BUFFER_LEN > toread ? toread : HASH_BUFFER_LEN; */

      ssize_t nread = os_read (fd, curs, max_nread);

        if(nread <= 0) {
        unix_error(errno, "tiger_safe_fd: Read", Nothing);
      }
      curs += nread;
      toread -= nread;
    }

    tiger_hash(0, s, length, digest);
  } else {    
    if(pos+block_size/2 >=len){
      tiger_tree_fd(fd, len, pos, block_size/2, digest);
    } else {
      char digests_prefixed[1+DIGEST_LEN * 2];
      char *digests = digests_prefixed+1;
      tiger_tree_fd(fd, len, pos, block_size/2, digests);
      tiger_tree_fd(fd, len, pos+block_size/2, block_size/2, digests+DIGEST_LEN);
      tiger_hash(1,digests, 2*DIGEST_LEN, digest);
    }
  }
}

value tigertree_unsafe64_fd (value digest_v, value fd_v, value pos_v, value len_v)
{
  OS_FD fd = Fd_val(fd_v);
  OFF_T pos = Int64_val(pos_v);
  OFF_T len = Int64_val(len_v);
  unsigned char *digest = Bytes_val(digest_v);
/*  int nread; */

  os_lseek(fd, pos, SEEK_SET);

  tiger_tree_fd(fd, len, 0, tiger_block_size(len), digest);

  return Val_unit;
}