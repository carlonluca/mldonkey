/*******************************************************************


                     hashes


*******************************************************************/

#include <os_stubs.h>

#define ML_HASH(HASH_NAME,HASH_CONTEXT,HASH_INIT,HASH_APPEND,HASH_FINISH) \
value HASH_NAME##_unsafe64_fd (value digest_v, value fd_v, value pos_v, value len_v) \
{ \
  OS_FD fd = Fd_val(fd_v); \
  OFF_T pos = Int64_val(pos_v); \
  OFF_T len = Int64_val(len_v); \
  unsigned char *digest = Bytes_val(digest_v); \
  HASH_CONTEXT context; \
  ssize_t nread; \
 \
  HASH_INIT (&context); \
  os_lseek(fd, pos, SEEK_SET); \
 \
  while (len!=0){ \
    size_t max_nread = HASH_BUFFER_LEN > len ? len : HASH_BUFFER_LEN; \
 \
    nread = os_read (fd, hash_buffer, max_nread); \
 \
    if(nread < 0) { \
      unix_error(errno, "md4_safe_fd: Read", Nothing); \
    } \
 \
    if(nread == 0){ \
      HASH_FINISH (&context, digest); \
 \
      return Val_unit; \
    } \
 \
    HASH_APPEND (&context, hash_buffer, nread); \
    len -= nread; \
  } \
  HASH_FINISH (&context, digest); \
 \
  return Val_unit; \
} \
\
value HASH_NAME##_unsafe_string(value digest_v, value string_v, value len_v) \
{ \
  unsigned char *digest = Bytes_val(digest_v); \
  const unsigned char *string = String_val(string_v); \
  long len = Long_val(len_v); \
  HASH_CONTEXT context; \
 \
  HASH_INIT (&context); \
  HASH_APPEND (&context, string, len); \
  HASH_FINISH (&context, digest); \
  \
  return Val_unit; \
} \
 \
value HASH_NAME##_unsafe_file (value digest_v, value filename_v, value file_size) \
{ \
  const char *filename  = String_val(filename_v); \
  unsigned char *digest = Bytes_val(digest_v); \
  FILE *file; \
  HASH_CONTEXT context; \
  size_t len; \
 \
  if ((file = fopen (filename, "rb")) == NULL) \
    raise_not_found(); \
 \
  else { \
    HASH_INIT (&context); \
    while ((len = fread (hash_buffer, 1, HASH_BUFFER_LEN, file)) >0) \
      HASH_APPEND (&context, hash_buffer, len); \
    HASH_FINISH (&context, digest); \
 \
    fclose (file); \
  } \
  return Val_unit; \
}