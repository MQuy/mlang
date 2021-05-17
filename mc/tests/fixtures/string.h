typedef unsigned int size_t;

char *strcpy(char *dest, const char *src);
char *strncpy(char *dest, const char *src, size_t count);
int strcmp(const char *str1, const char *str2);
size_t strlen(const char *str);
char *strcat(char *dest, const char *src);
int strncmp(const char *str1, const char *str2, size_t max_count);
char *strchr(const char *str, int val);
char *strrchr(const char *str, int ch);
void *memset(void *dst, int val, size_t size);
void *memcpy(void *dst, const void *src, size_t size);
int memcmp(const void *buf1, const void *buf2, size_t size);
