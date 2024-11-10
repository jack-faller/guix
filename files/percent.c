#include <stdio.h>
#include <string.h>

#define BETWEEN(CHR, A, B) (A <= (CHR) && (CHR) <= B)
int hex(char c) {
  if (BETWEEN(c, '0', '9'))
    return c - '0';
  else if (BETWEEN(c, 'A', 'F'))
    return c - 'A' + 10;
  else if (BETWEEN(c, 'a', 'f'))
    return c - 'a' + 10;
  else
    return -1;
}

int main(int argc, char **argv) {
  if (argc == 3) {
    if (0 == strcmp(argv[1], "encode")) {
      for (char *c = argv[2]; *c != '\0'; ++c) {
        if (BETWEEN(*c, '0', '9')
            || BETWEEN(*c, 'A', 'Z')
            || BETWEEN(*c, 'a', 'z')
            || *c == '-'
            || *c == '.'
            || *c == '_'
            || *c == '~')
          printf("%c", (int)*c);
        else
          printf("%%%.2hhX", *c);
      }
    } else if (0 == strcmp(argv[1], "decode")) {
      for (char *c = argv[2]; *c != '\0'; ++c) {
        int a, b;
        if (*c != '%')
          printf("%c", (int)*c);
        else if (c[1] == '\0' || c[2]=='\0') {
          fprintf(stderr, "Not enough characters in hex sequence %.2s.\n", c);
          return 1;
        } else if (a = hex(c[1]), b = hex(c[2]), a == -1 || b == -1) {
          fprintf(stderr, "Invalid hex sequence %.3s.\n", c);
          return 1;
        } else {
          printf("%c", a * 16 + b);
          c += 2;
        }
      }
    } else {
      fprintf(stderr, "Usage: percent [encode/decode] <string>.\n");
      return 1;
    }
  }
  printf("\n");
  return 0;
}
