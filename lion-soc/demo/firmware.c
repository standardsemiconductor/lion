#include <stdint.h>
#include <stddef.h>
#include "firmware.h"

void write_uart(uint8_t data) {
  while (((uint8_t *) UART)[2] & 0x1); // wait until transmitter empty
  *((uint8_t *) UART) = data;          // transmit byte
}

uint8_t read_uart() {
  while (((uint8_t *) UART)[2] & 0x2); // wait until receiver full
  return ((uint8_t *) UART)[1];
}

void print_char(char ch) {
  write_uart((uint8_t) ch);
}

void print_str(const char *p) {
  while (*p != 0)
    print_char(*p++);
}

void print_dec(unsigned int val) {
  char buffer[10];
  char *p = buffer;
  while (val || p == buffer) {
    *(p++) = val % 10;
    val = val / 10;
  }
  while (p != buffer)
    print_char('0' + *(--p));
}

void print_hex(unsigned int val, int digits) {
  for (int i = (4*digits)-4; i >= 0; i -= 4)
    print_char("0123456789ABCDEF"[(val >> i) % 16]);
}

char* read_line(char* str, int n) {
  uint8_t c;
  int i = 0;

  if (n <= 0)
    return str;

  while (i < n - 1 && ((c = read_uart()) != 0)) { // 0 ascii null
    str[i++] = c;
    if (c == '\n' || c == '\r')
      break;
  }
  str[i] = '\0'; // add NUL character at end

  if (i > 0) {
    return str;
  } else {
    return NULL; // no character at EOF
  }
}
