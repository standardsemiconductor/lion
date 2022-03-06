#include <stdint.h>
#include "../firmware.h"

#define CR0  0x8
#define BR   0x9
#define ONR  0xA
#define OFR  0xB
#define BCRR 0x5
#define BCFR 0x6
#define PWRR 0x1
#define PWRG 0x2
#define PWRB 0x3

void write_led(uint8_t, uint8_t);
void init_led();

int main() {
  uint8_t uart_input;
  init_led();
  for(;;) {
    uart_input = read_uart();
    if (uart_input == 0x72) { // 'r'
      write_led(PWRR, 0xFF);
      write_led(PWRG, 0x00);
      write_led(PWRB, 0x00);
    } else if (uart_input == 0x67) { // 'g'
      write_led(PWRR, 0x00);
      write_led(PWRG, 0xFF);
      write_led(PWRB, 0x00);
    } else if (uart_input == 0x62) { // 'b'
      write_led(PWRR, 0x00);
      write_led(PWRG, 0x00);
      write_led(PWRB, 0xFF);
    }
    write_uart(uart_input);
  }
  return 0;
}

void write_led(uint8_t addr, uint8_t val) {
  *LED = ((uint32_t) addr << 8) | (uint32_t) val;
}

void init_led() {
  write_led(CR0,  0x80);
  write_led(BR,   0x12);
  write_led(ONR,  0x80);
  write_led(OFR,  0x80);
  write_led(BCRR, 0xCE);
  write_led(BCFR, 0xCE);
  // set LED off
  write_led(PWRR, 0x00);
  write_led(PWRG, 0x00);
  write_led(PWRB, 0x00);
}
