#include <stdint.h>

#ifndef _FIRMWARE_H_
#define _FIRMWARE_H_

#define LED  ((volatile uint32_t *) 0x0)
#define UART ((volatile uint32_t *) 0x4)
#define SPI  ((volatile uint32_t *) 0x8)

void    write_uart(uint8_t);
uint8_t read_uart();

void print_chr(char);
void print_str(const char *);
void print_dec(unsigned int);
void print_hex(unsigned int, int);

char* read_line(char*, int);

#endif /* _FIRMWARE_H_ */
