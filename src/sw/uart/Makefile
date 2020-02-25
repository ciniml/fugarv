.PHONY: all clean

TARGET_NAME := uart
TARGET := $(TARGET_NAME).bin $(TARGET_NAME).lst
ARCH_PREFIX ?= riscv32-unknown-elf-
CC = $(ARCH_PREFIX)gcc
LD = $(ARCH_PREFIX)ld
OBJCOPY = $(ARCH_PREFIX)objcopy
OBJDUMP = $(ARCH_PREFIX)objdump

CFLAGS = -march=rv32i -mabi=ilp32 -g -Os
LDFLAGS = -march=rv32i -mabi=ilp32 -g -Os -nostartfiles -nostdlib -lc -lgcc

all: $(TARGET)

$(TARGET_NAME).elf: main.o crt.o
	$(CC) $(LDFLAGS) -T link.ld -o $@ $^

$(TARGET_NAME).bin: $(TARGET_NAME).elf
	$(OBJCOPY) -O binary $< $@

$(TARGET_NAME).lst: $(TARGET_NAME).elf
	$(OBJDUMP) -dSC $< > $@

%.o: %.S
	$(CC) $(CFLAGS) -c -o $@ $<

%.o: %.c
	$(CC) $(CFLAGS) -c -o $@ $<

clean:
	-@$(RM) *.o *.elf *.hex *.bin *.lst