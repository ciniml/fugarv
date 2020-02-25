.PHONY: all clean

TARGET := loadbin
ARCH_PREFIX ?= arm-linux-gnueabihf-

CFLAGS = -g -Os
CXXFLAGS = $(CFLAGS) -std=gnu++11
LDFLAGS = -g -Os -static
CC = $(ARCH_PREFIX)gcc
CXX = $(ARCH_PREFIX)g++
LD = $(ARCH_PREFIX)gcc

all: $(TARGET)

loadbin: main.o
	$(CXX) $(LDFLAGS) -o $@ $^

%.o: %.S
	$(CC) $(CFLAGS) -c -o $@ $<

%.o: %.c
	$(CC) $(CFLAGS) -c -o $@ $<

%.o: %.cpp
	$(CXX) $(CXXFLAGS) -c -o $@ $<

clean:
	-@$(RM) *.o *.elf *.hex