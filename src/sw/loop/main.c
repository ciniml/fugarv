#include <stdint.h>

int main(void)
{
    for(volatile uint32_t i = 0;; i++);
    return 0;
}