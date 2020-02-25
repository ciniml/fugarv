#include <stdint.h>

static volatile uint32_t* UART_RX_FIFO  = (volatile uint32_t*)0x00008000U;
static volatile uint32_t* UART_TX_FIFO  = (volatile uint32_t*)0x00008004U;
static volatile uint32_t* UART_STAT_REG = (volatile uint32_t*)0x00008008U;
static volatile uint32_t* UART_CTRL_REG = (volatile uint32_t*)0x0000800cU;


static void put_string(const char* s)
{
    for(;*s;s++) {
        while( (*UART_STAT_REG & 8) );
        *UART_TX_FIFO = *s;
    }
}
int main(void)
{
    *UART_CTRL_REG = 3;

    put_string("Hello, from FugaRV!\n");
    for(volatile uint32_t i = 0;; i++);
    return 0;
}