#include <cstdio>
#include <cstring>
#include <unistd.h>
#include <fstream>
#include <iostream>
#include <vector>
#include <fcntl.h>
#include <sys/mman.h>

static constexpr std::uint32_t instmemOffset = 0x40000000u;

int main(int argc, char* argv[])
{
    if( argc < 2 ) {
        std::cerr << "Usage: " << argv[0] << " <bin file>" << std::endl;
        return 1;
    }
    std::ifstream binFile(argv[1]);
    if(!binFile) {
        std::cerr << "Error: failed to open file - " << argv[1] << std::endl;
        return 1;
    }

    binFile.seekg(0, std::ios_base::end);
    auto binSize = binFile.tellg();
    binFile.seekg(0);

    std::vector<std::uint8_t> buffer(binSize);
    binFile.read(reinterpret_cast<char*>(buffer.data()), buffer.size());

    auto fd = open("/dev/mem", O_SYNC|O_RDWR);
    if( fd < 0 ) {
        std::cerr << "Error: failed to open /dev/mem" << std::endl;
        return 1;
    }

    void* instmem = mmap(0, 32*1024, PROT_READ|PROT_WRITE, MAP_SHARED, fd, instmemOffset);
    if( instmem == MAP_FAILED ) {
        perror("Error: failed to mmap");
        return 1;
    }
    std::cerr << "Transferring " << buffer.size() << " bytes..." << std::endl;
    std::memcpy(instmem, buffer.data(), buffer.size());

    munmap(instmem, 32*1024);
    close(fd);
    return 0;
}